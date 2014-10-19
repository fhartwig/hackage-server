-- Takes a reversed log file on the standard input and outputs web page.

module Distribution.Server.Pages.Recent (
    recentPage,
    recentFeed,
  ) where

import Distribution.Server.Packages.Types
import qualified Distribution.Server.Users.Users as Users
import Distribution.Server.Users.Users (Users)
import Distribution.Server.Pages.Template
         ( hackagePageWithHead )
import Distribution.Server.Util.Rss (mkChannelElems)

import Distribution.Package
         ( PackageIdentifier, packageName, packageVersion, PackageName(..) )
import Distribution.PackageDescription
         ( GenericPackageDescription(packageDescription)
         , PackageDescription(synopsis)  )
import Distribution.Text
         ( display )

import qualified Text.XHtml.Strict as XHtml
import Text.XHtml
         ( Html, URL, (<<), (!) )
import qualified Text.RSS as RSS
import Text.RSS
         ( RSS(RSS) )
import Network.URI
         ( URI(..), uriToString )
import Data.Time.Clock
         ( UTCTime )
import Data.Time.Format
         ( formatTime )
import System.Locale
         ( defaultTimeLocale )


-- | Takes a list of package info, in reverse order by timestamp.
--
recentPage :: Users -> [PkgInfo] -> Html
recentPage users pkgs =
  let log_rows = map (makeRow users) (take 20 pkgs)
      docBody = [XHtml.h2 << "Recent additions",
          XHtml.table ! [XHtml.align "center"] << log_rows]
      rss_link = XHtml.thelink ! [XHtml.rel "alternate",
                                  XHtml.thetype "application/rss+xml",
                                  XHtml.title "Hackage RSS Feed",
                                  XHtml.href rssFeedURL] << XHtml.noHtml
   in hackagePageWithHead [rss_link] "recent additions" docBody

makeRow :: Users -> PkgInfo -> Html
makeRow users PkgInfo {
      pkgInfoId = pkgid
    , pkgUploadData = (time, userId)
  } =
  XHtml.tr <<
    [XHtml.td ! [XHtml.align "right"] <<
            [XHtml.toHtml (showTime time), nbsp, nbsp],
     XHtml.td ! [XHtml.align "left"] << display user,
     XHtml.td ! [XHtml.align "left"] <<
            [nbsp, nbsp, XHtml.anchor !
                           [XHtml.href (packageURL pkgid)] << display pkgid]]
  where nbsp = XHtml.primHtmlChar "nbsp"
        user = Users.userIdToName users userId

showTime :: UTCTime -> String
showTime = formatTime defaultTimeLocale "%c"

-- | URL describing a package.
packageURL :: PackageIdentifier -> URL
packageURL pkgid = "/package/" ++ display pkgid

rssFeedURL :: URL
rssFeedURL = "/recent.rss"

recentAdditionsURL :: URL
recentAdditionsURL = "/recent.html"

recentFeed :: Users -> URI -> UTCTime -> [PkgInfo] -> RSS
recentFeed users hostURI now pkgs = RSS
  "Recent additions"
  (hostURI { uriPath = recentAdditionsURL})
  desc
  (mkChannelElems now)
  [ releaseItem users hostURI pkg | pkg <- take 20 pkgs ]
  where
    desc = "The 20 most recent additions to Hackage, the Haskell package database."

releaseItem :: Users -> URI -> PkgInfo -> [RSS.ItemElem]
releaseItem users hostURI pkgInfo@(PkgInfo {
      pkgInfoId = pkgId
    , pkgUploadData = (time, userId)
  }) =
  [ RSS.Title title
  , RSS.Link uri
  , RSS.Guid True (uriToString id uri "")
  , RSS.PubDate time
  , RSS.Description desc
  ]
  where
    uri   = hostURI { uriPath = packageURL pkgId }
    title = unPackageName (packageName pkgId) ++ " " ++ display (packageVersion pkgId)
    body  = synopsis (packageDescription (pkgDesc pkgInfo))
    desc  = "<i>Added by " ++ display user ++ ", " ++ showTime time ++ ".</i>"
         ++ if null body then "" else "<p>" ++ body
    user = Users.userIdToName users userId

unPackageName :: PackageName -> String
unPackageName (PackageName name) = name
