{-# LANGUAGE NamedFieldPuns #-}
module Distribution.Server.Features.PackageFeed (
    PackageFeedFeature(..),
    PackageFeedResource(..),
    initPackageFeedFeature,
  ) where

import Distribution.Server.Features.Core (CoreFeature(..), CoreResource(..),
    packageInPath, corePackagePage)
import Distribution.Server.Features.Users (UserFeature, queryGetUserDb)
import Distribution.Server.Framework

import Distribution.Package (PackageName, PackageIdentifier(..))
import Distribution.Server.Packages.Types (PkgInfo(..))
import Distribution.Server.Users.Users (Users, userIdToName)
import Distribution.Server.Util.Rss (mkChannelElems)

import Data.Time.Clock (UTCTime, getCurrentTime)
import Distribution.Text (display)
import Network.URI (URI(..))
import qualified Text.RSS as RSS
import Text.RSS (RSS(RSS))

-- | The PackageFeed feature provides an RSS feed of the last 10 uploads
-- for every package.
data PackageFeedFeature = PackageFeedFeature
    { packageFeedFeatureInterface :: HackageFeature
    , packageFeedResource :: PackageFeedResource
    }

instance IsHackageFeature PackageFeedFeature where
    getFeatureInterface = packageFeedFeatureInterface

data PackageFeedResource = PackageFeedResource
    {
    packageFeed :: Resource
    }

initPackageFeedFeature :: ServerEnv 
                       -> UserFeature
                       -> CoreFeature
                       -> IO PackageFeedFeature
initPackageFeedFeature env@ServerEnv{serverVerbosity} users core = do
    loginfo serverVerbosity "Initialising package feed feature, start"
    let feature = packageFeedFeature env users core
    loginfo serverVerbosity "Initialising package feed feature, end"
    return feature

packageFeedFeature :: ServerEnv
                   -> UserFeature
                   -> CoreFeature
                   -> PackageFeedFeature
packageFeedFeature env userFeature CoreFeature{coreResource} =
    PackageFeedFeature
        { packageFeedFeatureInterface =
            (emptyHackageFeature "packageFeed") {
                featureResources = [packageFeedResource]
              , featureState     = []
            }
        , packageFeedResource = PackageFeedResource packageFeedResource
        }
  where
    packageFeedResource =
        (extendResourcePath "/feed.:format" $ corePackagePage coreResource)
            { resourceDesc =
                [(GET, "Get a feed of recent package uploads")]
            , resourceGet = [("rss", servePackageFeed)]
            }

    servePackageFeed :: DynamicPath -> ServerPartE Response
    servePackageFeed dpath = do
        packageName <- packageInPath coreResource dpath 
        packages <- getRecentUploads packageName
        toResponse <$> packagesToRss packageName packages

    getRecentUploads :: PackageName -> ServerPartE [PkgInfo]
    getRecentUploads packageName = do
        pkgs <- lookupPackageName coreResource packageName
        return $ take 10 (reverse pkgs)

    packagesToRss :: PackageName -> [PkgInfo] -> ServerPartE RSS
    packagesToRss packageName packages = do
        users <- queryGetUserDb userFeature
        now   <- liftIO $ getCurrentTime
        return $
            mkRssFeed
                (display packageName)
                (serverBaseURI env)
                now
                packages
                users

pkgToRssItem :: Users -> URI -> PkgInfo -> RSS.Item
pkgToRssItem users baseUri pkg =
    [ RSS.Title $ display pkgId
    , RSS.Link uri
    , RSS.PubDate uploadTime
    , RSS.Description $ "Uploaded by: " ++ display uploader
    ]
  where
    (uploadTime, userId) = pkgUploadData pkg
    pkgId = pkgInfoId pkg
    uploader = userIdToName users userId
    uri = packageURL baseUri pkgId

packageURL :: URI -> PackageIdentifier -> URI
packageURL baseURI pkgId = baseURI { uriPath = "/package" </> display pkgId }

packageFeedURL :: URI -> String -> URI
packageFeedURL baseURI packageName =
    baseURI { uriPath = "/package" </> packageName </> "feed.rss"}

mkRssFeed :: String -> URI -> UTCTime -> [PkgInfo] -> Users -> RSS
mkRssFeed packageName baseURI now pkgInfos users = RSS
    packageName
    (packageFeedURL baseURI packageName)
    ("The last 10 uploaded versions of the " ++ packageName ++ " package")
    (mkChannelElems now)
    (map (pkgToRssItem users baseURI) pkgInfos)
