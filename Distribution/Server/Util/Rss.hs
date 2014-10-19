module Distribution.Server.Util.Rss (mkChannelElems) where

import Data.Time.Clock (UTCTime)
import qualified Text.RSS as RSS

mkChannelElems :: UTCTime -> [RSS.ChannelElem]
mkChannelElems now =
  [ RSS.Language "en"
  , RSS.ManagingEditor email
  , RSS.WebMaster email
  , RSS.ChannelPubDate now
  , RSS.LastBuildDate   now
  , RSS.Generator "rss-feed"
  ]
  where
    email = "duncan@haskell.org (Duncan Coutts)"
