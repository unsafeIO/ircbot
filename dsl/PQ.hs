{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module PQ where

import Control.Monad (ap)
import Data.Dynamic (Typeable)
import Data.Function (on)
import Data.List (sortBy)
import Data.Maybe (fromMaybe, maybeToList)
import Data.Text (Text)
import qualified Data.Text as T
import Lens.Micro
import Lens.Micro.Extras (view)
import Web.Pixiv
import Web.Pixiv.Types.Lens

type Done = ()

-- | Pixiv query
--
-- A simple eDSL to be evaluated on the flight
data PQ a where
  PIllustRanking :: RankMode -> PQ [Illust]
  PRelated :: Int -> PQ [Illust]
  PSearch :: Text -> PQ [Illust]
  PIllust :: Int -> PQ Illust
  PUserBookmarks :: Int -> PQ [Illust]
  PUserIllusts :: Int -> PQ [Illust]
  PLastId :: PQ Int
  PAsIllust :: IllustLike a => PQ a -> PQ Done
  PAsString :: Show a => PQ a -> PQ Done
  PBind :: PQ a -> (a -> PQ b) -> PQ b
  PLit :: a -> PQ a
  PShuffle :: [a] -> PQ [a]
  PBrowse :: PQ Done
  deriving (Typeable)

instance Functor PQ where
  fmap f fa = PBind fa (PLit . f)

instance Applicative PQ where
  pure = PLit
  (<*>) = ap

instance Monad PQ where
  (>>=) = PBind

type IllustsFilter = [Illust] -> [Illust]

class IllustLike a where
  toIllusts :: a -> [Illust]

instance IllustLike Illust where
  toIllusts = pure

instance IllustLike [Illust] where
  toIllusts = id

-----------------------------------------------------------------------------
-- core functions
-----------------------------------------------------------------------------
sendPic :: IllustLike a => PQ a -> PQ Done
sendPic = PAsIllust

sendStr :: Show a => PQ a -> PQ Done
sendStr = PAsString

-----------------------------------------------------------------------------

lastId :: PQ Int
lastId = PLastId

pixiv :: Int -> PQ Illust
pixiv = PIllust

ranking :: RankMode -> PQ [Illust]
ranking = PIllustRanking

related :: Int -> PQ [Illust]
related = PRelated

related' :: PQ [Illust]
related' = PLastId `PBind` PRelated

search :: Text -> PQ [Illust]
search = PSearch

work :: Int -> PQ [Illust]
work = PUserIllusts

bookmark :: Int -> PQ [Illust]
bookmark = PUserBookmarks

echo :: Show a => a -> PQ Done
echo = PAsString . PLit

shuffle :: [a] -> PQ [a]
shuffle = PShuffle

browse :: PQ Done
browse = PBrowse

-----------------------------------------------------------------------------
-- helper functions
-----------------------------------------------------------------------------

hasTag :: Text -> IllustsFilter
hasTag tag = hasTags [tag]

hasTags :: [Text] -> IllustsFilter
hasTags tags = filter $ \x -> all (`elem` allTags x) tags

hasNoTag :: Text -> IllustsFilter
hasNoTag tag = hasNoTags [tag]

hasNoTags :: [Text] -> IllustsFilter
hasNoTags tags = filter $ \x -> not $ any (`elem` allTags x) tags

allTags :: Illust -> [Text]
allTags illust = concat $ (\x -> [x ^. name] <> maybeToList (x ^. translatedName)) <$> (illust ^. tags)

tagString :: Illust -> Text
tagString = T.concat . allTags

sortIllust :: Ord a => SimpleGetter Illust a -> IllustsFilter
sortIllust f = sortBy (compare `on` view f)

-----------------------------------------------------------------------------
-- help message
-----------------------------------------------------------------------------

-- | example -- send five day-ranking illustrations
example :: PQ Done
example = ranking DayR18 <&> take 5 & sendPic

-- | get this by running: > help
help :: String
help =
  concat
    [ "Example: ranking DayR18 <$> take 5 & sendPic",
      "Available functions and types: https://github.com/unsafeIO/ircbot/blob/master/dsl/PQ.hs | ",
      "https://github.com/The-closed-eye-of-love/pixiv | ",
      "https://github.com/monadfix/microlens"
    ]
