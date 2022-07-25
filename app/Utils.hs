{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Utils where

import Control.Applicative ((<|>))
import Control.Concurrent
import Control.Monad (forever, void)
import Control.Monad.IO.Class
import qualified Data.ByteString.Lazy.Char8 as LBS.Char8
import Data.Char (isDigit)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text.IO as T
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
import Lens.Micro
import Network.HTTP.Types (ResponseHeaders)
import Network.IRC.Client hiding (password)
import qualified Text.HTML.TagSoup as S
import Text.Pretty.Simple
import Text.Read (readMaybe)
import Types
import Web.Pixiv
import Web.Pixiv.Types.Lens

setupTokenRefresh :: IRCBot ()
setupTokenRefresh = forkIRC . forever $ do
  token <- runPixivInIRC "refresh token" getAccessToken
  liftIO $ putStrLn "refresh token result:"
  pPrint token
  liftIO $ threadDelay 1800000000

forkIRC :: IRC s () -> IRC s ()
forkIRC x = void $ fork x

identifyNick :: IRCBot (Message Text)
identifyNick = getBotConfig >>= \BotConfig {password} -> pure $ Privmsg "NickServ" $ Right $ "identify " <> password

myIRCLogger origin x = do
  now <- getCurrentTime
  T.putStrLn $
    T.unwords
      [ T.pack $ formatTime defaultTimeLocale "%c" now,
        if origin == FromServer then "--->" else "<---",
        decodeUtf8 x
      ]

joinChannels l = mapM_ (send . Join) l

data PUrl = PIllust Int | PUser Int deriving (Show)

extractPUrl :: Text -> Maybe PUrl
extractPUrl url = illust <|> user
  where
    extract f prefix url = (T.stripPrefix prefix url >>= (readMaybe . takeWhile isDigit . T.unpack)) <&> f
    illust = extract PIllust "/artworks/" url
    user = extract PUser "/users/" url

imageUrlToCF :: Text -> Text
imageUrlToCF = T.replace "i.pximg.net" "setu.libido.workers.dev"

-- | Like 'extractHighestQualityImageUrl' in pixiv, but exclude the original picture
extractLargeImageUrl :: ImageUrls -> Maybe Text
extractLargeImageUrl x = x ^. large <|> x ^. medium <|> x ^. squareMedium

isFc :: Text -> Bool
isFc = T.isInfixOf "fars.ee"

illegalChars :: [Char]
illegalChars = ['/', '（', '）', '：', ' ', '-', '(', ')', ':', '+', '!', '！', '、', '。', '·', '－', '\'', '.', '☆', '「', '」', '・', '?', '？', '○', '＠', '&']

replaceChars :: Text -> Text
replaceChars = T.map (\it -> if it `elem` illegalChars then '_' else it)

anySub :: (Foldable t, Functor t) => t Text -> Text -> Bool
anySub list target = or $ fmap (`T.isInfixOf` target) list

parseRankMode :: Text -> Maybe RankMode
parseRankMode = \case
  "day" -> Just Day
  "week" -> Just Week
  "month" -> Just Month
  "day-r18" -> Just DayR18
  "week-r18" -> Just WeekR18
  "week-r18g" -> Just WeekR18G
  "week-original" -> Just WeekOriginal
  _ -> Nothing

imageUrlsToTelegraph :: [Text] -> Text
imageUrlsToTelegraph urls = "[" <> T.intercalate "," (single <$> urls) <> "]"
  where
    single url = "{\"tag\": \"img\", \"attrs\": {\"src\":\"" <> url <> "\"}}"

extractWebpageTitle ::
  -- | content
  LBS.Char8.ByteString ->
  Maybe Text
extractWebpageTitle content =
  let parsed = S.parseTags content
      f = dropWhile (not . S.isTagOpenName "title") parsed
   in case f of
        ((S.TagOpen "title" _) : S.TagText title : _) -> Just . decodeUtf8 $ LBS.Char8.toStrict title
        _ -> Nothing

extractContentSizeAndType :: ResponseHeaders -> Maybe (Text, Float)
extractContentSizeAndType headers =
  let t = decodeUtf8 <$> lookup "Content-Type" headers
      s = (/ 1024) . read . T.unpack . decodeUtf8 <$> lookup "Content-Length" headers
   in (,) <$> t <*> s
