{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Utils where

import Control.Applicative ((<|>))
import Control.Concurrent
import qualified Control.Exception as CE
import Control.Monad (forever, msum, void, when)
import Control.Monad.IO.Class
import Data.Aeson.QQ.Simple
import Data.Char (isDigit)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text.IO as T
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
import qualified Network.HTTP.Client as C
import Network.IRC.Client
import Text.Pretty.Simple
import Text.Read (readMaybe)
import Types
import Web.Pixiv
import Web.Pixiv.Auth (Token)

setupTokenRefersh :: IRCBot ()
setupTokenRefersh = forkIRC . forever $ do
  token <- runPixivInIRC "refersh token" getAccessToken
  liftIO $ putStrLn "refersh token result:"
  pPrint token
  liftIO $ threadDelay 1800000000

forkIRC :: IRC s () -> IRC s ()
forkIRC x = void $ fork x

identifyNick :: Message Text
identifyNick = Privmsg "NickServ" $ Right "identify 12345"

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
    extract f prefix url = T.stripPrefix prefix url >>= (readMaybe . takeWhile isDigit . T.unpack) >>= pure . f
    illust = extract PIllust "https://www.pixiv.net/artworks/" url
    user = extract PUser "https://www.pixiv.net/users/" url

imageUrlToCF :: Text -> Text
imageUrlToCF = T.replace "i.pximg.net" "setu.libido.workers.dev"

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
