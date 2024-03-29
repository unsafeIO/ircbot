{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module API where

import Control.Monad.Catch (try)
import Control.Monad.IO.Class
import Data.Aeson (decode, decodeStrict, encode, object, withObject, (.:), (.=))
import Data.Aeson.Types (parseMaybe)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Lazy (toStrict)
import qualified Data.ByteString.Lazy.Char8 as LBS.Char8
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Lens.Micro
import Network.HTTP.Client
import Network.HTTP.Client.MultipartFormData
import Network.HTTP.Types (Header)
import Servant.Client (ClientError)
import System.Exit
import System.IO.Temp (withSystemTempFile)
import System.Process (readProcessWithExitCode)
import qualified Text.HTML.TagSoup as S
import qualified Text.HTML.TagSoup.Match as S
import Types
import Utils
import qualified Web.Pixiv.API as P
import Web.Pixiv.Types
import qualified Web.Pixiv.Types.Lens as L

getIllustDetail :: Int -> IRCBot (Either ClientError Illust)
getIllustDetail illustId = runPixivInIRC "getIllustDetail" $ P.getIllustDetail illustId

getIllustRelated :: Int -> IRCBot (Either ClientError Illust)
getIllustRelated illustId = runPixivInIRC "getIllustRelated" $ randomP (P.getIllustRelated illustId 1)

searchIllust :: Text -> IRCBot (Either ClientError (Maybe Illust))
searchIllust word = runPixivInIRC "searchIllust" $ do
  illusts <- P.searchIllust PartialMatchForTags word (Just True) Nothing Nothing 1
  if not (null illusts)
    then Just <$> randomP (pure illusts)
    else pure Nothing

getIllustRanking :: Maybe RankMode -> IRCBot (Either ClientError [Illust])
getIllustRanking mode = runPixivInIRC "getIllustRanking" $ P.getIllustRanking mode 1

getTrendingTags :: IRCBot (Either ClientError [TrendingTag])
getTrendingTags = runPixivInIRC "getTrendingTags" $ P.getTrendingTags Nothing

getUserBookmarks :: Text -> IRCBot (Either ClientError Illust)
getUserBookmarks username = runPixivInIRC "fetchUserBookmark" $
  randomP $ do
    u <- headP $ P.searchUser username 1
    p <- (\u -> (^. _1) <$> P.getUserBookmarks (u ^. L.user . L.userId) Public Nothing) `traverse` u
    pure $ concat p

getUserBookmarks' :: Int -> IRCBot (Either ClientError Illust)
getUserBookmarks' userId = runPixivInIRC "getUserBookmarks'" $ randomP $ (^. _1) <$> P.getUserBookmarks userId Public Nothing

getUserIllusts :: Text -> IRCBot (Either ClientError Illust)
getUserIllusts username = runPixivInIRC "getUserIllusts" $
  randomP $ do
    u <- headP $ P.searchUser username 1
    p <- (\u -> P.getUserIllusts (u ^. L.user . L.userId) (Just TypeIllust) 1) `traverse` u
    pure $ concat p

getUserDetailAndAnIllust :: Int -> IRCBot (Either ClientError (UserDetail, Maybe Illust))
getUserDetailAndAnIllust userId = runPixivInIRC "getUserDetailAndAnIllust" $ do
  user <- P.getUserDetail userId
  illusts <- P.getUserIllusts userId (Just TypeIllust) 1
  pure (user, illusts ^? _head)

getIllustComments :: Int -> IRCBot (Either ClientError Comments)
getIllustComments illustId = runPixivInIRC "getIllustComments" $ P.getIllustComments illustId 1

shortenUrl :: Text -> IRCBot (Either HttpException Text)
shortenUrl url = try $ do
  fc <- parseRequest "https://fars.ee/u"
  req <- formDataBody [partBS "c" $ encodeUtf8 url] $ setQueryString [("u", Just "1")] fc
  manager <- getManager
  result <- liftIO $ httpLbs req manager
  pure $ decodeUtf8 . toStrict . responseBody $ result

uploadPB :: FilePath -> ByteString -> IRCBot (Either HttpException Text)
uploadPB filename bs = try $ do
  fc <- parseRequest "https://fars.ee"
  req <- formDataBody [partFileRequestBody "c" filename $ RequestBodyBS bs] $ setQueryString [("u", Just "1")] fc
  manager <- getManager
  result <- liftIO $ httpLbs req manager
  pure $ decodeUtf8 . toStrict . responseBody $ result

ua :: [Header]
ua = [("user-agent", "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/85.0.4183.102 Safari/537.36")]

withPerformRequest :: Text -> (Either HttpException (Response LBS.Char8.ByteString) -> IRCBot a) -> IRCBot a
withPerformRequest url f =
  try
    ( do
        req <- parseRequest $ T.unpack url
        manager <- getManager
        liftIO $ httpLbs req {requestHeaders = ua} manager
    )
    >>= f

google :: Text -> IRCBot (Either HttpException (Maybe Text))
google q = try $ do
  BotConfig {..} <- getBotConfig
  req <- parseRequest "https://www.googleapis.com/customsearch/v1"
  let params = [("key", Just $ encodeUtf8 googleKey), ("cx", Just $ encodeUtf8 googleCX), ("q", Just $ encodeUtf8 q)]
  manager <- getManager
  result <- liftIO $ httpLbs (setQueryString params req) manager
  let json = decode $ responseBody result
  pure $
    json >>= \x -> case items x of
      (GoogleItem {..} : _) -> Just $ T.init $ T.unlines [title, snippet, link]
      _ -> Nothing

hl :: IRCBot (Either HttpException (Text, Text, Text))
hl = try $ do
  req <- parseRequest "http://m.laohuangli.net"
  manager <- getManager
  result <- liftIO $ httpLbs req {requestHeaders = ua} manager
  let parsed = S.parseTags $ toStrict $ responseBody result
      yi = takeWhile (not . S.tagCloseLit "td") $ dropWhile (not . S.tagOpenLit "td" (S.anyAttrLit ("class", "suit_cont"))) parsed
      ji = takeWhile (not . S.tagCloseLit "td") $ dropWhile (not . S.tagOpenLit "td" (S.anyAttrLit ("class", "taboo_cont"))) parsed
      dt = takeWhile (not . S.tagCloseLit "div") $ dropWhile (not . S.tagOpenLit "div" (S.anyAttrLit ("class", "date_select"))) parsed
      lu = takeWhile (not . S.tagCloseLit "p") $ dropWhile (not . S.tagOpenLit "div" (S.anyAttrLit ("class", "date_info_box"))) parsed
      go acc (S.TagOpen "span" [("class", "t6left")] : S.TagText txt : S.TagClose "span" : xs) = go (txt : acc) xs
      go acc (S.TagOpen "span" [] : _ : S.TagOpen "a" _ : S.TagText txt : S.TagClose "a" : _ : S.TagClose "span" : xs) = go (txt : acc) xs
      go acc (_ : [S.TagClose "span"]) = acc
      go acc (_ : xs) = go acc xs
      go acc _ = acc
      resultYi = decodeUtf8 $ BS.intercalate ", " $ go [] yi
      resultJi = decodeUtf8 $ BS.intercalate ", " $ go [] ji
      date = case dt of
        S.TagOpen "div" _ : S.TagOpen "span" [] : S.TagText txt : _ -> decodeUtf8 txt
        _ -> ""
      lunard = case dropWhile (not . S.tagOpenLit "div" (S.anyAttrLit ("class", "item_tit"))) lu of
        S.TagOpen "div" [("class", "item_tit")] : S.TagText s1 : S.TagClose "div" : _ : S.TagOpen "p" _ : S.TagText s2 : _ -> decodeUtf8 s1 <> ", " <> decodeUtf8 s2
        _ -> ""
      info = date <> ", " <> lunard
  pure (info, resultYi, resultJi)

-- | Upload each large image of an illust. Illust must be multi page
uploadToTelegraph :: Illust -> IRCBot (Either HttpException (Maybe Text))
uploadToTelegraph i = try $ do
  BotConfig {..} <- getBotConfig
  let urls = i ^. L.metaPages ^.. each . L.imageUrls . L.large . _Just
      title = "pixiv-" <> (i ^. L.illustId & show)
      content = imageUrlsToTelegraph $ imageUrlToCF <$> urls
      obj =
        object
          [ "access_token" .= telegraphToken,
            "title" .= title,
            "content" .= content,
            "author_name" .= nick
          ]
  initReq <- parseRequest "https://api.telegra.ph/createPage"
  let req = initReq {method = "POST", requestBody = RequestBodyLBS $ encode obj, requestHeaders = [("Content-Type", "application/json")]}
  manager <- getManager
  result <- liftIO $ httpLbs req manager
  let parsePageUrl = withObject "Page" $ \o -> do
        r <- o .: "result"
        r .: "url"
      body = responseBody result
  liftIO $ LBS.Char8.putStrLn body
  pure $ parseMaybe parsePageUrl =<< decode body

canonicalPixivUrl :: Text -> IRCBot (Maybe PUrl)
canonicalPixivUrl url
  | "pixiv" `T.isInfixOf` url =
    do
      req <- parseRequest $ T.unpack url
      manager <- getManager
      -- Actually we don't need look into the body
      -- result <- liftIO $ httpLbs req {requestHeaders = ua} manager
      -- let parsed = S.parseTags $ responseBody result
      -- f = find (S.tagOpenLit "link" $ S.anyAttrLit ("rel", "canonical")) parsed
      -- case f of
      -- Just (S.TagOpen _ attrs) -> pure (extractPUrl . decodeUtf8 . toStrict . snd =<< find (\(k, _v) -> k == "href") attrs)
      -- _ -> pure Nothing
      updatedReq <- liftIO $ getOriginalRequest <$> httpNoBody req {requestHeaders = ua} manager
      pure . extractPUrl . decodeUtf8 . path $ updatedReq
canonicalPixivUrl _ = pure Nothing

getImageTypeAndResolution :: ByteString -> IO (Maybe (Text, Text))
getImageTypeAndResolution bs = withSystemTempFile "pb-img" $ \fp h -> do
  BS.hPut h bs
  (code, T.pack -> stdout, _) <- readProcessWithExitCode "magick" ["identify", fp] ""
  pure $ case code of
    ExitSuccess
      | [_, ty, res, _, _, _, _, _, _] <- T.split (== ' ') stdout -> pure (ty, res)
    _ -> Nothing

getVideoResolutionAndDuration :: ByteString -> IO (Maybe FFProbeResult)
getVideoResolutionAndDuration bs = withSystemTempFile "pb-video" $ \fp h -> do
  BS.hPut h bs
  (code, decodeStrict . encodeUtf8 . T.pack -> stdout, _) <-
    readProcessWithExitCode
      "ffprobe"
      [ "-v",
        "quiet",
        "-print_format",
        "json",
        "-show_format",
        "-show_streams",
        fp
      ]
      ""
  pure $ case code of
    ExitSuccess -> stdout
    _ -> Nothing
