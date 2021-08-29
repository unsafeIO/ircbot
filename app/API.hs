{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module API where

import Config
import Control.Monad.Catch (try)
import Control.Monad.IO.Class
import Data.Aeson (decode, encode, object, withObject, (.:), (.=))
import Data.Aeson.Types (parseMaybe)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Lazy (toStrict)
import qualified Data.ByteString.Lazy.Char8 as LBS.Char8
import Data.List (find)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Lens.Micro
import Network.HTTP.Client
import Network.HTTP.Client.MultipartFormData
import Servant.Client (ClientError)
import qualified Text.HTML.TagSoup as S
import qualified Text.HTML.TagSoup.Match as S
import Types
import Utils
import qualified Web.Pixiv.API as P
import Web.Pixiv.Types
import qualified Web.Pixiv.Types.Lens as L
import Web.Pixiv.Utils

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
    u <- headP $ P.searchUser username Nothing 1
    p <- (\u -> (^. _1) <$> P.getUserBookmarks (u ^. L.user . L.userId) Public Nothing) `traverse` u
    pure $ concat p

getUserBookmarks' :: Int -> IRCBot (Either ClientError Illust)
getUserBookmarks' userId = runPixivInIRC "getUserBookmarks'" $ randomP $ (^. _1) <$> P.getUserBookmarks userId Public Nothing

getUserIllusts :: Text -> IRCBot (Either ClientError Illust)
getUserIllusts username = runPixivInIRC "getUserIllusts" $
  randomP $ do
    u <- headP $ P.searchUser username Nothing 1
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

ua = [("user-agent", "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/85.0.4183.102 Safari/537.36")]

getTitle :: Text -> IRCBot (Either HttpException (Maybe Text))
getTitle url = try $ do
  req <- parseRequest $ T.unpack url
  manager <- getManager
  result <- liftIO $ httpLbs req {requestHeaders = ua} manager
  let parsed = S.parseTags $ responseBody result
      f = dropWhile (not . S.isTagOpenName "title") parsed
  case f of
    ((S.TagOpen "title" _) : S.TagText title : _) -> return (Just . decodeUtf8 . toStrict $ title)
    _ -> return Nothing

getContentTypeAndSize :: Text -> IRCBot (Either HttpException (Maybe (Text, Float)))
getContentTypeAndSize url = try $ do
  req <- parseRequest $ T.unpack url
  manager <- getManager
  result <- liftIO $ httpLbs req {requestHeaders = ua} manager
  let t = decodeUtf8 <$> lookup "Content-Type" (responseHeaders result)
      s = (/ 1024) . read . T.unpack . decodeUtf8 <$> lookup "Content-Length" (responseHeaders result)
  pure $ (,) <$> t <*> s

google :: Text -> IRCBot (Either HttpException (Maybe Text))
google q = try $ do
  req <- parseRequest "https://www.googleapis.com/customsearch/v1"
  let params = [("key", Just googleKey), ("cx", Just googleCX), ("q", Just $ encodeUtf8 q)]
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
  let parsed = S.parseTags $ toStrict $responseBody result
      yi = takeWhile (not . S.tagCloseLit "td") $ dropWhile (not . S.tagOpenLit "td" (S.anyAttrLit ("class", "suit_cont"))) parsed
      ji = takeWhile (not . S.tagCloseLit "td") $ dropWhile (not . S.tagOpenLit "td" (S.anyAttrLit ("class", "taboo_cont"))) parsed
      go acc (S.TagOpen "span" [("class", "t6left")] : S.TagText txt : S.TagClose "span" : xs) = go (txt : acc) xs
      go acc (_ : [S.TagClose "span"]) = acc
      go acc (_ : xs) = go acc xs
      go acc _ = acc
      resultYi = decodeUtf8 $ BS.intercalate ", " $ go [] yi
      resultJi = decodeUtf8 $ BS.intercalate ", " $ go [] ji
      _f = dropWhile (not . S.tagOpenLit "div" (S.anyAttrLit ("class", "item_tit")))
      info = decodeUtf8 $ case _f . tail . _f $ parsed of
        -- emprt_r_n => \r\n, not sure why it becomes a tag text
        (S.TagOpen "div" [("class", "item_tit")] : S.TagText a : S.TagClose "div" : _empty_r_n : S.TagOpen "p" [] : S.TagText b : S.TagClose "p" : _) -> a <> ", " <> b
        _ -> "gg"
  pure (info, resultYi, resultJi)

uploadToTelegraph :: Illust -> IRCBot (Either HttpException (Maybe Text))
uploadToTelegraph i = try $ do
  let urls = extractImageUrlsFromIllust i
      title = "pixiv-" <> (i ^. L.illustId & show)
      content = imageUrlsToTelegraph $ imageUrlToCF <$> urls
      obj =
        object
          [ "access_token" .= telegraphToken,
            "title" .= title,
            "content" .= content,
            "author_name" .= myNick
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
      result <- liftIO $ httpLbs req {requestHeaders = ua} manager
      let parsed = S.parseTags $ responseBody result
          f = find (S.tagOpenLit "link" $ S.anyAttrLit ("rel", "canonical")) parsed
      case f of
        Just (S.TagOpen _ attrs) -> pure (extractPUrl . decodeUtf8 . toStrict . snd =<< find (\ (k, _v) -> k == "href") attrs)
        _ -> pure Nothing
canonicalPixivUrl _ = pure Nothing
