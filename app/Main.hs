{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall #-}

module Main where

import API
import Config
import qualified Control.Exception as CE
import Control.Monad (forM_, when)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Lazy (toStrict)
import Data.IORef
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Eval
import GHC.IO (unsafePerformIO)
import Lens.Micro
import Network.IRC.Client
import PQ (tagString)
import Parser
import Servant.Client (ClientError)
import Text.Pretty.Simple
import Text.Printf (printf)
import Text.Read (readMaybe)
import Types
import Utils
import qualified Web.Pixiv.API as P
import Web.Pixiv.Download
import qualified Web.Pixiv.Types as P
import qualified Web.Pixiv.Types.Lens as J
import Web.Pixiv.Utils

main :: IO ()
main = do
  let conn =
        tlsConnection (WithDefaultConfig "irc.libera.chat" 6697)
          & logfunc .~ myIRCLogger
          & username .~ myUserName
          & realname .~ myRealName
          & onconnect
            .~ ( defaultOnConnect
                   >> send identifyNick
                   >> setupTokenRefersh
                   >> evalDaemon
               )

      myHandler = EventHandler (matchType _Privmsg) $ \s (_, y) -> case s of
        Channel _channelName senderName -> case y of
          Left _ -> return ()
          Right msg ->
            forkIRC $ do
              case parseMessage msg of
                -- Parse error
                Left _ -> return ()
                Right (ParsedMessage bridgedSender m) ->
                  let sender = fromMaybe senderName bridgedSender
                      replyF txt = replyTo s $ sender <> ": " <> txt
                      handle e = case e of
                        -- #pixiv id=...
                        [PixivID picId] -> do
                          liftIO $ writeIORef lastId picId
                          info <- getIllustDetail picId
                          -- enable short url
                          sendPic replyF info True
                        [URL (isFc -> isPb), e'] -> do
                          case e' of
                            --  https://fars.ee/... #pixiv id=...
                            PixivID picId -> do
                              liftIO $ writeIORef lastId picId
                              info <- getIllustDetail picId
                              -- enable short url if no pb
                              sendPic replyF info (not isPb)
                            URL url -> do
                              url' <- canonicalPixivUrl url
                              case url' of
                                --  https://fars.ee/... https://www.pixiv.net/artworks/...
                                Just (PIllust picId) -> do
                                  liftIO $ writeIORef lastId picId
                                  info <- getIllustDetail picId
                                  --- enable short url if no pb
                                  sendPic replyF info (not isPb)
                                Just (PUser userId) -> do
                                  info <- getUserDetailAndAnIllust userId
                                  sendUserAndAnIllust replyF info
                                _ -> return ()
                            _ -> return ()
                        -- A single url
                        [URL url] -> do
                          url' <- canonicalPixivUrl url
                          case url' of
                            -- This url indicates a pixiv artwork
                            Just (PIllust picId) -> do
                              liftIO $ writeIORef lastId picId
                              info <- getIllustDetail picId
                              -- enable short url
                              sendPic replyF info True
                            Just (PUser userId) -> do
                              info <- getUserDetailAndAnIllust userId
                              sendUserAndAnIllust replyF info
                            -- Unknown url
                            _ ->
                              if isFc url
                                then do
                                  cts <- getContentTypeAndSize url
                                  case cts of
                                    Right (Just (ct, cs, mimg)) -> 
                                      replyF $ "⇪PB 文件类型: " 
                                        <> ct 
                                        <> ", 文件大小: " 
                                        <> T.pack (printf "%.2f KiB" cs)
                                        <> maybe "" (\(imgType, imgRes) -> ", 图片类型: " <> imgType <> ", 图片尺寸: " <> imgRes) mimg
                                    _ -> replyF "⇪PB"
                                else do
                                  result <- getTitle url
                                  -- send title if no error
                                  case result of
                                    Right title ->
                                      replyF $
                                        "⇪网页标题: " <> case title of
                                          Just x -> x
                                          _ -> "未知"
                                    Left err -> pPrint err

                        -- A single command
                        (c@(Command _ _) : xs) -> myCommandHandler replyF c >> handle xs
                        -- PQ
                        (Eval ev : xs) -> evalIRC ev replyF (\short illust -> sendPic replyF (Right illust) short) lastId >> handle xs
                        _ -> return ()
                   in liftIO (print m) >> handle m
        _ -> return ()

      cHandler = EventHandler (matchType _Notice) $ \x (_, y) -> case x of
        User "NickServ" -> when (Right ("You are now identified for \STX" <> myUserName <> "\STX.") == y) $ do
          joinChannels myChannels
          send (Nick myNick)
        _ -> return ()

      cfg =
        defaultInstanceConfig myUserName
          & handlers .~ (myHandler : cHandler : defaultEventHandlers)
  state <- initMyState pixivToken
  let run = runClient conn cfg state in run `CE.catch` (\(e :: CE.SomeException) -> print e >> run)

lastId :: IORef Int
lastId = unsafePerformIO $ newIORef (-1)
{-# NOINLINE lastId #-}

sendUserAndAnIllust :: (Text -> IRCBot ()) -> Either ClientError (P.UserDetail, Maybe P.Illust) -> IRCBot ()
sendUserAndAnIllust replyF (Right (userDetail, mIllust)) = do
  profileUrl <- case userDetail ^. J.user . J.profileImageUrls of
    P.ImageUrls {P._medium = Just url} -> do
      s <- shortenUrl $ imageUrlToCF url
      Just <$> case s of
        Right x -> pure $ T.strip x
        Left err -> pPrint err >> pure "QAQ"
    _ -> pure Nothing
  replyF $
    T.concat
      [ "⇪User id: " <> T.pack (show $ userDetail ^. J.user . J.userId),
        " | 姓名: " <> userDetail ^. J.user . J.name,
        " | 关注者: " <> T.pack (show $ userDetail ^. J.profile . J.totalFollowUsers),
        " | 地区: " <> let r = userDetail ^. J.profile . J.region in if T.null r then "未知" else r,
        maybe "" (" | 头像: " <>) profileUrl
      ]
  maybe (pure ()) (\x -> replyF "第一幅作品:" >> sendPic replyF (Right x) True) mIllust
sendUserAndAnIllust replyF (Left err) = pPrint err >> replyF (T.pack $ show err)

-- | Handle pixiv
sendPic :: (Text -> IRCBot ()) -> Either ClientError P.Illust -> Bool -> IRCBot ()
sendPic replyF (Right illust) enableShort
  | Just url <- extractLargeImageUrl $ illust ^. J.imageUrls =
    do
      let isUgoira = illust ^. J.illustType == P.TypeUgoira
          illustId = illust ^. J.illustId
          isSingle = isSinglePageIllust illust
          enableShort' = enableShort && not isUgoira && isSingle
          enableTelegraph = enableShort && not isUgoira && not isSingle
      -- make short url if enable
      short <-
        if enableShort'
          then do
            s <- shortenUrl $ imageUrlToCF url
            case s of
              Right x | T.isPrefixOf "https" x -> pure $ T.strip x
              Right _ -> pure "QAQ"
              Left err -> pPrint err >> pure "QAQ"
          else return ""

      telegraph <-
        if enableTelegraph
          then do
            result <- uploadToTelegraph illust
            case result of
              Left e -> pPrint e >> pure ""
              Right (Just x) -> pure x
              Right _ -> pure "QAQ"
          else pure ""

      let translated =
            fmap (\it -> "#" <> replaceChars it) $
              (illust ^. J.tags) <&> (\x -> case x ^. J.translatedName of Just t -> t; Nothing -> x ^. J.name)
          tagTxt = T.intercalate " " translated
          messy = tagString illust
          idTxt = T.pack (show illustId)

      -- send tags and short url
      replyF $
        T.concat
          [ "⇪Pixiv id: " <> idTxt,
            " | 作者: #" <> illust ^. J.user . J.name,
            " | 标题: " <> illust ^. J.title,
            " | 标签: " <> tagTxt,
            if enableShort' then " | " <> short else "",
            if isUgoira then " | 转码中" else "",
            if enableTelegraph then " | " <> telegraph else "",
            if enableShort then " | 原始链接: pixiv.net/i/" <> idTxt else ""
          ]

      when isUgoira $ do
        result <- runPixivInIRC "processUgoira" $ do
          meta <- P.getUgoiraMetadata illustId
          lbs <- liftToPixivT $ downloadUgoiraToMP4 meta Nothing
          liftIO $ putStrLn $ maybe "No stderr" fst lbs
          pure $ snd <$> lbs
        case result of
          Right (Just lbs) -> do
            pb <- uploadPB (show illustId <> ".mp4") $ toStrict lbs
            case pb of
              Right x -> replyF x
              Left err -> pPrint err >> replyF "动图上传失败"
          _ -> replyF "动图处理失败"

      -- 性癖
      when (anySub ["漏尿", "放尿", "お漏らし", "おもらし", "おしっこ"] messy) $
        replyF "⇪ #oc诱捕器"

      when (anySub ["調教", "束縛", "機械姦", "緊縛", "縛り", "鼻フック", "監禁", "口枷"] messy) $
        replyF "⇪ #空指针诱捕器"
  | otherwise = replyF "Unable to extract image url from illust."
sendPic replyF (Left err) _ = pPrint err >> replyF "未能从 pixiv 获取插图"

-- | Handle command
myCommandHandler :: (Text -> IRCBot ()) -> Entry -> IRCBot ()
myCommandHandler replyF (Command cmd args) = case cmd of
  "ping" -> replyF "pong"
  "echo" -> replyF args
  "pixiv" -> case readMaybe @Int (T.unpack args) of
    Just picId -> do
      liftIO $ writeIORef lastId picId
      info <- getIllustDetail picId
      sendPic replyF info True
    _ -> replyF $ "未能解析 pixiv id: " <> args
  "tags" -> case readMaybe @Int (T.unpack args) of
    Just picId -> do
      liftIO $ writeIORef lastId picId
      info <- getIllustDetail picId
      sendPic replyF info False
    _ -> replyF $ "未能解析 pixiv id: " <> args
  "related" -> do
    pixivId <- liftIO $ readIORef lastId
    result <- getIllustRelated pixivId
    sendPic replyF result True
  "last" -> do
    pixivId <- liftIO $ readIORef lastId
    result <- getIllustDetail pixivId
    sendPic replyF result True
  "lastc" -> do
    pixivId <- liftIO $ readIORef lastId
    result <- getIllustComments pixivId
    case result of
      Right (P._comments -> xs) ->
        if null xs
          then replyF "无评论"
          else do
            forM_ (take 5 xs) $ \x ->
              replyF $ T.concat ["[", x ^. J.user . J.name, "]: ", x ^. J.comment]
      Left err -> pPrint err >> replyF "未能获取评论"
  "source" -> replyF "https://github.com/unsafeIO/ircbot"
  "google" -> do
    if T.null args
      then replyF "搜啥?"
      else do
        result <- google args
        case result of
          Right (Just x) -> replyF x
          Right Nothing -> replyF "QAQ"
          Left err -> pPrint err >> replyF "QAQ"
  "today" -> do
    result <- hl
    case result of
      Right ((d, y, j)) -> do
        replyF d
        replyF $ "宜:" <> y
        replyF $ "忌:" <> j
      _ -> replyF "QAQ"
  "search" -> do
    result <- searchIllust args
    case result of
      Right (Just pic) -> sendPic replyF (Right pic) True
      _ -> replyF $ "未找到 \"" <> args <> "\""
  "ranking" -> do
    case parseRankMode args of
      Just mode -> do
        result <- getIllustRanking $ Just mode
        case result of
          Right x -> forM_ (take 5 x) $ \r -> do
            sendPic replyF (Right r) True
          Left err -> pPrint err >> replyF "未能获取排行榜"
      _ -> replyF $ "未知排行模式: " <> args
  "bookmark" -> do
    result <- getUserBookmarks args
    sendPic replyF result True
  "bookmark'" -> case readMaybe @Int (T.unpack args) of
    Just userId -> do
      result <- getUserBookmarks' userId
      sendPic replyF result True
    _ -> replyF $ "未能解析 pixiv id: " <> args
  "work" -> do
    result <- getUserIllusts args
    sendPic replyF result True
  "trending" -> do
    result <- getTrendingTags
    case result of
      Right x -> forM_ (take 5 x) $ \r -> do
        replyF $ "人气标签: #" <> replaceChars (r ^. J.trendTag)
        sendPic replyF (Right $ r ^. J.illust) True
      Left err -> pPrint err >> replyF "未能获取人气标签"
  "help" ->
    replyF $
      T.intercalate
        " | "
        [ "'ping",
          "'echo [...]",
          "'pixiv [illustId]",
          "'tags [illustId]",
          "'related",
          "'last",
          "'lastc",
          "'source",
          "'google [keywords]",
          "'today",
          "'search [tag]",
          "'ranking [day|day-r18|week|week-r18|week-r18g|week-original|month]",
          "'bookmark [username]",
          "'bookmark' [userId]",
          "'work [username]",
          "'trending",
          "'help"
        ]
  _ -> return ()
myCommandHandler _ _ = pure ()
