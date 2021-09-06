{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wall #-}

module Eval (evalDaemon, evalIRC) where

import API (uploadPB)
import Control.Concurrent (newEmptyMVar, putMVar, readMVar)
import Control.Concurrent.STM (atomically, writeTQueue)
import Control.Concurrent.STM.TQueue (readTQueue)
import qualified Control.Exception as CE
import Control.Monad.Catch (try)
import Control.Monad.Except
import Data.IORef (IORef, readIORef)
import Data.Maybe (fromMaybe, fromJust, catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Debug.Trace (traceIO)
import GHC (getPrintUnqual, getSessionDynFlags, findModule, mkModuleName, getModuleInfo, modInfoExports, lookupName, TyThing (AnId))
#if !MIN_VERSION_GLASGOW_HASKELL(9,0,0,0)
import InteractiveEval (isDecl, runDecls)
import Outputable (Outputable, ppr, showSDocForUser)
import PprTyThing (pprTyThingHdr)
#else
import GHC.Runtime.Eval (isDecl, runDecls)
import GHC.Utils.Outputable (Outputable, ppr, showSDocForUser)
import GHC.Parser.Lexer (mkParserFlags)
import GHC.Core.Ppr.TyThing (pprTyThingHdr)
#endif
import Language.Haskell.Interpreter
import PQ (Done, IllustLike (toIllusts), PQ (..))
import Servant.Client (ClientError)
import System.Random
import qualified System.Random.Shuffle as S
import System.Timeout (timeout)
import Types
import Utils (forkIRC)
import Web.Pixiv (Illust, IllustType (TypeIllust), Publicity (Public), SearchTarget (PartialMatchForTags))
import Web.Pixiv.API

showGHC :: (MonadInterpreter m, Outputable a) => a -> m Text
showGHC a = do
  (unqual, df) <- runGhc $ getPrintUnqual >>= \x -> (x,) <$> getSessionDynFlags
  pure $ T.pack $ showSDocForUser df unqual (ppr a)

traceI :: String -> Interpreter ()
traceI = liftIO . traceIO

evalDaemon :: IRCBot ()
evalDaemon =
  forkIRC $
    getEvalQueue >>= \chan ->
      liftIO
        ( runInterpreter $ do
            setupEval
            forever $
              join $ liftIO $ atomically $ readTQueue chan
        )
        >>= \case
          Left err -> liftIO $ putStrLn $ "Eval daemon exited with: " <> show err
          Right x -> pure x

evalEnqueue :: Interpreter a -> IRCBot (Maybe (Either InterpreterError a))
evalEnqueue action = do
  chan <- getEvalQueue
  var <- liftIO newEmptyMVar
  liftIO $
    atomically $
      writeTQueue chan $ try action >>= liftIO . putMVar var
  liftIO $ timeout 5000000 $ readMVar var

setupEval :: Interpreter ()
setupEval = do
  set
    [ languageExtensions
        := [ OverloadedStrings,
             FlexibleInstances,
             ScopedTypeVariables,
             RankNTypes,
             TypeApplications,
             NoMonomorphismRestriction,
             ExtendedDefaultRules
           ]
    ]
  loadModules ["dsl/PQ.hs"]
  setImportsQ
    [ ("Prelude", Nothing),
      ("PQ", Nothing),
      ("Lens.Micro", Nothing),
      ("Lens.Micro.Extras", Nothing),
      ("Data.Text", Just "T"),
      ("Web.Pixiv.Types", Nothing),
      ("Web.Pixiv.Types.Lens", Nothing),
      ("Data.List", Nothing),
      ("Data.Function", Nothing)
    ]

evalIRC :: Text -> (Text -> IRCBot ()) -> (Bool -> Illust -> IRCBot ()) -> IORef Int -> IRCBot ()
evalIRC (T.unpack -> msg) replyF sendPic lastRef = do
  result <- evalEnqueue $ do
    isMsgDecl <- runGhc $ getSessionDynFlags >>= \df ->
#if !MIN_VERSION_GLASGOW_HASKELL(9,0,0,0)
      let pf = df
#else
      let pf = mkParserFlags df
#endif
      in pure (isDecl pf msg)
    if isMsgDecl
      then Left . T.unpack . (\x -> "声明: " <> T.intercalate ", " x) <$> (runGhc (runDecls msg) >>= mapM showGHC)
      else do
        ty <- typeOf msg
        traceI $ "The type of [" <> msg <> "] is " <> ty
        case ty of
          "PQ Done" -> traceI "Interpret to PQ Done" >> Right <$> interpret msg (as :: PQ Done)
          "PQ Illust" -> traceI "Wrap to PQ Done" >> Right <$> interpret (unwords ["PAsIllust", parens msg]) (as :: PQ Done)
          "PQ [Illust]" -> traceI "Wrap to PQ Done" >> Right <$> interpret (unwords ["PAsIllust", parens msg]) (as :: PQ Done)
          "PQ String" -> traceI "Wrap to PQ Done" >> Right <$> interpret (unwords ["PAsString", parens msg]) (as :: PQ Done)
          _ -> traceI "Eval it to String" >> Left <$> interpret (unwords ["show", parens msg]) (as :: String)
  case result of
    Nothing -> replyF "求值超时了!"
    Just (Left err) ->
      uploadPB "eval" (encodeUtf8 $ showError err) >>= \case
        Left _ -> replyF "未能上传求值错误失败"
        Right l -> replyF $ "求值错误! 输出: " <> l
    Just (Right (Left s)) -> liftIO (render s) >>= replyF
    Just (Right (Right c)) ->
      runExceptT (myEval c) >>= \case
        Left err -> replyF $ showText err
        Right _ -> pure ()
  where
    sendErrorOrIllusts :: IllustLike a => a -> ExceptT ClientError IRCBot ()
    sendErrorOrIllusts (toIllusts -> x) = case x of
      [] -> lift $ replyF "无事可做"
      x' -> lift $ mapM_ (sendPic True) x'
    myEval :: PQ a -> ExceptT ClientError IRCBot a
    myEval = \case
      PIllustRanking mode -> ExceptT $ runPixivInIRC "eval.getIllustRanking" $ concat <$> sequence [getIllustRanking (Just mode) page | page <- [1 .. 2]]
      PRelated i -> ExceptT $ runPixivInIRC "eval.getIllustRelated" (getIllustRelated i 1)
      PSearch word -> ExceptT $ runPixivInIRC "eval.searchIllust" (searchIllust PartialMatchForTags word (Just True) Nothing Nothing 1)
      PIllust i -> ExceptT $ runPixivInIRC "eval.getIllustDetail" (getIllustDetail i)
      PUserBookmarks u -> ExceptT $ runPixivInIRC "eval.getUserBookmarks" (fst <$> getUserBookmarks u Public Nothing)
      PUserIllusts u -> ExceptT $ runPixivInIRC "eval.getUserIllusts" (getUserIllusts u (Just TypeIllust) 1)
      PLastId -> liftIO $ readIORef lastRef
      PAsIllust e -> do
        result <- myEval e
        sendErrorOrIllusts result
      PAsString e -> do
        result <- myEval e
        lift $ liftIO (render $ show result) >>= replyF
      PBind e f -> do
        x <- myEval e
        myEval $ f x
      PLit x -> pure x
      PShuffle xs -> do
        stdGen <- liftIO getStdGen
        pure $ S.shuffle' xs (length xs) stdGen
      PBrowse -> do
        r <- lift $ evalEnqueue $ runGhc $ do
            mInfo <- fromJust <$> (findModule (mkModuleName "PQ") Nothing >>= getModuleInfo)
            exports <- catMaybes <$> mapM lookupName (modInfoExports mInfo)
            (unqual, df)<- getPrintUnqual >>= \x -> (x,) <$> getSessionDynFlags
            pure [T.pack $ showSDocForUser df unqual (pprTyThingHdr ty) | ty@(AnId _) <- exports]
        lift $ case r of
          Just(Right xs) -> do
            replyF $ T.intercalate ", " xs
          _ -> replyF "未能获取可用函数"

render :: String -> IO Text
render x = do
  r <- liftIO $ CE.try @CE.SomeException $ timeout 5000000 $ CE.evaluate $ take 1024 x
  case r of
    Left err -> pure $ showText err
    Right s -> pure $ T.pack $ fromMaybe "打印结果超时了!" s

showText :: Show a => a -> Text
showText = T.pack . show

showError :: InterpreterError -> Text
showError =
  \case
    (UnknownError x) -> "未知错误: " <> T.pack x
    (WontCompile xs) -> "编译错误: " <> T.unlines [T.pack $ errMsg x | x <- xs]
    (NotAllowed x) -> T.pack x
    (GhcException x) -> "GHC 异常: " <> T.pack x

-- copied from https://hackage.haskell.org/package/mueval-0.9.3/docs/src/Mueval-Resources.html#limitResources

-- limitResources :: Bool -> IO ()
-- limitResources t = nice 20 >> when t (mapM_ (uncurry setResourceLimit) limits)

-- stackSizeLimitSoft,
--   stackSizeLimitHard,
--   totalMemoryLimitSoft,
--   totalMemoryLimitHard,
--   dataSizeLimitSoft,
--   openFilesLimitSoft,
--   openFilesLimitHard,
--   fileSizeLimitSoft,
--   fileSizeLimitHard,
--   dataSizeLimitHard,
--   cpuTimeLimitSoft,
--   cpuTimeLimitHard,
--   coreSizeLimitSoft,
--   coreSizeLimitHard,
--   zero ::
--     ResourceLimit
-- totalMemoryLimitSoft = dataSizeLimitSoft
-- totalMemoryLimitHard = dataSizeLimitHard
-- stackSizeLimitSoft = zero
-- stackSizeLimitHard = zero
-- openFilesLimitSoft = openFilesLimitHard
-- openFilesLimitHard = ResourceLimit 7
-- fileSizeLimitSoft = fileSizeLimitHard
-- fileSizeLimitHard = ResourceLimit 10800
-- dataSizeLimitSoft = dataSizeLimitHard
-- dataSizeLimitHard = ResourceLimit $ 6 ^ (12 :: Int)
-- cpuTimeLimitSoft = ResourceLimit 4
-- cpuTimeLimitHard = ResourceLimit 5
-- coreSizeLimitSoft = coreSizeLimitHard
-- coreSizeLimitHard = zero
-- zero = ResourceLimit 0

-- limits :: [(Resource, ResourceLimits)]
-- limits =
--   [ (ResourceStackSize, ResourceLimits stackSizeLimitSoft stackSizeLimitHard),
--     -- (ResourceTotalMemory, ResourceLimits totalMemoryLimitSoft totalMemoryLimitHard),
--     -- (ResourceOpenFiles, ResourceLimits openFilesLimitSoft openFilesLimitHard),
--     (ResourceFileSize, ResourceLimits fileSizeLimitSoft fileSizeLimitHard),
--     (ResourceDataSize, ResourceLimits dataSizeLimitSoft dataSizeLimitHard),
--     (ResourceCoreFileSize, ResourceLimits coreSizeLimitSoft coreSizeLimitHard),
--     (ResourceCPUTime, ResourceLimits cpuTimeLimitSoft cpuTimeLimitHard)
--   ]
