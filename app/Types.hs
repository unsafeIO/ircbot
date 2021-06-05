{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Types where

import Control.Concurrent.MVar
import Control.Concurrent.STM (TQueue, newTQueueIO)
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Aeson
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Data.Time (getCurrentTime)
import GHC.Conc
import GHC.Generics (Generic)
import Language.Haskell.Interpreter
import Lens.Micro
import Network.HTTP.Client (Manager)
import Network.HTTP.Client.TLS (newTlsManager)
import Network.IRC.Client
import Servant.Client (ClientEnv, ClientError)
import Servant.Client.Internal.HttpClient (ClientEnv (..))
import System.Random (randomRIO)
import Text.Pretty.Simple
import Web.Pixiv.Auth
import Web.Pixiv.Types
import Web.Pixiv.Types.PixivT

type IRCBot = IRC MyState

type EvalQueue = TQueue (Interpreter ())

data MyState = MyState
  { pixivState :: MVar PixivState,
    clientEnv :: ClientEnv,
    evalQueue :: EvalQueue
  }

runPixivInIRC :: String -> PixivT IO a -> IRCBot (Either ClientError a)
runPixivInIRC tag m = do
  ircState <- getIRCState
  let tVar = ircState ^. userState
  MyState {..} <- liftIO . readTVarIO $ tVar
  time <- liftIO getCurrentTime
  liftIO $ putStrLn $ LT.unpack (pShow time) <> " - Run <" <> tag <> "> with pixiv state:"
  liftIO $ readMVar pixivState >>= pPrint
  liftIO . runClientT clientEnv . flip runReaderT pixivState $ unPixivT m

getManager :: IRCBot Manager
getManager = do
  ircState <- getIRCState
  let tVar = ircState ^. userState
  ClientEnv {manager} <- clientEnv <$> (liftIO . readTVarIO $ tVar)
  pure manager

getEvalQueue :: IRCBot EvalQueue
getEvalQueue = do
  ircState <- getIRCState
  MyState {evalQueue} <- liftIO $ readTVarIO $ ircState ^. userState
  pure evalQueue

randomP :: (MonadIO m) => PixivT m [a] -> PixivT m a
randomP m = do
  xs <- m
  rand <- liftIO $ randomRIO (0, length xs -1)
  pure $ xs !! rand

headP :: (MonadIO m) => PixivT m [a] -> PixivT m (Maybe a)
headP m = do
  xs <- m
  pure $ xs ^? _head

initMyState :: Text -> IO MyState
initMyState (Token -> token) = do
  manager <- liftIO newTlsManager
  clientEnv <- mkDefaultClientEnv manager
  t <- getCurrentTime
  pixivState <- liftIO $ newMVar . flip PixivState (Just "zh-CN") =<< computeTokenState manager (RefreshToken token) t
  evalQueue <- newTQueueIO
  pure MyState {..}

-----------------------------------------------------------------------------

data GoogleItem = GoogleItem
  { title :: Text,
    link :: Text,
    snippet :: Text
  }
  deriving (Show, Generic)

instance FromJSON GoogleItem

newtype GoogleResult = GoogleResult
  { items :: [GoogleItem]
  }
  deriving (Show, Generic)

instance FromJSON GoogleResult
