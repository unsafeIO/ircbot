{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module SASL where

import Control.Monad (forM_)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as BS64
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.IRC.Client

newtype SASLPayload = SASLPayload ByteString

mkPlainSASL :: Text -> Text -> Text -> SASLPayload
mkPlainSASL authorization authentication pass = SASLPayload . TE.encodeUtf8 . T.intercalate "\0" $ [authorization, authentication, pass]

sendSASLPayload :: SASLPayload -> IRC s ()
sendSASLPayload (SASLPayload (BS64.encode -> bs)) = forM_ (mkAuthMsg <$> chucked bs) sendBS
  where
    mkAuthMsg msg = rawMessage "AUTHENTICATE" [msg]
    chucked b
      | BS.null b = ["+"]
      | BS.length b >= 400 = BS.take 400 b : chucked (BS.drop 400 b)
      | otherwise = [b]

sendSASLCapReq :: IRC s ()
sendSASLCapReq = sendBS $ rawMessage "CAP" ["REQ", "sasl"]

sendCapEnd :: IRC s ()
sendCapEnd = sendBS $ rawMessage "CAP" ["END"]

sendAuthPlain :: IRC s ()
sendAuthPlain = sendBS $ rawMessage "AUTHENTICATE" ["PLAIN"]

saslHandlers :: IRC s () -> [EventHandler s]
saslHandlers onSuccess =
  [ EventHandler (matchNumeric 903) (\_ _ -> sendCapEnd >> onSuccess),
    EventHandler (matchNumeric 904) (\_ _ -> error "Failed to authenticate")
  ]
