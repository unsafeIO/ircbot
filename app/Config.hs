{-# LANGUAGE OverloadedStrings #-}

module Config where

import Data.ByteString (ByteString)
import Data.Text

myNick :: Text
myNick = ""

myUserName :: Text
myUserName = ""

myRealName :: Text
myRealName = myUserName

myPassword :: Text
myPassword = ""

myChannels :: [Text]
myChannels =
  []

pixivToken :: Text
pixivToken = ""

googleKey :: ByteString
googleKey = ""

googleCX :: ByteString
googleCX = ""

telegraphToken :: Text
telegraphToken = ""
