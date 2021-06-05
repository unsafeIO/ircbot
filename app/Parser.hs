{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Parser where

import Config
import Control.Monad (guard)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char

data ParsedMessage = ParsedMessage {bridgedSender :: Maybe Text, entries :: [Entry]}
  deriving (Show)

data Entry = PixivID Int | URL Text | Command Text Text | Eval Text deriving (Show)

findBridgedName :: Parsec Void Text Text
findBridgedName = do
  _ <- "["
  name <- manyTill anySingle "]"
  return $ T.pack name

parseMessage :: Text -> Either (ParseErrorBundle Text Void) ParsedMessage
parseMessage = parse p "Message"
  where
    p = do
      bridgedSender <- optional findBridgedName
      entries <- filter legal <$> many pe
      return ParsedMessage {..}
    pe = try findEval <|> try findUrl <|> try findCommand <|> try findPixivId

legal :: Entry -> Bool
legal (URL a) = not $ "exhentai" `T.isInfixOf` a || "twitter" `T.isInfixOf` a
legal _ = True

findEval :: Parsec Void Text Entry
findEval = do
  _ <- space
  _ <- ">"
  _ <- space
  txt <- manyTill anySingle eof
  return . Eval $ T.pack txt

findPixivId :: Parsec Void Text Entry
findPixivId = do
  _ <- manyTill anySingle "#pixiv id"
  _ <- space
  _ <- "="
  _ <- space
  nums <- some digitChar
  return . PixivID $ read nums

findCommand :: Parsec Void Text Entry
findCommand =
  try
    ( do
        skipManyTill anySingle (string (myNick <> ":"))
        space
        cmd <- manyTill anySingle (() <$ spaceChar <|> eof)
        space
        args <- many $ anySingleBut '\''
        guard . not . null $ cmd
        return $ Command (T.pack cmd) (T.pack args)
    )
    <|> do
      skipManyTill anySingle "'"
      cmd <- manyTill anySingle (() <$ spaceChar <|> eof)
      space
      args <- many $ anySingleBut '\''
      guard . not . null $ cmd
      return $ Command (T.pack cmd) (T.pack args)

findUrl :: Parsec Void Text Entry
findUrl = do
  (_, s) <- manyTill_ anySingle (try "https://" <|> "http://")
  chars <- manyTill anySingle ((() <$ spaceChar) <|> eof)
  return . URL $ s <> T.pack chars
