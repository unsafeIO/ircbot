{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Parser where

import Control.Monad.Reader
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Regex.TDFA

type Parser = ReaderT Text (Parsec Void Text)

data ParsedMessage = ParsedMessage {bridgedSender :: Maybe Text, entries :: [Entry]}
  deriving (Show)

data Entry = PixivID Int | URL Text | Command Text Text | Eval Text deriving (Show)

findBridgedName :: Parser Text
findBridgedName = do
  _ <- string "["
  name <- manyTill anySingle $ string "]"
  return $ T.pack name

parseMessage :: Text -> Text -> Either (ParseErrorBundle Text Void) ParsedMessage
parseMessage myNick = parse (runReaderT p myNick) "Message"
  where
    p = do
      bridgedSender <- optional findBridgedName
      entries <- filter legal <$> many pe
      return ParsedMessage {..}
    pe = try findEval <|> try findUrl <|> try findCommand <|> try findPixivId

legal :: Entry -> Bool
legal (URL a) = not $ "exhentai" `T.isInfixOf` a || "twitter" `T.isInfixOf` a
legal _ = True

findEval :: Parser Entry
findEval = do
  _ <- space
  _ <- string ">"
  _ <- space
  txt <- manyTill anySingle eof
  return . Eval $ T.pack txt

findPixivId :: Parser Entry
findPixivId = do
  _ <- manyTill anySingle $ string "#pixiv id"
  _ <- space
  _ <- string "="
  _ <- space
  nums <- some digitChar
  return . PixivID $ read nums

findCommand :: Parser Entry
findCommand =
  try
    ( do
        myNick <- ask
        _ <- skipManyTill anySingle (string (myNick <> ":"))
        space
        cmd <- manyTill anySingle (void spaceChar <|> eof)
        space
        args <- many $ anySingleBut '\''
        guard . not . null $ cmd
        return $ Command (T.pack cmd) (T.pack args)
    )
    <|> do
      _ <- skipManyTill anySingle $ string "'"
      cmd <- manyTill anySingle (void spaceChar <|> eof)
      space
      args <- many $ anySingleBut '\''
      guard . not . null $ cmd
      return $ Command (T.pack cmd) (T.pack args)

findUrl :: Parser Entry
findUrl = do
  (T.pack -> chars) <- lookAhead $ manyTill anySingle eof
  case matchRegex chars urlRegex of
    Just (a, b, _, x) -> do
      _ <- string a
      _ <- string b
      return . URL $
        if "https://" `T.isPrefixOf` x || "http://" `T.isPrefixOf` x
          then x
          else "http://" <> x
    _ -> Text.Megaparsec.empty

urlRegex :: Text
urlRegex = "([-a-zA-Z0-9@:%._\\+~#=]{1,256}\\.[a-zA-Z0-9()]{1,6}\\b[-a-zA-Z0-9()@:%_\\+.~#?&//=]*)"

matchRegex :: T.Text -> T.Text -> Maybe (T.Text, T.Text, T.Text, T.Text)
matchRegex message regex = case message =~~ regex of
  Just (a :: T.Text, b :: T.Text, c :: T.Text, [x]) -> Just (a, b, c, x)
  _ -> Nothing
