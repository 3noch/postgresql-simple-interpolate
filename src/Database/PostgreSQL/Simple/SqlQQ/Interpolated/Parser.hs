{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Interpolated SQL queries

-- This module was largely copied from
-- https://github.com/tmhedberg/here/blob/8a616b358bcc16bd215a78a8f6192ad9df8224b6/src/Data/String/Here/Interpolated.hs
module Database.PostgreSQL.Simple.SqlQQ.Interpolated.Parser where

import Data.Char (isDigit, isLetter)
import Data.Functor (($>))
import Control.Monad (unless)
import Control.Monad.State (evalStateT, get, modify)
import Control.Monad.Trans (lift)
import Language.Haskell.Meta (parseExp)
import Language.Haskell.TH (Exp, Q)
import Text.Parsec ( ParseError, anyChar, between, char, eof, incSourceColumn
                   , getInput, lookAhead, manyTill, noneOf, parse, setInput, statePos
                   , string, try, updateParserState, (<|>)
                   )
import Text.Parsec.String (Parser)

data StringPart = Lit String | Esc Char | Anti (Q Exp)

data HsChompState = HsChompState { quoteState :: QuoteState
                                 , braceCt :: Int
                                 , consumed :: String
                                 , prevCharWasIdentChar :: Bool
                                 }

data QuoteState = None | Single EscapeState | Double EscapeState deriving (Eq, Ord, Show)

data EscapeState = Escaped | Unescaped deriving (Bounded, Enum, Eq, Ord, Show)

parseInterpolated :: String -> Either ParseError [StringPart]
parseInterpolated = parse pInterp ""

pInterp :: Parser [StringPart]
pInterp = manyTill pStringPart eof

pStringPart :: Parser StringPart
pStringPart = pAnti <|> pEsc <|> pLit

pAnti :: Parser StringPart
pAnti = Anti <$> between (try pAntiOpen) pAntiClose pAntiExpr

pAntiOpen :: Parser String
pAntiOpen = string "${"

pAntiClose :: Parser String
pAntiClose = string "}"

pAntiExpr :: Parser (Q Exp)
pAntiExpr = pUntilUnbalancedCloseBrace >>= either fail (pure . pure) . parseExp

pUntilUnbalancedCloseBrace :: Parser String
pUntilUnbalancedCloseBrace = evalStateT go $ HsChompState None 0 "" False
  where
    go = do
      c <- lift anyChar
      modify $ \st@HsChompState {consumed} -> st {consumed = c:consumed}
      HsChompState{..} <- get
      let next = setIdentifierCharState c *> go
      case quoteState of
        None -> case c of
          '{' -> incBraceCt 1 *> next
          '}' | braceCt > 0 -> incBraceCt (-1) *> next
              | otherwise -> stepBack $> reverse (tail consumed)
          '\'' -> unless prevCharWasIdentChar (setQuoteState $ Single Unescaped)
               *> next
          '"' -> setQuoteState (Double Unescaped) *> next
          _ -> next
        Single Unescaped -> do case c of '\\' -> setQuoteState (Single Escaped)
                                         '\'' -> setQuoteState None
                                         _ -> pure ()
                               next
        Single Escaped -> setQuoteState (Single Unescaped) *> next
        Double Unescaped -> do case c of '\\' -> setQuoteState (Double Escaped)
                                         '"' -> setQuoteState None
                                         _ -> pure ()
                               next
        Double Escaped -> setQuoteState (Double Unescaped) *> next
    stepBack = lift $
      updateParserState
        (\s -> s {statePos = incSourceColumn (statePos s) (-1)})
        *> getInput
        >>= setInput . ('}':)
    incBraceCt n = modify $ \st@HsChompState {braceCt} ->
      st {braceCt = braceCt + n}
    setQuoteState qs = modify $ \st -> st {quoteState = qs}
    setIdentifierCharState c = modify $ \st ->
      st
        {prevCharWasIdentChar = or [isLetter c, isDigit c, c == '_', c == '\'']}

pEsc :: Parser StringPart
pEsc = Esc <$> (char '\\' *> anyChar)

pLit :: Parser StringPart
pLit = fmap Lit $
  try (litCharTil $ try $ lookAhead pAntiOpen <|> lookAhead (string "\\"))
    <|> litCharTil eof
  where litCharTil = manyTill $ noneOf ['\\']
