{-# LANGUAGE TemplateHaskell #-}

-- | Interpolated SQL queries
module Database.PostgreSQL.Simple.SqlQQ.Interpolated (isql, quoteInterpolatedSql) where

import Language.Haskell.TH (Exp, Q, appE, listE, sigE, tupE, varE)
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Data.List (foldl')
import Database.PostgreSQL.Simple.ToField (Action, toField)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Text.Parsec (ParseError)

import Database.PostgreSQL.Simple.SqlQQ.Interpolated.Parser (StringPart (..), parseInterpolated)

-- | Quote a SQL statement with embedded antiquoted expressions.
--
-- The result of the quasiquoter is a tuple, containing the statement string and a list
-- of parameters. For example:
--
-- @[isql|SELECT field FROM table WHERE name = ${map toLower "ELLIOT"} LIMIT ${10}|]@
--
-- produces
--
-- @("SELECT field FROM table WHERE name = ? LIMIT ?", [Escape "elliot", Plain "10"])@
--
-- How the parser works:
--
-- Any expression occurring between @${@ and @}@ will be replaced with a @?@
-- and passed as a query parameter.
--
-- Characters preceded by a backslash are treated literally. This enables the
-- inclusion of the literal substring @${@ within your quoted text by writing
-- it as @\\${@. The literal sequence @\\${@ may be written as @\\\\${@.
--
-- Note: This quasiquoter is a wrapper around 'Database.PostgreSQL.Simple.SqlQQ.sql'
-- which also "minifies" the query at compile time by stripping whitespace and
-- comments. However, there are a few "gotchas" to be aware of so please refer
-- to the documentation of that function for a full specification.
--
-- This quasiquoter only works in expression contexts and will throw an error
-- at compile time if used in any other context.
isql :: QuasiQuoter
isql = QuasiQuoter
  { quoteExp = quoteInterpolatedSql
  , quotePat = error "isql quasiquoter does not support usage in patterns"
  , quoteType = error "isql quasiquoter does not support usage in types"
  , quoteDec = error "isql quasiquoter does not support usage in declarations"
  }

combineParts :: [StringPart] -> (String, [Q Exp])
combineParts = foldl' step ("", [])
  where
    step (s, exprs) subExpr = case subExpr of
      Lit str -> (s <> str, exprs)
      Esc c -> (s <> [c], exprs)
      Anti e -> (s <> "?", exprs <> [e]) -- TODO: Make this not slow

applySql :: [StringPart] -> Q Exp
applySql parts =
  let
    (s', exps) = combineParts parts
  in
  tupE [quoteExp sql s', sigE (listE $ map (appE (varE 'toField)) exps) [t| [Action] |]]

-- | The internal parser used by 'isql'.
quoteInterpolatedSql :: String -> Q Exp
quoteInterpolatedSql s = either (handleError s) applySql (parseInterpolated s)

handleError :: String -> ParseError -> Q Exp
handleError expStr parseError = error $
  "Failed to parse interpolated expression in string: "
    ++ expStr
    ++ "\n"
    ++ show parseError
