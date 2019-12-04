module Benchmarks where

import Database.PostgreSQL.Simple.SqlQQ.Interpolated
import Database.PostgreSQL.Simple.SqlQQ.Interpolated.Parser

import Data.List (foldl')
import Criterion
import Criterion.Main
import Language.Haskell.TH

main = defaultMain
    [ bench "combineParts foldl' length 2" $
        nf combinePartsL' sql2
    , bench "combineParts foldl' length 100" $
        nf combinePartsL' sql100
    , bench "combineParts foldr length 2" $ nf combinePartsR sql2
    , bench "combineParts foldr length 100" $ nf combinePartsR sql100
    ]


sql2 :: [Either String Int]
sql2 =
    [ Left "SELECT field FROM table WHERE name = "
    , Right 1
    , Left " LIMIT "
    , Right 2
    ]

sql100 :: [Either String Int]
sql100 = concat [ [Left " ", Right n ] | n <- [1..100]]

-- | The same as combineParts in the library, using foldl'
combinePartsL' :: [Either String Int] -> (String, [Int])
combinePartsL' = foldl' step ("", [])
  where
    step (s, exprs) subExpr = case subExpr of
      Left str -> (s <> str, exprs)
      Right e -> (s <> "?", exprs <> [e]) -- TODO: Make this not slow

combinePartsR :: [Either String Int] -> (String, [Int])
combinePartsR = foldr step ("", [])
  where step subExpr (s, exprs) = case subExpr of
            Left str -> (str <> s, exprs)
            Right e -> ("?" <> s, e : exprs)
