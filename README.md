# `postgresql-simple-interpolate`

Write natural SQL statements in Haskell using a QuasiQuotes!

```haskell
{-# LANGUAGE QuasiQuotes #-}

import Data.Char (toLower)
import Database.PostgreSQL.Simple.SqlQQ.Interpolated (isql)

main = do
  putStr "What's your name? "
  name <- getLine
  let limit = 10
  conn <- ... -- open db
  uncurry (query conn) [isql|SELECT field FROM table WHERE name = ${map toLower name} LIMIT ${limit}|]
|]
```

## Hacking

With Nix you can quickly play around with this using a PostgreSQL database:

```
nix-shell -p '(import ./. {}).haskellPackages.ghcWithPackages (p: [p.gargoyle-postgresql-connect p.postgresql-simple-interpolate])' --run ghci
```

## Acknowledgements

This library is basically just a copy of the [`here` package](https://github.com/tmhedberg/here) by Taylor M. Hedberg with slight modifications!
