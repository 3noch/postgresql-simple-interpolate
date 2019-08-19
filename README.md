# `postgresql-simple-interpolate`

Write natural SQL statements in Haskell using a QuasiQuotes!

```haskell
{-# LANGUAGE QuasiQuotes #-}

import Data.Char (toLower)
import qualified Database.PostgreSQL.Simple as Pg
import Database.PostgreSQL.Simple.SqlQQ.Interpolated (isql)
import Control.Exception (bracket)

main :: IO ()
main = do
  bracket (Pg.connectPostgreSQL "host=localhost") Pg.close $ \conn -> do
    let limit = 10
    ages <- uncurry (query conn) [isql|SELECT age FROM table WHERE name = ${map toLower "CLIVE"} LIMIT ${limit}|]
    print (ages :: [Pg.Only Int])
|]
```

## Hacking

With Nix you can quickly play around with this using a PostgreSQL database:

```
nix-shell -p '(import ./. {}).haskellPackages.ghcWithPackages (p: [p.gargoyle-postgresql-connect p.postgresql-simple-interpolate])' --run ghci
```

Then run

```haskell
:set -XQuasiQuotes
import Gargoyle.PostgreSQL.Connect (withDb)
import Data.Pool (withResource)
import Database.PostgreSQL.Simple (Only (..), query)
import Database.PostgreSQL.Simple.SqlQQ.Interpolated (isql)
[isql|SELECT ${1 + 1}|]
-- ("SELECT ?",[Plain "2"])
withDb "db" $ \pool -> withResource pool $ \c -> (uncurry (query c) [isql|SELECT ${1 + 1}, ${reverse "HELLO"}::text|] :: IO [(Int, String)])
-- [(2,"OLLEH")]
```


## Acknowledgements

This library is basically just a copy of the [`here` package](https://github.com/tmhedberg/here) by Taylor M. Hedberg with slight modifications!
