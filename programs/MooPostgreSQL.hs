module Main
    ( main
    )
where

import Database.HDBC.PostgreSQL (connectPostgreSQL)
import Prelude  hiding (lookup)
import System.Environment (getArgs, setEnv)
import System.Exit

import Database.Schema.Migrations.Backend.HDBC (hdbcBackend)
import Moo.Core
import Moo.Main

main :: IO ()
main = do
  -- make sure the configuration validation does not complain about a missing
  -- db type
  setEnv "DBM_DATABASE_TYPE" "postgresql"

  args <- getArgs
  (_, opts, _) <- procArgs args
  conf <-
    loadConfiguration $ _configFilePath opts
  case conf of
      Left e -> putStrLn e >> exitFailure
      Right preliminaryConf -> do
        let Left preliminaryDatabaseConf =  _backendOrConf preliminaryConf
            connectionString = _connectionString preliminaryDatabaseConf
        connection <- connectPostgreSQL connectionString
        let backend = hdbcBackend connection
            finalConf = preliminaryConf { _backendOrConf = Right backend }
        mainWithConf args finalConf

