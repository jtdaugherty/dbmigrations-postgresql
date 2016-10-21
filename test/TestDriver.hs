{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Database.Schema.Migrations.Backend (DatabaseType(PostgreSQL))
import Database.Schema.Migrations.Backend.HDBC (hdbcBackend)
import Database.Schema.Migrations.Test.BackendTest as BackendTest

import Control.Exception (catch, catches, finally, Handler(..), SomeException )
import Database.HDBC ( IConnection(disconnect) )
import qualified Database.HDBC as HDBC
import qualified Database.HDBC.PostgreSQL as PostgreSQL
import System.Exit
import System.IO ( stderr )
import System.Process ( system )
import Test.HUnit

data PostgreSQLBackendConnection =
    forall a. HDBC.IConnection a => HDBCConnection a

instance BackendConnection PostgreSQLBackendConnection where
    getDatabaseType _ = PostgreSQL
    migrationBackend databaseType (HDBCConnection c) =
        hdbcBackend databaseType c
    commit (HDBCConnection c) = HDBC.commit c
    withTransaction (HDBCConnection c) transaction =
        HDBC.withTransaction c (transaction . HDBCConnection)
    getTables (HDBCConnection c) = HDBC.getTables c
    catchAll (HDBCConnection _) act handler =
        act `catches` [ Handler (\(_ :: HDBC.SqlError) -> handler) ]


loadTests :: IO [Test]
loadTests = do

  pgConn <- setupPostgresDb

  let backendConnection :: PostgreSQLBackendConnection = HDBCConnection pgConn
      testAct = (BackendTest.tests backendConnection)
                `finally`
                (disconnect pgConn >> teardownPostgresDb)
  return [ ("PostgreSQL backend tests") ~: test testAct ]

tempDatabase :: String
tempDatabase = "dbmigrations_test"

ignoreException :: SomeException -> IO ()
ignoreException _ = return ()

setupPostgresDb :: IO PostgreSQL.Connection
setupPostgresDb = do
  teardownPostgresDb `catch` ignoreException

  -- create database
  status <- system $ "createdb " ++ tempDatabase
  case status of
    ExitSuccess -> return ()
    ExitFailure _ -> error $ "Failed to create PostgreSQL database " ++ (show tempDatabase)

  -- return test db connection
  PostgreSQL.connectPostgreSQL $ "dbname=" ++ tempDatabase

teardownPostgresDb :: IO ()
teardownPostgresDb = do
  -- drop database
  status <- system $ "dropdb " ++ tempDatabase ++ " 2>/dev/null"
  case status of
    ExitSuccess -> return ()
    ExitFailure _ -> error $ "Failed to drop PostgreSQL database " ++ (show tempDatabase)

main :: IO ()
main = do
  tests_ <- loadTests
  (testResults, _) <- runTestText (putTextToHandle stderr False) $ test tests_
  if errors testResults + failures testResults > 0
    then exitFailure
    else exitSuccess
