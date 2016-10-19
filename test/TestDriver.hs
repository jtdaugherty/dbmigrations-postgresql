module Main where
import Test.HUnit
import System.Exit
import System.Process ( system )
import System.IO ( stderr )

import qualified BackendTest

import Control.Exception ( finally, catch, SomeException )

import Database.HDBC ( IConnection(disconnect) )
import qualified Database.HDBC.PostgreSQL as PostgreSQL

loadTests :: IO [Test]
loadTests = do

  pgConn <- setupPostgresDb

  let testAct = (BackendTest.tests (BackendTest.HDBCConnection pgConn))
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
  tests <- loadTests
  (testResults, _) <- runTestText (putTextToHandle stderr False) $ test tests
  if errors testResults + failures testResults > 0
    then exitFailure
    else exitSuccess
