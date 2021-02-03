{-# LANGUAGE OverloadedStrings #-}
module Main where

import           App
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Data.Aeson                  (KeyValue ((.=)), object)
import           Database.Persist.Sqlite
import           Network.Wai.Middleware.Cors
import           Web.Scotty

main :: IO ()
main = do
  runStderrLoggingT $ do
    pool <- createSqlitePool "todos.db" 10

    liftIO $ scotty 3000 $ do
      middleware simpleCors
      defaultHandler (\err -> json (object ["error" .= err]))
      application pool
