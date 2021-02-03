{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
module App where

import           Control.Monad.IO.Class     (MonadIO (..))
import           Control.Monad.Logger       (runStderrLoggingT)
import           Control.Monad.Trans.Reader (ReaderT)
import           Data.Aeson                 (FromJSON (parseJSON), withObject,
                                             (.:))
import           Data.Pool                  (Pool)
-- import           Data.Text
import qualified Database.Persist           as P
import           Database.Persist.Sqlite    hiding (delete, get)
import           Database.Persist.TH
import           Network.HTTP.Types.Status  (status201, status204)
import           Web.Scotty

share [mkPersist sqlSettings] [persistLowerCase|
Todo sql=todos json
  task String
  is_done Bool
  deriving Show
|]

newtype TodoBody =
  TodoBody
  { task :: String
  }

instance FromJSON TodoBody where
    parseJSON = withObject "TodoBody" $ \v -> TodoBody
        <$> v .: "task"

runDB :: (MonadIO m, BackendCompatible SqlBackend backend) => Pool backend -> ReaderT backend IO a -> m a
runDB pool query = runStderrLoggingT $ liftIO $ runSqlPool query pool

application :: Pool SqlBackend -> ScottyM ()
application pool = do
  get "/todos" $ do
    todos <- runDB pool $ P.selectList [] [] :: ActionM [Entity Todo]

    json todos

  -- {
  --   task: "xxxx"
  -- }
  post "/todos" $ do
    requestBody <- jsonData

    let todo = Todo
                  { todoTask = task requestBody
                  , todoIs_done = False
                  }

    id <- runDB pool $ insert todo

    status status201

    json (Entity id todo)

  put "/todos/:id" $ do
    id <- param "id"
    todo <- jsonData

    runDB pool $ update (toSqlKey id) [TodoTask =. todoTask todo, TodoIs_done =. todoIs_done todo]

    status status204

  delete "/todos/:id" $ do
    id <- param "id"

    runDB pool $ P.delete (toSqlKey id :: Key Todo)

    status status204

