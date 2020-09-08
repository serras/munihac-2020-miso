{-# language DataKinds         #-}
{-# language TypeOperators     #-}
{-# language DeriveGeneric     #-}
{-# language DeriveAnyClass    #-}
{-# language OverloadedStrings #-}
{-# language TypeApplications  #-}

module Main where

import Control.Monad.IO.Class
import Control.Monad.STM
import Control.Concurrent.STM.TVar
import Data.Aeson
import Data.List (find)
import GHC.Generics

import Servant
import Servant.API
import Servant.Server
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.Servant.Options

data ListItem
  = ListItem {
      liId   :: String
    , liText :: String
    , liDone :: Bool
    }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

type API =    "items"  :> Get '[JSON] [ListItem]
         :<|> "items"  :> ReqBody '[JSON] ListItem :> Post '[JSON] NoContent
         :<|> "toggle" :> Capture "liId" String :> Post '[JSON] ListItem

main :: IO ()
main = do
  var <- newTVarIO defaultInitialItems
  run 8080 $
    cors (const $ Just $ simpleCorsResourcePolicy { corsRequestHeaders = [ "content-type" ] }) $
      provideOptions (Proxy @API) $
        serve (Proxy @API) (server var)

server :: TVar [ListItem] -> Server API
server v = getItems v :<|> addItem v :<|> toggleItem v

defaultInitialItems :: [ListItem]
defaultInitialItems
  = [ ListItem "lunch"    "Have lunch"           True
    , ListItem "workshop" "Give a Miso workshop" False
    ]

getItems :: TVar [ListItem] -> Handler [ListItem]
getItems
  = liftIO . readTVarIO

addItem :: TVar [ListItem] -> ListItem -> Handler NoContent
addItem var new
  = do liftIO $ atomically $ modifyTVar' var (<> [new])
       pure NoContent

toggleItem :: TVar [ListItem] -> String -> Handler ListItem
toggleItem var itemToToggle
  = do v <- liftIO $ atomically $ do
              store <- readTVar var
              let item = find (\li -> liId li == itemToToggle) store
              case item of
                Nothing
                  -> pure Nothing
                Just item'
                  -> do let newItem = item' { liDone = not (liDone item') }
                            newStore = map (\li -> if liId li == itemToToggle
                                                      then newItem else li) store
                        writeTVar var newStore
                        pure (Just newItem)
       case v of
         Nothing   -> throwError $ err404 { errBody = "item not found" }
         Just item -> pure item
      