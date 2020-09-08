{-# language NamedFieldPuns    #-}
{-# language OverloadedStrings #-}
{-# language RecordWildCards   #-}
{-# language DeriveGeneric     #-}
{-# language DeriveAnyClass    #-}
{-# language DataKinds         #-}
{-# language TypeApplications  #-}
{-# language TypeOperators     #-}

module Main where

import System.Random
import Data.Aeson
import Data.Proxy
import Data.Time.LocalTime
import GHC.Generics

import Servant.API
import Servant.Client.Ghcjs
import Servant.Client.Internal.XhrClient

import Miso
import Miso.Effect.Storage
import Miso.String (MisoString, fromMisoString, toMisoString, null)

main :: IO ()
main = startApp App { .. }
  where
    initialAction = LoadItems
    model         = Model [] Loading ""
    update        = updateModel
    view          = viewModel
    events        = defaultEvents
    subs          = []
    mountPoint    = Nothing
    logLevel      = Off

data ListItem
  = ListItem {
      liId   :: MisoString
    , liText :: MisoString
    , liDone :: Bool
    }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data LoadStatus
  = Loading | LoadError ClientError | Finished
  deriving (Show, Eq)

data Model
  = Model {
      items :: [ListItem]
    , loadStatus :: LoadStatus
    , newItemText :: MisoString
    }
  deriving (Show, Eq) 

type API =    "items"  :> Get '[JSON] [ListItem]
         :<|> "items"  :> ReqBody '[JSON] ListItem :> Post '[JSON] NoContent
         :<|> "toggle" :> Capture "liId" String :> Post '[JSON] ListItem

-- generate the client
getItems   :: ClientM [ListItem]
addItem    :: ListItem -> ClientM NoContent
toggleItem :: String -> ClientM ListItem
getItems :<|> addItem :<|> toggleItem = client (Proxy @API)

-- server where to connect
clientEnv :: ClientEnv
clientEnv = ClientEnv (BaseUrl Http "localhost" 8080 "")

data Action
  = None
  | ToggleState       { toggleLiId   :: MisoString }
  | ChangeNewItemText { newText      :: MisoString }
  | AddToDoClick 
  | AddToDo           { newListItem  :: ListItem }
  | LoadItems
  | SetInitialItems   { initialItems :: [ListItem] }
  | SetLoadError      { loadError    :: ClientError }
  deriving (Show, Eq)

updateModel :: Action -> Model -> Effect Action Model
updateModel None m = noEff m
updateModel (ToggleState toggleLiId) m
  = let new = map (\li@ListItem { .. }
                     -> if liId == toggleLiId
                           then li { liDone = not liDone }
                           else li ) (items m)
    in m { items = new } <# do
      runClientMOrigin (toggleItem $ fromMisoString toggleLiId) clientEnv
      pure None
updateModel (ChangeNewItemText new) m
  = noEff $ m { newItemText = new }
updateModel AddToDoClick m@Model { newItemText }
  = m { newItemText = "" } <# do
      tme <- toMisoString . show <$> getZonedTime
      let li = ListItem { liId = tme, liText = newItemText, liDone = False }
      runClientMOrigin (addItem li) clientEnv
      pure $ AddToDo li
updateModel (AddToDo li) m
  = noEff $ m { items = items m <> [li] } 
updateModel LoadItems m
  = m <# do store <- runClientMOrigin getItems clientEnv
            case store of
              Left  e -> pure $ SetLoadError e
              Right i -> pure $ SetInitialItems i
updateModel (SetInitialItems items) m
  = noEff $ m { items, loadStatus = Finished }
updateModel (SetLoadError e) m
  = noEff $ m { loadStatus = LoadError e }

bootstrapUrl :: MisoString
bootstrapUrl = "https://stackpath.bootstrapcdn.com/bootstrap/4.5.2/css/bootstrap.min.css"

viewModel :: Model -> View Action
viewModel m
  = div_ [ class_ "container"]
         [ header m
         , case loadStatus m of
             Loading     -> div_ [ class_ "spinner-border text-warning" ] [ ]
             LoadError e -> div_ [ class_ "alert alert-danger" ]
                                 [ span_ [ class_ "badge badge-danger" ] [ text "Error" ]
                                 , text " "
                                 , text (toMisoString $ show e) ]
             Finished    -> ul_ [ class_ "list-group" ] (map viewListItem (items m))
         , link_ [ rel_ "stylesheet"
                 , href_ bootstrapUrl ] ]

viewListItem :: ListItem -> View Action
viewListItem ListItem { .. }
  = li_ [ class_ "list-group-item"]
        [ div_  [ class_ "custom-control custom-checkbox"]
                [ input_ [ class_   "custom-control-input"
                         , type_    "checkbox"
                         , value_   "on"
                         , checked_ liDone
                         , id_      liId
                         , onChange (\_ -> ToggleState liId) ]
                , label_ [ class_ (if liDone
                                      then "custom-control-label text-muted"
                                      else "custom-control-label")
                         , for_   liId ]
                         [ text liText ]] ]

header :: Model -> View Action
header Model { newItemText }
  = nav_ [ class_ "navbar navbar-dark bg-dark"]
         [ h2_ [ class_ "bd-title text-light" ]
               [ text "To-do "
               , span_ [ class_ "badge badge-warning"]
                       [ text "in miso!"] ]
         , form_ [ class_ "form-inline" ]
                 [ input_  [ class_       "form-control mr-sm-2"
                           , type_        "text" 
                           , placeholder_ "Do this" 
                           , value_       newItemText 
                           , onChange     ChangeNewItemText ]
                 , button_ [ class_   "btn btn-outline-warning"
                           , type_    "button"
                           , onClick  AddToDoClick
                           , disabled_ (Miso.String.null newItemText) ]
                           [ text "New to-do" ] ] ]