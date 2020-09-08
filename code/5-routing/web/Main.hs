{-# language NamedFieldPuns    #-}
{-# language OverloadedStrings #-}
{-# language RecordWildCards   #-}
{-# language DeriveGeneric     #-}
{-# language DeriveAnyClass    #-}
{-# language MultiWayIf        #-}

module Main where

import System.Random
import Data.Aeson
import Data.Time.LocalTime
import GHC.Generics

import Miso
import Miso.Effect.Storage
import Miso.String (MisoString, toMisoString, null)

main :: IO ()
main = do
  currentUri <- getCurrentURI
  startApp App { model = Model currentUri [] True "", .. }
  where
    initialAction = LoadItems
    update        = updateModel
    view          = viewModel
    events        = defaultEvents
    subs          = [ uriSub ChangeURI ]
    mountPoint    = Nothing
    logLevel      = Off

data ListItem
  = ListItem {
      liId   :: MisoString
    , liText :: MisoString
    , liDone :: Bool
    }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data Model
  = Model {
      currentUri :: URI
    , items :: [ListItem]
    , isLoading :: Bool
    , newItemText :: MisoString
    }
  deriving (Show, Eq) 

localStorageKey :: MisoString
localStorageKey = "items"

defaultInitialItems :: [ListItem]
defaultInitialItems
  = [ ListItem "lunch"    "Have lunch"           True
    , ListItem "workshop" "Give a Miso workshop" False
    ]

data Action
  = None
  | ToggleState       { toggleLiId   :: MisoString }
  | ChangeNewItemText { newText      :: MisoString }
  | AddToDoClick 
  | AddToDo           { newListItem  :: ListItem }
  | LoadItems
  | SetInitialItems   { initialItems :: [ListItem] }
  | ChangeURI         { newURI       :: URI }
  deriving (Show, Eq)

updateModel :: Action -> Model -> Effect Action Model
updateModel None m = noEff m
updateModel (ToggleState toggleLiId) m
  = let new = map (\li@ListItem { .. }
                     -> if liId == toggleLiId
                           then li { liDone = not liDone }
                           else li ) (items m)
    -- in noEff new
    in m { items = new } <# updateLocalStorage new
updateModel (ChangeNewItemText new) m
  = noEff $ m { newItemText = new }
updateModel AddToDoClick m@Model { newItemText }
  = m { newItemText = "" } <# do
      tme <- toMisoString . show <$> getZonedTime
      pure $ AddToDo $ ListItem { liId = tme, liText = newItemText, liDone = False }
updateModel (AddToDo li) m
  = let new = items m <> [li] in
    m { items = new } <# updateLocalStorage new
updateModel LoadItems m
  = m <# do store <- getLocalStorage localStorageKey
            case store of
              Left  _ -> pure $ SetInitialItems defaultInitialItems
              Right i -> pure $ SetInitialItems i
updateModel (SetInitialItems items) m
  = noEff $ m { items, isLoading = False }
updateModel (ChangeURI newUri) m
  = noEff $ m { currentUri = newUri }

updateLocalStorage :: [ListItem] -> JSM Action
updateLocalStorage new = do
  setLocalStorage localStorageKey new
  pure None

bootstrapUrl :: MisoString
bootstrapUrl = "https://stackpath.bootstrapcdn.com/bootstrap/4.5.2/css/bootstrap.min.css"

viewModel :: Model -> View Action
viewModel m
  = div_ [ class_ "container"]
         [ header m
         , if | uriFragment (currentUri m) == "#about"
              -> p_ [ class_ "lead" ]
                    [ text "Built for MuniHac 2020" ]
              | isLoading m
              -> div_ [ class_ "spinner-border text-warning" ] [ ]
              | otherwise
              -> ul_ [ class_ "list-group" ] (map viewListItem (items m))
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
         , a_ [ class_ "nav-link text-light", href_ "#about" ]
              [ text "About" ]
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