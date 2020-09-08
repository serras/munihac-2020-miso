{-# language NamedFieldPuns    #-}
{-# language OverloadedStrings #-}
{-# language RecordWildCards   #-}

module Main where

import System.Random
import Data.Time.LocalTime

import Miso
import Miso.String (MisoString, toMisoString)

main :: IO ()
main = startApp App { .. }
  where
    initialAction = None
    model         = initialItems
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
  deriving (Show, Eq)

type Model = [ListItem]

initialItems :: Model
initialItems
  = [ ListItem "lunch"    "Have lunch"           True
    , ListItem "workshop" "Give a Miso workshop" False
    ]

data Action
  = None
  | ToggleState { toggleLiId  :: MisoString }
  | RandomToDo
  | AddToDo     { newListItem :: ListItem }
  deriving (Show, Eq)

updateModel :: Action -> Model -> Effect Action Model
updateModel None m = noEff m
updateModel (ToggleState toggleLiId) m
  = let new = map (\li@ListItem { .. }
                     -> if liId == toggleLiId
                           then li { liDone = not liDone }
                           else li ) m
    -- in noEff new
    in new <# do consoleLog ("toggled " <> toggleLiId) >> pure None
updateModel RandomToDo m
  = m <# do txt <- (["hey", "ho", "miso"] !!) <$> randomRIO (0, 2)
            tme <- toMisoString . show <$> getZonedTime
            let liId   = tme
                liText = txt <> " at " <> tme
            pure $ AddToDo $ ListItem { liDone = False, .. }
updateModel (AddToDo li) m
  = noEff (m <> [li])

bootstrapUrl :: MisoString
bootstrapUrl = "https://stackpath.bootstrapcdn.com/bootstrap/4.5.2/css/bootstrap.min.css"

viewModel :: Model -> View Action
viewModel m
  = div_ [ class_ "container"]
         [ header
         , ul_ [ class_ "list-group" ] (map viewListItem m)
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

header :: View Action
header
  = nav_ [ class_ "navbar navbar-dark bg-dark"]
         [ h2_ [ class_ "bd-title text-light" ]
               [ text "To-do "
               , span_ [ class_ "badge badge-warning"]
                       [ text "in miso!"] ]
         , form_ [ class_ "form-inline" ]
                 [ button_ [ class_  "btn btn-outline-warning"
                           , type_   "button"
                           , onClick RandomToDo ]
                 [ text "New (random) to-do" ] ] ]