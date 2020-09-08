{-# language NamedFieldPuns    #-}
{-# language OverloadedStrings #-}
{-# language RecordWildCards   #-}
{-# language OverloadedLists   #-}

module Main where

import Data.Foldable (asum)
import Data.List (transpose, (!!))
import Data.Map ()
import Data.Maybe (isJust)

import Miso
import Miso.String (MisoString)

main :: IO ()
main = startApp App { .. }
  where
    initialAction = None
    model         = Model { grid = emptyGrid }
    update        = updateModel
    view          = viewModel
    events        = defaultEvents
    subs          = []
    mountPoint    = Nothing
    logLevel      = Off

data Square
  = X | O
  deriving (Show, Eq)

type Grid = [[Maybe Square]]

emptyGrid, aGrid :: Grid
emptyGrid = replicate 3 (replicate 3 Nothing)
aGrid = [ [ Just X,  Nothing, Nothing ]
        , [ Nothing, Just O,  Nothing ]
        , replicate 3 Nothing ]

hasWinner :: Grid -> Maybe Square
hasWinner g
  = asum (map isWinnerRow thingToCheck)
  where
    thingToCheck
      = g ++ transpose g 
          ++ [ [g !! 0 !! 0, g !! 1 !! 1, g !! 2 !! 2]
             , [g !! 0 !! 2, g !! 1 !! 1, g !! 2 !! 0] ]
    isWinnerRow :: [Maybe Square] -> Maybe Square
    isWinnerRow row
      | all isJust row, all (== head row) row
      = head row
      | otherwise
      = Nothing

data Model
  = Model { grid :: Grid }
  deriving (Show, Eq)

data Action
  = None
  | ClickSquare Int Int
  deriving (Show, Eq)

updateModel :: Action -> Model -> Effect Action Model
updateModel None m
  = noEff m
updateModel (ClickSquare rowId colId) m
  = noEff m

bootstrapUrl :: MisoString
bootstrapUrl = "https://stackpath.bootstrapcdn.com/bootstrap/4.5.2/css/bootstrap.min.css"

viewModel :: Model -> View Action
viewModel m
  = div_ [ class_ "container"]
         [ headerView
         -- , newGameView m
         , contentView m
         -- , statsView m
         , link_ [ rel_ "stylesheet"
                 , href_ bootstrapUrl ] ]

headerView :: View Action
headerView
  = nav_ [ class_ "navbar navbar-dark bg-dark"]
         [ h2_ [ class_ "bd-title text-light" ]
               [ text "Tic-Tac-Toe "
               , span_ [ class_ "badge badge-warning"]
                       [ text "in miso!"] ] ]

newGameView :: Model -> View Action
newGameView _
  = nav_ [ class_ "navbar navbar-light bg-light"]
         [ form_ [ class_ "form-inline" ]
                 [ input_  [ class_       "form-control mr-sm-2"
                           , type_        "text" 
                           -- , value_    player1Name 
                           -- , onChange  undefined
                           , placeholder_ "Player 1" ]
                 -- , select_ [ class_       "custom-select"
                 --           , style_ [("margin-right", "15px")] ]
                 --           (flip map ["A", "B"] $ \option ->
                 --              option_ [ ] [ text option])
                 , input_  [ class_       "form-control mr-sm-2"
                           , type_        "text" 
                           -- , value_    player2Name 
                           -- , onChange  undefined
                           , placeholder_ "Player 2" ]
                 -- , select_ [ class_       "custom-select"
                 --           , style_ [("margin-right", "15px")] ]
                 --           (flip map ["A", "B"] $ \option ->
                 --              option_ [ ] [ text option])
                 , button_ [ class_       "btn btn-outline-warning"
                           , type_        "button"
                           -- , onClick   undefined
                           , disabled_    False ]
                           [ text "New game" ] ] ]

contentView :: Model -> View Action
contentView Model { .. }
  = div_ [ style_ [("margin", "20px")]]
         [ gridView grid
         , alertView "who is the winner?" ]

gridView :: Grid -> View Action
gridView grid
  = div_ [ style_ [("margin", "20px")]]
         [ div_ [ class_ "row justify-content-around align-items-center" ]
                [ h3_ [ ] [ text "Player 1"]
                , div_ [ style_ [("display", "inline-block")] ]
                       [ div_ [ style_ [ ("display", "grid")
                                       , ("grid-template-rows", "1fr 1fr 1fr")
                                       , ("grid-template-columns", "1fr 1fr 1fr")
                                       , ("grid-gap", "2px") ] ]
                               ( flip concatMap (zip [0 ..] grid) $ \(rowId, row) ->
                                   flip map (zip [0 ..] row) $ \(colId, sq) ->
                                     cell rowId colId sq )]
                , h3_ [ ] [ text "Player 2"] ] ]
  where
    cell :: Int -> Int -> Maybe Square -> View Action
    cell rowId colId square
      = div_ [ style_ [("width", "100px"), ("height", "100px")] ]
             [ button_ [ type_  "button"
                       , style_  [ ("width", "100%"), ("height", "100%")
                                 , ("font-size", "xxx-large") ]
                       , class_  "btn btn-outline-secondary"
                       , onClick (ClickSquare rowId colId) ]
                       [ text "·" ] ]

alertView :: MisoString -> View Action
alertView v
  = div_ [ class_ "alert alert-warning"
         , style_ [("text-align", "center")] ]
         [ h4_ [ class_ "alert-heading" ]
               [ text v ] ]

fakeStats :: [MisoString]
fakeStats
  = [ "A - B, won by A in 3 moves"
    , "Quijote - Sancho, won by Sancho in 5 moves" ]

statsView :: Model -> View Action
statsView _
  = div_ [ class_ "row justify-content-around align-items-center"
         , style_ [("margin-bottom", "20px")] ]
         [ ul_ [ class_ "list-group"]
               ( flip map fakeStats $ \elt ->
                   ul_ [ class_ "list-group-item" ] [ text elt ] ) ]