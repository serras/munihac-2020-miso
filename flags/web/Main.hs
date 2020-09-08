{-# language NamedFieldPuns    #-}
{-# language OverloadedStrings #-}
{-# language RecordWildCards   #-}
{-# language OverloadedLists   #-}
{-# language DeriveGeneric     #-}
{-# language DeriveAnyClass    #-}
{-# language DataKinds         #-}
{-# language TypeApplications  #-}
{-# language TypeOperators     #-}

module Main where

import Data.Aeson (FromJSON(..))
import Data.List (find)
import Data.Proxy
import GHC.Generics

import Servant.API
import Servant.Client.Ghcjs
import Servant.Client.Internal.XhrClient

import Miso
import Miso.String (MisoString)

main :: IO ()
main = startApp App { .. }
  where
    initialAction = Load
    model         = Model Loading Nothing
    update        = updateModel
    view          = viewModel
    events        = defaultEvents
    subs          = []
    mountPoint    = Nothing
    logLevel      = Off

data Country
  = Country {
      name :: MisoString
    , flag :: MisoString
    }
  deriving (Show, Eq, Generic, FromJSON)

data LoadStatus
  = Loading
  | Loaded [Country]
  | Error
  deriving (Show, Eq)

data Model
  = Model { 
      loadStatus      :: LoadStatus
    , selectedCountry :: Maybe MisoString
    }
  deriving (Show, Eq)

type API = "all"  :> Get '[JSON] [Country]
getCountries :: ClientM [Country]
getCountries = client (Proxy @API)
clientEnv :: ClientEnv
clientEnv = ClientEnv (BaseUrl Http "restcountries.eu" 8080 "/rest/v2")

data Action
  = None
  | Load
  | LoadError
  | LoadFinished [Country]
  | CountrySelected MisoString
  deriving (Show, Eq)

updateModel :: Action -> Model -> Effect Action Model
updateModel None m = noEff m
updateModel Load m
  = m <# do store <- runClientMOrigin getCountries clientEnv
            case store of
              Left  _ -> pure LoadError
              Right i -> pure $ LoadFinished i
updateModel LoadError m
  = noEff $ m { loadStatus = Error }
updateModel (LoadFinished countries) m
  = noEff $ m { loadStatus = Loaded countries }
updateModel (CountrySelected c) m
  = m { selectedCountry = Just c } <# do consoleLog c >> pure None

bootstrapUrl :: MisoString
bootstrapUrl = "https://stackpath.bootstrapcdn.com/bootstrap/4.5.2/css/bootstrap.min.css"

viewModel :: Model -> View Action
viewModel m
  = div_ [ class_ "container"]
         [ headerView m
         , flagView m
         , link_ [ rel_ "stylesheet"
                 , href_ bootstrapUrl ] ]

headerView :: Model -> View Action
headerView m
  = nav_ [ class_ "navbar navbar-dark bg-dark"]
         [ h2_ [ class_ "bd-title text-light" ]
               ( [ text "Flags of the World "
                 , span_ [ class_ "badge badge-warning"]
                         [ text "in miso!"] ]
               <> extra (loadStatus m) ) ]
  where
    extra :: LoadStatus -> [View Action]
    extra Loading
      = [ span_ [ style_ [("margin-left", "10px")] ]
                [ span_ [ class_ "spinner-border text-warning" ] [ ] ] ]
    extra (Loaded countries)
      = [ form_ [ class_ "form-inline"
                , style_ [("margin-left", "10px"), ("display", "inline")] ] 
                [ select_ [ class_ "custom-select"
                          , onChange CountrySelected ]
                          (flip map countries $ \c ->
                             option_ [ ] [ text (name c) ]) ] ]
    extra Error
      = [ span_ [ class_ "badge badge-warning"] [ text "error" ] ]

flagView :: Model -> View Action
flagView (Model (Loaded countries) (Just selectedCountry))
  | Just c <- find ((== selectedCountry) . name) countries
  = div_ [ style_ [("margin", "20px")]]
         [ div_ [ class_ "row justify-content-around align-items-center" ]
                [ img_ [ src_ (flag c) ] ] ]
flagView _ = div_ [] []