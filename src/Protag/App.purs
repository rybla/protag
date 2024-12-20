module Protag.App where

import Prelude

import Control.Monad.State (get)
import Data.Argonaut.Decode (fromJsonString, printJsonDecodeError)
import Data.Argonaut.Encode (toJsonString)
import Data.Either (Either(..))
import Data.Int as Int
import Data.Lens (Lens', set, to, (%=), (.=), (^.))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Variant (case_)
import Effect.Aff (Aff)
import Effect.Class.Console as Console
import Halogen as H
import Halogen.HTML (HTML)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Protag.Common (SceneIndex(..), InputGameState)
import Protag.Game as Game
import Protag.Utility (inj, on, prop)
import Type.Prelude (Proxy(..))

example_game_state :: InputGameState
example_game_state =
  { player: { name: "Kellan Veylor", health: 20 }
  , scene_index: IntroSceneIndex
  }

component :: forall query input output. H.Component query input output Aff
component = H.mkComponent { initialState, eval, render }
  where
  initialState _ =
    -- { mode: inj @"init" unit }
    { mode: inj @"playGame" example_game_state }

  eval = H.mkEval H.defaultEval
    { handleAction = case_
        # on @"newCharacter" (const do prop @"mode" .= inj @"newCharacter" unit)
        # on @"loadGame" (const do prop @"mode" .= inj @"loadGame" unit)
        # on @"playGame" (\game -> do prop @"mode" .= inj @"playGame" game)
    }

  render state =
    HH.div
      [ HP.style "margin: auto; height: 700px; width: 800px; box-shadow: 0 0 5px 5px rgba(0, 0, 0, 0.5); display: flex; flex-direction: column;" ]
      [ HH.div
          [ HP.style "font-size: 1.5em; font-variant: small-caps; text-align: center; background-color: rgba(172, 145, 118, 0.5)" ]
          [ HH.text "Protag" ]
      , case_
          # on @"init"
              ( const $
                  HH.div
                    [ HP.style "display: flex; flex-direction: column; gap: 1em;" ]
                    [ HH.button
                        [ HE.onClick $ const $ inj @"newCharacter" unit ]
                        [ HH.text "new" ]
                    , HH.button
                        [ HE.onClick $ const $ inj @"loadGame" unit ]
                        [ HH.text "load" ]
                    ]
              )
          # on @"newCharacter" (const $ HH.slot (Proxy @"createCharacter") unit createCharacter_component unit (inj @"playGame"))
          # on @"loadGame" (const $ HH.slot (Proxy @"loadGame") unit loadGame_component unit (inj @"playGame"))
          # on @"playGame" (\game_state -> HH.slot_ (Proxy @"playGame") unit Game.component { game_state })
          $ state.mode
      ]

createCharacter_component :: forall query input. H.Component query input InputGameState Aff
createCharacter_component = H.mkComponent { initialState, eval, render }
  where
  initialState _ =
    { game_state: example_game_state }

  eval = H.mkEval H.defaultEval
    { handleAction = case_
        # on @"modify_game_state"
            ( \f -> do
                Console.log $ "[App.createCharacter.modify_game_state]"
                prop @"game_state" %= f
            )
        # on @"submit"
            ( const do
                state <- get
                H.raise state.game_state
            )
    }

  render state =
    let
      renderInput :: forall a. String -> Lens' InputGameState a -> (a -> String) -> (String -> a) -> HTML _ _
      renderInput label l toString fromString =
        HH.div
          [ HP.style "display: flex; flex-direction: row; gap: 0.5em;" ]
          [ HH.div
              [ HP.style "font-weight: bold;" ]
              [ HH.text label ]
          , HH.input
              [ HP.value (state.game_state ^. l <<< to toString)
              , HE.onValueInput (\x -> inj @"modify_game_state" (set l (x # fromString)))
              ]
          ]
    in
      HH.div
        [ HP.style "display: flex; flex-direction: column; gap: 0.5em; align-items: center" ]
        [ renderInput "player name" (prop @"player" <<< prop @"name") identity identity
        , renderInput "player health" (prop @"player" <<< prop @"health") show (Int.fromString >>> fromMaybe 0)
        , HH.div
            []
            [ HH.text (toJsonString state.game_state) ]
        , HH.div
            []
            [ HH.button [ HE.onClick $ const $ inj @"submit" unit ] [ HH.text "Submit" ] ]
        ]

loadGame_component :: forall query input. H.Component query input InputGameState Aff
loadGame_component = H.mkComponent { initialState, eval, render }
  where
  initialState _ =
    { game_state_string: ""
    , mb_status: Nothing @String
    }

  eval = H.mkEval H.defaultEval
    { handleAction = case_
        # on @"submit"
            ( const do
                state <- get
                case fromJsonString state.game_state_string :: _ InputGameState of
                  Left err -> prop @"mb_status" .= pure (printJsonDecodeError err)
                  Right game_state -> H.raise game_state
            )
    }

  render state =
    HH.div
      [ HP.style "display: flex; flex-direction: column; gap: 0.5em; align-items: center" ]
      [ HH.textarea
          [ HP.value state.game_state_string ]
      , HH.button
          [ HE.onClick $ const $ inj @"submit" unit ]
          [ HH.text "Submit" ]
      , HH.div_ case state.mb_status of
          Nothing -> []
          Just status -> [ HH.text status ]
      ]

