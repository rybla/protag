module Protag.App where

import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Protag.Common (Game)
import Protag.Game as Game
import Type.Prelude (Proxy(..))

component :: forall query output scenes state. H.Component query { game :: Game scenes state } output Aff
component = H.mkComponent { initialState, eval, render }
  where
  initialState { game } =
    { game
    , game_slotId: 0
    }

  eval = H.mkEval H.defaultEval

  render state =
    HH.div
      [ HP.style "margin: auto; height: 800px; width: 800px; box-shadow: 0 0 5px 5px rgba(0, 0, 0, 0.5); display: flex; flex-direction: column;" ]
      [ HH.div
          [ HP.style "font-size: 1.5em; font-variant: small-caps; text-align: center; background-color: rgba(172, 145, 118, 0.5)" ]
          [ HH.text "Protag" ]
      , HH.slot_ (Proxy @"game") state.game_slotId (Game.component state.game.params) state.game.input
      ]
