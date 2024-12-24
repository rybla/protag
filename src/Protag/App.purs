module Protag.App where

import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Protag.Game as Game
import Protag.Game.Story1 as Story1
import Type.Prelude (Proxy(..))

component :: forall query input output. H.Component query input output Aff
component = H.mkComponent { initialState, eval, render }
  where
  initialState _input = {}

  eval = H.mkEval H.defaultEval

  render _state =
    HH.div
      [ HP.style "margin: auto; height: 800px; width: 800px; box-shadow: 0 0 5px 5px rgba(0, 0, 0, 0.5); display: flex; flex-direction: column;" ]
      [ HH.div
          [ HP.style "font-size: 1.5em; font-variant: small-caps; text-align: center; background-color: rgba(172, 145, 118, 0.5)" ]
          [ HH.text "Protag" ]
      , HH.slot_ (Proxy @"game") 0 (Game.component Story1.game_params) Story1.game_input
      ]
