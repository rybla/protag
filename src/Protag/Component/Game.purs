module Protag.Component.Game where

import Prelude
import Protag.Common

import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

component :: H.Component GameComponentQuery GameComponentInput GameComponentOutput Aff
component = H.mkComponent { initialState, eval, render }
  where
  initialState input =
    { impl: input.impl
    , state: input.impl.initialState
    }

  eval = H.mkEval H.defaultEval

  render { impl, state } =
    HH.div
      [ HP.classes [ HH.ClassName "Game" ] ]
      [ impl.render state ]

