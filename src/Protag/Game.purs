module Protag.Game where

import Prelude

import Halogen as H
import Halogen.HTML as HH

component = H.mkComponent { initialState, eval, render }
  where
  initialState {} = {}
  eval = H.mkEval H.defaultEval
  render {} = HH.div [] [ HH.text "[Game]" ]

