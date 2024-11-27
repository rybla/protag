module Protag.Impl.Game where

import Prelude

import Halogen.HTML as HH
import Protag.Common (GameImpl)

impl :: GameImpl
impl =
  { initialState:
      { title: "Protag"
      }
  , render: \state ->
      HH.div []
        [ HH.h1 [] [ HH.text state.title ] ]
  }

