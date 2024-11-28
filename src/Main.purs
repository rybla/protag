module Main where

import Prelude

import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver as HVD
import Protag.Component.Game as Protag.Component.Game
import Protag.Impl.Game (game_impl)

main :: Effect Unit
main = do
  let component = Protag.Component.Game.component game_impl
  HA.runHalogenAff (HVD.runUI component {} =<< HA.awaitBody)

