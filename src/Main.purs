module Main where

import Prelude

import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver as HVD
import Protag.Component.Game as Protag.Component.Game
import Protag.Impl.Game (impl)

main :: Effect Unit
main = HA.runHalogenAff (HVD.runUI Protag.Component.Game.component { impl } =<< HA.awaitBody)

