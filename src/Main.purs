module Main where

import Prelude

import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver as HVD
import Protag.Game as Protag.Game

main :: Effect Unit
main = HA.runHalogenAff (HVD.runUI Protag.Game.component {} =<< HA.awaitBody)

