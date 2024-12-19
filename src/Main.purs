module Main where

import Prelude

import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver as HVD
import Protag.App as Protag.App

main :: Effect Unit
main = HA.runHalogenAff (HVD.runUI Protag.App.component {} =<< HA.awaitBody)

