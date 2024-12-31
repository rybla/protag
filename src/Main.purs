module Main where

import Prelude

import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver as HVD
import Protag.App as Protag.App
import Protag.Game.Story1 as Story1

main :: Effect Unit
main = HA.runHalogenAff (HVD.runUI Protag.App.component { game: Story1.game } =<< HA.awaitBody)

