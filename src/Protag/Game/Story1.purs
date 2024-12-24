module Protag.Game.Story1 where

import Prelude
import Protag.Language

import Data.Array as Array
import Data.Foldable (sequence_)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Protag.Common (GameHTML, InputGameState, GameState)
import Protag.Utility (inj, on)
import Protag.Variant (case_)
import Type.Prelude (Proxy(..))

game_params = { story, renderScene }
game_input = { inputGameState }

--------------------------------------------------------------------------------

type Scenes = ("intro" :: Unit)

inputGameState :: InputGameState Scenes
inputGameState =
  { player: { name: "Kellan Veylor", health: 20 }
  , scene: inj @"intro" unit
  }

renderScene :: GameState Scenes -> GameHTML Scenes
renderScene state =
  case_
    # on @"intro"
        ( \_ ->
            HH.div
              []
              [ HH.img
                  [ HP.src "/assets/approaching_snowy_town.png"
                  , HP.style "max-height: 100%; max-width: 100%;"
                  ]
              ]
        )
    $ state.scene

story :: Instruction Scenes Unit
story = do
  print $ HH.text "print1"
  print $ HH.text "print2"
  print $ HH.text "print3"
  let
    render_opt = case_
      # on @"A" (\_ -> HH.text "this is option A")
      # on @"B" (\_ -> HH.text "this is option B")
  sequence_ $ Array.replicate 4 do
    opt <- choice (HH.text "choose A or B") Proxy render_opt
    print $ HH.text $ "you chose option " <> show opt
  reply <- prompt $ HH.text "what is your name?"
  print $ HH.text $ "your name is: " <> show reply
  pure unit

