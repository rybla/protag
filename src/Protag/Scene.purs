module Protag.Scene where

import Prelude

import Control.Monad.Trans.Class (lift)
import Data.Array as Array
import Data.Lens ((%=), (.=))
import Data.String as String
import Effect.Class.Console as Console
import Halogen.HTML as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Protag.Common (SceneComponent, SceneIndex(..))
import Protag.Utility (prop, todo)

getSceneComponent :: SceneIndex -> SceneComponent
getSceneComponent = todo "getSceneComponent"
