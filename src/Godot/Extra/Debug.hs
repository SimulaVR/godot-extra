module Godot.Extra.Debug where

import           Godot.Extra.Prelude
import           Godot.Gdnative.Internal
import           Godot.Gdnative.Types
import qualified Godot.Gdnative.Internal.Api   as Api

debugPrint :: Show a => Text -> a -> IO a
debugPrint msg a = do
  Api.godot_print =<< (toLowLevel $ msg `mappend` (pack $ show a) :: IO GodotString)
  return a
