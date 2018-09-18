module Godot.Extra.Debug where

import           Godot.Extra.Prelude
import           Godot.Extra.Util

debugPrint :: Show a => Text -> a -> IO a
debugPrint msg a = do
  godotPrint $ msg `mappend` (pack $ show a)
  return a
