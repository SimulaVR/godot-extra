{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Godot.Extra.Instances where

import qualified Godot.Gdnative.Internal.Api   as G
import           Godot.Gdnative.Internal
import           Godot.Gdnative.Types

import           Godot.Extra.Prelude


-- We need these instances for functions that requires them

type instance TypeOf 'HaskellTy Bool = Bool
instance GodotFFI Bool Bool where
  fromLowLevel = return
  toLowLevel = return

type instance TypeOf 'HaskellTy Float = Float
instance GodotFFI Float Float where
  fromLowLevel = return
  toLowLevel = return

type instance TypeOf 'HaskellTy Int = Int
instance GodotFFI Int Int where
  fromLowLevel = return
  toLowLevel = return

type instance TypeOf 'HaskellTy () = ()
instance GodotFFI () () where
  fromLowLevel = return . const ()
  toLowLevel = return . const ()

type instance TypeOf 'HaskellTy GodotObject = GodotObject
instance GodotFFI GodotObject GodotObject where
  fromLowLevel = return
  toLowLevel = return

-- This should ideally be `[Variant 'HaskellTy]`, but that would
-- require `AsVariant` to handle both `LibType`s.
type instance TypeOf 'HaskellTy GodotArray = [GodotVariant]
instance GodotFFI GodotArray [GodotVariant] where
  fromLowLevel vs = do
    size <- fromIntegral <$> G.godot_array_size vs
    let maybeNext n v =
          if n == (size - 1)
          then Nothing
          else Just (v, n + 1)
    let variantAt n =
          maybeNext n <$> (G.godot_array_get vs n)
    unfoldrM variantAt 0

  toLowLevel vs = do
    array <- G.godot_array_new
    mapM_ (G.godot_array_append array) vs
    return array
