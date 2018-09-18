{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
module Godot.Extra.Properties where

import           Godot.Methods                 as G
import           Godot.Api
import           Godot.Gdnative.Internal                  ( GodotString
                                                          , GodotVariant
                                                          , GodotObject
                                                          )
import           Godot.Gdnative.Types          as G

import           Godot.Extra.Prelude
import           Godot.Extra.Variants
import           Godot.Extra.Util
import           Godot.Extra.Types
import           Godot.Extra.Instances                    ( )


{-
 -data Property low where
 -  (:.)
 -    :: forall low high. (Typeable low, AsVariant low, GodotFFI low high, high ~ (TypeOf 'HaskellTy low))
 -    => GodotObject
 -    -> Text
 -    -> Property low
 -}


-- | Get the value of an objects property
get
  :: forall low high
   . ( Typeable low
     , AsVariant low
     , GodotFFI low high
     , high ~ TypeOf 'HaskellTy low
     )
  => Text
  -> GodotObject
  -> IO (Maybe (HighLevelOf low))
get prop obj = do
  whenMaybeM (propExists obj prop) $ do
    vt <- VT <$> (G.get obj =<< G.toLowLevel prop) :: IO (VariantOf low)
    highFromVariant True vt


propExists :: GodotObject -> Text -> IO Bool
propExists obj prop = do
  classDB <- getSingleton Godot_ClassDB "ClassDB"
  clsName <- get_class obj :: IO GodotString
  G.class_get_property_list classDB clsName False
    >>= G.fromLowLevel
    >>= anyM isProp
 where
  isProp :: GodotVariant -> IO Bool
  isProp vt = do
    (High res) <- highFromVariant True (VT vt) :: IO (HighLevelOf GodotString)
    return (res == prop)
