{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE DataKinds             #-}
module Godot.Extra.Types where

import           Godot.Gdnative.Types
import           Godot.Gdnative.Internal
import           Godot.Internal.Dispatch
import qualified Godot.Methods                 as G
import           Data.Text                     as T
import           Godot.Extra.Prelude


-- | Type to help with type conversions
data HighLevelOf low where
  High
    :: forall low high. (GodotFFI low high, high ~ (TypeOf 'HaskellTy low))
    => high
    -> HighLevelOf low


withHigh
  :: forall low high high'
   . (GodotFFI low high, high ~ (TypeOf 'HaskellTy low))
  => (high -> high')
  -> HighLevelOf low
  -> high'
withHigh f (High high) = f high


asClass
  :: (GodotObject :< a, a :< b)
  => (GodotObject -> b)
  -> Text
  -> a
  -> IO (Maybe b)
asClass constr cls a = do
  isClass' <- a `isClass` cls
  return $ if isClass' then Just $ constr $ safeCast a else Nothing


asClass'
  :: (GodotObject :< a, a :< b) => (GodotObject -> b) -> Text -> a -> IO b
asClass' constr cls a = asClass constr cls a >>= \case
  Just a' -> return a'
  Nothing -> error $ "Could not cast to " `append` cls


asObj :: (GodotObject :< a) => a -> GodotObject
asObj a = safeCast a


isClass :: GodotObject :< a => a -> Text -> IO Bool
isClass obj cls = do
  clsStr <- toLowLevel cls
  G.is_class (asObj obj) clsStr


-- If at some point all Godot types gets an instance of GodotClass, this would be
-- preferable to the current impl.
{-
 -asClass
 -  :: forall a b
 -   . (GodotClass a, GodotClass b, GodotObject :< a, a :< b)
 -  => (GodotObject -> b)
 -  -> a
 -  -> IO (Maybe b)
 -asClass constr a = do
 -  let obj = safeCast a :: GodotObject
 -  isSubclass <- G.is_class obj #<< (pack $ godotClassName @b)
 -  return $ if isSubclass then Just $ constr obj else Nothing
 -}
