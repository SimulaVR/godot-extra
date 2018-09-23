{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds             #-}
module Godot.Extra.Util where

import           Universum.Monad.Maybe

import           Godot.Api
import qualified Godot.Gdnative.Internal.Api   as Api
import qualified Godot.Methods                 as G
import           Godot.Gdnative.Types
import           Godot.Gdnative.Types
import           Godot.Internal.Dispatch                  ( (:<)
                                                          , safeCast
                                                          )
import           Godot.Gdnative.Internal                  ( GodotNodePath
                                                          , GodotObject
                                                          )

import           Godot.Extra.Prelude
import           Godot.Extra.Types


godotPrint :: Text -> IO ()
godotPrint str = Api.godot_print =<< toLowLevel str


instance' :: (GodotObject :< a) => (GodotObject -> a) -> Text -> IO (Maybe a)
instance' constr className = do
  classDB <- getClassDB
  vt      <- (G.instance' classDB =<< toLowLevel className) >>= fromLowLevel
  case fromVariant vt :: Maybe GodotObject of
    Just obj -> asClass constr className obj
    Nothing  -> return Nothing


unsafeInstance :: (GodotObject :< a) => (GodotObject -> a) -> Text -> IO a
unsafeInstance constr className = instance' constr className >>= \case
  Just a  -> return a
  Nothing -> error $ "Could not instance " `mappend` className


load :: (GodotResource :< a) => (GodotObject -> a) -> Text -> Text -> IO a
load constr clsName url = do
  rl       <- getSingleton Godot_ResourceLoader "ResourceLoader"
  url'     <- toLowLevel url
  clsName' <- toLowLevel clsName
  res      <- G.load rl url' clsName' False
  res & asClass constr clsName >>= \case
    Just a -> return a
    Nothing ->
      error $ unwords ["Could not instantiate ", url, " as a ", clsName]


newNS :: (GodotObject :< a)
  => (GodotObject -> a) -> Text -> [Variant 'GodotTy] -> Text -> IO a
newNS constr clsName args url = do
  load GodotNativeScript "NativeScript" url
    >>= (flip G.new args :: GodotNativeScript -> IO GodotObject)
    >>= asClass' constr clsName


sceneInstance :: (GodotNode :< a)
  => Int -> (GodotObject -> a) -> Text -> Text -> IO a
sceneInstance genEditState constr clsName url =
  load GodotPackedScene "PackedScene" url
    >>= flip G.instance' genEditState
    >>= asClass' constr clsName


-- | Convenience function for moving a node from one parent to another.
-- If the node is a spatial, the global transform is transferred.
reparent :: GodotNode -> GodotNode -> IO ()
reparent node newParent = do
  mTf <-
    node & asClass GodotSpatial "Spatial" >>= maybeMapM G.get_global_transform
  parent <- G.get_parent node
  G.remove_child parent (safeCast node)
  G.add_child newParent (safeCast node) True
  whenJust mTf $ \tf -> do
    spat <- node & asClass' GodotSpatial "Spatial"
    G.set_global_transform spat tf


getNode :: (GodotNode :< a) => a -> NodePath -> IO (Maybe GodotNode)
getNode self np = do
  np'     <- toLowLevel np :: IO GodotNodePath
  hasNode <- (safeCast self :: GodotNode) `G.has_node` np'
  if hasNode
    then Just <$> G.get_node (safeCast self :: GodotNode) np'
    else return Nothing


getEngine :: IO Godot_Engine
getEngine = getSingleton Godot_Engine "Engine"


getClassDB :: IO Godot_ClassDB
getClassDB = getSingleton Godot_ClassDB "ClassDB"


getSingleton :: (GodotObject :< b) => (GodotObject -> b) -> Text -> IO b
getSingleton constr name = do
  engine <- getEngine
  name' <- toLowLevel name
  b <- G.has_singleton engine name'
  if b
    then G.get_singleton engine name' >>= asClass' constr name
    else error $ "No singleton named " `mappend` name


unref :: (GodotReference :< a) => a -> IO ()
unref ref = whenM (G.unreference ref') $ do
  clsName <- G.get_class ref' >>= fromLowLevel
  godotPrint $! fold ["Unreferencing: ", clsName]
  Api.godot_object_destroy $! obj
  where ref'@(GodotReference obj) = safeCast ref


andUnref :: (GodotReference :< a) => (a -> IO b) -> a -> IO b
andUnref f ref = do
  !result <- f ref
  unref ref
  return result

clamp :: (Floating a, Ord a) => a -> a -> a -> a
clamp min' max' val = max min' $ min max' val
