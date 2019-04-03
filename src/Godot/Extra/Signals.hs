{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
module Godot.Extra.Signals
  ( withGodotSignalArgumentArray
  , makeGodotSignal
  , registerGodotSignal
  , makeObjectSignalArgument
  )
where

import qualified Godot.Nativescript            as GDNS
import           Godot.Nativescript            as GDNS
                                                          ( GodotClass(..)
                                                          , GdnativeHandle
                                                          )
import           Godot.Gdnative.Internal
import           Godot.Gdnative.Types                     ( AsVariant
                                                          , toLowLevel
                                                          , toVariant
                                                          , Variant(..)
                                                          , LibType(..)
                                                          , fromLowLevel
                                                          )

import           Godot.Extra.Prelude

import qualified Data.Text                     as T

import Foreign
import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.Marshal.Utils
import Foreign.Storable


withGodotSignalArgumentArray :: [GDNS.SignalArgument] -> ((Ptr GodotSignalArgument, CInt) -> IO a) -> IO a
withGodotSignalArgumentArray listOfSignalArguments c_func = do
  listOfGodotSignalArguments <- mapM GDNS.asGodotSignalArgument listOfSignalArguments
  let sizeOfArray = (length listOfSignalArguments) * (sizeOf (undefined :: GodotSignalArgument))
  allocaBytes sizeOfArray $ \godotSignalArgumentArrayPtr ->
    withGodotSignalArguments listOfGodotSignalArguments 0 godotSignalArgumentArrayPtr c_func 
    where
      withGodotSignalArguments :: [GodotSignalArgument] -> Int -> Ptr GodotSignalArgument -> ((Ptr GodotSignalArgument, CInt) -> IO a) -> IO a

      -- Passing a non-empty list copies the list's head into the
      -- godotSignalArgumentArrayPtr (with offset gsArgIndex); we do this
      -- recursively until the list is empty.
      withGodotSignalArguments (gsArg:listOfGSArgs) gsArgIndex godotSignalArgumentArrayPtr c_func = do
        with gsArg $ \gsArgPtr -> do
          copyGodotSignalArgument (godotSignalArgumentArrayPtr `plusPtr` (gsArgIndex * (sizeOf (undefined :: GodotSignalArgument)))) gsArgPtr
          withGodotSignalArguments listOfGSArgs (gsArgIndex + 1) godotSignalArgumentArrayPtr c_func

      -- Passing an empty list just calls the c_func.
      withGodotSignalArguments [] gsArgIndex godotSignalArgumentArrayPtr c_func = c_func (godotSignalArgumentArrayPtr, fromIntegral gsArgIndex)

      copyGodotSignalArgument:: Ptr GodotSignalArgument -> Ptr GodotSignalArgument -> IO ()
      copyGodotSignalArgument dest src = copyBytes dest src (sizeOf (undefined :: GodotSignalArgument))

-- | Helper function to quickly make a GodotSignal with idiomatic Haskell types.
-- | This forces the Godot signal to not have any default arguments.
makeGodotSignal :: String -> [GDNS.SignalArgument] -> IO (GodotSignal)
makeGodotSignal stringSignalName listOfSignalArguments = do
  godotSignalName' <- toLowLevel (pack stringSignalName)
  withGodotSignalArgumentArray listOfSignalArguments $ \(godotSignalArgs', _) ->
    return GodotSignal { godotSignalName = godotSignalName'
                       , godotSignalNumArgs = fromIntegral $ length listOfSignalArguments
                       , godotSignalArgs = godotSignalArgs'
                       , godotSignalNumDefaultArgs = 0
                       , godotSignalDefaultArgs = nullPtr
                       }

-- | When we already have a GodotSignal constructed, we can use this function to
-- | register it (instead of registerSignal, which constructs a GodotSignal from
-- | arguments before registering it.
registerGodotSignal :: forall a. GodotClass a
                    => GdnativeHandle
                    -> Proxy a
                    -> GodotSignal
                    -> IO ()
registerGodotSignal pHandle _ godotSignal = do
  let clsName = godotClassName @a
  withCString clsName $ \clsNamePtr -> do
    godot_nativescript_register_signal pHandle clsNamePtr godotSignal

-- Makes SignalArgument (with type GodotVariantTypeObject) from String. Doesn't
-- include signal hints or default arguments.
makeObjectSignalArgument :: String -> GDNS.SignalArgument
makeObjectSignalArgument signalArgumentName = 
  GDNS.SignalArgument { signalArgumentName     = (pack signalArgumentName) :: Text
                  , signalArgumentType         = GodotVariantTypeObject :: GodotVariantType
                  , signalArgumentHint         = GodotPropertyHintNone  :: GodotPropertyHint
                  , signalArgumentHintString   = (pack "") :: Text
                  , signalArgumentUsage        = godotPropertyUsageDefault :: GodotPropertyUsageFlags
                  , signalArgumentDefaultValue = VariantNil :: (Variant 'GodotTy) -- Am assuming this is default argument for Godot GUI editor
  }

-- Example usage:
-- classSignals = [ makeGodotSignal "signal_name"  [ makeObjectSignalArgument "signal_arg1"
--                                                 , makeObjectSignalArgument "signal_arg2" ]
--                , makeGodotSignal "signal_name2" []
--                ]