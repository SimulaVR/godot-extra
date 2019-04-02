{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
module Godot.Extra.Register
  ( GdnativeHandle
  , ClassExport(..)
  , GodotClass(..)
  , Registerer(..)
  , GodotMethod(..)
  , GFunc
  , RPC(..)
  , registerClass
  , registerMethod
  , func
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
                                                          )

import           Godot.Extra.Prelude

import qualified Data.Text                     as T


type GFunc cls = cls -> Vector GodotVariant -> IO GodotVariant


data GodotMethod cls where
  GodotMethod
    :: { methodRPCMode :: RPC
       , methodName :: Text
       , methodFunc :: cls -> Vector GodotVariant -> IO GodotVariant
       }
    -> GodotMethod cls


data RPC
  = NoRPC
  | Remote
  | Sync
  | Master
  | Slave


-- | Instead of fixing upstream @GodotClass@ we'll just "extend" it.
-- This is because I'm still not sure of this approach so I can't justify
-- an upstream change. Note that to register your class type you will
-- have to make it an instance of both @GodotClass@ and @ClassExport@.
class (GodotClass a, Typeable a) => ClassExport a where
  classExtends :: Text
  classInit :: GodotObject -> IO a
  classMethods :: [GodotMethod a]


data RegTy = GClass | GMethod
data family Registerer (x :: RegTy) cls

data instance Registerer 'GClass cls = ClassExport cls =>
  RegClass
    GdnativeHandle
    (GodotObject -> IO cls)

data instance Registerer 'GMethod cls =
  RegMethod
    GdnativeHandle
    (GodotMethod cls)

-- | Convenient way of registering a class with all its methods.
-- Used like: @registerClass $ RegClass desc $ classInit \\@MyClass@
registerClass :: forall a . ClassExport a => Registerer 'GClass a -> IO ()
registerClass (RegClass desc constr) = do
  GDNS.registerClass desc (unpack extends) constr destr
  forM_ methods regMtd
    {->>= printMethods-}
 where
  {-clsName = pack $ godotClassName @a-}
  extends = classExtends @a
  destr _ _ = return ()
  methods = classMethods @a
  regMtd mtd@GodotMethod {..} = do
    registerMethod (RegMethod desc mtd)
    return methodName
  {-
   -printMethods mtdNames =
   -  godotPrint
   -    $  unlines
   -    $  [ "Registered class:"
   -       , "  Name: " `mappend` clsName
   -       , "  Extends: " `mappend` extends
   -       , "  Member methods:"
   -       ]
   -    ++ (map ("    " `mappend`) mtdNames)
   -}


registerMethod :: forall a . GodotClass a => Registerer 'GMethod a -> IO ()
registerMethod (RegMethod desc GodotMethod {..}) = do
  GDNS.registerMethod desc name rpcMode method
 where
  method  = \_ -> methodFunc
  -- ^ First argument will always be the object that the method is registered
  -- to, which we don't care about (since it's typically kept inside our custom class
  -- anyway) so we'll ignore it when dealing with these functions to keep things simple.
  name    = unpack methodName
  rpcMode = case methodRPCMode of
    NoRPC  -> GodotMethodRpcModeDisabled
    Remote -> GodotMethodRpcModeRemote
    Sync   -> GodotMethodRpcModeSync
    Master -> GodotMethodRpcModeMaster
    Slave  -> GodotMethodRpcModeSlave


-- | Example usage:
-- func NoRPC "_unhandled_input" ["InputEvent"] $
--   \self [evObj] ->
--     (fromGodotVariant evObj :: IO GodotObject)
--       >>= asClass GodotInputEventKey "InputEventKey"
--       >>= flip whenJust (handleInputKey self)
func :: (ClassExport cls, AsVariant a)
  => RPC -> Text -> [Text] -> (cls -> [GodotVariant] -> IO a) -> GodotMethod cls
func rpc mthdName argTypes fn =
  GodotMethod rpc mthdName $ \self args -> do
    let argList = toList args
    if length argList == length argTypes
      then toLowLevel . toVariant =<< fn self argList
      else methodArgsErr mthdName argTypes
 where
  methodArgsErr :: Text -> [Text] -> a
  methodArgsErr fnName expectedArgs =
    error $ mappend
      (T.concat ["Error: ", fnName, ": Expected arguments: "])
      (T.intercalate ", " expectedArgs)

{- Insertion pending resolution of godot-haskell gdwlroots branch issue.

withGodotSignalArgumentArray :: [SignalArgument] -> ((Ptr GodotSignalArgument, CInt) -> IO a) -> IO a
withGodotSignalArgumentArray listOfSignalArguments c_func = do
  listOfGodotSignalArguments <- mapM asGodotSignalArgument listOfSignalArguments
  let sizeOfArray = (length listOfSignalArguments) * (sizeOf (undefined :: GodotSignalArgument))
  allocaBytes sizeOfArray $ \godotSignalArgumentArrayPtr ->
    withGodotSignalArguments listOfGOdotSignalArguments 0 godotSignalArgumentArrayPtr c_func 
    where
      withGodotSignalArguments [GodotSignalArgument] -> Int -> Ptr GodotSignalArgument -> ((Ptr GodotSignalArgument, CInt) -> IO a) -> IO a

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
makeGodotSignal :: String -> [SignalArgument] -> IO (GodotSignal)
makeGodotSignal stringSignalName listOfSignalArgs = do
  withGodotSignalArgumentArray listOfSignalArguments $ \godotSignalArgs' ->
    return GodotSignal { godotSignalName = fromLowLevel (pack stringSignalName)
                       , godotSignalNumArgs = length listOfSignalArgs
                       , godotSignalArgs = godotSignalArgs'
                       , godotSignalNumDefaultArgs = 0
                       , godotSignalDefaultArgs = nullPtr
                       }

-}