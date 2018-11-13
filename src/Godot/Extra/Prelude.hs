module Godot.Extra.Prelude
  ( module Prelude
  , whenMaybe
  , whenMaybeM
  , maybeMapM
  , unfoldrM
  )
where

import           Universum                     as Prelude
                                                   hiding ( get
                                                          , set
                                                          )

import           Control.Category              as Prelude
                                                          ( (>>>) )
import           Data.Text                     as Prelude
                                                          ( pack
                                                          , unpack
                                                          , unlines
                                                          , lines
                                                          , unwords
                                                          , words
                                                          )


-- | Like 'when', but return either 'Nothing' if the predicate was 'False',
--   of 'Just' with the result of the computation.
--
-- > whenMaybe True  (print 1) == fmap Just (print 1)
-- > whenMaybe False (print 1) == return Nothing
whenMaybe :: Applicative m => Bool -> m a -> m (Maybe a)
whenMaybe b x = if b then Just <$> x else pure Nothing

-- | Like 'whenMaybe', but where the test can be monadic.
whenMaybeM :: Monad m => m Bool -> m a -> m (Maybe a)
whenMaybeM mb x = do
  b <- mb
  if b then Just <$> x else return Nothing

-- | Monadic version of fmap specialised for Maybe
maybeMapM :: Monad m => (a -> m b) -> (Maybe a -> m (Maybe b))
maybeMapM _ Nothing  = return Nothing
maybeMapM m (Just x) = Just <$> m x

unfoldrM :: Monad m => (b -> m (Maybe (a, b))) -> b -> m [a]
unfoldrM f b = f b >>= \case
  Just (a, b') -> return . (a :) =<< unfoldrM f b'
  _            -> return []
