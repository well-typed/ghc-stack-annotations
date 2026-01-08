module GHC.Stack.Annotation.UnliftIO
  (
  -- * Push stack frame annotations in 'MonadUnliftIO' code.
  --
  annotateStackIO,
  annotateStackStringIO,
  annotateStackShowIO,
  annotateCallStackIO,
  ) where

import qualified GHC.Stack.Annotation as Annotation
import Data.Typeable (Typeable)
import Control.Monad.IO.Unlift (MonadUnliftIO(..))
import GHC.Stack.Types (HasCallStack)

annotateStackIO :: (Typeable a, Annotation.StackAnnotation a, MonadUnliftIO m) => a -> m b -> m b
annotateStackIO anno action =
  withRunInIO $ \runInIO -> Annotation.annotateStackIO anno (runInIO action)

annotateStackStringIO :: MonadUnliftIO m => String -> m b -> m b
annotateStackStringIO msg action =
  withRunInIO $ \runInIO -> Annotation.annotateStackStringIO msg (runInIO action)

annotateStackShowIO :: (Show a, MonadUnliftIO m) => a -> m b -> m b
annotateStackShowIO value action =
  withRunInIO $ \runInIO -> Annotation.annotateStackShowIO value (runInIO action)

annotateCallStackIO :: (HasCallStack, MonadUnliftIO m) => m b -> m b
annotateCallStackIO action =
  withRunInIO $ \runInIO -> Annotation.annotateCallStackIO (runInIO action)
