{-# LANGUAGE CPP #-}
module GHC.Stack.Annotation.Compat where

#include "macros.h"
import Control.Exception
import Data.Typeable
import GHC.Stack (withFrozenCallStack)
import GHC.Stack.Types
import System.IO.Unsafe (unsafePerformIO)
import GHC.Stack.Annotation.Compat.Class
import GHC.Stack.Annotation.Types
#if defined(SUPPORT_STACK_ANN)
import qualified GHC.Stack.Annotation.Experimental as Annotation
#endif

-- ----------------------------------------------------------------------------
-- Annotate the CallStack with custom data
-- ----------------------------------------------------------------------------

-- See Note [User-defined stack annotations for better stack traces]

-- | @'annotateStack' anno b@ annotates the evaluation stack of @b@
-- with the value of @anno@.
--
-- When decoding the call stack, the annotation frames can be used to add more
-- information to stack traces.
--
-- WARNING: forces the evaluation of @b@ to WHNF.
{-# NOINLINE annotateStack #-}
annotateStack :: forall a b. (HasCallStack, Typeable a, StackAnnotation a) => a -> b -> b
annotateStack ann b = unsafePerformIO $
  annotateStackIO ann (evaluate b)

-- | @'annotateCallStack' b@ annotates the evaluation stack of @b@
-- with the current 'callstack'.
--
-- When decoding the call stack, the annotation frames can be used to add more
-- information to stack traces.
--
-- WARNING: forces the evaluation of @b@ to WHNF.
{-# NOINLINE annotateCallStack #-}
annotateCallStack :: HasCallStack => b -> b
annotateCallStack b = unsafePerformIO $ withFrozenCallStack $
  annotateCallStackIO (evaluate b)


-- | @'annotateStackString' msg b@ annotates the evaluation stack of @b@
-- with the value @msg@.
--
-- When decoding the call stack, the annotation frames can be used to add more
-- information to stack traces.
--
-- WARNING: forces the evaluation of @b@ to WHNF.
annotateStackString :: forall b . HasCallStack => String -> b -> b
annotateStackString ann =
  annotateStack (StringAnnotation (callStackHeadSrcLoc ?callStack) ann)

-- | @'annotateStackShow' showable b@ annotates the evaluation stack of @b@
-- with the value @showable@.
--
-- When decoding the call stack, the annotation frames can be used to add more
-- information to stack traces.
--
-- WARNING: forces the evaluation of @b@ to WHNF.
annotateStackShow :: forall a b . (HasCallStack, Typeable a, Show a) => a -> b -> b
annotateStackShow ann =
  annotateStack (ShowAnnotation (callStackHeadSrcLoc ?callStack) ann)

-- | @'annotateStackIO' showable b@ annotates the evaluation stack of @b@
-- with the value @showable@.
--
-- When decoding the call stack, the annotation frames can be used to add more
-- information to stack traces.
annotateStackIO :: forall a b . (HasCallStack, Typeable a, StackAnnotation a) => a -> IO b -> IO b
#if defined(SUPPORT_STACK_ANN)
annotateStackIO ann act = Annotation.annotateStackIO (CompatAnnotation ann) act
#else
annotateStackIO _ann = id
#endif

-- | @'annotateStackStringIO' msg b@ annotates the evaluation stack of @b@
-- with the value @msg@.
--
-- When decoding the call stack, the annotation frames can be used to add more
-- information to stack traces.
annotateStackStringIO :: forall b . HasCallStack => String -> IO b -> IO b
annotateStackStringIO ann =
  annotateStackIO (StringAnnotation (callStackHeadSrcLoc ?callStack) ann)

-- | @'annotateStackShowIO' msg b@ annotates the evaluation stack of @b@
-- with the value @msg@.
--
-- When decoding the call stack, the annotation frames can be used to add more
-- information to stack traces.
annotateStackShowIO :: forall a b . (HasCallStack, Show a) => a -> IO b -> IO b
annotateStackShowIO ann =
  annotateStackIO (ShowAnnotation (callStackHeadSrcLoc ?callStack) ann)

-- | @'annotateCallStackIO' b@ annotates the evaluation stack of @b@ with the
-- current 'callstack'.
--
-- When decoding the call stack, the annotation frames can be used to add more
-- information to stack traces.
annotateCallStackIO :: HasCallStack => IO a -> IO a
annotateCallStackIO =
  annotateStackIO (CallStackAnnotation ?callStack)
