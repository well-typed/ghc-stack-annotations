{-# LANGUAGE CPP #-}

module GHC.Stack.Annotation (
  -- * The root of Stack Annotation Types
  SomeStackAnnotation(..),
  -- * Displaying Stack Annotations
  StackAnnotation(..),
  -- * Annotation helpers
  ShowAnnotation(..),
  StringAnnotation(..),
  -- * 'CallStack' annotations
  CallStackAnnotation(..),
  -- * Push stack frame annotations in 'IO' code.
  --
  --
  annotateStackIO,
  annotateStackStringIO,
  annotateStackShowIO,
  annotateCallStackIO,
  -- * Push stack frame annotations in non-'IO' code.
  --
  -- | These variants all evaluate the code to be annotated to WHNF.
  -- Otherwise, the stack annotations will not be shown in stack traces,
  -- as the computation is immediately "evaluated" to a thunk, popping the
  -- annotation frames from the stack.
  -- If the pure computation throws an exception later, the annotation frame
  -- will not be present, thus missing in the stack trace.
  --
  -- Note, you will encounter similar issues if the exception is thrown
  -- during evaluation of a nested value, for example @Just (error "Oh, no!")@.
  annotateStack,
  annotateStackString,
  annotateStackShow,
  annotateCallStack,
) where


#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 914
import GHC.Stack.Annotation.Experimental (
  SomeStackAnnotation(..),
  StackAnnotation(..),
  ShowAnnotation(..),
  StringAnnotation(..),
  CallStackAnnotation(..),
  annotateStackIO,
  annotateStackStringIO,
  annotateStackShowIO,
  annotateCallStackIO,
  annotateStack,
  annotateStackString,
  annotateStackShow,
  annotateCallStack,
  )
#else

-- The following source code is a 1:1 copy of ghc-experimental:
--  GHC.Stack.Annotation.Experimental.hs
-- As such, we reproduce the copyright notice here.

-- Copyright (c) 2023, ghc-devs@haskell.org
--
-- All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are met:
--
--     * Redistributions of source code must retain the above copyright
--       notice, this list of conditions and the following disclaimer.
--
--     * Redistributions in binary form must reproduce the above
--       copyright notice, this list of conditions and the following
--       disclaimer in the documentation and/or other materials provided
--       with the distribution.
--
--     * Neither the name of ghc-devs@haskell.org nor the names of other
--       contributors may be used to endorse or promote products derived
--       from this software without specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
-- "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
-- LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
-- A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
-- OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
-- SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
-- LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
-- DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
-- THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
-- (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
-- OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

import Control.Exception
import Data.Typeable
import GHC.Exception
import GHC.Stack (withFrozenCallStack)
import GHC.Stack.Types
import System.IO.Unsafe (unsafePerformIO)

-- ----------------------------------------------------------------------------
-- StackAnnotation
-- ----------------------------------------------------------------------------

-- | 'StackAnnotation's are types which can be pushed onto the call stack
-- as the payload of 'AnnFrame' stack frames.
--
class StackAnnotation a where
  displayStackAnnotation :: a -> String

-- ----------------------------------------------------------------------------
-- Annotations
-- ----------------------------------------------------------------------------

-- |
-- The @SomeStackAnnotation@ type is the root of the stack annotation type hierarchy.
-- When the call stack is annotated with a value of type @a@, behind the scenes it is
-- encapsulated in a @SomeStackAnnotation@.
--
data SomeStackAnnotation where
  SomeStackAnnotation :: forall a. (Typeable a, StackAnnotation a) => a -> SomeStackAnnotation

instance StackAnnotation SomeStackAnnotation where
  displayStackAnnotation (SomeStackAnnotation a) = displayStackAnnotation a

data StringAnnotation where
  StringAnnotation :: String -> StringAnnotation

instance StackAnnotation StringAnnotation where
  displayStackAnnotation (StringAnnotation str) = str

-- | Use the 'Show' instance of a type to display as the 'StackAnnotation'.
data ShowAnnotation where
  ShowAnnotation :: forall a . Show a => a -> ShowAnnotation

instance StackAnnotation ShowAnnotation where
  displayStackAnnotation (ShowAnnotation showAnno) = show showAnno

-- | A 'CallStack' stack annotation.
newtype CallStackAnnotation = CallStackAnnotation CallStack

instance Show CallStackAnnotation where
  show (CallStackAnnotation cs) = prettyCallStack cs

-- | Displays the first entry of the 'CallStack'
instance StackAnnotation CallStackAnnotation where
  displayStackAnnotation (CallStackAnnotation cs) = case getCallStack cs of
    [] -> "<unknown source location>"
    ((fnName,srcLoc):_) -> fnName ++ ", called at " ++ prettySrcLoc srcLoc

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
annotateStack :: forall a b. (Typeable a, StackAnnotation a) => a -> b -> b
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
annotateStackString :: forall b . String -> b -> b
annotateStackString ann =
  annotateStack (StringAnnotation ann)

-- | @'annotateStackShow' showable b@ annotates the evaluation stack of @b@
-- with the value @showable@.
--
-- When decoding the call stack, the annotation frames can be used to add more
-- information to stack traces.
--
-- WARNING: forces the evaluation of @b@ to WHNF.
annotateStackShow :: forall a b . (Typeable a, Show a) => a -> b -> b
annotateStackShow ann =
  annotateStack (ShowAnnotation ann)

-- | @'annotateStackIO' showable b@ annotates the evaluation stack of @b@
-- with the value @showable@.
--
-- When decoding the call stack, the annotation frames can be used to add more
-- information to stack traces.
annotateStackIO :: forall a b . (Typeable a, StackAnnotation a) => a -> IO b -> IO b
annotateStackIO _ann = id
{-# NOINLINE annotateStackIO #-}

-- | @'annotateStackStringIO' msg b@ annotates the evaluation stack of @b@
-- with the value @msg@.
--
-- When decoding the call stack, the annotation frames can be used to add more
-- information to stack traces.
annotateStackStringIO :: forall b . String -> IO b -> IO b
annotateStackStringIO ann =
  annotateStackIO (StringAnnotation ann)

-- | @'annotateStackShowIO' msg b@ annotates the evaluation stack of @b@
-- with the value @msg@.
--
-- When decoding the call stack, the annotation frames can be used to add more
-- information to stack traces.
annotateStackShowIO :: forall a b . (Show a) => a -> IO b -> IO b
annotateStackShowIO ann =
  annotateStackIO (ShowAnnotation ann)

-- | @'annotateCallStackIO' b@ annotates the evaluation stack of @b@ with the
-- current 'callstack'.
--
-- When decoding the call stack, the annotation frames can be used to add more
-- information to stack traces.
annotateCallStackIO :: HasCallStack => IO a -> IO a
annotateCallStackIO =
  annotateStackIO (CallStackAnnotation ?callStack)

#endif
