{-# LANGUAGE CPP #-}
module GHC.Stack.Annotation.Compat.Class (
  StackAnnotation(..),
  SomeStackAnnotation(..),
  CompatAnnotation(..),
  ) where

#include "macros.h"

import Data.Typeable ( Typeable )

#if defined(SUPPORT_STACK_ANN_SRCLOC)
import GHC.Stack.Annotation.Experimental (StackAnnotation(..), SomeStackAnnotation(..))
import qualified GHC.Stack.Annotation.Experimental as Annotation
#elif defined(SUPPORT_STACK_ANN)
import GHC.Stack.Annotation.Experimental (SomeStackAnnotation(..))
import qualified GHC.Stack.Annotation.Experimental as Annotation
import GHC.Stack (SrcLoc, prettySrcLoc)
#else
import GHC.Stack (SrcLoc, prettySrcLoc)
#endif

-- The following source code is a partial copy of ghc-internal:
--  GHC.Internal.Stack.Annotation
-- It copies the `StackAnnotation` type class and its documentation.
--
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


#if !defined(SUPPORT_STACK_ANN_LATEST)
-- | 'StackAnnotation's are types which can be pushed onto the call stack
-- as the payload of 'AnnFrame' stack frames.
--
class StackAnnotation a where
  -- | Display a human readable string for the 'StackAnnotation'.
  --
  -- This is supposed to be the long version of 'displayStackAnnotationShort'
  -- and may contain a source location.
  --
  -- If not provided, 'displayStackAnnotation' is derived from 'stackAnnotationSourceLocation'
  -- and 'displayStackAnnotationShort'.
  displayStackAnnotation :: a -> String

  -- | Get the 'SrcLoc' of the given 'StackAnnotation'.
  --
  -- This is optional, 'SrcLoc' are not strictly required for 'StackAnnotation', but
  -- it is still heavily encouraged to provide a 'SrcLoc' for better IPE backtraces.
  stackAnnotationSourceLocation :: a -> Maybe SrcLoc

  -- | The description of the StackAnnotation without any metadata such as source locations.
  --
  -- Prefer implementing 'displayStackAnnotationShort' over 'displayStackAnnotation'.
  displayStackAnnotationShort :: a -> String

  {-# MINIMAL displayStackAnnotation | displayStackAnnotationShort #-}

  displayStackAnnotation ann =
    displayStackAnnotationShort ann
      ++ case stackAnnotationSourceLocation ann of
          Nothing -> ""
          Just srcLoc -> ", called at " ++ prettySrcLoc srcLoc

  stackAnnotationSourceLocation _ann = Nothing

  displayStackAnnotationShort = displayStackAnnotation
#endif


-- Only define this is, if `ghc-experimental` doesn't define it already. Otherwise, just re-export it.

#if !defined(SUPPORT_STACK_ANN)
-- |
-- The @SomeStackAnnotation@ type is the root of the stack annotation type hierarchy.
-- When the call stack is annotated with a value of type @a@, behind the scenes it is
-- encapsulated in a @SomeStackAnnotation@.
--
data SomeStackAnnotation where
  SomeStackAnnotation :: forall a. (Typeable a, StackAnnotation a) => a -> SomeStackAnnotation
#endif

#if defined(SUPPORT_STACK_ANN_LATEST)
{- No instance needed -}
#elif defined(SUPPORT_STACK_ANN_SRCLOC)
instance StackAnnotation SomeStackAnnotation where
  displayStackAnnotation (SomeStackAnnotation ann) =
    Annotation.displayStackAnnotation ann
  stackAnnotationSourceLocation (SomeStackAnnotation ann) =
    Annotation.stackAnnotationSourceLocation ann
  displayStackAnnotationShort (SomeStackAnnotation ann) =
    Annotation.displayStackAnnotationShort ann
#elif defined(SUPPORT_STACK_ANN)
instance StackAnnotation SomeStackAnnotation where
  displayStackAnnotation (SomeStackAnnotation ann) =
    Annotation.displayStackAnnotation ann
#else
instance StackAnnotation SomeStackAnnotation where
  displayStackAnnotation (SomeStackAnnotation ann) =
    displayStackAnnotation ann
  stackAnnotationSourceLocation (SomeStackAnnotation ann) =
    stackAnnotationSourceLocation ann
  displayStackAnnotationShort (SomeStackAnnotation ann) =
    displayStackAnnotationShort ann
#endif

-- ----------------------------------------------------------------------------
-- Backwards compatibility wrapper
-- ----------------------------------------------------------------------------

-- | A newtype that bridges the locally defined 'StackAnnotation' to GHC's 'ghc-experimental:StackAnnotation'.
--
newtype CompatAnnotation a = CompatAnnotation a

#if defined(SUPPORT_STACK_ANN)
instance (Typeable a, StackAnnotation a) => Annotation.StackAnnotation (CompatAnnotation a) where
  displayStackAnnotation (CompatAnnotation a) =
    displayStackAnnotation a
#if defined(SUPPORT_STACK_ANN_SRCLOC)
  stackAnnotationSourceLocation (CompatAnnotation a) =
    stackAnnotationSourceLocation a
  displayStackAnnotationShort (CompatAnnotation a) =
    displayStackAnnotationShort a
#endif
#endif
