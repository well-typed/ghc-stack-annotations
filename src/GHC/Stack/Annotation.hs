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

import GHC.Stack.Annotation.Compat
import GHC.Stack.Annotation.Compat.Class
import GHC.Stack.Annotation.Types
