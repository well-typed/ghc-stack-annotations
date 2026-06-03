{-# LANGUAGE CPP #-}

module Compat where

#include "macros.h"

import GHC.Stack.Annotation

#if defined(SUPPORT_STACK_ANN)
import GHC.Stack.CloneStack (cloneMyStack)
import qualified Data.Maybe as Maybe
import GHC.InfoProv
import GHC.Exts.Heap.Closures
import Unsafe.Coerce

-- TODO: We should not depend on ghc-internal, expose decoding functions from ghc-experimental
import qualified GHC.Internal.Stack.Decode as Decode
#endif


collectStackAnnotations :: IO [SomeStackAnnotation]
collectStackAnnotations = do
#if defined(SUPPORT_STACK_ANN)
  stack <- cloneMyStack
  frames <- Decode.decodeStackWithIpe stack
  pure $ Maybe.mapMaybe go frames
  where
    -- TODO: GHC should expose this decoding function
    go :: (StackFrame, Maybe InfoProv) -> Maybe SomeStackAnnotation
    go (frame, _ipeInfo) = case frame of
      AnnFrame {annotation = Box someStackAnno } ->
        case unsafeCoerce someStackAnno of
          SomeStackAnnotation ann ->
            Just $ SomeStackAnnotation ann
      _  ->
        Nothing
#else
  -- There are no stack annotations in GHC <914
  pure []
#endif
