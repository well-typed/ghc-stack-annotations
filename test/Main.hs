{-# LANGUAGE CPP #-}

module Main where

#include "macros.h"

import Test.Tasty
import Test.Tasty.HUnit
import GHC.Stack.Annotation
import Compat
import Test.Tasty.ExpectedFailure (ignoreTestBecause)
import qualified Data.Maybe as Maybe
import GHC.Stack.Types (SrcLoc(..), getCallStack)

main :: IO ()
main = do
  defaultMain (needsStackAnnotations tests)

tests :: TestTree
tests = testGroup "tests"
  [ testGroup "simple"
    [ testCase "string" $ do
          annos <- annotateStackStringIO "Test" $ do
            collectStackAnnotations

          length annos @?= 1
    , testCase "show" $ do
        annos <- annotateStackShowIO @(Int, Int) (4, 5) $ do
          collectStackAnnotations

        length annos @?= 1
    , testCase "callstack" $ do
        annos <- annotateCallStackIO $ do
          collectStackAnnotations

        length annos @?= 1
    , testCase "custom" $ do
        annos <- annotateStackIO CustomAnnotation $ do
          collectStackAnnotations

        length annos @?= 1
    , testCase "all" $ do
        annos <-
          annotateStackIO CustomAnnotation $
            annotateCallStackIO $
              annotateStackShowIO @(Int, Int) (4, 5) $
                annotateStackStringIO "Test" $ do
                  collectStackAnnotations

        length annos @?= 4
    ]
  , needsSourceLocations $ testGroup "source locations"
    [ testCase "string" $ do
        annos <- annotateStackStringIO "Test" $ do
          collectStackAnnotations

        length annos @?= 1
        let srcLocs = Maybe.mapMaybe stackAnnotationSourceLocation annos
        length srcLocs @?= 1
        fmap srcLocModule srcLocs @?= ["Main"]
    , testCase "show" $ do
        annos <- annotateStackShowIO @(Int, Int) (4, 5) $ do
          collectStackAnnotations

        length annos @?= 1
        let srcLocs = Maybe.mapMaybe stackAnnotationSourceLocation annos
        length srcLocs @?= 1
        fmap srcLocModule srcLocs @?= ["Main"]
    , testCase "callstack" $ do
        annos <- annotateCallStackIO $ do
          collectStackAnnotations

        length annos @?= 1
        let srcLocs = Maybe.mapMaybe stackAnnotationSourceLocation annos
        length srcLocs @?= 1
        fmap srcLocModule srcLocs @?= ["Main"]
    , testCase "custom" $ do
        annos <- annotateStackIO annoWithSrcLoc $ do
          collectStackAnnotations

        length annos @?= 1
        let srcLocs = Maybe.mapMaybe stackAnnotationSourceLocation annos
        length srcLocs @?= 1
    , testCase "all" $ do
        annos <-
          annotateStackIO CustomAnnotation $
            annotateStackIO annoWithSrcLoc $
              annotateCallStackIO $
                annotateStackShowIO @(Int, Int) (4, 5) $
                  annotateStackStringIO "Test" $ do
                    collectStackAnnotations

        length annos @?= 5
        let srcLocs = Maybe.mapMaybe stackAnnotationSourceLocation annos
        length srcLocs @?= 4
        -- No source location for CustomAnnotation is missing
        fmap srcLocModule srcLocs @?= replicate 4 "Main"
    ]
  ]

data CustomAnnotation = CustomAnnotation

-- This is implicitly a test, that we can define instances compatible with the latest
-- ghc-experimental version of 'StackAnnotation'.
instance StackAnnotation CustomAnnotation where
  displayStackAnnotation CustomAnnotation = "CustomAnnotation"

annoWithSrcLoc :: HasCallStack => CustomAnnotationWithSrcLoc
annoWithSrcLoc =
  CustomAnnotationWithSrcLoc loc
  where
    loc = case getCallStack ?callStack of
      [] -> Nothing
      ((_, srcLoc):_) -> Just srcLoc

data CustomAnnotationWithSrcLoc = CustomAnnotationWithSrcLoc (Maybe SrcLoc)

-- This is implicitly a test, that we can define instances compatible with the latest
-- ghc-experimental version of 'StackAnnotation'.
instance StackAnnotation CustomAnnotationWithSrcLoc where
  displayStackAnnotationShort CustomAnnotationWithSrcLoc{} = "CustomAnnotation"
  stackAnnotationSourceLocation (CustomAnnotationWithSrcLoc loc) = loc

needsStackAnnotations :: TestTree -> TestTree
needsStackAnnotations =
#if defined(SUPPORT_STACK_ANN)
  id
#else
  ignoreTestBecause "Stack Annotations not supported on GHC <9.14"
#endif

needsSourceLocations :: TestTree -> TestTree
needsSourceLocations =
#if !defined(SUPPORT_STACK_ANN_SRCLOC)
  ignoreTestBecause "Stack Annotations not supported on GHC <9.14.2"
#else
  id
#endif
