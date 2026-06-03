module GHC.Stack.Annotation.Types where

import GHC.Stack.Annotation.Compat.Class (StackAnnotation(..))
import GHC.Stack

-- | A 'String' only annotation with an optional source location.
data StringAnnotation where
  StringAnnotation :: !(Maybe SrcLoc) -> String -> StringAnnotation

instance StackAnnotation StringAnnotation where
  displayStackAnnotationShort (StringAnnotation _srcLoc str) =
    str

  stackAnnotationSourceLocation (StringAnnotation srcLoc _str) =
    srcLoc

-- | Use the 'Show' instance of a type to display as the 'StackAnnotation'.
data ShowAnnotation where
  ShowAnnotation :: forall a . Show a => !(Maybe SrcLoc) -> a -> ShowAnnotation

instance StackAnnotation ShowAnnotation where
  displayStackAnnotationShort (ShowAnnotation _srcLoc showAnno) =
    show showAnno

  stackAnnotationSourceLocation (ShowAnnotation srcLoc _showAnno) =
    srcLoc

-- | A 'CallStack' stack annotation.
--
-- Captures the whole 'CallStack'.
newtype CallStackAnnotation = CallStackAnnotation CallStack

instance Show CallStackAnnotation where
  show (CallStackAnnotation cs) = prettyCallStack cs

-- | Displays the first entry of the 'CallStack'
instance StackAnnotation CallStackAnnotation where
  stackAnnotationSourceLocation (CallStackAnnotation cs) =
    callStackHeadSrcLoc cs

  displayStackAnnotationShort (CallStackAnnotation cs) =
    callStackHeadFunctionName cs

callStackHeadSrcLoc :: CallStack -> Maybe SrcLoc
callStackHeadSrcLoc cs =
  case getCallStack cs of
    [] -> Nothing
    (_, srcLoc):_ -> Just srcLoc

callStackHeadFunctionName :: CallStack -> String
callStackHeadFunctionName cs =
  case getCallStack cs of
    [] -> "<unknown source location>"
    (fnName, _):_ -> fnName
