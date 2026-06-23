# Revision history for ghc-stack-annotations

## 0.2.0.0 -- 23-06-2026

### Breaking changes

* Support GHC 10.1 and 9.14.2 ([#5](https://github.com/well-typed/ghc-stack-annotations/pull/5)).
  * Supports `stackAnnotationSourceLocation` and `displayStackAnnotationShort` from the updated `StackAnnotation` type class.
* Add stack annotation variants in `MonadUnliftIO` ([#3](https://github.com/well-typed/ghc-stack-annotations/pull/3)).
  * Adds module `GHC.Stack.Annotation.UnliftIO`.

## 0.1.0.0 -- 09-12-2025

* Introduce `GHC.Stack.Annotation`, which is a backwards compatibility module, allowing
    users to use `annotateStack*` without having to resort to `CPP` for older GHC versions.
