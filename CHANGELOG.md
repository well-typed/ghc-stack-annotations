# Revision history for ghc-stack-annotations

## 0.2.0.0 -- UNRELEASED

### Breaking changes

* Add stack annotation variants in `MonadUnliftIO` ([#3](https://github.com/well-typed/ghc-stack-annotations/pull/3)).
  * Adds module `GHC.Stack.Annotation.UnliftIO`.

## 0.1.0.0 -- 09-12-2025

* Introduce `GHC.Stack.Annotation`, which is a backwards compatibility module, allowing
    users to use `annotateStack*` without having to resort to `CPP` for older GHC versions.
