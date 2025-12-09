# Revision history for ghc-stack-annotations

## 0.1.0.0 -- 09-12-2025

* Introduce `GHC.Stack.Annotation`, which is a backwards compatibility module, allowing
    users to use `annotateStack*` without having to resort to `CPP` for older GHC versions.
