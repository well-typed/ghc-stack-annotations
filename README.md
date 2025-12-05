# `ghc-stack-annotations`

A compatibility library for the RTS callstack annotations introduced in GHC 9.14.

```haskell
handleGetRequest :: Int -> IO Int
handleGetRequest number = annotateStackStringIO "My User Annotation" $ do
    bigOperationThatCouldFail number
```

The annotation primop called by `handleGetRequest` pushes a stack frame which contains a user-annotation.
The stack decoding logic can interpret these frames and display the user-annotated
information during exceptions and sample profiling.

* [Better Haskell stack traces via user annotations](https://www.well-typed.com/blog/2025/09/better-haskell-stack-traces/)

Since the annotation primop has only been introduced in GHC-9.14, the annotation
functions do not do anything if used on earlier GHC versions.

IPE backtraces which include annotation stack frames offers a number of advantages over the existing backtrace collection mechanisms:

* It is not necessary to modify the function API (unlike `HasCallStack`)
* A "continuous chain" of modifications is not necessary (unlike `HasCallStack`)
* The annotations work in all ways of compilation (unlike cost centre stacks)
* The backtrace is expressed in terms of predictable source locations (unlike some IPE backtraces)

Further, tools such as [`ghc-stack-profiler`](https://github.com/well-typed/ghc-stack-profiler) can also use the annotation stack frame
to provide improved profiles.
