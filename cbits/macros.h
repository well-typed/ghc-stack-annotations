#if !defined(MIN_VERSION_ghc_experimental)
#define MIN_VERSION_ghc_experimental(x, y, z) false
#endif

#if MIN_VERSION_ghc_experimental(9,1402,0)
#define SUPPORT_STACK_ANN_LATEST
#endif

#if MIN_VERSION_ghc_experimental(9,1402,0)
#define SUPPORT_STACK_ANN_SRCLOC
#endif

#if MIN_VERSION_ghc_experimental(9,1401,0)
#define SUPPORT_STACK_ANN
#endif
