#if defined(INTERPRETER) || defined(DEBUGGER) || defined(CROSSDEBUGGER)
#define NEED_INTERPRETER
#endif
#if defined(COMPILER) || defined(CROSSDEBUGGER)
#define NEED_COMPILER
#endif
#ifdef COMPILER_THRESHOLD
#define PROFILE_FRAGMENTS
#endif
