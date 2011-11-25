
#ifdef LCENGINE_EXPORTS
#define LCENGINE_API __declspec(dllexport)
#else
#define LCENGINE_API __declspec(dllimport)
#endif

LCENGINE_API void stub(void){}