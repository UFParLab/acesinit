#include <f77_name.h>
#include <f_types.h>

#ifdef FC_MAIN
int FC_MAIN(int argc, const char * const* argv) 
#elif defined(F77_MAIN)
int F77_MAIN(int argc, const char * const* argv)
#else
int main (int argc, const char* const* argv)
#endif
{
       F77_NAME(setup_main, SETUP_MAIN)();
}

