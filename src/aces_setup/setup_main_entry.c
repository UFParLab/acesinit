#include <f77_name.h>
#include <f_types.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>

// Based on the example provided at 
// http://www.ibm.com/developerworks/aix/library/au-unix-getopt.html

static const char *optString = "o:h?";

#ifdef FC_MAIN
int FC_MAIN(int argc, char* const* argv) 
#elif defined(F77_MAIN)
int F77_MAIN(int argc, char* const * argv)
#else
int main (int argc, char* const* argv)
#endif
{
    int opt = 0;
    char *fName = NULL;
    char *defaultName = "data";

    opt = getopt(argc, argv, optString);
    while ( opt != -1) {
        switch (opt) {
            case 'o':
                fName = optarg;
                //printf ("DEBUG : %s\n", optarg);
                break;
            case 'h' : case '?':
            default :
                fprintf (stderr, "Usage : %s -o <output_name_without_.dat>\n", argv[0]);
                return -1;
                break;
        }

        opt = getopt ( argc, argv, optString ) ;

    }

    if (fName == NULL){
        fName = defaultName;
    }

    int len = strlen(fName);
    char *datName = (char*)malloc(len + 4);   // "fName.dat + \0"
    sprintf(datName, "%s.dat", fName);

    printf ("Output is set as %s\n", fName);
    
    F77_NAME(setup_main, SETUP_MAIN)(fName, datName);
}

