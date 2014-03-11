/**
 * This main function is the entry point to setup_main which reads in the 
 * ZMAT, GENBAS, default_jobs & sial_config to generate a data file containing
 * initialization data to be read in by aces4.
 * Example usage of the generated executable :
 * ./acesinit 
 * will invoke setup_main and create 2 files : 
 *  data.dat
 *  data.h
 * which will contain the initialization data.
 * Another example usage:
 * ./acesinit -o testd
 * will invoke setup_main and create 2 files:
 *  testd.dat
 *  testd.h
 * ./acesinit -h or ./acesinit -? will print the usage message.
 */
#include <f77_name.h>
#include <f_types.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>

// Based on the example provided at 
// http://www.ibm.com/developerworks/aix/library/au-unix-getopt.html

// The list of options that getopt will parse for
// "o:" switch -o requires an argument
// "h" switch -h does not require an argument
// "?" switch -? does not require an argument
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
            case 'o':   // The user wants to specify an output name.
                fName = optarg;
                //printf ("DEBUG : %s\n", optarg);
                break;
            case 'h' : case '?':    // The help message is printed
            default :
                fprintf (stderr, "Usage : %s -o <output_name_without_.dat>\n", argv[0]);
                return -1;
                break;
        }

        opt = getopt ( argc, argv, optString ) ;

    }

    // If used did not specify the "-o" switch, use the default output name.
    if (fName == NULL){
        fName = defaultName;
    }

    int len = strlen(fName);
    char *datName = (char*)malloc(len + 4);   // "fName.dat + \0"
    sprintf(datName, "%s.dat", fName);

    printf ("Output is set as %s\n", fName);
    
    F77_NAME(setup_main, SETUP_MAIN)(fName, datName);
}

