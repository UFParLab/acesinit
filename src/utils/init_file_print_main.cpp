/**
 * Dumps the contents of any init file to screen.
 */
#include <cstdio>
#include <string>
#include <stdlib.h>
#include <unistd.h>

#include "io_utils.h"
#include "setup_reader.h"

// Based on the example provided at 
// http://www.ibm.com/developerworks/aix/library/au-unix-getopt.html

// The list of options that getopt will parse for
// "d:" switch -d requires an argument (data file)
// "h" switch -h does not require an argument
// "?" switch -? does not require an argument
static const char *optString = "d:h?";

int main (int argc, char* const* argv)
{
    int opt = 0;
    char *fName = "data.dat";

    opt = getopt(argc, argv, optString);
    while ( opt != -1) {
        switch (opt) {
            case 'd':   // The user wants to specify an output name.
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


    setup::BinaryInputFile input_file(fName);
    setup::SetupReader setup_reader;
    setup_reader.read(&input_file);
    std::cout << "SETUP READER DATA : " << std::endl;

    setup_reader.dump_data();

    std::cout << "END OF SETUP READER DATA " << std::endl;
    
    return 0;
}

