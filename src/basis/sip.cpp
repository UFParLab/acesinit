/*! abort.cpp
 * 
 *
 *  Created on: Jul 6, 2013
 *      Author: Beverly Sanders
 */

#include "sip.h"
#include <iostream>
#include <cstdlib>
#include <stdexcept>


//TODO  this should be changed to call mpi_abort
void sip_abort(const std::string& message) {
	std::cerr << message << std::endl;
	exit(EXIT_FAILURE);
}


namespace sip {



const int SETUP_MAGIC = 23121991;
const int SETUP_VERSION = 1;
//const int NUM_INDEX_TYPES = 7;


const int SIOX_MAGIC = 70707;
const int SIOX_VERSION = 1;
const int SIOX_RELEASE = 2;

const int MAX_OMP_THREADS = 8;


void check(bool condition, std::string message, int line){
	if (condition) return;
	std::cerr << "FATAL ERROR: " << message;
	if (line > 0){
		std::cerr << " at line " << line;
	}
	std::cerr << std::endl;
//  MPI_ABORT
//	exit(EXIT_FAILURE);
	throw std::logic_error("logic error");
}


bool check_and_warn(bool condition, std::string message, int line){
	if (condition) return true;
	std::cerr << "WARNING:  "  << message;
	if (line > 0){
		std::cerr << " at line " << line;
	}
	std::cerr << std::endl;
	return false;
}



} //namespace sip
