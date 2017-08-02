/*
 * query_checkpoint.cpp
 *
 *  Created on: Feb 23, 2017
 *      Author: Beverly Sanders
 */

#include <cstddef>
#include "query_checkpoint.h"
#include "worker_persistent_array_manager.h"




sip::WorkerPersistentArrayManager *manager = NULL;


#ifdef __cplusplus
extern "C" {
#endif


void open_checkpoint(const char * file_name){
	   std::string name = std::string(file_name);
       
       
  	    manager = new sip::WorkerPersistentArrayManager();

		manager->init_from_checkpoint(name);
}

void get_persistent_static(const char* label, int* num_elems, int **extents, double **values){
		manager->get_contiguous_from_checkpoint(std::string("array_a"), num_elems, extents, values);
}
       
void close_checkpoint(){
  delete manager;
  } 


#ifdef __cplusplus
 }
#endif

