/* Provides definitions of fortran callable routines to read values from worker checkpoint files
 *
 * query_checkpoint.h
 *
 *  Created on: Feb 23, 2017
 *      Author: Beverly Sanders
 */

#ifndef QUERY_CHECKPOINT_H_
#define QUERY_CHECKPOINT_H_

/*!  The following routines are callable from Fortran */

#ifdef __cplusplus
extern "C" {
#endif

/*!Call first to initialize with name of checkpoint file
 *
 * @param checkpoint file name
 */
  void open_checkpoint(const char * file_name);
  
  /*! extract and return the persistent static array with the given label */
  void get_persistent_static(const char* label, int* num_elems, int **extents, double **values);
  
  /*! close the current checkpoint file and release associated resources.
   *
   * In particular, this operation will release the memory holding values returned from
   * get_persistent_static.  If a longer lifetime is desired, the client should
   * make a copy of the array
   */
  void close_checkpoint();
  

#ifdef __cplusplus
    }
#endif
#endif //QUERY_CHECKPOINT_H_
