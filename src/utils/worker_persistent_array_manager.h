/*
 * worker_persistent_array_manager.h
 *
 *  Created on: Apr 21, 2014
 *      Author: njindal
 */

#ifndef WORKER_PERSISTENT_ARRAY_MANAGER_H_
#define WORKER_PERSISTENT_ARRAY_MANAGER_H_

#include "block.h"
//#include "id_block_map.h"

class TestControllerParallel;

namespace sip {

class Interpreter;

/**
 * Data structure used for persistent Scalars and arrays.
 *
 * In the SIAL program:
 *     set_persistent array1 "label1"
 * causes array1 with the string slot for the string literal to be saved
 * in the persistent_array_map_.  In the HAVE_MPI build, if array1 id distributed,
 * messages are sent to all servers to add the entry to the server's persistent_array_map_.
 * Sending these messages is the responsibility of the worker.
 *
 * After the sial program is finished, save_marked_arrays() is invoked at
 * both workers and servers.  The values of marked (i.e. those that are
 * in the persistent_array_map_) scalars are then copied into the scalar_value_map_
 * with the string corresponding to the string_slot as key.  Marked contiguous arrays,
 * and the block map for distributed arrays are MOVED to this class's contiguous_array_map_
 * or distribute_array_map_ respectively. The string itself is used as a key because string
 * slots are assigned by the sial compiler and are only valid in a single sial program.
 *
 * In a subsequent SIAL program, the restore_persisent_array command causes
 * the indicated object to be restored to the SIAL program and removed from the
 * persistent_array data structures.  Scalar values are copied;
 * For contiguous and distributed, pointers to the block or the block map are copied.
 * It is the responsibility of the worker in a parallel program  to check whether
 * the requested object is a scalar or contiguous. If so, restore_persistent_array
 * is called on the local persistent_array_manager.  If it is a distributed/served
 * array, workers send a message to servers to restore the array.
 *
 * A consequence of this design is that  any object can only be restored once.
 * If it is needed again in subsequent SIAL programs, set_persistent needs to be
 * invoked in the current SIAL program. These semantics were chosen to allow clear
 * ownership transfer of allocated memory without unnecessary copying or garbage.
 *
 * Predefined scalars and arrays cannot be made persistent.  Their value are preserved
 * between programs in the SetupReader object already.
 *
 * The SIAL compiler ensures that an object of set_persistent commands is not predefined and
 * is either a scalar or a static, distributed, or served array.
 */
class WorkerPersistentArrayManager {


public:

	/**
	 * Type of map for storing persistent contiguous arrays between SIAL programs.  Only used at workers
	 */
	typedef std::map<std::string, Block::BlockPtr> LabelContiguousArrayMap;
	/**
	 * Type of map for storing persistent distributed and served arrays between SIAL programs.
	 */
//	typedef std::map<std::string, IdBlockMap<Block>::PerArrayMap*> LabelDistributedArrayMap;
	/**
	 * Type of map for storing persistent scalars between SIAL programs.  Only used at workers
	 */
	typedef std::map<std::string, double> LabelScalarValueMap;

	/**
	 * Type of map for storing the array id and slot of label for scalars and arrays that have been marked persistent.
	 */
	typedef std::map<int, int> ArrayIdLabelMap;	// Map of arrays marked for persistence

	WorkerPersistentArrayManager() ;
	~WorkerPersistentArrayManager() ;

	/**
	 * Gets the data and shape of a contiguous array with the given label.  This should
	 * be called after init_from_checkpoint.
	 * This routine is not called by the sia runtime, but is used
	 * in tools that read checkpoints for other purposes.
	 *
	 * The returned values are via a pointer to memory owned by this object and are deleted
	 * when this object is.  Clients that need a different lifetime should make a copy.
	 *
	 * @param[in] label   the label used in the sial program when the persistent static was saved
	 * @param[out] num_elems  the number of elements in the array
	 * @param[out] extents  the size of each dimension of the static array
	 * @param[out] values  pointer to array containing values of array
	 */
	void get_contiguous_from_checkpoint(const std::string& label, int* num_elems,  int** extents, double** values);


	/** Initializes the persistent data structures from the checkpoint file.
	 *
	 * Precondition:  caller is not a server.
	 *
	 */
	void init_from_checkpoint(const std::string& filename);

	friend std::ostream& operator<< (std::ostream&, const WorkerPersistentArrayManager&);

private:
	/** holder for saved contiguous arrays*/
	LabelContiguousArrayMap contiguous_array_map_;
	/** holder for saved distributed arrays*/
	//LabelDistributedArrayMap distributed_array_map_;
	/** holder for saved scalar values */
	LabelScalarValueMap scalar_value_map_;
	/** holder for arrays and scalars that have been marked as persistent */
	ArrayIdLabelMap persistent_array_map_;

	/** Stores previously saved persistent arrays. Used by tests */
	ArrayIdLabelMap old_persistent_array_map_;

	friend class ::TestControllerParallel;

	DISALLOW_COPY_AND_ASSIGN(WorkerPersistentArrayManager);

};

} /* namespace sip */

#endif /* WORKER_PERSISTENT_ARRAY_MANAGER_H_ */
