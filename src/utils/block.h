/**
 * blocks.h
 *
 * This file contains several classes used to represent blocks.
 *
 * BlockId:  uniquely identifies a block and is used to look up the block in the
 * block map.  The BlockId contains the array name, index values, and if a subblock,
 * a pointer to the parent BlockId.
 *
 * BlockShape:  wraps an array with the number of elements in the block for
 * each index.  Unused indices are given the value 1, so that Shape objects don't depend
 * on knowing the rank.
 *
 * BlockSelector: contains the array id and the ids of each index.  One can obtain a
 * BlockId from a BlockSelector by looking up the current values of each index.
 *
 * Block: contains the shape, size (for convenience), and a pointer to the actual data
 * of the block. Various primitive operations on blocks are implemented in this class.
 *
 *  Created on: Aug 8, 2013
 *      Author: Beverly Sanders
 */

#ifndef BLOCKS_H_
#define BLOCKS_H_

//#include "config.h"
#include <bitset>
//#include <utility>   // for std::rel_ops.  Allows relational ops for BlockId to be derived from == and <
#include "sia_defs.h"
//#include "sip.h"
#include "array_constants.h"
//#include "sip_interface.h"
#include "block_shape.h"
#include <iostream>

namespace sip {
class Block;






/** A block of data together with its shape. Size is derived from the shape
 * and is stored for convenience.  The shape of a block cannot change once
 * created, although the data may.  In contrast to BlockIds, which are
 * small enough to freely copy, Blocks need to manage their data carefully.  Also,
 * generally, data structures will hold Block pointers, allowing for polymorphism
 * later if we ever want to have some other kind of block later.
 *
 * The BlockManager is solely responsible for creating and destroying blocks
 * and freeing and allocating data memory.
 */
class Block {
public:

	typedef Block* BlockPtr;
	typedef double * dataPtr;
	typedef int permute_t[MAX_RANK];

    /** Constructs a new Block with the given shape and allocates memory for its data
     * @param shape of new Block
     *
     * This constructor updates MemoryTracker::global, but will throw an exception
     * if there is not enough memory available.  Thus the caller should generally
     * catch the exception, or alternatively, allocate memory and pass it to the 2 param constructor.
     */
	explicit Block(BlockShape);


    /** Constructs a new Block with the given shape and data
     *
     * @param  shape
     * @param  pointer to data
     */
	Block(BlockShape, dataPtr);

	/** Constructs a new Block for a scalar.  size_ and all elements of the shape are set to 1
	 * This allows scalars to be handled uniformly with non-scalars
	 *
	 * @param pointer to scalar
	 */
	explicit Block(dataPtr);


	/**
	 * Deletes data in block if any.  If an MPI request associated with this
	 * block is pending, it waits until it has been satisfied and issues a warning.
	 * Updates the global MemoryTracker
	 */
	~Block();

    int size();
    const BlockShape& shape();
    dataPtr get_data();

private:

	BlockShape shape_;
    int size_;
	dataPtr data_;

	// Why bitset is a good idea
	// http://www.drdobbs.com/the-standard-librarian-bitsets-and-bit-v/184401382
	enum BlockStatus {
		onHost			= 0,	// Block is on host
		onGPU			= 1,	// Block is on device (GPU)
		dirtyOnHost 	= 2,	// Block dirty on host
		dirtyOnGPU 	    = 3		// Block dirty on device (GPU)
	};
	std::bitset<4> status_;

	// No one should be using the compare operator.
	// TODO Figure out what to do with the GPU pointer.
	bool operator==(const Block& rhs) const;


	DISALLOW_COPY_AND_ASSIGN(Block);
};


} /* namespace sip */

#endif /* BLOCKS_H_ */
