/*
 * blocks.cpp
 *
 *  Created on: Aug 8, 2013
 *      Author: basbas
 */

//#include "config.h"
#include "block.h"
#include <iostream>
#include <cstring>
//#include "sip.h"
//#include "sip_tables.h"


namespace sip {

Block::Block(BlockShape shape) :
		shape_(shape)
{
	size_ = shape.num_elems();

	//c++ feature:parens cause allocated memory to be initialized to zero
	// but is also expensive. This is being removed and the block is being zeroed out
	// wherever it needs to be.
	try{
	data_ = new double[size_];

	status_[Block::onHost] = true;
	status_[Block::onGPU] = false;
	status_[Block::dirtyOnHost] = false;
	status_[Block::dirtyOnGPU] = false;
	}
	catch (const std::bad_alloc& ba){
		std::cerr << "Not enough memory in Block::Block(BlockShape shape)" << std::endl << std::flush;
		throw ba;
	}

}

//TODO consider whether data should be a Vector, or otherwise add checks for
//arrays out of bounds
Block::Block(BlockShape shape, dataPtr data):
		shape_(shape),
		data_(data)
{
	size_ = shape.num_elems();

	status_[Block::onHost] = true;
	status_[Block::onGPU] = false;
	status_[Block::dirtyOnHost] = false;
	status_[Block::dirtyOnGPU] = false;
}

Block::Block(dataPtr data):
	data_(data)
{
	std::fill(shape_.segment_sizes_+0, shape_.segment_sizes_+MAX_RANK, 1);
	size_ = 1;

	status_[Block::onHost] = true;
	status_[Block::onGPU] = false;
	status_[Block::dirtyOnHost] = false;
	status_[Block::dirtyOnGPU] = false;
}



/** The MPI_State destructor blocks until the request is no longer pending.
 * We do not need to check this here. It is important that
 */
Block::~Block() {
	// Original Assumption was that all blocks of size 1 are scalar blocks.
	// This didn't turn out to be true (sliced contiguous array blocks could also be size 1).
	// Memory leaks were being caused by this. Now this is fixed by setting data_ to nullptr
	// for blocks that wrap scalars.
	//Assumption: if size==1, data_ points into the scalar table.
	//if (data_ != nullptr && size_ >1) {

	if (data_ != NULL) {
		delete[] data_;
		data_ = NULL;
	}
}

int Block::size() {
	return size_;
}

const BlockShape& Block::shape() {
	return shape_;
}


Block::dataPtr Block::get_data() {
	return data_;
}


bool Block::operator==(const Block& rhs) const{
	if (this == &rhs) return true;
	return (size_ == rhs.size_) && (shape_ == rhs.shape_)
			&& std::equal(data_ + 0, data_+size_, rhs.data_);
}


} /* namespace sip */
