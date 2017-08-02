/*
 * worker_persistent_array_manager.cpp
 *
 *  Created on: Apr 21, 2014
 *      Author: njindal
 */

#include "worker_persistent_array_manager.h"
#include <algorithm>
//#include "id_block_map.h"
#include "io_utils.h"

namespace sip {

	WorkerPersistentArrayManager::WorkerPersistentArrayManager() {}

	WorkerPersistentArrayManager::~WorkerPersistentArrayManager() {

		WorkerPersistentArrayManager::LabelContiguousArrayMap::iterator cit;
		for (cit = contiguous_array_map_.begin(); cit != contiguous_array_map_.end(); ++cit){
			delete cit -> second;
			cit -> second = NULL;
		}

		//WorkerPersistentArrayManager::LabelDistributedArrayMap::iterator dit;
		//for (dit = distributed_array_map_.begin(); dit != distributed_array_map_.end(); ++dit){
		//	IdBlockMap<Block>::delete_blocks_from_per_array_map(dit -> second);
		//	delete dit -> second;
		//	dit -> second = nullptr;
		//}
	}

	void WorkerPersistentArrayManager::init_from_checkpoint(const std::string& filename){
//		if(! SIPMPIAttr::get_instance().is_worker()) return;  //only workers do this

//		std::cerr<< "WorkerPersistentArrayManager::init_from_checkpoint(" << filename << ")" << std::endl << std::flush;
		//CHECK(contiguous_array_map_.empty(), "initializing nonempty persistent contiguous_array_map_ from checkpoint");
		//CHECK(scalar_value_map_.empty(), "initializing nonempty persistent scalar_value_map_ from checkpoint");
			setup::InputStream * file;
			setup::BinaryInputFile *bfile = new setup::BinaryInputFile(filename);  //checkpoint file opened here
			file = bfile;
			//restore scalars
//			std::cerr<< "WorkerPersistentArrayManager opened the file " << filename << std::endl << std::flush;
			int num_scalars = file->read_int();
//			std::cerr << "\nnum_scalars=" << num_scalars << std::endl << std::flush;
			for (int i=0; i < num_scalars; ++i){
				std::string name = file->read_string();
				double value = file->read_double();
				scalar_value_map_[name] = value;
//				std::cerr << "initializing persistent scalar "<< name << "=" << value << " from restart" << std::endl << std::flush;
			}
			//restore contiguous arrays
			int num_arrays = file->read_int();
//			std::cerr << "\n num_arrays=" << num_arrays << std::endl << std::flush;
			for (int i = 0; i < num_arrays; i++) {
				// Name of array
				std::string name = file->read_string();
				// Rank of array
				int rank = file->read_int();
				// Dimensions
				int * dims = file->read_int_array(&rank);
				// Number of elements
				int num_data_elems = 1;
				for (int i=0; i<rank; i++){
					num_data_elems *= dims[i];
				}
//				std::cerr << "initializing persistent array" << name  << " from restart with rank= "<< rank << "["
//						<< dims[0] << ","
//						<< dims[1] << ","
//						<< dims[2] << ","
//						<< dims[3] << ","
//						<< dims[4] << ","
//						<< dims[5] << "]" << std::endl << std::flush;

				double * data = file->read_double_array(&num_data_elems);
        		array::segment_size_array_t dim_sizes;
				std::copy(dims+0, dims+rank,dim_sizes);
				if (rank < MAX_RANK) std::fill(dim_sizes+rank, dim_sizes+MAX_RANK, 1);
				sip::BlockShape shape(dim_sizes, rank);
				delete [] dims;

				Block::BlockPtr block = new Block(shape,data);
				contiguous_array_map_[name]=block;
//				std::cerr << *block << std::endl << std::flush;

			}

//			std::cerr << "dumping worker's persistent array map " << std::endl << *this << std::endl << std::flush;

	}

	void WorkerPersistentArrayManager::get_contiguous_from_checkpoint(const std::string& label, int* num_elems,  int** extents, double** values){
		LabelContiguousArrayMap::iterator it = contiguous_array_map_.find(
				label);
		//CHECK(it != contiguous_array_map_.end(),
		//		"contiguous array to restore with label " + label
		//				+ " not found");
		std::cerr<< "restoring contiguous " << label << "=" /*<< *(it -> second)*/ << std::endl;
		Block* b = it-> second;
		BlockShape shape = b->shape();
		*num_elems = shape.num_elems();
		*extents = shape.segment_sizes_;
		*values = b->get_data();
		return;
	}

	//std::ostream& operator<<(std::ostream& os, const WorkerPersistentArrayManager& obj){
	//	os << "********WORKER PERSISTENT ARRAY MANAGER********" << std::endl;
	//	os << "Marked arrays: size=" << obj.persistent_array_map_.size() << std::endl;
	//	WorkerPersistentArrayManager::ArrayIdLabelMap::const_iterator mit;
	//	for (mit = obj.persistent_array_map_.begin(); mit != obj.persistent_array_map_.end(); ++mit){
	//		os << mit -> first << ": " << mit -> second << std::endl;
	//	}
	//	os << "ScalarValueMap: size=" << obj.scalar_value_map_.size()<<std::endl;
	//	WorkerPersistentArrayManager::LabelScalarValueMap::const_iterator it;
	//	for (it = obj.scalar_value_map_.begin(); it != obj.scalar_value_map_.end(); ++it){
	//		os << it->first << "=" << it->second << std::endl;
	//	}
	//	os << "ContiguousArrayMap: size=" << obj.contiguous_array_map_.size() << std::endl;
	//	WorkerPersistentArrayManager::LabelContiguousArrayMap::const_iterator cit;
	//	for (cit = obj.contiguous_array_map_.begin(); cit != obj.contiguous_array_map_.end(); ++cit){
	//		os << cit -> first << std::endl;
	//	}
	//	os << "Distributed/ServedArrayMap: size=" << obj.distributed_array_map_.size() << std::endl;
	//	WorkerPersistentArrayManager::LabelDistributedArrayMap::const_iterator dit;
	//	for (dit = obj.distributed_array_map_.begin(); dit != obj.distributed_array_map_.end(); ++dit){
	//		os << dit -> first << std::endl;
	//		//os << dit -> second << std::endl;
	//	}
	//	os<< "*********END OF WORKER PERSISTENT ARRAY MANAGER******" << std::endl;
	//	return os;
	//}

} /* namespace sip */
