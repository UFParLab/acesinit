/** phase1_writer.cpp
 *
 *  Created on: Jun 23, 2013
 *      Author: Bev
 */

#include "setup_writer.h"
#include <algorithm>
#include <assert.h>
#include <cctype>

namespace setup {

// Debugging Method
void printArray(std::string prefix, std::string name, int rank, int *dims, double *data){
	// Debug
	std :: cout << prefix << name << ", (" << dims[0];
	for (int i=1; i<rank ; i++){
		std :: cout << ", " << dims[i];
	}
	int num_elems = 1;
	for (int i=0; i<rank; i++){
		num_elems *= dims[i];
	}
	std :: cout << "), [" << num_elems << "], {" << data[0];
	for (int i=1; i<num_elems; i++){
		std :: cout << ", " << data[i];
	}
	std :: cout <<"}" << std :: endl;
}


SetupWriter::SetupWriter(std::string jobname, OutputFile* file):
		jobname_(jobname),
		file(file) {
}

SetupWriter::~SetupWriter(){
	delete file;
}

void SetupWriter::addPredefinedIntHeader(std::string name, int val) {
	std::transform(name.begin(), name.end(), name.begin(), ::tolower);
	if (header_constants_.count(name)){
		std::cerr << "Duplicate predefined int " << name << std::endl;
	}
	header_constants_[name] = val;
}

void SetupWriter::addPredefinedIntData(std::string name, int val) {
	std::transform(name.begin(), name.end(), name.begin(), ::tolower);
	if (data_constants_.count(name)){
		std::cerr << "Duplicate predefined int " << name << std::endl;
	}
	if (header_constants_.count(name)){
	    std::cerr << "Duplicate predefined int " << name << std::endl;
	}
	data_constants_[name] = val;
}

void SetupWriter::addSialProgram(std::string name) {
	sial_programs_.push_back(name);
}

void SetupWriter::addSegmentInfo(array::IndexType_t index_type_num, int num_segments, int * segment_sizes){
	if (segments_.count(index_type_num)){
		std::cerr << "Duplicate segments array for " << index_type_num << std::endl;
	}
	// Put copy of array into table
	int * alloc_segment_sizes = new int[num_segments];
	std::copy(segment_sizes, segment_sizes+num_segments, alloc_segment_sizes);

	segments_[index_type_num] = std::pair<int, int*>(num_segments, alloc_segment_sizes);
}

void SetupWriter::addPredefinedScalar(std::string name, double value){
	std::transform(name.begin(), name.end(), name.begin(), ::tolower);
	if (scalars_.count(name)){
		std::cerr << "Duplicate predefined scalar " << name << std::endl;
	}
	scalars_[name] = value;
}


void SetupWriter::addPredefinedContiguousArray(std::string name, int rank, int * dims,
			double * data){

	//printArray("before adding to DS", name, rank, dims, data);

	std::transform(name.begin(), name.end(), name.begin(), ::tolower);
	if (predef_arr_.count(name)){
		std::cerr << "Duplicate predefined array " << name << std::endl;
	}
	// Put copy of array into table
	int num_elems = 1;
	for (int i=0; i<rank; i++){
		num_elems *= dims[i];
	}
	double * alloc_data = new double[num_elems];
	std::copy(data, data+num_elems, alloc_data);
	int * alloc_dims = new int[rank];
	std::copy(dims, dims+rank, alloc_dims);

	std::pair<int *, double *> dataPair = std::pair<int *, double *>(alloc_dims, alloc_data);
	predef_arr_[name] = std::pair<int, std::pair<int *, double *> >(rank, dataPair);


	//printArray("After adding to DS", name, predef_arr_[name].first, predef_arr_[name].second.first, predef_arr_[name].second.second);

}

void SetupWriter::addSialFileConfigInfo(std::string sialfile, std::string key, std::string value){
	std::transform(sialfile.begin(), sialfile.end(), sialfile.begin(), ::tolower);
	std::transform(key.begin(), key.end(), key.begin(), ::tolower);
	std::transform(value.begin(), value.end(), value.begin(), ::tolower);
	if (configs_.count(sialfile) == 0){
		std::map<std::string, std::string> configMap;
		configMap[key] = value;
		configs_[sialfile] = configMap;
	} else {
		std::map<std::string, std::string> &configMap = configs_[sialfile];
		configMap[key] = value;
		configs_[sialfile] = configMap;
	}
}


//TODO  add timestamp
void SetupWriter::write_header_file(){
	//write predefined ints to header file
	std::ofstream header;
	std::string header_name = jobname_ + ".h";
	header.open(header_name.data(), std::ios::trunc);
	header << "#ifndef SETUP_HEADER_H_" << std::endl;
	header << "#define SETUP_HEADER_H_" << std::endl;
	for (PredefInt::iterator it=header_constants_.begin();
			it!= header_constants_.end(); ++it){
		 header << "#define " << it->first << " " << it->second << std::endl;
	}
	header << "#endif //SETUP_HEADER_H_" << std::endl;
	header.close();
}

void SetupWriter::write_data_file() {
	file -> write_int(sip::SETUP_MAGIC);
	file -> write_int(sip::SETUP_VERSION);
	//write sial program names
	int num_sial_programs = sial_programs_.size();
	file -> write_int(num_sial_programs);
	for (SialProg::iterator it=sial_programs_.begin();
			it!= sial_programs_.end(); ++it){
		 file -> write_string(*it);
	}
	//write predefined ints
	int nints = data_constants_.size();
	file->write_int(nints);
	for (PredefInt::iterator it=data_constants_.begin();
			it!= data_constants_.end(); ++it){
		 file -> write_string(it->first);
		 file -> write_int(it->second);
	}
	//write predefined scalars
	int nscalars = scalars_.size();
	file->write_int(nscalars);
	for (PredefScalar::iterator it=scalars_.begin();
			it!= scalars_.end(); ++it){
		 file -> write_string(it->first);
		 file -> write_double(it->second);
	}
	//write segment info
    int num_segment_size_arrays = segments_.size();
	file->write_int(num_segment_size_arrays);
	for (SegSizeArray::iterator it=segments_.begin();
			it!= segments_.end(); ++it){
		 file -> write_int(it->first);  // index type
		 int nsegs = it->second.first; //num_segments
		 file -> write_int_array(nsegs, it->second.second); //array of seg sizes
	}
	//write predefined contiguous array
	int num_predef_arrays = predef_arr_.size();
	file->write_int(num_predef_arrays);
	for (PredefArrMap::iterator it = predef_arr_.begin(); it != predef_arr_.end(); ++it){
		// Array Name
		file->write_string(it->first);
		// Array Rank
		int array_rank = it->second.first;
		file->write_int(array_rank);
		// Array Dimensions
		int * array_dims = it->second.second.first;
		file->write_int_array(array_rank, array_dims);
		// Array Data
		int num_data_elems = 1;
		for (int i=0; i<array_rank; i++){
			num_data_elems *= array_dims[i];
		}
		double * array_data = it->second.second.second;
		file->write_double_array(num_data_elems, array_data);

		//printArray("To file", it->first, array_rank,array_dims, array_data);

	}
	// Write configuration per sial file
	std::cout<<"Writing config info to file...";
	int num_sialfile_configs = configs_.size();
	file->write_int(num_sialfile_configs);
	for (FileConfigMap::iterator it = configs_.begin(); it != configs_.end(); ++it){
		// Sial file name
		file->write_string(it->first);
		// Key value map
		std::map<std::string, std::string> &kvConfig = it->second;
		int num_config_elems = kvConfig.size();
		file->write_int(num_config_elems);
		for (KeyValueMap::iterator it2 = kvConfig.begin(); it2 != kvConfig.end(); ++it2){
			file->write_string(it2->first);
			file->write_string(it2->second);
		}
	}
}



}/* namespace setup */


