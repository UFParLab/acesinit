/*! SetupReader.cpp
 * 
 *
 *  Created on: Jul 3, 2013
 *      Author: Beverly Sanders
 */

#include "setup_reader.h"
#include "assert.h"
#include <stdexcept>

namespace setup {

SetupReader::SetupReader() {}

SetupReader::~SetupReader() {
}

void SetupReader::read(InputFile * file) {
	this->file = file;
	read_and_check_magic();
	read_and_check_version();
	read_sial_programs();
	read_predefined_ints();
	read_predefined_scalars();
	read_segment_sizes();
	read_predefined_arrays();
	read_predefined_integer_arrays();
	read_sialfile_configs();
}

std::ostream& operator<<(std::ostream& os, const SetupReader & obj) {
	os << "Sial program list:" << std::endl;
	SetupReader::SialProgList::const_iterator it;
	for (it = obj.sial_prog_list_.begin(); it != obj.sial_prog_list_.end();
			++it) {
		os << *it << std::endl;
	}
	os << "Predefined int map:" << std::endl;
	SetupReader::PredefIntMap::const_iterator iti;
	for (iti = obj.predefined_int_map_.begin();
			iti != obj.predefined_int_map_.end(); ++iti) {
		os << iti->first << "=" << iti->second << std::endl;
	}

	os << "Predefined scalar map:" << std::endl;
	SetupReader::PredefScalarMap::const_iterator its;
	for (its = obj.predefined_scalar_map_.begin();
			its != obj.predefined_scalar_map_.end(); ++its) {
		os << its->first << "=" << its->second << std::endl;
	}
	os << "Segment table info:" << std::endl;
	SetupReader::SetupSegmentInfoMap::const_iterator itg;
	for (itg = obj.segment_map_.begin(); itg != obj.segment_map_.end(); ++itg) {
		os << itg->first << ":[";
		std::vector<int>::const_iterator sit;
		for (sit = (itg->second).begin(); sit != (itg->second).end(); ++sit) {
			os << (sit == (itg->second).begin()?"":",") << *sit;
		}
		os << ']' << std::endl;
	}

	os << "Predefined arrays:" << std::endl;
	SetupReader::PredefArrMap::const_iterator itp;
	for (itp = obj.predef_arr_.begin(); itp != obj.predef_arr_.end(); ++itp){
		int rank = itp->second.first;
		int * dims = itp->second.second.first;
		double * data = itp->second.second.second;
		int num_elems = 1;
		for (int i = 0; i < rank; i++) {
			num_elems *= dims[i];
		}
		os << itp->first << ":{ rank : ";
		os << rank << " } (";
		os << dims[0];
		for (int i=1; i<rank; i++){
			os <<" ," << dims[i];
		}
		os << "), [";
		os << data [0];
		for (int i=1; i<num_elems; i++){
			os << ", " << data[i];
		}
		os << "]" << std::endl;
	}

	os << "Predefined integer arrays:" << std::endl;
	SetupReader::PredefIntArrMap::const_iterator itpi;
	for (itpi = obj.predef_int_arr_.begin(); itpi != obj.predef_int_arr_.end(); ++itpi){
		int rank = itpi->second.first;
		int * dims = itpi->second.second.first;
		int * data = itpi->second.second.second;
		int num_elems = 1;
		for (int i = 0; i < rank; i++) {
			num_elems *= dims[i];
		}
		os << itpi->first << ":{ rank : ";
		os << rank << " } (";
		os << dims[0];
		for (int i=1; i<rank; i++){
			os <<" ," << dims[i];
		}
		os << "), [";
		os << data [0];
		for (int i=1; i<num_elems; i++){
			os << ", " << data[i];
		}
		os << "]" << std::endl;
	}

	os << "Sial file configurations:" << std::endl;
	SetupReader::FileConfigMap::const_iterator itc;
	for (itc = obj.configs_.begin(); itc != obj.configs_.end(); ++itc){
		std::string fileName = itc->first;
		os << "[" << fileName << "]" << "{";
		SetupReader::KeyValueMap kvMap = itc->second;
		SetupReader::KeyValueMap::const_iterator itkv;
		itkv = kvMap.begin();
		if (itkv != kvMap.end()){
			std::string key = itkv->first;
			std::string val = itkv->second;
			os << key <<":"<< val<< ", ";
			++itkv;
		}
		for (; itkv != kvMap.end(); ++itkv){
			std::string key = itkv->first;
			std::string val = itkv->second;
			os << ", "<<key <<":"<< val<< ", ";
		}
		os << "}";
	}

	os << std::endl;
	return os;
}

void SetupReader::dump_data() {
	dump_data(std::cout);
}

void SetupReader::dump_data(std::ostream& os) {
	std::cout << this;
}

int SetupReader::predefined_int(std::string name) {
	return predefined_int_map_.at(name);
}

double SetupReader::predefined_scalar(std::string name) {
	return predefined_scalar_map_.at(name);
}

void SetupReader::read_and_check_magic() {
	int fmagic = file->read_int();
	sip::check(fmagic == sip::SETUP_MAGIC,
			std::string("setup data file has incorrect magic number"));
}

void SetupReader::read_and_check_version() {
	int fversion = file->read_int();
	sip::check(fversion == sip::SETUP_VERSION,
			std::string("setup data file has incorrect version"));
}

void SetupReader::read_sial_programs() {
	int n = file->read_int();
	for (int i = 0; i < n; ++i) {
		std::string prog = file->read_string();
		sial_prog_list_.push_back(prog);
	}
}

void SetupReader::read_predefined_ints() {
	int n = file->read_int();
	for (int i = 0; i < n; ++i) {
		std::string name = file->read_string();
		int value = file->read_int();
		predefined_int_map_[name] = value;
	}
}

void SetupReader::read_predefined_scalars() {
	int n = file->read_int();
	for (int i = 0; i < n; ++i) {
		std::string name = file->read_string();
		double value = file->read_double();
		predefined_scalar_map_[name] = value;
	}
}

void SetupReader::read_segment_sizes() {
	int n = file->read_int();  //number of segment size entries
	for (int i = 0; i < n; ++i) {
		array::IndexType_t index_type = array::intToIndexType_t(file->read_int());
		int num_segments;
		int * seg_sizes = file->read_int_array(&num_segments);
		segment_map_[index_type] = std::vector<int>(seg_sizes,
				seg_sizes + num_segments);
		delete [] seg_sizes;
	}
}

void SetupReader::read_predefined_arrays(){
	int n = file->read_int();
	for (int i = 0; i < n; i++) {
		// Name of array
		std::string name = file->read_string();
		// Rank of array
		int rank = file->read_int();
		// Dimensions
		int * dims = file->read_int_array(&rank);
		// Data
		int num_data_elems = 1;
		for (int i=0; i<rank; i++){
			num_data_elems *= dims[i];
		}
		double * data = file->read_double_array(&num_data_elems);
		std::pair<int *, double *> dataPair = std::pair<int *, double *>(dims, data);
		predef_arr_[name] = std::pair<int, std::pair<int *, double *> >(rank, dataPair);
	}
}

void SetupReader::read_predefined_integer_arrays(){
	int n = file->read_int();
	for (int i = 0; i < n; i++) {
		// Name of array
		std::string name = file->read_string();
		// Rank of array
		int rank = file->read_int();
		// Dimensions
		int * dims = file->read_int_array(&rank);
		// Data
		int num_data_elems = 1;
		for (int i=0; i<rank; i++){
			num_data_elems *= dims[i];
		}
		int * data = file->read_int_array(&num_data_elems);
		std::pair<int *, int *> dataPair = std::pair<int *, int *>(dims, data);
		predef_int_arr_[name] = std::pair<int, std::pair<int *, int *> >(rank, dataPair);
	}
}

void SetupReader::read_sialfile_configs(){
	int num_sialfiles = file->read_int();
	for (int i=0; i<num_sialfiles; i++){
		// Name of Sial File
		std::string sialfile = file->read_string();
		// Number of config entries
		int num_entries = file->read_int();
		KeyValueMap kvMap;
		for (int j=0; j<num_entries; j++){
			std::string key = file->read_string();
			std::string val = file->read_string();
			kvMap[key] = val;
		}
		configs_[sialfile] = kvMap;
	}
}



} /* namespace setup */
