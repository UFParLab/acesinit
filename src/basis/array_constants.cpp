/*
 * array_constants.cpp  Implementations of utility routines involving array constants
 *
 *  Created on: Aug 31, 2013
 *      Author: Beverly Sanders
 */

#include "array_constants.h"
#include <stdexcept>
#include <iostream>



namespace array{

int unused_index_slot = -1;  //chosen to throw out of bounds exception if accessed
int unused_index_value = 1;
int unused_index_segment_size = 1;

/*! precondition:  index_name is all lower case
 * This is used in the setup and does not need to be part
 * of the sip.
 * TODO  Is this the right place for this???
 */
IndexType_t map_index_type(std::string name){
	if (name == "aoindex"){return aoindex; }
    if (name == "laindex"){return laindex; }
	if (name == "moaindex"){return moaindex; }
	if (name == "mobindex"){return mobindex;}
	if (name == "moindex"){return moindex;}
	if (name == "simple"){return simple;}
	if (name == "subindex"){return subindex;}
	//TODO change to throw exception or terminate?????
	std::cout<< "WARNING:  Undefined index name, defaulting to aoindex "<< name;
    return aoindex;
}


ArrayType_t intToArrayType_t(int j) {
	switch (j) {
	case 2:
		return served_array_t;
	case 108:
		return static_array_t;
//	case 203:
//		return distributed_array_t;
	case 24:
		return temp_array_t;
	case 140:
		return scalar_value_t;
	case 172:
			return predefined_scalar_value_t;
	case 0:
		return local_array_t;
	default:
		throw std::domain_error(std::string("invalid conversion to ArrayType_t"));
	}
}


IndexType_t intToIndexType_t(int j){
	switch (j) {
	case 1001:
		return aoindex;
	case 1002:
		return moindex;
	case 1003:
		return moaindex;
	case 1004:
		return mobindex;
	case 1005:
		return simple;
	case 1006:
		return laindex;
	case 1007:
		return subindex;
	default:
		throw std::domain_error("invalid conversion to IndexType_t");
	}
}


int  indexType_tToInt(IndexType_t t){
	switch (t) {
	case aoindex:
		return 1001;
	case moindex:
		return 1002;
	case moaindex:
		return 1003;
	case mobindex:
		return 1004;
	case simple:
		return 1005;
	case laindex:
		return 1006;
	case subindex:
		return 1007;
	case unused:
		return 1008;
	default:
		throw std::domain_error("error in indexType_tToInt:  this shouldn't happen");
	}
}


bool is_integer_attr(int attr){
	return ((attr & attr_integer)==attr_integer);
}
bool is_sip_consistent_attr(int attr){
	return ((attr & attr_sip_consistent) == attr_sip_consistent);
}
bool is_contiguous_attr(int attr){
	return ((attr & attr_contiguous) == attr_contiguous);
}
bool is_auto_allocate_attr(int attr){
	return ((attr & attr_auto_allocate) == attr_auto_allocate);
}
bool is_scope_extent_attr(int attr){
	return ((attr & attr_scope_extent) == attr_scope_extent);
}
bool is_predefined_attr(int attr){
	return ((attr & attr_predefined) == attr_predefined);
}
bool is_persistent_attr(int attr){
	return ((attr & attr_persistent)==attr_persistent);
}

bool is_scalar_attr(int attr){
	return ((attr & attr_scalar)==attr_scalar );
}
bool is_predefined_scalar_attr(int attr){
	   return (attr & (attr_predefined | attr_scalar)) == (attr_predefined | attr_scalar);
}


} // namespace array

