/*! setup_reader.h
 * 
 *
 *  Created on: Jul 3, 2013
 *      Author: Beverly Sanders
 */

#ifndef SETUPREADER_H_
#define SETUPREADER_H_

#include <iostream>
#include <fstream>
#include <map>
#include <queue>
#include <string>
#include "sip.h"
#include "io_utils.h"
#include "array_constants.h"


namespace array {
   class Block;
}

namespace setup {


/*!
 *
 */
class SetupReader {

public:

	SetupReader();
	virtual ~SetupReader();
    void read(InputFile *);

	void dump_data();  	//for debugging;
	void dump_data(std::ostream&);  //for debugging;

	typedef std::map<std::string, int> PredefIntMap;
	typedef std::map<std::string, double> PredefScalarMap;
	typedef std::vector<int> SegmentDescriptor;
	typedef std::map<array::IndexType_t, std::vector<int> > SetupSegmentInfoMap;
	typedef std::vector<std::string> SialProgList;
	typedef std::map<std::string, std::pair<int, std::pair<int *, double *> > > PredefArrMap;
	typedef std::map<std::string, std::string> KeyValueMap;
	typedef std::map<std::string, KeyValueMap > FileConfigMap;

    // Added by Dr. Sanders
	typedef std::map<std::string, array::Block*> PredefinedContiguousArrayMap;

	int predefined_int(std::string);
    double predefined_scalar(std::string);
    array::Block * predefined_contiguous_array(std::string);
    int num_segments(array::IndexType_t);
    //int * segment_array(sip::IndexType_t);
    SialProgList sial_prog_list_;
	PredefIntMap predefined_int_map_;
	PredefScalarMap predefined_scalar_map_;
	SetupSegmentInfoMap segment_map_;
	PredefArrMap predef_arr_; 	// Map of predefined arrays
	FileConfigMap configs_;		// Map of sial files to their configurations in the form of a key-value map


    // Added by Dr. Sanders
	PredefinedContiguousArrayMap predefined_contiguous_array_map_;

	friend std::ostream& operator<<(std::ostream&, const SetupReader &);


private:
	void read_and_check_magic();
	void read_and_check_version();
	void read_sial_programs();
    void read_predefined_ints();
    void read_predefined_scalars();
    void read_segment_sizes();
    void read_predefined_arrays();
    void read_sialfile_configs();


	void dump_predefined_int_map(std::ostream&);
	void dump_predefined_scalar_map(std::ostream&);
	void dump_sial_prog_list(std::ostream&);
	void dump_segment_map(std::ostream&);
	void dump_predefined_map(std::ostream&);

	InputFile* file;




	DISALLOW_COPY_AND_ASSIGN(SetupReader);

};

} /* namespace setup */

#endif /* SETUPREADER_H_ */
