/*! io_utils.cpp  Contains routines to read and write primitive types and arrays from
 * binary and text files.
 * 
 *
 *  Created on: Jul 7, 2013
 *      Author: Beverly Sanders
 */


#include "io_utils.h"
#include <stdexcept>
#include <sstream>
#include <stdio.h>
#include <stdlib.h>

namespace setup{

/* Default constructors and destructors for OutputFile */
OutputFile::OutputFile():file(NULL){}

OutputFile::~OutputFile(){
	if(file){file->close(); delete file;}
}

/* BinaryOutputFile */
BinaryOutputFile::BinaryOutputFile(const std::string& name){
	file = new std::ofstream(name.c_str(), std::ios::binary | std::ios::trunc);
}
BinaryOutputFile::~BinaryOutputFile(){}

void BinaryOutputFile::write_string(const std::string& aString) {
        //trim trailing spaces
        size_t endpos = aString.find_last_not_of(" ");
        std::string trimmed = (endpos == std::string::npos) ? "" : aString.substr(0,endpos+1);
	int length = (int) trimmed.length() + 1; //extra for null
	write_int(length);
//std::cout << "writing string " << trimmed << ", length=" << length << std::endl;
	file->write(trimmed.c_str(), length);
}

void BinaryOutputFile::write_int( int value) {
	file->write(reinterpret_cast<char *>(&value), sizeof(int));
}

void BinaryOutputFile::write_int_array(int size, int  values[]) {
	write_int(size);
	file->write(reinterpret_cast<char *>(values), sizeof(int) * size);
}

void BinaryOutputFile::write_double(double value) {
	file->write(reinterpret_cast<char *>(&value), sizeof(double));
}

void BinaryOutputFile::write_double_array(int size, double values[]) {
	write_int(size);
	file->write( reinterpret_cast<char *>(values), sizeof(double) * size);
}

void BinaryOutputFile::write_size_t_val(size_t value) {
	file->write( reinterpret_cast<char *>(&value), sizeof(size_t));
}

InputFile::InputFile():file(NULL){}

InputFile::~InputFile(){
		if ((file)) {
		  if (file->is_open()){  file->close();}
		  delete file;
	}
}
/* BinaryInputFile */
BinaryInputFile::BinaryInputFile(const std::string& name):file_name_(name){
	file= new std::ifstream(name.c_str(), std::ifstream::binary);
	if (!file->is_open()){
		std :: cerr << "File "<< name  <<" could not be opened !";
		exit(-1);
	}
	//assert (file->is_open());  //TODO  better error handling
}
BinaryInputFile::~BinaryInputFile(){}

std::string BinaryInputFile::get_file_name(){
		return file_name_;
}

std::string BinaryInputFile::read_string() {
	int length = read_int();
	char *chars = new char[length+1];
	file-> read(chars, length);
    sip::check(file->good(), std::string("malformed input file ") + file_name_);
	chars[length]= '\0';
	std::string s(chars);
	delete [] chars;
	return s;
}

int BinaryInputFile::read_int() {
	int value;
	file->read( reinterpret_cast<char *>(&value), sizeof(value));
	sip::check(file->good(), std::string( "error in read_int of input file ") + file_name_ + "\n");
	return value;
}

double BinaryInputFile::read_double() {
	double value;
	file->read( reinterpret_cast<char *>(&value), sizeof(value));
	sip::check(file->good(), std::string( "error in read_double of input file ") + file_name_ + "\n");
	return value;
}

int * BinaryInputFile::read_int_array(int * size) {
	int sizec = read_int();
	int * values = new int[sizec];
	file->read( reinterpret_cast<char *>(values), sizeof(int) * sizec);
	*size = sizec;
	sip::check(file->good(), std::string( "error in read_int_array of input file ") + file_name_ + "\n");
	return values;
}

double * BinaryInputFile::read_double_array(int *size){
	int sizec = read_int();
	double * values = new double[sizec];
	file->read( reinterpret_cast<char *>(values), sizeof(double) * sizec);
	sip::check(file->good(), std::string( "error in read_double_array of input file ") + file_name_ + "\n");
	*size = sizec;
	return values;
}

std::string * BinaryInputFile::read_string_array(int * size){
	int sizec = read_int();
	std::string * strings = new std::string[sizec];
	for (int i = 0; i < sizec; ++i){
		strings[i] = read_string();
	}
	*size = sizec;
	sip::check(file->good(), std::string( "error in read_int_array of input file ") + file_name_ + "\n");
	return strings;
}

//TODO figure out what is going on with int vs size_t
double * BinaryInputFile::read_double_array_siox(int *size){
	int sizec = read_int();
//	std::cout << "size of double array " << sizec << std::endl;
	double * values = new double[sizec];
	file->read( reinterpret_cast<char *>(values), sizeof(double) * sizec);
	*size = sizec;
//	std::cout << "returning from read_double_array_siox" << std::endl;
	return values;
}

size_t BinaryInputFile::read_size_t(){
	size_t value;
	file->read( reinterpret_cast<char *>(&value), sizeof(value));
	return value;
}


} /*namespace setup*/
