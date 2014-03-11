/*! io_utils.h
 * 
 *
 *  Created on: Jul 7, 2013
 *      Author: Beverly Sanders
 */

#ifndef IO_UTILS_H_
#define IO_UTILS_H_

#include <string>
#include <iostream>
#include <fstream>
#include "sip.h"


namespace setup{

   class OutputFile {
	public:
		virtual void write_string(const std::string&)=0;
		virtual void write_int(int)=0;
		virtual void write_int_array(int size, int[] )=0;
		virtual void write_double(double)=0;
		virtual void write_double_array(int size, double[])=0;
		virtual void write_size_t_val(size_t)=0;
		virtual ~OutputFile();
	protected:
		OutputFile();  //must use subclass
		std::ofstream *file;
	private:
		DISALLOW_COPY_AND_ASSIGN(OutputFile);
   };

	class BinaryOutputFile: public OutputFile{
	public:
		std::string get_file_name();
		virtual void write_string(const std::string&);
		virtual void write_int(int);
		virtual void write_int_array(int size, int[] );
		virtual void write_double(double);
		virtual void write_double_array(int size, double[]);
		virtual void write_size_t_val(size_t);
		BinaryOutputFile(const std::string&);
		virtual~BinaryOutputFile();
	private:
		std::string file_name_;
		DISALLOW_COPY_AND_ASSIGN(BinaryOutputFile);
	};

	class InputFile {
	public:
		virtual bool is_open(){
			return file!= NULL && file -> is_open();
		}
		virtual std::string read_string()=0;
		virtual int read_int()=0;
		virtual double read_double()=0;
		virtual int * read_int_array(int * size)=0;
		virtual double * read_double_array(int *size)=0;
		virtual double * read_double_array_siox(int *size)= 0;
		virtual size_t read_size_t()=0;
		virtual std::string * read_string_array(int * size)=0;
		virtual ~InputFile();
	protected:
		InputFile();
		std::ifstream *file;
	private:
		DISALLOW_COPY_AND_ASSIGN(InputFile);
	};

	class BinaryInputFile : public InputFile{
	public:
		virtual std::string read_string();
		virtual int read_int();
		virtual double read_double();
		virtual int * read_int_array(int * size);
		virtual double * read_double_array(int *size);
		virtual double * read_double_array_siox(int *size);
		virtual size_t read_size_t();
		virtual std::string * read_string_array(int * size);
		std::string get_file_name();
		BinaryInputFile(const std::string&);
        virtual ~BinaryInputFile();
	private:
        std::string file_name_;
		DISALLOW_COPY_AND_ASSIGN(BinaryInputFile);
	};

} /*namespace setup */

#endif /* IO_UTILS_H_ */
