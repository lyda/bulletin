	parameter subject_flag = '1'x
	parameter subject_present = '2'x
	parameter since_flag = '4'x
	parameter before_flag = '10'x
	parameter sequence_flag = '40'x
	parameter sequence_present = '80'x
	parameter old_flag = '100'x
	parameter add_flag = '200'x
	parameter delete_flag = '400'x
	parameter file_present = '800'x
	parameter directory_flag = '1000'x
	integer*4 parameters,since_buffer(2),before_buffer(2)
	integer*4 seq(2)
	character*9 subject_buffer
	character*64 file_name
	common /bulpar/ parameters,subject_buffer,since_buffer,
	1	before_buffer,seq,file_name

	integer*4 lincnt
	common /lincnt/ lincnt
