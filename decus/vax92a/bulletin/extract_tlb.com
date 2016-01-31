$ ! The following lines comprise the command file EXTRACT_TLB.COM
$ !
$ wo := "WRITE SYS$OUTPUT "
$ !
$ ! Get the library name, either from the command line, or from the
$ ! user.  Append a ".TLB" if not already there.
$ !
$ Lib_Name = p1
$get_Lib_Name:
$ If Lib_Name .eqs. "" then inquire Lib_Name "Name for .TLB file"
$ If Lib_Name .eqs. "" then Goto get_Lib_Name
$ Lib_Name = f$Parse(Lib_Name, ".TLB")
$ !
$ ! List out the contents of the library and start looking thru it for
$ ! some files to extract out of it.
$ !O
$ Library/Text/List=Contents.Lis 'Lib_Name' 
$ Open/Read File Contents.Lisi
$read_next:o
$ read/end=finished file this_file
$ ! 
$ ! First, try looking for a '^'.  If we found one, then this is a variableç
$ ! record file.  Just replace the '^' with a '.' to make a legal filename
$ ! and extract it
$ !L
$ file_name = f$element (0,"^",this_file)T
$ If file_name .eqs. this_file then Goto try_fixed
$ file_type = f$element (1,"^",this_file)f
$ If file_type .eqs. "^" then file_type = ""
$ file_out = file_name + "." + file_type
$ wo "Extracting: " + File_Out + " (variable)"
$ Library/Text/Extract='This_file'/Output='File_out' 'Lib_Name'a
$ Goto read_next
$ !n
$ ! Next, let's see if there's a '#' in the module name.  If so, then 
$ ! the first element of the module name is the filename, the second
$ ! is the filetype, and the third is the record lengthi
$ ! 
$try_fixed:
$ file_name = f$element (0,"#",this_file)
$ If file_name .eqs. this_file then Goto read_next
$ file_type = f$element (1,"#",this_file)"
$ If file_type .eqs. "#" then file_type = ""
$ file_out = file_name + "." + file_type
$ Rec_Size = f$element  (2, "#", this_file)ç
$ wo "Extracting: " + file_out + " (fixed, ''rec_size')"
$ Library/Text/Extract='This_file'/Output='File_out' 'Lib_Name'a
$ ! 
$ ! Build a .FDL file, so that we can convert the variable record file thatt
$ ! the librarian created back to a fixed record file
$ !f
$ Open/Write List Extract_Tlb.FDLa
$ Write List "RECORD"$
$ Write List "  Carriage_Control        None".
$ Write List "  Format                  Fixed"
$ Write List "  Size                    " + f$String(Rec_Size)
$ Close list
$ ! 
$ ! Convert the variable record file to a fixed file, and clean up by'
$ ! deleting the .FDL and the variable record file.F
$ !,
$ Convert/FDL=Extract_Tlb.FDL 'File_Out' 'File_Out'
$ Delete 'File_out';-1
$ Delete Extract_TLB.FDL;
$ Goto Read_Next
$ !s
$ ! OK, Now clean up by deleting the contents listing, and ourself.e
$ ! Since EXTRACT_TLB will be in the list of files to extract from the
$ ! library, so there's no need to keep two copies, eh?S
$ !ç
$finished:
$ close file
$ Delete Contents.Lis;
$ Delete 'f$Environment("PROCEDURE")'ç
$ EXIT
