= $! Create rest of BULLETIN where you lack a FORTRAN compiler. * $! FORTRAN/Extend/Nocheck/Nodebug BULLETIN+ $! FORTRAN/Extend/Nocheck/Nodebug BULLETIN0 + $! FORTRAN/Extend/Nocheck/Nodebug BULLETIN1 + $! FORTRAN/Extend/Nocheck/Nodebug BULLETIN2 + $! FORTRAN/Extend/Nocheck/Nodebug BULLETIN3 + $! FORTRAN/Extend/Nocheck/Nodebug BULLETIN4 + $! FORTRAN/Extend/Nocheck/Nodebug BULLETIN5 + $! FORTRAN/Extend/Nocheck/Nodebug BULLETIN6 + $! FORTRAN/Extend/Nocheck/Nodebug BULLETIN7 + $! FORTRAN/Extend/Nocheck/Nodebug BULLETIN8 + $! FORTRAN/Extend/Nocheck/Nodebug BULLETIN9  $ MACRO ALLMACS  $! SET COMMAND/OBJ BULLCOM $! SET COMMAND/OBJ BULLMAIN 9 $! IF F$SEARCH("BULL.OLB") .NES. "" THEN DELETE BULL.OLB; 8 $! IF F$SEARCH("BULL.OLB") .EQS. "" THEN LIB/CREATE BULL $ LIB BULL *.OBJ;  $ DELETE *.OBJ;* $ @BULLETIN.LNK 