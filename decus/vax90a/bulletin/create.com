 $ FORTRAN/EXTEND BULLETIN  $ FORTRAN/EXTEND BULLETIN0 $ FORTRAN/EXTEND BULLETIN1 $ FORTRAN/EXTEND BULLETIN2 $ FORTRAN/EXTEND BULLETIN3 $ FORTRAN/EXTEND BULLETIN4 $ FORTRAN/EXTEND BULLETIN5 $ FORTRAN/EXTEND BULLETIN6 $ FORTRAN/EXTEND BULLETIN7 $ FORTRAN/EXTEND BULLETIN8 $ FORTRAN/EXTEND BULLETIN9 $ MAC ALLMACS  $ SET COMMAND/OBJ BULLCOM  $ SET COMMAND/OBJ BULLMAIN8 $ IF F$SEARCH("BULL.OLB") .NES. "" THEN DELETE BULL.OLB;7 $ IF F$SEARCH("BULL.OLB") .EQS. "" THEN LIB/CREATE BULL  $ LIB BULL *.OBJ;  $ DELETE *.OBJ;* $ @BULLETIN.LNK 