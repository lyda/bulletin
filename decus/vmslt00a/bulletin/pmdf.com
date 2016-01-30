$link/exe=pmdf_exe:bulletin_master.exe -
    pmdf_exe:bulletin_master.obj/sysexe,pmdf_exe:pmdfshr_link.opt/opt, -
    pmdf_com:ident.opt/opt,pmdf_exe:rmspro, -
    bull.olb/lib,sys$share:vaxcrtl/lib
