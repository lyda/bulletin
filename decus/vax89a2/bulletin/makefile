# Makefile for BULLETIN

Bulletin : Bulletin.Exe Bull.Hlb

Bulletin.Exe : Bull.Olb
   Link /NoTrace Bull.Olb/Lib /Inc=Bulletin$Main,Sys$System:Sys.Stb/Sel -
        /NoUserlib /Exe=Bulletin.Exe

Bull.Olb : Bulletin.Obj Bulletin0.Obj Bulletin1.Obj Bulletin2.Obj  \
           Bulletin3.Obj Bulletin4.Obj Bulletin5.Obj Bulletin6.Obj \
           Bulletin7.Obj Bulletin8.Obj Bulletin9.Obj \
           Bullcom.Obj Bullmain.Obj Allmacs.Obj
   Library /Create Bull.Olb *.Obj
   Purge /Log *.Obj,*.Exe

Bulletin.Obj : Bulletin.For Bullfiles.Inc Bulldir.Inc Bullfolder.Inc \
               Bulluser.Inc
   Fortran /Extend /NoList Bulletin.For

Bulletin0.Obj : Bulletin0.For Bulldir.Inc Bulluser.Inc Bullfolder.Inc \
                Bullfiles.Inc
   Fortran /Extend /NoList Bulletin0.For

Bulletin1.Obj : Bulletin1.For Bulldir.Inc Bullfolder.Inc Bulluser.Inc \
                Bullfiles.Inc
   Fortran /Extend /NoList Bulletin1.For

Bulletin2.Obj : Bulletin2.For Bulldir.Inc Bulluser.Inc Bullfolder.Inc \
                Bullfiles.Inc
   Fortran /Extend /NoList Bulletin2.For

Bulletin3.Obj : Bulletin3.For Bulldir.Inc Bullfolder.Inc Bulluser.Inc \
                Bullfiles.Inc
   Fortran /Extend /NoList Bulletin3.For

Bulletin4.Obj : Bulletin4.For Bullfolder.Inc Bulluser.Inc Bullfiles.Inc \
                Bulldir.Inc
   Fortran /Extend /NoList Bulletin4.For

Bulletin5.Obj : Bulletin5.For Bulldir.Inc Bulluser.Inc Bullfolder.Inc \
                Bullfiles.Inc
   Fortran /Extend /NoList Bulletin5.For

Bulletin6.Obj : Bulletin6.For Bulldir.Inc Bulluser.Inc Bullfolder.Inc \
                Bullfiles.Inc
   Fortran /Extend /NoList Bulletin6.For

Bulletin7.Obj : Bulletin7.For Bulldir.Inc Bulluser.Inc Bullfolder.Inc \
                Bullfiles.Inc
   Fortran /Extend /NoList Bulletin7.For

Bulletin8.Obj : Bulletin8.For Bulldir.Inc Bulluser.Inc Bullfolder.Inc \
                Bullfiles.Inc
   Fortran /Extend /NoList Bulletin8.For

Bulletin9.Obj : Bulletin9.For Bulldir.Inc Bulluser.Inc Bullfolder.Inc \
                Bullfiles.Inc
   Fortran /Extend /NoList Bulletin9.For

Allmacs.Obj : Allmacs.mar
   Macro   /NoList Allmacs.Mar

Bullcom.Obj : Bullcom.cld
   Set Command /Obj Bullcom.Cld

Bullmain.Obj : Bullmain.cld
   Set Command /Obj Bullmain.Cld

Bull.Hlb : Bullcoms1.Hlp Bullcoms2.Hlp
   Library /Create /Help Bull.Hlb Bullcoms1.Hlp, Bullcoms2.Hlp
   Purge Bull.Hlb
*.hlb :
        lib/help/cre $*

