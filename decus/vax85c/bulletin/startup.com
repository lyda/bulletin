 �$ UIC := 'F$GETJPI("","UIC") �$ SET UIC [1,4] : �$ SET PROTECT=(SYSTEM:RWE,OWNER:RWE,WORLD,GROUP)/DEFAULT �$ RUN BULLETIN# �ADD/PERMANENT/SYSTEM INSTRUCT.TXT 1 �INFORMATION ON HOW TO USE THE BULLETIN UTILITY.  �EXIT �$ SET UIC 'UIC' 3 �$ DEFAULT := 'F$FILE("SYS$LOGIN:LOGIN.COM","PRO") # �$ SET PROTECT=('DEFAULT')/DEFAULT 