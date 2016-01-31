





                          BULLETIN



     BULLETIN provides an effective  means  for  the  system

managers  to  disseminate information to users in a friendly

fashion.



                          Features



      o  Bulletins of general interest can be  sent  to  all

         users

      o  Bulletins can be filed by subject  for  perusal  by

         interested users

      o  Users  are  presented  with  bulletins  only   once

         (unless they request old ones)

      o  Users  are   informed   of   new   information   on

         specialized topics only once

      o  Users may optionally file or print bulletins





                         Setting up



The nicest feature about bulletin is  its  friendly  nature.

Unwanted and indecipherable information is not forced down a

users' throats, but is there for any interested user.  Thus,

for  example,  if  you  file a bulletin under the subject of

PASCAL, all users will be informed (only once!)  that  there

is new information on PASCAL.  Interested users can read the

new bulletins, while those not using PASCAL can ignore them.

There  are  three things to set up for most effective use of

the program.



     1.  Put the BULLETIN help  file  in  your  system  help

         file:

          $  LIBR/HELP SYS$HELP:HELPLIB BULLETIN





     2.  If you have  a  system  wide  login.com  file,  you

         should enter the command:

         $ if "''f$mode()" .eqs.  "INTERACTIVE" then -

                 $bulletin

         If  you  enter   this   command   before   enabling

         control-Y,  this  will  force  users  to  read  new

         BULLETINS  filed   under   the   general   interest

         category.



     3.  Encourage  users  to  place   in   their   personal

         LOGIN.COM   files   BULLETIN   commands   to   read

         information on subjects of interest to them.





Refer to the first bulletin and the  HELP  BULLETIN  command

for more info.

