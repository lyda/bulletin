	implicit integer (a - z)
	include 'bullcom.for'
	integer stat
	logical lstat
	equivalence (lstat,stat)
	logical is,isnt

	open (unit=4,file='SYS$OUTPUT',carriagecontrol='fortran')
	open (unit=5,file='SYS$COMMAND')
	call directory
	end


	subroutine directory
	implicit integer (a - z)
	include 'bullcom.for'
	integer*4 t1(2),t2(2)
	character*23 date1,date2

	open (unit=1,file='sys$public:sysbulsub.dat',status='old',
	1	dispose='keep',access='keyed',organization='indexed',
	1	recordtype='fixed',recordsize=17,key=(1:9:character),
	1	shared,readonly,err=100)
	open (unit=3,file='sys$disk:[]sysbull.dat',status='old',
	1	dispose='keep',access='keyed',organization='indexed',
	1	recordtype='fixed',recordsize=25,key=(1:9:character),
	1	shared,readonly,err=90)
3	continue
	read (1,keyge='         ',err=100) subject_buffer,t1
	lc = 0
	t2(1) = 0
	t2(2) = 0
	read (3,key=subject_buffer,err=5,end=5) subject_buffer,t2
5	continue
	subject_buffer = '         '
10	continue
	  if (timcmp(t1,t2) .gt. 0) then
	    if (lc.eq.0) then
	      call outln(4,
     *'    Subject    Date of last entry       Date last read')
	      call outln(4,
     *'    -------    ------------------       --------------')
	      lc = 1
	      endif
	    call lib$sys_asctim(,date1,t1)
	    if (t2(1) .eq. 0 .and. t2(2) .eq.0) then
	      date2 = 'Never read'
	    else
	      call lib$sys_asctim(,date2,t2)
	      endif
	    call outln(4,'    '//subject_buffer//'  '//date1//'  '//date2)
	    endif
	  read (1,err=100,end=100) subject_buffer,t1
	  t2(1) = 0
	  t2(2) = 0
	  read (3,key=subject_buffer,err=10,end=10) subject_buffer,t2
	  go to 10
90	continue
	write (4,95)
95	format(' User never logged in!!')
	go to 110
100	continue
	close (unit=3)
110	continue
	close (unit=1)
	return
	end


	subroutine outln(lun,dat)
	implicit integer (a - z)
	parameter pagesz = 24
	character*(*) dat
	include 'bullcom.for'
 
	if (lun.eq.4) then
	  if (lincnt.ge.pagesz-4) then
	    write (4,10)
10	    format(/'$Type <cr> to continue ')
	    read (5,20,end=25,err=25)
20	    format()
25	    continue
	    lincnt = 0
	    endif
	  lincnt = lincnt+1
	  endif
	write (lun,30) dat
30	format(1x,a)
	return
	end

 
	integer function timcmp(t1,t2)
	integer*4 t1(2),t2(2),dif(2)

	call lib$subx(t1,t2,dif)
	if (dif(2).ne.0) then
	  timcmp = dif(2)
	else if (dif(1).ne.0) then
	  timcmp = 1
	else
	  timcmp = 0
	  endif
	return
	end
 
