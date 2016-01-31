c
c	Program by:
c		Robert K. Stodola
c		The Institute for Cancer Research
c		7701 Burholme Avenue
c		Philadelphia, PA   19111
c
	implicit integer (a - z)
	include 'bullcom.for'
	character*132 inline
	integer stat
	logical lstat
	equivalence (lstat,stat)
	logical is,isnt

	lincnt = 0
	stat = lib$get_foreign(inline,,len)
	stat = bullparse(inline(1:len))
	open (unit=4,file='SYS$OUTPUT',carriagecontrol='fortran')
	open (unit=5,file='SYS$COMMAND')
	if (.not.stat) call lib$stop(%val(stat))
	if (is(delete_flag)) then
	  call delete
	else if (is(add_flag)) then
	  call add
	else if (is(directory_flag)) then
	  call directory
	else
	  call list
	  endif
	end


	subroutine delete
	implicit integer (a - z)
	include 'bullcom.for'
	integer stat
	logical lstat
	equivalence (lstat,stat)
	logical is,isnt
	integer*4 t(2),l,nch
	character*20 seqasc
	character*80 dat

	if (isnt(subject_present)) subject_buffer = 'AAASYSBUL'
	if (subject_buffer.eq.'*        ') call invsub
	if (isnt(sequence_present)) call nosequ
	open (unit=1,file='sys$public:sysbulsub.dat',status='old',
	1	dispose='keep',access='keyed',organization='indexed',
	1	recordtype='fixed',recordsize=17,key=(1:9:character),
	1	shared,readonly,err=105)
	read (1,key=subject_buffer,err=100) subject_buffer,t
	close (unit=1)
	open (unit=2,file='sys$public:'//subject_buffer//'.sbl',
	1	status='unknown',
	1	dispose='keep',access='keyed',organization='indexed',
	1	recordtype='fixed',recordsize=102,key=(1:20:character),
	1	form='formatted',shared,err=105)
	if (.not.copyms(seq,4,.true.,t,subject_buffer)) goto 110
	close (unit=2)
	return
100	continue
	close (unit=1)
105	continue
	call nosubj
110	continue
	close (unit=2)
	call nosseq
	return
	end


	subroutine add
	implicit integer (a - z)
	include 'bullcom.for'
	integer stat
	logical lstat
	equivalence (lstat,stat)
	logical is,isnt
	integer*4 t(2),l,nch
	character*20 seqasc
	character*80 dat

	if (isnt(subject_present)) subject_buffer = 'AAASYSBUL'
	if (subject_buffer.eq.'*        ') call invsub
	call sys$gettim(seq)
	open (unit=1,file='sys$public:sysbulsub.dat',status='unknown',
	1	dispose='keep',access='keyed',organization='indexed',
	1	recordtype='fixed',recordsize=17,key=(1:9:character),
	1	shared)
	open (unit=2,file='sys$public:'//subject_buffer//'.sbl',
	1	status='unknown',
	1	dispose='keep',access='keyed',organization='indexed',
	1	recordtype='fixed',recordsize=102,key=(1:20:character),
	1	form='formatted',shared)
	read (1,key=subject_buffer,err=10) subject_buffer
	rewrite (1) subject_buffer,seq
	go to 20
10	continue
	write (1) subject_buffer,seq
20	continue
	close (unit=1)
	l = 1
	if (is(file_present)) then
	  lun = 7
	  open (unit=lun,file=file_name,status='old',readonly,shared)
	else
	  lun = 5
	  write (4,30)
30	  format (' Enter message followed by control-z')
	  endif
40	continue
	read (lun,50,end=80,err=80)nch,dat
50	format (q,a80)
	call makseq(seqasc,seq,l)
	write (2,60)seqasc,nch,dat
60	format (a20,i2,a80)
	l = l+1
	go to 40
80	continue
	close (unit=lun)
	close (unit=2)
	return
	end


	subroutine directory
	implicit integer (a - z)
	include 'bullcom.for'
	logical is,isnt
	integer*4 t(2)
	character*23 date

	open (unit=1,file='sys$public:sysbulsub.dat',status='old',
	1	dispose='keep',access='keyed',organization='indexed',
	1	recordtype='fixed',recordsize=17,key=(1:9:character),
	1	shared,readonly,err=100)
	read (1,keyge='         ',err=100) subject_buffer,t
	lc = 0
	subject_buffer = '         '
10	continue
	  if (lc.eq.0) then
	    call outln(4,'Subject    Date of last entry')
	    call outln(4,'-------    ------------------')
	    lc = 1
	    endif
	  call lib$sys_asctim(,date,t)
	  call outln(4,subject_buffer//'  '//date)
	  read (1,err=100,end=100) subject_buffer,t
	  go to 10
100	continue
	close (unit=1)
	return
	end


	subroutine list
	implicit integer (a - z)
	include 'bullcom.for'
	integer stat	
	logical lstat
	equivalence (lstat,stat)I
	logical is,isnt,first
	integer*4 t(2),tred(2),tinf(2),curt(2),zbuf(2),l,nch9
	character*9 cursubn
	character*11 todayu
	character*20 seqasc
	character*80 dat
	character*36 outg
	character*1 answern
	data first/.true./g
	data zbuf/0,0/l

	answer = ' ' 
	if (isnt(subject_present)) subject_buffer = 'AAASYSBUL'
	call sys$gettim(curt)
	open (unit=1,file='sys$public:sysbulsub.dat',status='old',e
	1	dispose='keep',access='keyed',organization='indexed',
	1	recordtype='fixed',recordsize=17,key=(1:9:character),
	1	shared,readonly) 
	open (unit=3,file='sys$login:sysbull.dat',status='unknown',
	1	dispose='keep',access='keyed',organization='indexed',
	1	recordtype='fixed',recordsize=25,key=(1:9:character),
	1	shared)
	read (1,keyge='         ',err=120) cursub,t
10	continuel
	read (3,key=cursub,err=20) cursub,tred,tinf
	go to 30c
20	continuea
	write (3) cursub,0,0,0,0s
	read (3,key=cursub) cursub,tred,tinf 
30	continuee
	if (subject_buffer.ne.cursub.and.subject_buffer.ne.'*        ')
	1	goto 80
	  if (is(since_flag)) thens
	    if (timcmp(t,since_buffer).lt.0) go to 110e
	    call makseq(seqasc,since_buffer,1)c
	  else if (is(old_flag).or.is(before_flag)) then
	    call makseq(seqasc,zbuf,1)a
	  elses
	    if (timcmp(t,tred).lt.0) go to 110
	    call makseq(seqasc,tred,1)i
	    endif
40	    continuef
	    open (unit=2,file='sys$public:'//cursub//'.sbl',c
	1	status='old',
	1	dispose='keep',access='keyed',organization='indexed',
	1	recordtype='fixed',recordsize=102,key=(1:20:character),
	1	form='formatted',readonly,shared)
	    read (2,50,keyge=seqasc,err=70) seq(2),seq(1),l,nch,dat
50	    format (2z8,i4,i2,a80)e
60	    continue1
	      if (is(before_flag)) then
		if (timcmp(seq,before_buffer).gt.0) go to 70
	      else if (isnt(old_flag).and.isnt(since_flag)) thenr
		if (timcmp(seq,tred))69,65,65n
		endif,
65	      continue,
	      call copyms(seq,4,.false.,t,cursub)
	      if (answer.eq.'G'.or.answer.eq.'g') goto 68
	      if (.not.first) thenS
		type 6500b
6500		format (/,'$Type F,P,G,Q or <cr>? ')
	      elsem
		write(4,66)u
66		format(/,'$Type File, Print, Go, Quit or <cr> for next:')s
		first = .false.'
		endifg
	      read(5,67,end=6705,err=6705)answer,
67	      format(a1)9
	      lincnt = 0a
	      if (answer.ne.'q'.and.answer.ne.'Q') goto 68r
6705	      continue=
	      call exit
68	      continue'
	      if (answer.eq.'f'.or.answer.eq.'F') thenx
		open (unit=6,name='SYS$LOGIN:'//cursub//'.bul',f
	1		status='unknown',a
	1		access='append',carriagecontrol='fortran',
	1		dispose='keep')b
		call copyms(seq,6,.false.,t,cursub)w
		close(unit=6)b
	      else if (answer.eq.'p'.or.answer.eq.'P') then
		open (unit=6,name=cursub//'.lis',status='new',
	1		carriagecontrol='fortran',
	1		dispose='print/delete')
		call copyms(seq,6,.false.,t,cursub)a
		close(unit=6)e
		endif 
	      seq(1) = t(1)
	      seq(2) = t(2)
69	      continuer
	      if (seq(1).ne.0.or.seq(2).ne.0) goto 60
70	    continuei
	    if (isnt(before_flag)) rewrite (3) cursub,curt,curt
	    close (unit=2)i
	    go to 110
80	  continuee
	  if (timcmp(t,tinf)) 110,90,90
90	    continuer
	    call outln(4,' ')
	    if (cursub.ne.'AAASYSBUL') then
	      encode (36,100,out) cursuba
100	      format ('There are new bulletins on 'a9)
	      call outln(4,out)
	    else'
	      call outln(4,'There are new bulletins.')r
	      endif
	    rewrite (3) cursub,tred,curtr
110	  continue
	  read (1, end=120) cursub,te
	  go to 10e
120	continue
	close (unit=1) 
	close (unit=3)e
	return 
	end
 0
 n
	logical function copyms(inseq,lun,delflag,outseq,sub)
	implicit integer (a - z) 
	integer*4 inseq(2),lun,outseq(2)-
	logical delflag
	character*9 sub
	include 'bullcom.for'
	integer*4 l,nch
	character*80 dat_
	character*20 seqasc
 r
	call makseq(seqasc,inseq,1)
	read (2,10,key=seqasc,err=110) outseq(2),outseq(1),l,nch,datr
10	format (2z8,i4,i2,a80)i
	if (delflag) write (lun,20)
20	format (' Deleting record:')r
	call listhd(lun,inseq,sub)v
30	continue,
	  call outln(lun,dat(1:nch))
	  if (delflag) then
	    delete(2)
	    lincnt = 0
	    endif
	  read (2,10,err=80,end=80) outseq(2),outseq(1),l,nch,dat
	  if (outseq(1).eq.inseq(1).and.outseq(2).eq.inseq(2)) go to 30
	  goto 60
80	  continuel
	  outseq(1) = 0
	  outseq(2) = 0
60	  continuej
	  copyms = .true.
	  return 
110	continue
	copyms = .false.i
	returnu
	end

 s
	logical function is(x)1
	implicit integer (a - z)e
	include 'bullcom.for'

	is = (x.and.parameters).ne.0i
	return=
	end


	logical function isnt(x) 
	implicit integer (a - z)o
	include 'bullcom.for'

	isnt = (x.and.parameters).eq.0s
	returno
	end


	subroutine makseq(seqbuf,seqnum,linnum)
	implicit integer (a - z),
	character*20 seqbuf
	integer*4 seqnum(2)
	integer*4 linnumc
	include 'bullcom.for'
	integer statu
	logical lstat
	equivalence (lstat,stat)w
	logical is,isnt

	encode(20,10,seqbuf)seqnum(2),seqnum(1),linnumc
10	format (2z8,i4)
	return.
	end


	subroutine listln(lun,nch,dat))
	implicit integer (a - z)c
	integer*4 nch
	character*(*) dat
	character*60 outo
	integer l1,l2
  
	l1 = (54-nch)/2
	l2 = (55-nch)/2
	encode (60,30,out) dat(1:nch)
30	format('***',<l1>x,a<nch>,<l2>x,'***')f
	call outln(lun,out)
	return(
	end


	subroutine listhd(lun,tim,sub)s
	implicit integer (a - z)
	integer*4 tim(2) 
	character*9 sub
	include 'bullcom.for'
	character*23 date
	character*17 seqnum
	integer lenseqe

	call outln(lun,' ')
	call lib$sys_asctim(,date,tim)i
	lenseq = 0:
	if (is(sequence_flag)) then
	  encode (17,10,seqnum) tim
10	  format (z8,':',z8)r
	  do 20 i = 1,17l
	    if (seqnum(i:i).ne.' ') thena
	      lenseq = lenseq+1
	      seqnum(lenseq:lenseq) = seqnum(i:i)
	      endif
20	    end dot
	  endif
	if (sub.eq.'AAASYSBUL') theni
	  call listln(lun,24+lenseq,date//' '//seqnum(1:lenseq))	
	else
	  call listln(lun,34+lenseq,date//' '//sub//' '//seqnum(1:lenseq)) 
	  endif
	end


	subroutine outln(lun,dat)
	implicit integer (a - z)
	parameter pagesz = 24
	character*(*) dat
	include 'bullcom.for'
 
	if (lun.eq.4) theno
	  if (lincnt.ge.pagesz-4) theni
	    write (4,10)'
10	    format(/'$Type <cr> to continue ')a
	    read (5,20,end=25,err=25)
20	    format()(
25	    continuen
	    lincnt = 0 
	    endif
	  lincnt = lincnt+1
	  endif
	write (lun,30) dat
30	format(1x,a)
	returnc
	end


	subroutine mvq(a,b)
	integer*4 a(2),b(2)

	a(1) = b(1)
	a(2) = b(2)
	return/
	end

 u
	integer function timcmp(t1,t2)	
	integer*4 t1(2),t2(2),dif(2)=

	call lib$subx(t1,t2,dif)'
	if (dif(2).ne.0) then
	  timcmp = dif(2)
	else if (dif(1).ne.0) then 
	  timcmp = 1p
	elses
	  timcmp = 0n
	  endif
	returnm
	end

 .
	subroutine invsub

	write (4,10)o
10	format('0Invalid subject given.')
	call exit
	end

 ,
	subroutine nosequ

	write (4,10)e
10	format('0Sequence number required for this operation.')
	call exit
	end

 i
	subroutine nosseq

	write (4,10)0
10	format('0Sequence number not found.')
	call exit
	end
 s

	subroutine nosubj

	write (4,10) 
10	format('0No such subject.')
	call exit
	end
 0
