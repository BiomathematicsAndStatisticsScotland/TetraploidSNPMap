c     Tetrasimain.for --- A Fortran-77 program to simulate cosegregation
c         between marker loci and QTLs in a tetrasomic outbred population in
c		a repeated simulation study. Modified by CAH 9/2/2000 to allow
c         more general simulation

c     modified 6-7/6/00 to print out offspring genotypes as well as phenotypes,
c     to help checking of reconstruct programme.

c	modified 16/8/00 to simulate ntrt i.i.d traits

c
c	program modified 8/9/2004 to simply generate data and print out in a form 
c	close to that of the .loc file for analysis

c	programme modified 1/2/2011 to simulate SNPs

c	programme modified to read and check segregation of real SNP data

c	9th June 2011 programme modified to look at associations between simplex/duplex etc
c	28th June programme modified to handle coding of simplex + parents correctly.

c     4/6/2012 change to allocatable
c     4/6/2012 fix bug caused by tsimpct not being initialised as 0.
c     14/9/2012 calls to dclink and dtreep changed for latest Fortran

c     19/12/2012 Modified from SNPmatch_with_miss_v2.for to read in data without raw theta scores.
c     id, ngp,thetaP1,thetaP2,bothngp,bothng no longer used
c     bothct changed from real to integer
c     some blank lines added in printing of results to .bpr file
c     printing of higher dosage markers changed so that all are printed to .out file
c     unnecessary printing of lpat from chimatrix routine for .out file suppressed

c     31/1/2013 length of stem and related files increased from 12 to 60 to allow for longer paths
c     in prinout of patterns, expected proportion suppressed and observed proportion included
c     lengths of names changed to 40, and removal of solcap_snp_ at start is omitted
c     try to print out list or original and changed names
c     reorder info for tab for parent 1 and tab for parent 2 so it all occurs together

c     1/2/2013 changed labelling to handle more than 78 groups, use A1, B1...A2 etc and 'select case' rather than 'if'

c     7/5/2013 look at including double reduction

c     28/11/2013 inserted check for duplicate markers
c     28/11/2013 changed labelling to be written as separate name scname

c     20/3/2014 v5 created to write out data in better format for Thanasis

c     Error codes for this program
c     L0001 "More markers than expected"
c     L0002 "Fewer markers than expected"
c     L0003 "More offspring than expected"
c     L0004 "Fewer offspring than expected"
c     L0005 "Unexpected character in data"
c     L0006 "Marker name too long, max 40 characters"
c     L0007 "Marker does not segregate"
c     L0008 "Offspring score not compatible with parental data"
c     L0009 "Both parental have null scores"

c     7/10/2014 modify output to write to .out file

c     v8 add in different cluster routine (non-imsl)
c     24/10/2014 rename as SNPmatch_noimsl and get rid of all imsl calls

c     4/6/2015 change clustering levels for simplex codes from (25,30,35) to (10,20,30)
c     2/11/2015 add in LR test for alpha - scrap this: only fitting DR when no DR is impossible so no baseline for comparison
c     12/11/2015 changed name in output from 3 to 1 markers to double simplex markers 

c     Edited 13/6/2016 to remove exception in CHISQN calc that gave problems in cluster analysis
c     Edited 8/8/16 to trap error if one parent score is missing

c      INCLUDE 'link_fnl_shared.h'
      
c      USE IMSL_LIBRARIES
      implicit double precision (a-h,o-z)
      
	character*40, allocatable, save :: locname(:),fsimpname(:),bname(:)
	character*40, allocatable, save :: msimpname(:),mnsimpname(:)
      character*40, allocatable, save :: bothname(:),fnsimpname(:)
      character*40, allocatable, save :: tsimpname(:),scname(:)
      character*40, allocatable, save :: wkname(:),fcodename(:)
      character*40, allocatable, save :: mcodename(:)
      
           
	
	real(8), allocatable, save :: prec(:),rec(:),clevel(:)
	real(8), allocatable, save :: bfit(:)
	real(8), allocatable, save :: dist(:,:)
      
	integer, allocatable,save :: fnsimp(:,:),mnsimp(:,:),tsimp(:,:)
      integer, allocatable,save :: fsimp(:,:),msimp(:,:),both(:,:)
      integer, allocatable,save :: bothtest(:,:),markSNP(:,:),wkSNP(:,:)
	integer, allocatable, save :: iclson(:),icrson(:),mt(:)
	integer, allocatable,save ::  SNPp1(:),SNPp2(:),wkp1(:),wkp2(:)
      integer, allocatable,save ::  bftype(:),bmtype(:)
      integer, allocatable,save :: bbftype(:),bbmtype(:)
      
      

c	parameter(maxind=200,mxloci=6000)

	
			
	integer gtp(2,10,8),g1(2,4),g2(2,4),gelp(36,36),df, p1,p2
	integer fsimpct,msimpct,fnsimpct,mnsimct,tsimpct,iperm(100),bothct
      integer, allocatable,save :: o(:),omiss(:)
      real(8), allocatable, save :: sigx(:,:),chisq(:,:)
	 
	double precision sigma,chiprob
	dimension y(10,100),yb(10),yv(10)
	dimension chist(100)
     
	character name*40,ctxt*40,txtindi*2
	character stem*60,infile*64,outfile1*64,outfile2*64
	character outfile3*64,outfile4*64,outfile5*67
c	character stem*(*),infile*(*),outfile1*(*),outfile2*(*)
c	character outfile3*(*),outfile4*(*)
     

	common /seed/ idum
c	common /gparameter/ prec
c	common /revent/ rec

	write (*,*) "Input file stem (file.loc)"
	read (*,"(a)") stem

	infile = trim(stem) // ".loc"
	outfile1 = trim(stem) // ".out"
	outfile2 = trim(stem) // ".p1m"
	outfile3 = trim(stem) // ".p2m"
	outfile4 = trim(stem) // ".bpm"
      outfile5 = trim(stem) // ".modloc"

	open(3,file=infile,status='old')
	open(4,file=outfile1,status='unknown')
	open(5,file=outfile2,status='unknown')
	open(7,file=outfile3,status='unknown')
	open(8,file=outfile4,status='unknown')
      open(9,file=outfile5,status='unknown')


c	open(6,file='simann3b.out',status='unknown')

	read(3,*) n,nloci
	write(4,*) n,nloci
 	write(9,*) n,nloci
     
      allocate (locname(nloci),fsimpname(nloci),bname(nloci),
     &  msimpname(nloci),mnsimpname(nloci),bothname(nloci),
     & fnsimpname(nloci), tsimpname(nloci),mt(nloci),scname(nloci),
     & wkname(nloci),fcodename(nloci),mcodename(nloci))
      
      allocate (fnsimp(n,nloci),mnsimp(n,nloci),tsimp(n,nloci),
     & fsimp(n,nloci),msimp(n,nloci),both(n,nloci),bothtest(n,nloci),
     & markSNP(n,nloci),wkSNP(n,nloci))
      
      allocate (iclson(nloci),icrson(nloci),SNPp1(nloci),SNPp2(nloci),
     & bftype(nloci),bmtype(nloci),
     & bbftype(nloci),bbmtype(nloci),wkp1(nloci),wkp2(nloci))
      
      allocate (o(n),omiss(n))
      
      allocate (prec(nloci),rec(nloci),clevel(nloci),bfit(nloci))
      
      allocate (sigx(nloci,nloci),chisq(nloci,nloci),dist(nloci,nloci))
      
	do i=1,n
	  do j=1,nloci
	    markSNP(i,j) = 0
	  end do
	end do

c	for general data
	do j=1,nloci
	  read(3,*) locname(j),SNPp1(j),SNPp2(j),(markSNP(i,j),i=1,n)
c	  write(4,*) locname(j),SNPp1(j),SNPp2(j),(markSNP(i,j),i=1,n)
	end do

c	to read in a file simulated as allele dosages
c	do j=1,nloci
c	  read(3,*) locname(j),
c     &		SNPp1(j),SNPp2(j),(markSNP(i,j),i=1,n)
c	end do

	fsimpct=0
	msimpct=0
	fnsimpct=0
	mnsimpct=0
      tsimpct=0
	bothct=0

c     processing loci
      nwk=0
	loop1: do i= 1,nloci
	  p1=SNPp1(i)
	  p2=SNPp2(i)
	  name=locname(i)
c     check if A and B designation needs reversing        
	  if (p1+p2 .le. 4) then
		do j=1,n
	      o(j)=markSNP(j,i)
	    end do
	  elseif (p1 .ne. 9 .and. p2 .ne. 9) then
	    do j=1,n
	      if (markSNP(j,i) .eq. 9) then
	        o(j)=markSNP(j,i)
	      else
	        o(j)=4-markSNP(j,i)
	      endif
	    end do
	    p1=4-p1
	    p2=4-p2
	  endif
c      check for duplicates
c       if (i .gt. 1) then
c	 loop2:  do k=1,i-1
c	  pdiff = abs(p1-wkp1(k)) + abs(p2-wkp2(k))
c	  if (pdiff .gt. 0) then
c	    cycle loop2
c	  else
c	    ndiff = 0
c	    do l=1,n
c	      ndiff=ndiff+abs(o(l) - wkSNP(l,k))
c	    end do
c	    if (ndiff .eq. 0) then
c	      write(8,'(a6,i5,2x,a40,a22,i5,2x,a40)') 'Locus ',i,locname(i),
c     &		  ' is the same as locus ',k,wkname(k)
c	      cycle loop1  
c          endif
c        endif
c       enddo loop2
c       endif
c     drop out missing values and test segregation ratio
 	  nnl=0
	  do j=1,n
	    if (o(j) .ne. 9) then
	      nnl=nnl+1
	      omiss(nnl)=o(j)
	    endif
        enddo
c     test for and drop markers with parent missing        
       if (p1 .eq. 9 .or. p2 .eq. 9) then
           write(4,*)
        write(4,'(a5,2x,a10,20x,2a10,a6)') 'Count','Name      ',
     &   'P1 dosage','P2 dosage',' nmiss'
	  write(4,'(i5,2x,a40,2i10,i5)') i,name,p1,p2,n-nnl
        write(4,*) 'PM'
          write(4,'(a20,i5,2x,a40,a30)') 'Fault L0010: Locus ',i,
     &     locname(i), ' has missing parental dosage'
           cycle loop1
       endif 
c     test for and drop monomorphic markers        
       irange=maxval(omiss)-minval(omiss)
       if (irange .eq. 0) then
           write(4,*)
        write(4,'(a5,2x,a10,20x,2a10,a6)') 'Count','Name      ',
     &   'P1 dosage','P2 dosage',' nmiss'
	  write(4,'(i5,2x,a40,2i10,i5)') i,name,p1,p2,n-nnl
        write(4,*) 'MO'
          write(4,'(a20,i5,2x,a40,a15)') 'Fault L0007: Locus ',i,
     &     locname(i), ' is monomorphic'
           cycle loop1
       endif 
c     test for and drop markers with both parents = 0       
       if (p1 .eq. 0 .and. p2 .eq. 0) then
            write(4,*)
        write(4,'(a5,2x,a10,20x,2a10,a6)') 'Count','Name      ',
     &   'P1 dosage','P2 dosage',' nmiss'
	  write(4,'(i5,2x,a40,2i10,i5)') i,name,p1,p2,n-nnl
        write(4,*) '00'
          write(4,'(a20,i5,2x,a40,a33)') 'Fault L0009: Locus ',i,
     &     locname(i), ' has zero dosage for both parents'
           cycle loop1
       endif 
c     save remaining markers in wk and move to test segegation ratios       
       nwk=nwk+1
	  wkp1(nwk)=p1
	  wkp2(nwk)=p2
	  wkname(nwk)=locname(i)
  	  do j=1,n
	      wkSNP(j,nwk)=o(j)
        end do
c	  write(4,'(i4)') nnl
c	  write(4,'(<nnl>(i2))') (omiss(k),k=1,nnl)
          write(4,*)
        write(4,'(a5,2x,a10,20x,2a10,a6)') 'Count','Name      ',
     &   'P1 dosage','P2 dosage',' nmiss'
	  write(4,'(i5,2x,a40,2i10,i5)') i,name,p1,p2,n-nnl
	  call gtypepred(nnl,name,p1,p2,omiss,alpha,odds,nt,chiprob,txtindi)
c         write(4,'(a,f6.4,a,f6.4)') 'Chi sig = ',chiprob,'Alpha = ',
c     &     alpha


c	sort markers into types
	  if (p1 .eq. 1 .and. p2 .eq. 0 .and. chiprob .gt. 0.001 .and. 
     &   txtindi .eq. 'OK') then   !simplex  
	    fsimpct=fsimpct+1
	    fsimp(1:n,fsimpct)=o(1:n)
	    fsimpname(fsimpct)=locname(i)
          write(9,'(a40,2x,a4,<n>i2,2x,a2,2x,f7.4,2x,f5.2)') 
     &	  fsimpname(fsimpct),' 1 0',(fsimp(j,fsimpct),j=1,n),txtindi,
     &     chiprob,alpha
	  elseif (p1 .eq. 1 .and. p2 .eq. 0 .and. chiprob .le. 0.001 .and. 
     &   txtindi .eq. 'OK') then   !simplex  
          write(9,'(a40,2x,a4,<n>i2,2x,a2,2x,f7.4,2x,f5.2)') locname(i),
     &	  ' 1 0',(o(j),j=1,n),txtindi,chiprob,alpha
	  elseif (p1 .eq. 3 .and. p2 .eq. 0 .and. chiprob .gt. 0.001 .and. 
     &   txtindi .eq. 'OK') then   !simplex + par 
	    fsimpct=fsimpct+1
	    do j=1,n
	      if (o(j) .eq. 9) then
	        fsimp(j,fsimpct) = 9
	      else
	        fsimp(j,fsimpct) = 2-o(j)
	      endif
	    enddo
	    fsimpname(fsimpct)=locname(i)
          write(9,'(a40,2x,a4,<n>i2,2x,a2,2x,f7.4,2x,f5.2)') 
     &	  fsimpname(fsimpct),' 1 0',(fsimp(j,fsimpct),j=1,n),txtindi,
     &     chiprob,alpha
        elseif (p1 .eq. 3 .and. p2 .eq. 0 .and. chiprob .le. 0.001 .and. 
     &   txtindi .eq. 'OK') then   !simplex  
  	    do j=1,n
	      if (o(j) .ne. 9) then
		        o(j) = 2-o(j)
	      endif
	    enddo
          write(9,'(a40,2x,a4,<n>i2,2x,a2,2x,f7.4,2x,f5.2)') locname(i),
     &	  ' 1 0',(o(j),j=1,n),txtindi,chiprob,alpha
	  elseif (p1 .eq. 0 .and. p2 .eq. 1 .and. chiprob .gt. 0.001 .and. 
     &   txtindi .eq. 'OK') then !simplex
	    msimpct=msimpct+1
	    msimp(1:n,msimpct)=o(1:n)
	    msimpname(msimpct)=locname(i)
	   write(9,'(a40,2x,a4,<n>i2,2x,a2,2x,f7.4,2x,f5.2)') 
     &	  msimpname(msimpct),' 0 1',(msimp(j,msimpct),j=1,n),txtindi,
     &    chiprob,alpha
 	  elseif (p1 .eq. 0 .and. p2 .eq. 1 .and. chiprob .le. 0.001 .and. 
     &   txtindi .eq. 'OK') then   !simplex  
          write(9,'(a40,2x,a4,<n>i2,2x,a2,2x,f7.4,2x,f5.2)') locname(i),
     &	  ' 0 1',(o(j),j=1,n),txtindi,chiprob,alpha
       elseif (p1 .eq. 0 .and. p2 .eq. 3 .and. chiprob .gt. 0.001 .and. 
     &   txtindi .eq. 'OK') then !simplex + par
	    msimpct=msimpct+1
	    do j=1,n
	      if (o(j) .eq. 9) then
	        msimp(j,msimpct) = 9
	      else
	        msimp(j,msimpct) = 2-o(j)
	      endif
	    enddo
	    msimpname(msimpct)=locname(i)
	    write(9,'(a40,2x,a4,<n>i2,2x,a2,2x,f7.4,2x,f5.2)') 
     &	  msimpname(msimpct),' 0 1',(msimp(j,msimpct),j=1,n),txtindi,
     &     chiprob,alpha
	  elseif (p1 .eq. 0 .and. p2 .eq. 3 .and. chiprob .le. 0.001 .and. 
     &   txtindi .eq. 'OK') then   !simplex  
   	    do j=1,n
	      if (o(j) .ne. 9) then
		        o(j) = 2-o(j)
	      endif
	    enddo
         write(9,'(a40,2x,a4,<n>i2,2x,a2,2x,f7.4,2x,f5.2)') locname(i),
     &	  ' 0 1',(o(j),j=1,n),txtindi,chiprob,alpha
	  elseif (p1 .eq. 2 .and. p2 .eq. 0 .and. chiprob .gt. 0.01 .and. 
     &   txtindi .eq. 'OK') then !duplex
	    fnsimpct=fnsimpct+1
	    fnsimp(1:n,fnsimpct)=o(1:n)
	    fnsimpname(fnsimpct)=locname(i)
	   write(9,'(a40,2x,a4,<n>i2,2x,a2,2x,f7.4,2x,f5.2)') 
     &	  fnsimpname(fnsimpct),' 2 0',(fnsimp(j,fnsimpct),j=1,n),txtindi,
     &    chiprob,alpha
	  elseif (p1 .eq. 2 .and. p2 .eq. 0 .and. chiprob .le. 0.01 .and. 
     &   txtindi .eq. 'OK') then !duplex
	   write(9,'(a40,2x,a4,<n>i2,2x,a2,2x,f7.4,2x,f5.2)') locname(i),
     &	  ' 2 0',(o(j),j=1,n),txtindi,chiprob,alpha 
	  elseif (p1 .eq. 0 .and. p2 .eq. 2 .and. chiprob .gt. 0.01 .and. 
     &   txtindi .eq. 'OK') then !duplex
	    mnsimpct=mnsimpct+1
	    mnsimp(1:n,mnsimpct)=o(1:n)
	    mnsimpname(mnsimpct)=locname(i)
	   write(9,'(a40,2x,a4,<n>i2,2x,a2,2x,f7.4,2x,f5.2)') 
     &    mnsimpname(mnsimpct),' 0 2',
     &	  (mnsimp(j,mnsimpct),j=1,n),txtindi,chiprob,alpha 
	  elseif (p1 .eq. 0 .and. p2 .eq. 2 .and. chiprob .le. 0.01 .and. 
     &   txtindi .eq. 'OK') then !duplex
	   write(9,'(a40,2x,a4,<n>i2,2x,a2,2x,f7.4,2x,f5.2)') locname(i),
     &	  ' 0 2',(o(j),j=1,n),txtindi,chiprob,alpha 
	  elseif (p1 .eq. 1 .and. p2 .eq. 1 .and. chiprob .gt. 0.01 .and. 
     &   txtindi .eq. 'OK') then !DS
	    tsimpct=tsimpct+1
	    tsimp(1:n,tsimpct)=o(1:n)
	    tsimpname(tsimpct)=locname(i)
	   write(9,'(a40,2x,a4,<n>i2,2x,a2,2x,f7.4,2x,f5.2)') 
     &    tsimpname(tsimpct),' 1 1',
     &	  (tsimp(j,tsimpct),j=1,n),txtindi,chiprob,alpha
	  elseif (p1 .eq. 1 .and. p2 .eq. 1 .and. chiprob .le. 0.01 .and. 
     &   txtindi .eq. 'OK') then !DS
	   write(9,'(a40,2x,a4,<n>i2,2x,a2,2x,f7.4,2x,f5.2)') locname(i),
     &	  ' 1 1',(o(j),j=1,n),txtindi,chiprob,alpha
	  elseif (txtindi .eq. 'OK') then
	    bothct=bothct+1
	    bfit(bothct)=chiprob
	    both(1:n,bothct)=o(1:n)
	    bothname(bothct)=locname(i)
	    bftype(bothct)=p1
	    bmtype(bothct)=p2
	    write(9,'(a40,2x,2i2,<n>i2,2x,a2,2x,f7.4,2x,f5.2)') locname(i),p1,
     &	  p2,(o(j),j=1,n),txtindi,chiprob,alpha
	  endif
      end do loop1
      
      write(*, '(a, i)') 'total: ', nwk+700
      write(*, '(a, i)') 'position ', 50
      
      write (5,*) 'Nwork = ',nwk

	write(5,*)
	write(5,*) 'Simplex markers from parent 1'
	write(5,*) n,fsimpct
	do i=1,fsimpct
	  write(5,'(a40,2x,a4,2x,<n>i2)') fsimpname(i),' 1 0',
     &	  (fsimp(j,i),j=1,n)
	end do
	write(7,*)
	write(7,*) 'Simplex markers from parent 2'
	write(7,*) n,msimpct
	do i=1,msimpct
	  write(7,'(a40,2x,a4,2x,<n>i2)') msimpname(i),' 0 1',
     &	  (msimp(j,i),j=1,n)
	end do
	write(5,*)
	write(5,*) 'Duplex markers from parent 1'
	write(5,*) n,fnsimpct
	do i=1,fnsimpct
	  write(5,'(a40,2x,a4,2x,<n>i2)') fnsimpname(i),' 2 0',
     &	  (fnsimp(j,i),j=1,n) 
	end do
	write(7,*)
	write(7,*) 'Duplex markers from parent 2'
	write(7,*) n,mnsimpct
	do i=1,mnsimpct
	  write(7,'(a40,2x,a4,2x,<n>i2)') mnsimpname(i),' 0 2',
     &	  (mnsimp(j,i),j=1,n) 
	end do
	write(8,*)
	write(8,*) 'Double simplex markers '
	write(8,*) n,tsimpct
	do i=1,tsimpct
	  write(8,'(a40,2x,a4,2x,<n>i2)') tsimpname(i),' 1 1',
     &	  (tsimp(j,i),j=1,n)
	end do
c	write(4,*)
c	write(4,*) 'Higher dosage markers'
c	write(4,*) n,bothct   
	write(8,*)
	write(8,*) 'Other markers'
	write(8,*) n,bothct    ! This prints total count of other markers, but only data on well-fitting ones
c	ibfit=0
	i3to1=0
	do i=1,bothct
	  if (bfit(i) .lt. 0.0) then
	  write(8,*) bothname(i),' No configuration found '
	  write(8,'(2i2,<n>i2)') bftype(i),bmtype(i),
     &		(both(j,i),j=1,n)
        elseif (bfit(i) .ge. 0.0 .and. bfit(i) .le. 0.01) then
	    write(8,*) bothname(i),bfit(i)
	    write(8,'(2i2,<n>i2)') bftype(i),bmtype(i),
     &		(both(j,i),j=1,n)
	  else
	    if (bfit(i) .gt. 0.01) then   !11/7/2011, changed from 0.001 to 0.01 to ensure good fits
c	  ibfit=ibfit+1
	    i3to1=i3to1+1
	    bothtest(1:n,i3to1) = both(1:n,i)
	    bbftype(i3to1)=bftype(i)
	    bbmtype(i3to1)=bmtype(i)
	    bname(i3to1)=bothname(i)
 	    write(8,*) bothname(i),bfit(i)
c	    write(4,'(2i2,<n>i2)') bftype(i),bmtype(i),
c     &		(both(j,i),j=1,n)
	    write(8,'(a40,2x,2i2,2x,<n>i2)') bothname(i),bftype(i),
     &		bmtype(i),(both(j,i),j=1,n)
	    endif
	  end if
	end do

c	do single linkage cluster analysis of simplex markers from first parent
	
	if (fsimpct .gt. 2) then
	write(8,*)
	write(8,*) 'Single linkage clustering for parent 1'
	write(8,*)
	  ipar = 1
	  call simmatch(n,fsimpct,fsimp,fsimpname,fcodename,ipar)
	endif

c	do single linkage cluster analysis of simplex markers from second parent
	
	if (msimpct .gt. 2) then
	write(8,*)
	write(8,*) 'Single linkage clustering for parent 2'
	write(8,*)
	  ipar = 2
	  call simmatch(n,msimpct,msimp,msimpname,mcodename,ipar)
      endif

      scname='Blank'
      long_loop: do i=1,nwk
          write(*, '(a, i)') 'position ',  i+100
          do j=1,fsimpct
c           write(4,'(2i4,a40,i3,2x,a40,2i3)') i,j,fsimpname(j),
c     &      len_trim(fsimpname(j)),locname(i),len_trim(locname(i)),
c     &      index(fsimpname(j),trim(locname(i)))
             if (trim(fsimpname(j)) .eq. trim(wkname(i))) then 
                  scname(i) = fcodename(j)
                  write(4,'(i4,2x,3a40)') i,fsimpname(j),wkname(i),
     &            scname(i)
                  cycle long_loop
               endif
          enddo
           do j=1,msimpct
             if (trim(msimpname(j)) .eq. trim(wkname(i))) then 
                  scname(i) = mcodename(j)
                  write(4,'(i4,2x,3a40)') i,msimpname(j),wkname(i),
     &            scname(i)
                  cycle long_loop
               endif
          enddo
       enddo long_loop
      
      write(4,*)
      write(4,*) 'Renamed loci'
      do i=1,nwk
          write (4,'(a40,4x,a40)') wkname(i),scname(i)
      enddo
 
      
	write(4,*)
	write(4,*) 'Info for parent 1 tab'
      write(4,*) 'Simplex-duplex linkages for parent 1'
	itype=5 ! indicator of duplex 
	ifile=4
	call chicalc(n,ifile,itype,fsimpct,fnsimpct,3,fsimpname,
     &	fnsimpname,fsimp,fnsimp,chisq,sigx)
      write(*, '(a, i)') 'position ',  nwk + 200  
	write(4,*)
	write(4,*) 'Simplex-double simplex linkages for parent 1'
	itype=3
	ifile=4
	call chicalc(n,ifile,itype,fsimpct,tsimpct,3,fsimpname,tsimpname,
     &	fsimp,tsimp,chisq,sigx)
      write(*, '(a, i)') 'position ',  nwk + 300
	write(4,*)
	write(4,*) 'Other linkages to simplex markers for parent 1'
c	write(8,*)
c     write(8,*) 'Other linkages to simplex markers for parent 1'
	ifile=4
	itype=5
	call chimatrix(n,ifile,fsimpct,i3to1,fsimpname,bname,fsimp,
     &	bothtest,chisq,sigx)
      write(*,'(a, i)') 'position ',  nwk + 400
	write(4,*)
	write(4,*) 'Info for parent 2 tab'
	write(4,*) 'Simplex-duplex linkages for parent 2'
	itype=5 ! indicator of duplex     
	ifile=4
	call chicalc(n,ifile,itype,msimpct,mnsimpct,3,msimpname,
     &	mnsimpname,msimp,mnsimp,chisq,sigx)
      write(*, '(a, i)') 'position ',  nwk + 500
	write(4,*)
	write(4,*) 'Simplex-double simplex linkages for parent 2'
      itype=3
      ifile=4
	call chicalc(n,ifile,itype,msimpct,tsimpct,3,msimpname,tsimpname,
     &	msimp,tsimp,chisq,sigx)
      write(*, '(a, i)') 'position ',  nwk + 600
c     new lines
c	write(4,*)
c	write(4,*) 'Duplex-3 to 1 linkages for parent 1'
c	itype=5
c	ifile=4
c	call chicalc(n,ifile,itype,fnsimpct,tsimpct,fnsimpname,tsimpname,
c    &	fnsimp,tsimp,chisq,sigx)

c	write(4,*)
c	write(4,*) 'Duplex-3 to 1 linkages for parent 2'
c	ifile=4
c	call chicalc(n,ifile,itype,mnsimpct,tsimpct,mnsimpname,tsimpname,
c     &	mnsimp,tsimp,chisq,sigx)

c     added 23/1/2007
c	write(4,*)
c	write(4,*) 'Other linkages to 3 to 1 markers'
c	ifile=4
c	call chimatrix(n,ifile,tsimpct,i3to1,tsimpname,bname,tsimp,
c     &	bothtest,chisq,sigx)
	
c     end of new lines

	write(4,*)
	write(4,*) 'Other linkages to simplex markers for parent 2'
	write(8,*)
      write(8,*) 'Other linkages to simplex markers for parent 2'
	ifile=4
	itype=5
	call chimatrix(n,ifile,msimpct,i3to1,msimpname,bname,msimp,
     &	bothtest,chisq,sigx)
      write(*, '(a, i)') 'position ',  nwk + 700
	end


c	*******************************************************
	subroutine gtypepred(n,name,p1,p2,o1,alpha,odds,nt,chiprob,txtindi)

c	to predict parental genotypes from their phenotypes and phenotypes
c	of their offspring in a tetrasomic inheritance model. 
c	This program takes double reduction into account.

         
	implicit double precision (a-h,o-z)

c	parameter (maxind=200)

      real(8) work(n)
 

	integer p1,p2,g1(4),g2(4),o1(n)
	integer wkb(100),t1(4),t2(4)
	character name*40,txtindi*2

	dimension c(100),xlkhd(17),ipem(1)
c      dimension slkhd(17),ipem(17)
      
c	commented out 1/2/2011 as unsure what is necessary for SNPs
c	n1=0
c	n2=0
c	n12=0

c	do i=1,8
c	  if (p1(i).eq.1) then
c	    n1=n1+1
c	    g1(n1)=i
c	  end if
c	  if (p2(i).eq.1) then
c	    n2=n2+1
c		g2(n2)=i
c	  end if
c	  if (p1(i).eq.1.and.p2(i).eq.1) n12=n12+1
c	end do
c	do i=1,n1
c	  t1(i)=g1(i)
c	  g1(i)=0
c	end do
c	do i=1,n2
c	  t2(i)=g2(i)
c	  g2(i)=0
c	end do

	do i=1,n
	  work(i)=o1(i)
	end do

	nob=0
	do i=1,n
	  do while (work(i).ne.99)
	    nob=nob+1
	    wkb(nob)=work(i)
		c(nob)=1
	    do j=i+1,n
	      if (wkb(nob) .eq. work(j)) then
	        work(j)=99
			c(nob)=c(nob)+1
		  end if 
	    end do
	    work(i)=99
	  end do !while
	end do

c	write(4,'(1x,a)') 'The observed offspring band pattern: '
c	do i=1,nob
c	  write(4,'(1x,i2,1x,f8.4)') wkb(i),c(i)/n
c	end do

c	sortobserved band pattern
      prob=0.0d0
      odds=0.0d0
      alpha=0.0d0
      iprint = 0
	call SNPbandist(n,name,alpha,iprint,p1,p2,nob,wkb,c,nt,chiprob,prob)
      if (chiprob .ge. 0.0d0) then
          txtindi='OK'
          write(4,*) txtindi
          iprint = 1
      call SNPbandista(n,name,alpha,iprint,p1,p2,nob,wkb,c,nt,chiprob,
     & prob)
           write(4,'(a,f6.4,a,f6.4)') 'Chi sig = ',chiprob,' Alpha = ',
     &     alpha
      else
      
	do i=1,16
	  xlkhd(i)=0.0d0
c	  slkhd(i)=0.0d0
c	  ipem(i)=i
	end do
      chiprob0=chiprob
      
	do i=1,16
	  alpha=0.01d0*i
	  call SNPbandista(n,name,alpha,iprint,p1,p2,nob,wkb,c,nt,chiprob,prob)
	  xlkhd(i)=prob
        if (chiprob .gt. chiprob0) chiprob0=chiprob
      end do
      do i=1,16
          write(8,'(i3,f10.6)') i,xlkhd(i)
      enddo
      
      if (chiprob0 .ge. 0.0d0) then
          txtindi='DR'
          write(4,*) txtindi
c     change code just below to get rid of IMSL sort, ipem and slkhd become scalars          
c     also see no need for odds calc anymore
c	call dsvrgp(17,xlkhd,slkhd,ipem) !do sorting
c	alpha=0.01d0*(ipem(17)-1)
      ipem=maxloc(xlkhd)
      alpha=0.01d0*ipem(1)
      iprint = 1
      write(8,*) ipem(1),alpha
c	if (xlkhd(1).gt.0.0d0) then
c	  odds=2*(dlog(slkhd(17))-dlog(xlkhd(1)))
c	else
c	  odds=3.85d0
c	end if
c	write(4,'(1x,a,2f8.4)') 'MLE and LODs: ',alpha,odds
c	write(6,'(1x,a,2f8.4)') 'MLE and LODs: ',alpha,odds

c      if (odds .gt. 2.71) then   !10% point
  	  call SNPbandista(n,name,alpha,iprint,p1,p2,nob,wkb,c,nt,chiprob,
     &   prob)
        write (8,'(a6,a40,a30)') 'Locus ',name,
     &   ' may show double reduction'
        write(8,'(1x,a,3f8.4)') 'MLE and chi-prob: ',alpha,prob,
     &   chiprob
        write(4,'(a,f6.4,a,f6.4)') 'Chi sig = ',chiprob,' Alpha = ',
     &     alpha
	
c      endif
      
      else
          txtindi='NP'
           write(4,*) txtindi
           write (4,*) 'Observed frequencies'
           do j=0,4
           do i=1,nob
          if (wkb(i) .eq. j) then
	  write(4,'(1x,i1,1x,i5,1x,f9.5)') j,nint(c(i)),c(i)/(1.0d0*n)
          endif
      enddo
      end do

          alpha=-1.0d0
           write(4,'(a,f9.4,a,f9.4)') 'Chi sig = ',chiprob,' Alpha = ',
     &     alpha
	endif
      endif
      
	return
	end
c	***************************************************************
	subroutine SNPbandist(n,name,alpha,iprint,p1,p2,nob,wkb,c,nt,chiprob,
     & prob)

          
	implicit double precision (a-h,o-z)

c	parameter (maxind=200)

	integer g1(4),g2(4),p1gt(10,2),p2gt(10,2),zygote(100,4)
	integer p1,p2,bptn(100),genotype(8),phenotype(8)
	integer wkb(100)
	character name*40
	dimension mtx1(6,6),mtx2(6,6),p1gtfq(10),p2gtfq(10),freq(100)
	dimension prior(100),xlkhd(100),c(100)


c	write(6,10) 'Parental genotypes','Probability    # of bds  chi-2'
c	write(4,10) 'Parental genotypes','Probability    # of bds  chi-2'


c	convert SNP number to 4 digit code
	do k=1,4
	  g1(k)=1
	  g2(k)=1
	end do
	SNPpos1=5-p1
	SNPpos2=5-p2
	do k=SNPpos1,4
	  g1(k)=2
	enddo
	do k=SNPpos2,4
	  g2(k)=2
	enddo
c	write(4,'(a,2x,4i1,2x,4i1)') 'SNPpat',(g1(k),k=1,4),(g2(k),k=1,4)

c	do i=1,100
c	  do j=1,8
c	    cgtp(i,j)=0
c	  end do
c	  prior(i)=0.0d0
c	  xlkhd(i)=0.0d0
c	  nptn(i)=0
c	  chisqr(i)=0.0d0
c	end do

c	call btog(n1,ia1,ia2,mtx1)
c	call btog(n2,ib1,ib2,mtx2)

c	do i=1,ia1*ib1
c	  prior(i)=1.0d0/(ia1*ib1)
c	end do
	
c	ic=1
c	sum=0.0d0
c	write(7,*)
c	do i=1,ia1
c	  it=0
c	  do k=1,4
c	    g1(k)=0
c	  end do
c	  do k=1,ia2
c	    do l=1,mtx1(i,k)
c		  it=it+1
c	      g1(it)=p1(k)
c	    end do
c	  end do
c	  do j=1,ib1
c		do k=1,4
c		  g2(k)=0
c	    end do
c		it=0
c	    do k=1,ib2
c		  do l=1,mtx2(j,k)
c			it=it+1
c	        g2(it)=p2(k)
c		  end do
c	    end do
c	    ic=ic+1
c	    do k=1,4
c	      cgtp(ic,k)=g1(k)
c	      cgtp(ic,4+k)=g2(k)
c	    end do
c		write(4,'(1x,2i4)') i,j
c	    write(7,'(1x,2i4)') i,j
	   call gametogensis(alpha,g1,ngt1,p1gt,p1gtfq)
	   call gametogensis(alpha,g2,ngt2,p2gt,p2gtfq)
	   call zygt(p1gt,p2gt,p1gtfq,p2gtfq,ngt1,ngt2,ntype,zygote,freq)
	   call SNPgtob(ntype,zygote,freq,np,bptn)
c	   nptn(ic)=np
	   call SNPptsort(n,np,iprint,bptn,freq,nob,wkb,c,x2,chiprob,prob)
         
c	   chisqr(ic)=x2
c	   xlkhd(ic)=prob
c	   sum=sum+prior(ic)*xlkhd(ic)
c	  end do !j
c	end do !i


c	write(4,'(2i5)') p1,p2
c	write(4,'(1a)') 'Expected'
c	do i=1,np
c	  write(4,'(i5,2x,f6.4)') bptn(i),freq(i)
c	enddo
c	write(4,'(1a)') 'Observed'
c	do i=1,nob
c	  write(4,*) wkb(i),c(i)
c	enddo
c	write(4,'(a20,2x,a,2x,f10.2,2x,f10.4)') name,'Chi-squared', x2,
c     &	chiprob	
c	write(6,'(a20,2x,a,2x,f10.2,2x,f10.4)') name,'Chi-squared', x2,
c     &	chiprob	
c	nt=0
c	do i=1,ic
c	  pstr=prior(i)*xlkhd(i)/sum
c	  if (pstr.gt.0.0d0) then
c	    nt=nt+1
c		probt(nt)=pstr
c	    do k=1,8
c	      cgtp(nt,k)=cgtp(i,k)
c		end do
c	    write(4,20) (cgtp(nt,k),k=1,8),pstr,nptn(i),chisqr(i)
c	    write(6,20) (cgtp(nt,k),k=1,8),pstr,nptn(i),chisqr(i)
c	  end if
c	end do

10	format(1x,a,2x,a)
20	format(5x,4(i1),2x,4(i1),4x,f15.12,4x,i2,5x,f8.2)

	return
	end
c	***************************************************
	subroutine SNPptsort(n,np,iprint,bptn,freq,nob,wkb,c,x2,chiprob,prob)
      
c      USE svigp_int
c      USE permu_int

	implicit double precision (a-h,o-z)

	integer bptn(100),wkb(100),bbptn(np),b2ptn(100)
      integer iperm(np),ct(np),oddct(5)
	real(8) freq(100),c(100),ffreq(np),prop(np),f2freq(100),oddprop(5)
	
      do i=1,np
c          iperm(i) = i
c          ffreq(i) = freq(i)
          bbptn(i) = bptn(i)
      enddo
      
c      call svigp(bbptn,bbptn,iperm,n=np)
c      call d_permu(ffreq,iperm,ffreq)
      
      maxct=maxval(bbptn)
      minct=minval(bbptn)
      np1=maxct+1-minct
c      write(4,*) 'Pattern problem',maxct,minct,np,np1
      if (minct .eq. 0) then 
      outerloop1: do i=minct,maxct
          do j=1,np
              if (bptn(j) .eq. i) then
                  b2ptn(i+1) = bptn(j)
                  f2freq(i+1) = freq(j)
                  cycle outerloop1
              endif
          enddo
      enddo outerloop1
      
      else
      outerloop2: do i=minct,maxct
          do j=1,np
              if (bptn(j) .eq. i) then
                  b2ptn(i) = bptn(j)
                  f2freq(i) = freq(j)
                  cycle outerloop2
              endif
          enddo
      enddo outerloop2
      endif
          
                      
       
c	do i=1,np
c	  write(4,'(1x,i1,1x,f8.4)') b2ptn(i),f2freq(i)
c	end do
c	write(4,*)
c	do i=1,np
c	  write(4,'(1x,i1)') wkb(i)
c	end do
c	write(4,*)

	ndf=0
	do i=1,nob !I replaced np by nob on the 1st of Feb
	  idf=1
	  do j=1,np
		dif=1.0d0*abs(wkb(i)-b2ptn(j))
		if (dif.eq.0.0d0) idf=0
	  end do
	  ndf=ndf+idf
	end do
      
c	write(4,'(2x,a,i2)') 'ndf= ',ndf,'nob= ',nob,'np= ',np

       
      if (iprint .eq. 1) write (4,*) 'Observed frequencies' 
	if (ndf.gt.0) then
	  x2=-99.0d0
	  chiprob=-99.0d0
        if (iprint .eq. 1) then
	write(4,*) 
      do j=0,4
      do i=1,nob
          if (wkb(i) .eq. j) then
	  write(4,'(1x,i1,1x,f5.0)') j,c(i)
          endif
      enddo
      end do
        endif 
	else
	  x2=0.0d0
	  do i=1,np
	    ct(i)=0
	    do j=1,nob
		  dif=dabs(1.0d0*(b2ptn(i)-wkb(j)))
		  if (dif.eq.0.0d0) ct(i)=c(j)
	    end do
	    x2=x2+(n*f2freq(i)-ct(i))**2/(n*f2freq(i))
          prop(i)=ct(i)/(1.0d0*n)
c     line below changed 31/1/2013 to write out observed proportion rather than expected frequency          
          if (iprint .eq. 1) write(4,'(1x,i1,1x,i5,1x,2f9.5)') b2ptn(i),
     &     ct(i),prop(i),f2freq(i)
	  end do
	df=1.0d0*np-1.0d0
      ifault=0
	chiprob=1.0d0-CHISQN(x2, df,ifault)
      np1=np
	prob=pmltnr(n,np1,ct,f2freq)
	end if
c	moved line below out of if loop for testing on 9/1/2012
c	write(4,'(a,f5.2,2x,a,i4,2x,a,f5.2,2x,a,f6.4)') 'Chi = ',x2,'np = ',np,
c     & 'df = ',df,'Chi sig = ',chiprob

	return
	end	  
c	*****************************************
	subroutine SNPgtob(n,gtype,freq,ntype,ptype) 

c	to turn the genotypes into bands

	implicit double precision (a-h,o-z)

	integer gtype(100,4),ptype(100),tempg(4),tempp(8),work(100)

	real(8) freq(100),fwk(100)

	do i=1,100
	  ptype(i)=0
	  work(i)=0
	  fwk(i)=0.0d0
	end do

	sum=0.0d0
	do i=1,n
	  do k=1,4
	    if (gtype(i,k) .eq. 2) then
	      work(i) = work(i)+1
	    endif
	  end do
	  fwk(i)=freq(i)
	  sum=sum+freq(i)
	  freq(i)=0.0d0
	end do

	ntype=0
	do i=1,n
	  do while (work(i).ne.99)
	    ntype=ntype+1
	    freq(ntype)=fwk(i)
	    ptype(ntype)=work(i)
	    do j=i+1,n
	      if (ptype(ntype) .eq. work(j)) then
	        work(j)=99
	  	    freq(ntype)=freq(ntype)+fwk(j)
		  end if 
	    end do
	    work(i)=99
	  end do !while
      end do
      
      

c	do i=1,ntype
c	  write(4,'(a,2x,4i1,2x,i2,2x,f5.1)') 'SNPgtob',(gtype(i,j),j=1,4),
c     &   ptype(i),freq(i)
c	enddo

	return
	end
c     *********************************************************
	subroutine gametogensis(alpha,genotype,ntype,gamete,freq)

c	to turn out gametes and their frequencies. The program 
c	can take double reduction into account. The coefficient
c	of double reduction was represented by alpha, that is
c	the probability of two sister chromatids going to the
c	same gamete (Bradshaw, 1994 ,pp77).

	implicit double precision (a-h,o-z)

      integer genotype(4),gamete(10,2),work(10,2)

	dimension freq(10),dist(10)

	ntype=0
	do i=1,10
	  dist(i)=0.0d0
	  do j=1,2
	    gamete(i,j)=0
	  end do
	end do
	do i=1,10
	  if (i.le.6) then
	    dist(i)=(1.0d0-alpha)/6
	  else
	    dist(i)=alpha/4
	  end if
	  freq(i)=0.0d0
	end do
	
	if (alpha.gt.0.0d0) then
	  ng=10
	  work(1,1)=genotype(1)
	  work(1,2)=genotype(2)
	  work(2,1)=genotype(3)
	  work(2,2)=genotype(4)
	  work(3,1)=genotype(1)
	  work(3,2)=genotype(3)
	  work(4,1)=genotype(2)
	  work(4,2)=genotype(4)
	  work(5,1)=genotype(2)
	  work(5,2)=genotype(3)
	  work(6,1)=genotype(1)
	  work(6,2)=genotype(4)
	  work(7,1)=genotype(1)
	  work(7,2)=genotype(1)
	  work(8,1)=genotype(2)
	  work(8,2)=genotype(2)
	  work(9,1)=genotype(3)
	  work(9,2)=genotype(3)
	  work(10,1)=genotype(4)
	  work(10,2)=genotype(4)
	else
	  ng=6
	  work(1,1)=genotype(1)
	  work(1,2)=genotype(2)
	  work(2,1)=genotype(3)
	  work(2,2)=genotype(4)
	  work(3,1)=genotype(1)
	  work(3,2)=genotype(3)
	  work(4,1)=genotype(2)
	  work(4,2)=genotype(4)
	  work(5,1)=genotype(2)
	  work(5,2)=genotype(3)
	  work(6,1)=genotype(1)
	  work(6,2)=genotype(4)
	end if	
	 
	ntype=0
	do i=1,ng
	  do while (work(i,1).ne.99)
	    ntype=ntype+1
	    gamete(ntype,1)=work(i,1)
	    gamete(ntype,2)=work(i,2)
		freq(ntype)=dist(i)
	    do j=i+1,ng
	      if (work(i,1).eq.work(j,1).and.work(i,2).eq.work(j,2)) then
	        work(j,1)=99
		    freq(ntype)=freq(ntype)+dist(j)
		  end if
	    end do
	    work(i,1)=99
	  end do !while
	end do

	return
	end
c	**********************************************************
	subroutine zygt(fgt,mgt,ffg,fmg,nf,nm,ntype,genotype,freq)

	implicit double precision (a-h,o-z)

	integer fgt(10,2),mgt(10,2),genotype(100,4),work(100,4)

	logical event1,event2

	real(8) ffg(10),fmg(10),fwk(100),freq(100)

	do i=1,100
	  do j=1,4
	    genotype(i,j)=0
	    work(i,j)=0
	  end do
	  freq(i)=0.0d0
	end do

	nd=0
	do i=1,nf
	  do j=1,nm
	    nd=nd+1
	    do k=1,2
	      work(nd,k)=fgt(i,k)
		  work(nd,2+k)=mgt(j,k)
		end do
		fwk(nd)=ffg(i)*fmg(j)
	  end do
	end do

	ntype=0
	do i=1,nd
	  do while (work(i,1).ne.99) 
	    ntype=ntype+1
		do k=1,4
	      genotype(ntype,k)=work(i,k)
	    end do
	    freq(ntype)=fwk(i)
	    do j=i+1,nd
	      event1=(work(i,1).eq.work(j,1).and.work(i,2).eq.work(j,2).
     &	     and.work(i,3).eq.work(j,3).and.work(i,4).eq.work(j,4))
		  event2=(work(i,1).eq.work(j,3).and.work(i,2).eq.work(j,4).
     &	     and.work(i,3).eq.work(j,1).and.work(i,4).eq.work(j,2))
	      if (event1.or.event2) then
	        work(j,1)=99
			freq(ntype)=freq(ntype)+fwk(j)
	      end if
	    end do
	    work(i,1)=99
	  end do !while
	end do
	
c	do i=1,ntype
c	  write(4,'(4i2,2x,f5.1)') (genotype(i,k),k=1,4),freq(i)
c	enddo
	  
	return
	end

c	*******************************************************************************
	
	subroutine simmatch(n,nloci,simp,simpname,codename,ipar)

c	subroutine to do cluster analysis with simple matching of simplex markers
c	and write out in order of linkage groups.

	
c	parameter(maxind=200,mxloci=6000)

c      USE IMSL_LIBRARIES     
	implicit double precision (a-h,o-z)

      integer simp(n,nloci),ip(nloci)
	integer ficlus(nloci),fnclus(nloci),ficlson(nloci-1),ficrson(nloci-1)
      integer fmclus(nloci)
	real(8) fdist(nloci,nloci),fclevel(nloci-1),ffclevel(nloci-1)
      real(8) D(nloci,nloci)
      character*40  simpname(nloci),codename(nloci)
      character*10 tempname(nloci)
      
	dimension sscale(2)
	character ptext*4
			
           
      
 	fdist=0.0d0
	do i=2,nloci
	  do j=1,i-1
	    ncount=0
	    nmatch=0
	    do k=1,n
	      if (simp(k,i) .eq. 9 .or. simp(k,j) .eq. 9) then
	        cycle
	      else
	        ncount=ncount+1
	        if (simp(k,i)-simp(k,j) .eq. 0) then
	          nmatch=nmatch+1
	        end if
	      end if
	    end do
	    fdist(i,j)=nmatch/real(ncount)
	    fdist(j,i)=fdist(i,j)
	  end do
	end do

c	imeth=0
c	write(4,*)
c	do j=1,nloci
c	  write(4,'(<nloci>f6.3)') (fdist(j,i),i=1,nloci)
c	end do
c	call d_clink(dist=fdist,clevel=fclevel,iclson=ficlson,icrson=ficrson,
c    & npt=nloci,imeth=imeth,idist=1)
c	write(4,*)
c	write(4,*) 'Single linkage clustering'
c	write(4,*)
c      fclevel=1.0-fclevel
c	do lev=1,nloci-1
c	  write(8,*) lev,fclevel(lev),ficlson(lev),ficrson(lev)
c	end do
c	do lev=1,nloci-1
c	  ffclevel(lev)=fclevel(lev)+lev*0.001
c	end do
c      sscale(2) = 1.5d0*ffclevel(nloci-1)
c	call d_treep(iclson=ficlson,icrson=ficrson,clevel=ffclevel,nscale=1,
c     & scale=sscale,nodenm=simpname,node=nloci,imeth=1)
	

c	next statement is needed for main pop but not for Florian's data
c     not needed in general now name length has been increased
c	simpname(1:nloci)=simpname(1:nloci)(11:20)

c	write(*,'(a,i2)') 
c     &	"Input threshold recombination freq. for parent ",ipar
c	read (*,*) thresh

      codename=achar(95)
c	do ii=3,1,-1 old thresholds, replaced 4/6/2015
	do ii=3,1,-1
	thresh = 0.1*ii       !thresh = 0.3,0.2,0.1
c	thresh = 0.2+0.05*ii       !thresh = 0.35,0.3,0.25 old thresholds, replaced 4/6/2015
c	kk = nloci  
c	do lev=1,nloci-1
c	  if (fclevel(lev) .le. thresh) kk=kk-1
c	end do

c	call cnumb(nloci,ficlson,ficrson,kk,ficlus,fnclus)
      
      iclus1=2
      iclus2=nloci-1
      iMill=1   !single linkage method
      D=1.0d0-fdist
      call Milligan_cluster(nloci,simpname,D,iclus1,iclus2,iMill,
     & thresh,ip)
      
      maxip=maxval(ip)
	write(8,*)
	write(8,'(a,f5.2,a,i4)') 'Number of groups at threshold ',
     &	thresh, ' is ',maxip
      write(8,*) 'no. Milligan group is ',maxip
      fmclus = 0
      locloop: do jj=1,nloci
      do kkk=1,maxip
          if (ip(jj) .eq. kkk) then
              fmclus(kkk)=fmclus(kkk)+1
              cycle locloop
          endif
      enddo
      enddo locloop
      do kkk=1,maxip
          write(8,'(2i5)') kkk,fmclus(kkk)
      enddo
      
          
	
	write(8,*)	
	write(8,'(a,i2)') 'Linkage groups from parent ',ipar
c      do jj=1,nloci 
c      write(8,'(i5,2x,2i4)') jj,ficlus(jj),ip(jj)
c      enddo
      
	
	if (ipar .eq. 1) then
	  ptext = ' 1 0'
	elseif (ipar .eq. 2) then
	  ptext = ' 0 1'
	endif

      do kcount=1,maxip
	  write(8,*)	
	  write(8,*) 'Linkage group ',kcount
	  write(8,*) n, fmclus(kcount)

        select case (kcount)
            case (1:26)
c	  if (kcount .le. 26) then            !relabelling of groups
            do i=1,nloci
	    if (ip(i) .eq. kcount) then
	      codename(i)=(achar(kcount+64) // achar(95)) // 
     &		  codename(i)
            write(8,'(2a40)') codename(i),simpname(i)
	      write(8,'(a,<n>i2)') ptext,(simp(j,i),j=1,n) 
c           write(7,*) codename(i)
c	      write(7,'(a,<n>i2)') ptext,(simp(j,i),j=1,n) 
	    end if
            end do

            case (27:52)
c	    elseif (kcount .gt. 26 .and. kcount .le. 52) then 
	  do i=1,nloci
	    if (ip(i) .eq. kcount) then
	      codename(i)=((achar(kcount+38) // achar(kcount+38)) 
     &		  //achar(95)) // codename(i)
             write(8,'(2a40)') codename(i),simpname(i)
	      write(8,'(a,<n>i2)') ptext,(simp(j,i),j=1,n) 
c            write(7,*) codename(i)
c	      write(7,'(a,<n>i2)') ptext,(simp(j,i),j=1,n) 
	    end if
        end do

 
            case (53:78)
c	    elseif (kcount .gt. 52 .and. kcount .le. 78) then 
	  do i=1,nloci
	    if (ip(i) .eq. kcount) then
	      codename(i)=(((achar(kcount+12) // achar(kcount+12)) 
     &		  // achar(kcount+12)) //achar(95)) // codename(i)
            write(8,'(2a40)') codename(i),simpname(i)
	      write(8,'(a,<n>i2)') ptext,(simp(j,i),j=1,n) 
c            write(7,*) codename(i)
c	      write(7,'(a,<n>i2)') ptext,(simp(j,i),j=1,n) 
	    end if
        end do

            case (79:104)
c	    elseif (kcount .gt. 52 .and. kcount .le. 78) then 
	  do i=1,nloci
	    if (ip(i) .eq. kcount) then
	      codename(i)=((achar(kcount-14) // achar(49)) 
     &		   //achar(95)) // codename(i)
            write(8,'(2a40)') codename(i),simpname(i)
	      write(8,'(a,<n>i2)') ptext,(simp(j,i),j=1,n) 
c            write(7,*) codename(i)
c	      write(7,'(a,<n>i2)') ptext,(simp(j,i),j=1,n) 
	    end if
        end do

            case (105:130)
c	    elseif (kcount .gt. 52 .and. kcount .le. 78) then 
	  do i=1,nloci
	    if (ip(i) .eq. kcount) then
	      codename(i)=((achar(kcount-40) // achar(50)) 
     &		   //achar(95)) // codename(i)
            write(8,'(2a40)') codename(i),simpname(i)
	      write(8,'(a,<n>i2)') ptext,(simp(j,i),j=1,n) 
c            write(7,*) codename(i)
c	      write(7,'(a,<n>i2)') ptext,(simp(j,i),j=1,n) 
	    end if
        end do

            case (131:156)
c	    elseif (kcount .gt. 52 .and. kcount .le. 78) then 
	  do i=1,nloci
	    if (ip(i) .eq. kcount) then
	      codename(i)=((achar(kcount-66) // achar(51)) 
     &		   //achar(95)) // codename(i)
             write(8,'(2a40)') codename(i),simpname(i)
	      write(8,'(a,<n>i2)') ptext,(simp(j,i),j=1,n) 
c            write(7,*) codename(i)
c	      write(7,'(a,<n>i2)') ptext,(simp(j,i),j=1,n) 
	    end if
        end do

            case (157:182)
c	    elseif (kcount .gt. 52 .and. kcount .le. 78) then 
	  do i=1,nloci
	    if (ip(i) .eq. kcount) then
	      codename(i)=((achar(kcount-92) // achar(52)) 
     &		   //achar(95)) // codename(i)
            write(8,'(2a40)') codename(i),simpname(i)
	      write(8,'(a,<n>i2)') ptext,(simp(j,i),j=1,n) 
c            write(7,*) codename(i)
c	      write(7,'(a,<n>i2)') ptext,(simp(j,i),j=1,n) 
	    end if
        end do

            case (183:208)
c	    elseif (kcount .gt. 52 .and. kcount .le. 78) then 
	  do i=1,nloci
	    if (ip(i) .eq. kcount) then
	      codename(i)=((achar(kcount-118) // achar(53)) 
     &		   //achar(95)) // codename(i)
             write(8,'(2a40)') codename(i),simpname(i)
	      write(8,'(a,<n>i2)') ptext,(simp(j,i),j=1,n) 
c            write(7,*) codename(i)
c	      write(7,'(a,<n>i2)') ptext,(simp(j,i),j=1,n) 
	    end if
        end do

            case default
c	    elseif (kcount .gt. 78) then 
	  do i=1,nloci
	    if (ip(i) .eq. kcount) then
            write(8,'(2a40)') codename(i),simpname(i)
	      write(8,'(a,<n>i2)') ptext,(simp(j,i),j=1,n) 
c            write(7,*) codename(i)
c	      write(7,'(a,<n>i2)') ptext,(simp(j,i),j=1,n) 
	    end if
        end do
        end select
c	    endif 

	end do
c	if (ii .eq. 1) then
	if (ipar .eq. 1) then
	write(5,'(a,i2)') 'Relabelled simplex markers from parent ',ipar
	write(5,*) n,nloci
	do i=1,nloci
	  write(5,'(a40,2x,a40,2x,a4,<n>i2)') simpname(i),codename(i),ptext,
     &    (simp(j,i),j=1,n) 
	end do
	elseif (ipar .eq. 2) then
	write(7,'(a,i2)') 'Relabelled simplex markers from parent ',ipar
	write(7,*) n,nloci
	do i=1,nloci
	  write(7,'(a40,2x,a40,2x,a4,<n>i2)') simpname(i),codename(i),ptext,
     &   (simp(j,i),j=1,n) 
	end do
	endif

c	endif
	end do

	return
	end

c	********************************************************************************
	subroutine chicalc(n,ifile,itype,simpct,nsimpct,kk,simpname,
     &	nsimpname,simp,nsimp,chisq1,sigx1)


c	to work out distribution of offspring gel pattern.

     
      
	implicit double precision (a-h,o-z)

c	parameter(maxind=200,mxloci=6000)

      integer simpct,nsimpct,gelp(2,kk)
	integer n
      integer simp(n,simpct),nsimp(n,nsimpct),gelo(n,2)
      real(8) chisq1(simpct,nsimpct),sigx1(simpct,nsimpct)
      character*40  simpname(simpct),nsimpname(nsimpct)
      character*1 phase(simpct,nsimpct)
      

	
	integer cumcttot,df
    
	dimension lpat(kk),marg1(2),marg2(kk)
      dimension expect(2,kk)

         
      
	do i=1,kk
	lpat(i) = i-1
	enddo
		
	pair_loop: do l=1,simpct
	  do m=1,nsimpct
	    do i=1,n
		    if (simp(i,l) .eq. 9 .or. nsimp(i,m) .eq. 9) then
	          gelo(i,1) = 99
	          gelo(i,2) = 99
	        else
		      gelo(i,1)=simp(i,l)
			  gelo(i,2)=nsimp(i,m)
	        end if
c	        write(4,'(2i4)') (gelo(i,j),j=1,2)
		end do 	
	gelp=0

	cumcttot=0
	do i=1,2
	  do j=1,kk
	    do k=1,n
	      if (gelo(k,1).ne.99) then
	        idif=(lpat(i)-gelo(k,1))**2
                if (idif .eq. 0) then
	            idif=idif+(lpat(j)-gelo(k,2))**2
                  if (idif .eq. 0) then
	                gelp(i,j) = gelp(i,j)+1
				    gelo(k,1)=99
				end if
			  end if
		   end if
		 end do 
	     cumcttot=cumcttot+gelp(i,j)
c	     write(4,'(6i5)') l,m,i,j,idif,gelp(i,j)
	  end do
	end do        

	if (gelp(1,1) .gt. gelp(2,1)) then
	  phase(l,m) = 'C'
	else
	  phase(l,m) = 'R'
	endif

	marg1 = 0
	marg2 = 0


	do i=1,2
	  do j=1,kk
	    marg1(i)=marg1(i)+gelp(i,j)
	    marg2(j)=marg2(j)+gelp(i,j)
	  end do
	end do
c	im=ntype(m)
	
c     write (4,*)
c	write (4,'(a,i4,1x,a,a,i4,1x,a)') 'Pair of loci ',l,simpname(l),
c     &	' and ',m,nsimpname(m)
c      write (6,'(a,i4,1x,a,a,i4,1x,a)') 'Pair of loci ',l,simpname(l),
c     &	' and ',m,nsimpname(m)

c	write(6,*)  'L1 ',simpname(l)
c	write(6,*)  'L2 ',nsimpname(m)

c	write (4,*) 'Observed counts'
c	do i=1,2
c	  write (4,'((2(1x,i4)),6x,i6)') 
c    &	  (gelp(i,j),j=1,2),marg1(i)
c	end do
c	write (4,'(2(1x,i4))') (marg2(j),j=1,2)
c	write (4,*) 'Sum of total counts', cumcttot
      chisq1(l,m)=0.0
      do i=1,2
	  do j=1,kk
	  expect(i,j)=marg1(i)*marg2(j)/real(cumcttot)
	  if (expect(i,j) .gt. 0.0d0) then
        chisq1(l,m) = chisq1(l,m)+
     &    ((gelp(i,j)-expect(i,j))**2)/expect(i,j)
	  end if
	  end do
	end do

c	write(4,*) 'Expected counts'

c	do i=1,2
c	  do j=1,2
c	  write (4,"(x,f6.2)",advance="no") expect(i,j)
c	  end do
c      write (4,*) "  "
c	end do


	df=kk-1
      ifault=0
	sigx1(l,m)=1-CHISQN(1.0d0*chisq1(l,m),1.0d0*df,ifault)
	
c	write(4,'(1x,a,2x,f8.2,2x,a,f8.5)') 'Chi-squared statistic ', 
c     &	chisq(l,m),'Significance',sigx(l,m)
	end do
	end do pair_loop

	write(ifile,*)
c	write(ifile,*) 'Summary of significant linkages'
	if (itype .eq.5) then
	do m=1,nsimpct
	  do l=1,simpct
	    if (sigx1(l,m) .lt. 0.001) then
	      write(ifile,'(a40,2x,a40,2x,f8.2,2x,f10.8,2x,a)') 
     &		  nsimpname(m),simpname(l),chisq1(l,m),sigx1(l,m),phase(l,m)
	    endif
	  enddo
	enddo
	elseif (itype .eq. 3) then
	do m=1,nsimpct
	  do l=1,simpct
	    if (sigx1(l,m) .lt. 0.001 .and. phase(l,m) .eq. 'C') then
	      write(ifile,'(a40,2x,a40,2x,f8.2,2x,f10.8,2x,a)') 
     &		  nsimpname(m),simpname(l),chisq1(l,m),sigx1(l,m),phase(l,m)
	    endif
	  enddo
	enddo
	endif

      
	return
	end

c	************************************************
	subroutine chimatrix(n,ifile,fsimpct,i3to1,fsimpname1,bname1,fsimp1,
     &	bothtest1,chisq1,sigx1)


c	to work out distribution of offspring gel pattern.
     
	implicit double precision (a-h,o-z)

c	parameter(maxind=200,mxloci=6000)

      integer gelp(5,5),n
	integer cumcttot,df,fsimpct

      integer wkgel(n,fsimpct+i3to1),wkgel1(n,fsimpct+i3to1)
      integer ntype(fsimpct+i3to1),gelo(n,2)  
      integer fsimp1(n,fsimpct),bothtest1(n,i3to1)
      integer cumct(fsimpct+i3to1) 
 

      character*40 fsimpname1(fsimpct),bname1(i3to1)
      character*40 locname(fsimpct+i3to1)
      
      real(8) chisq1(fsimpct+i3to1,fsimpct+i3to1)
      real(8) sigx1(fsimpct+i3to1,fsimpct+i3to1)
      integer lcount(fsimpct+i3to1,5),lpat(fsimpct+i3to1,5)
      
	

	     
	
      dimension expect(5,5),marg1(5),marg2(5)

      
      
c      write (4,*) 'working gel for parent 1'
c      do i=1,n
c
c	  write (4,'(8(1x,i2))') (wkgel(i,1,j),j=1,8)
c	end do
c     write (4,*) 'working gel for parent 2'
c	do i=1,n
c	  write (4,'(8(1x,i2))') (wkgel(i,2,j),j=1,8)
c	end do
	wkgel=0
	wkgel(1:n,1:fsimpct)=fsimp1(1:n,1:fsimpct)
	locname(1:fsimpct)=fsimpname1(1:fsimpct)
	do i=1,i3to1
	  wkgel(1:n,fsimpct+i)=bothtest1(1:n,i)
	  locname(fsimpct+i)=bname1(i)
	end do
	
	nloci = fsimpct+i3to1


      locus_loop: do l=1,nloci
	  do i=1,5
	    lcount(l,i)=0
	    lpat(l,i)=0
	  end do

          do i=1,n 
	        if (wkgel(i,l) .eq. 9) then
	          wkgel1(i,l) = 99
c	          exit 
	        else
	          wkgel1(i,l)=wkgel(i,l)
	        end if
	  end do 
c      write (4,*)
c	write (4,'(a,i4,1x,a)') 'Locus ',l,locname(l)
c        do i=1,n
c	    write (4,'(1x,i2)') wkgel1(i,l)
c	  end do

	ntype(l)=0
	cumct(l)=0
	do i=1,n
	  do while (wkgel1(i,l).ne.99) !count patterns in parent l
	    ntype(l)=ntype(l)+1
	    lpat(l,ntype(l))=wkgel1(i,l)
	    lcount(l,ntype(l))=lcount(l,ntype(l))+1
		do j=i+1,n
		  idif=(wkgel1(i,l)-wkgel1(j,l))**2
		  if (idif.eq.0) then
			wkgel1(j,l)=99
			lcount(l,ntype(l))=lcount(l,ntype(l))+1
		  end if
		end do
		wkgel1(i,l)=99
	    cumct(l)=cumct(l)+lcount(l,ntype(l))
	  end do !while
	end do

c      do i=1,ntype(l)
c	  write(4,'(a,1x,i3,2x,5x,i5)') locname(l),lpat(l,i),lcount(l,i)
c	end do

c	write (4,'(a,i5,2x,a,2x,a,i50)') 'Sum of locus ',l,locname(l),
c     &	' counts', cumct(l)
      end do locus_loop

	
	pair_loop: do l=1,fsimpct
	  do m=fsimpct+1,fsimpct+i3to1
	    do i=1,n
		    if (wkgel(i,l) .eq. 9 .or. wkgel(i,m) .eq. 9) then
	          gelo(i,1) = 99
c	          exit 
	        else
		      gelo(i,1)=wkgel(i,l)
			  gelo(i,2)=wkgel(i,m)
	        end if
		end do 	
	    do i=1,5
	      do j=1,5
	        gelp(i,j)=0
	      end do
	    end do

	cumcttot=0
	do i=1,ntype(l)
	  do j=1,ntype(m)
	    do k=1,n
	      if (gelo(k,1).ne.99) then
		      idif=(lpat(l,i)-gelo(k,1))**2
                if (idif .eq. 0) then
	            idif=idif+(lpat(m,j)-gelo(k,2))**2
                  if (idif .eq. 0) then
	                gelp(i,j) = gelp(i,j)+1
				    gelo(k,1)=99
				end if
			  end if
		   end if
		 end do 
	     cumcttot=cumcttot+gelp(i,j)
	  end do
	end do        

      do i=1,5
	  marg1(i) = 0
	  marg2(i) = 0
	end do

	do i=1,ntype(l)
	  do j=1,ntype(m)
	    marg1(i)=marg1(i)+gelp(i,j)
	    marg2(j)=marg2(j)+gelp(i,j)
	  end do
	end do
	im=ntype(m)
	
      chisq1(l,m)=0.0
      do i=1,ntype(l)
	  do j=1,ntype(m)
	  expect(i,j)=marg1(i)*marg2(j)/real(cumcttot)
	  if (expect(i,j) .gt. 0.0d0) then
        chisq1(l,m) = chisq1(l,m)+
     &   ((gelp(i,j)-expect(i,j))**2)/expect(i,j)
	  end if
	  end do
	end do
	df=(ntype(l)-1)*(ntype(m)-1)
      ifault=0
	sigx1(l,m)=1-CHISQN(1.0d0*chisq1(l,m),1.0d0*df,ifault)

	if (sigx1(l,m) .lt. 0.001) then
      write (8,*)
	write (8,'(a,i4,1x,a,a,i4,1x,a)') 'Pair of loci ',l,locname(l),
     &	' and ',m,locname(m)
c      write (6,'(a,i4,1x,a,a,i4,1x,a)') 'Pair of loci ',l,locname(l),
c     &	' and ',m,locname(m)
	write (8,*) 'Size of matrix',ntype(l),ntype(m)
	write (8,*) 'Observed counts'
	do i=1,ntype(l)
	  write (8,'((<im>(1x,i4)),6x,i6)') 
     &	  (gelp(i,j),j=1,ntype(m)),marg1(i)
	end do
	write (8,'(<im>(1x,i4))') (marg2(j),j=1,ntype(m))
	write (8,*) 'Sum of total counts', cumcttot

c	write(4,*) 'Expected counts'

c	do i=1,ntype(l)
c	  do j=1,ntype(m)
c	  write (4,"(x,f6.2)",advance="no") expect(i,j)
c	  end do
c       write (4,*) "  "
c	end do

	write(8,'(1x,a,2x,f8.2,2x,a,f8.5)') 'Chi-squared statistic ', 
     &	chisq1(l,m),'Significance',sigx1(l,m)
	endif
	end do
	end do pair_loop

	write(ifile,*)
c	write(ifile,*) 'Summary of significant linkages'
	do m=fsimpct+1,fsimpct+i3to1
	  do l=1,fsimpct
	    if (sigx1(l,m) .lt. 0.001) then
	      write(ifile,'(a40,2x,a40,2x,f8.2,2x,f10.8,2x,a)') 
     &		  locname(m),locname(l),chisq1(l,m),sigx1(l,m)
	    endif
	  enddo
	enddo

      
	return
      end

     
c	********************************************************
	subroutine SNPbandista(n,name,alpha,iprint,p1,p2,nob,wkb,c,nt,chiprob,
     & prob)

	implicit double precision (a-h,o-z)

	parameter (maxind=300)

	integer g1(4),g2(4),p1gt(10,2),p2gt(10,2),zygote(100,4)
	integer p1,p2,bptn(100),genotype(8),phenotype(8)
	integer wkb(100)
      character name*40
	dimension mtx1(6,6),mtx2(6,6),p1gtfq(10),p2gtfq(10),freq(100),c(100)
c	dimension prior(100),xlkhd(100),slkhd(100),chisqr(100)

c	convert SNP number to 4 digit code
	do k=1,4
	  g1(k)=1
	  g2(k)=1
	end do
	SNPpos1=5-p1
	SNPpos2=5-p2
	do k=SNPpos1,4
	  g1(k)=2
	enddo
	do k=SNPpos2,4
	  g2(k)=2
	enddo
c	write(4,'(a,2x,4i1,2x,4i1)') 'SNPpata',(g1(k),k=1,4),(g2(k),k=1,4)
c	write(4,'(4i1,2x,4i1)') (g1(k),k=1,4),(g2(k),k=1,4)

	
	call gametogensis(alpha,g1,ngt1,p1gt,p1gtfq)
	call gametogensis(alpha,g2,ngt2,p2gt,p2gtfq)
	call zygt(p1gt,p2gt,p1gtfq,p2gtfq,ngt1,ngt2,ntype,zygote,freq)
	call SNPgtob(ntype,zygote,freq,np,bptn)
c	write(6,*)
c	write(4,'(8(i2))') ((bptn(k,l),l=1,8),k=1,np)
c	nptn=np
	call SNPptsort(n,np,iprint,bptn,freq,nob,wkb,c,x2,chiprob,prob)
c	write(4,'(f7.2,f10.4)') alpha,chiprob

	return
	end
c	****************************************************************
	function pmltnr(n,nt,c,q)

c     calculates log-likelihood for a multinomial distribution

	implicit double precision (a-h,o-z)

	integer c(100)

	dimension q(100)

	pmltnr=0.0d0
	cf=0.0d0
	sum=0.0d0
	nsum=0
	do i=1,nt
	  sum=sum+q(i)
	  nsum=nsum+c(i)
	end do

	if (nsum.lt.n) then
	  return
	else if (sum.lt.1.0d0) then
	  nt=nt+1
	  c(nt)=n-nsum
	  q(nt)=1.0d0-sum
	end if
		
	cf1=0.0d0
	do i=1,nsum
	  cf1=cf1+dlog(1.0d0*i)
	end do
	cf2=0.0d0
	do i=1,nt
	  do j=1,c(i)
	    cf2=cf2+dlog(1.0d0*j)
	  end do
	end do

	cf=cf1-cf2

	prod=0.0d0
	do i=1,nt
	  prod=prod+c(i)*dlog(q(i))
	end do

	pmltnr=dexp(cf+prod)

	return
      end

c      *********************************************************************

c change program to create subroutine Milligan_cluster and call to it


      subroutine Milligan_cluster(N,locname,D,ICLUS1,ICLUS2,imeth,
     & distlim,IP)
      
c     N =  number of points to be clustered
c     locname = names of points
c     D = distance matrix
c     ICLUS1 = smallest number of clusters, min 2
c     ICLUS2 = largest number of clusters, max N-1
c     imeth=clustering method. imeth=1 for single linkage, 3 for group average
c     distlim = distance at which to stop clustering, overrules ICLUS1
c     IP=group identifier
C***********************************************************************
C
C Generalized Agglomerative Hierarchical Clustering Algorithms
C Programmed By Glenn W. Milligan
C Prepared March 1980 - The Ohio State University
C Revised By Richard Cheng, October 1993
C
C***********************************************************************
C
C Complier Specific Information:
C
C     The present software code makes calls to several subroutines or
C     functions that may not be supported by other FORTRAN compilers.
C     First, these complier specific routines can be safely disabled
C     with no adverse effect to the actual clustering computations.
C     All routines are directed at improving the user interface.
C     For example, consider CALL BEEP. This sounds a warning beep
C     when the user should be aware of a data entry error or problem.
C     The second is the INQUIRE() function that tests for the existence
C     of the specified input data file.
C     The PAUSE() function simply waits for the user to hit any
C     key. The function IARGC() counts the number of command line
C     arguments present when HCINFLU.EXE was started.
C     Screen graphics functions include:
C     SET_VIDEO_MODE()
C     GET_VIDEO_MODE()
C     GRAPHICS_MODE()
C     CLEAR()
C     HOME()
C     SET_PALETTE()
C     TEXT_MODE
C     CLEAR_TEXT
C     Again, these functions can be disabled when adopting the
C     program for a different complier.
C
C
c     cut down to drop test of influence and plots, now to work on similarity matrix
C
c      PARAMETER(MAXPTS=8000)
C
      implicit double precision (a-h,o-z)
      
      REAL*8 D(N,N),XX(N),P(N*2)

      REAL*8 LEVEL(N)

      INTEGER ERR,IP(N),LK(N),LKI(N),LKJ(N)

      LOGICAL W(N),WD(N,N),TEST

      CHARACTER*40 locname(N)

c      DATA XXXX / 'X'/,BLANK / ' '/,PERIOD / '.'/
c      DATA MC /'Single Linkage Method','Complete Linkage Method',
c     *         'Group Average Method','Weighted Average Method',
c     *         'Centroid Method','Median Method',
c     *         'Ward''s Minimum Variance Method',
c     *         'Lance & William''s Beta-Flexible Method',
c     *         'Belbin, Faith, & Milligan''s Beta-Flexible Method',
c     *         'User Specified Method'/
c      DATA PAL /'4','3','3','3','3','3','3','3','3','3','3','3',
c     *           '3','3','3','3','4'/
C
C


c      IS=1    !keep this for now
      ISKIP = 0
      ITMP  = imeth  !single link

      LEV1 = N-ICLUS1
      LEV2 = N-ICLUS2
c      S = IS
      N1 = N-1
      N2 = N1-1
      XN = N
c      DO  I = 1, N
c         XR(I) = 0.0
c         DR(I) = 0.0
c         E(I,I) = 0.0
c      enddo




      IKOUNT = 1
      K = imeth   !Single link method
c      JR = 0

c      DO I = 1, N
c         DO J = 1, N
c            D(I,J) = E(I,J)*S
c         enddo
c      enddo
      DO IIXP = 1, N
         W(IIXP) = .FALSE.
         LK(IIXP) = 0
         P(IIXP) = 1.
c         GM(IIXP) = 0.0
         DO IIPJ = 1, N
            WD(IIPJ,IIXP) = .FALSE.
         enddo
      enddo
C
C Begin Clustering Program
C
      DO 2010 II = 1, N1
         DO 1850 NI = 2, N
            IF (W(NI)) GO TO 1850
            NI1 = NI-1
            DO 1840 NJ = 1, NI1
               IF (.NOT.W(NJ)) GO TO 1860
1840        CONTINUE
1850     CONTINUE
         GO TO 2970
1860     X = D(NI,NJ)
         DO 1880 I = 2, N
            IF (W(I)) GO TO 1880
            I1 = I-1
            DO 1870 J = 1, I1
               IF (W(J)) GO TO 1870
               IF ((D(I,J)-X).GT.0.0) GO TO 1870
               NI = I
               NJ = J
               X = D(I,J)
1870        CONTINUE
1880     CONTINUE
         XX(II) = X
         LKI(II) = NI
         LKJ(II) = NJ
         W(NJ) = .TRUE.
         DO 2000 L = 1, N
            IF (W(L)) GO TO 2000
            IF (L.EQ.NI) GO TO 2000
C
C Branch to Method
C
            GO TO (1890,1900,1910,1920,1930,1940,1950,1960,1970,1980), K
1890        IF ((D(NI,L)-D(NJ,L)).LE.0.0) GO TO 1990
            D(NI,L) = D(NJ,L)
            D(L,NI) = D(L,NJ)
            GO TO 1990

1900        IF ((D(NI,L)-D(NJ,L)).GE.0.0) GO TO 1990
            D(NI,L) = D(NJ,L)
            D(L,NI) = D(L,NJ)
            GO TO 1990

1910        D(L,NI) = (P(NI)/(P(NI)+P(NJ)))*D(L,NI)+(P(NJ)/(P(NI)+P(NJ))
     *         )*D(L,NJ)
            D(NI,L) = D(L,NI)
            GO TO 1990

1920        D(L,NI) = .5*D(L,NI)+.5*D(L,NJ)
            D(NI,L) = D(L,NI)
            GO TO 1990

1930        D(L,NI) = (P(NI)/(P(NI)+P(NJ)))*D(L,NI)+(P(NJ)/(P(NI)+P(NJ))
     *         )*D(L,NJ)-((P(NI)*P(NJ))/((P(NI)+P(NJ))**2.))*D(NI,NJ)
            D(NI,L) = D(L,NI)
            GO TO 1990

1940        D(L,NI) = .5*D(L,NI)+.5*D(L,NJ)-.25*D(NI,NJ)
            D(NI,L) = D(L,NI)
            GO TO 1990

1950        D(L,NI) = ((P(NI)+P(L))/(P(NI)+P(NJ)+P(L)))*D(L,NI)+((P(NJ)+
     *         P(L))/(P(NI)+P(NJ)+P(L)))*D(L,NJ)-(P(L)/(P(NI)+P(NJ)+P(L)
     *         ))*D(NI,NJ)
            D(NI,L) = D(L,NI)
            GO TO 1990

1960        D(L,NI) = .5*(1.-BETALW)*D(L,NI)+.5*(1.-BETALW)*D(L,NJ)+
     *         BETALW*D(NI,NJ)
            D(NI,L) = D(L,NI)
            GO TO 1990

1970        D(L,NI) = (1.-BETA)*(P(NI)/(P(NI)+P(NJ)))*D(L,NI)+(1.-BETA)*
     *         (P(NJ)/(P(NI)+P(NJ)))*D(L,NJ)+BETA*(D(NI,NJ))
            D(NI,L) = D(L,NI)
            GO TO 1990
1980        CONTINUE

1990  CONTINUE
2000     CONTINUE
         P(NI) = P(NI)+P(NJ)
c         write(9,*) II, 'distance matrix'
c         do MM=1,N
c             write (9,'(<N>(f8.4))') (D(MM,NN),NN=1,N)
c         enddo
c         write(9,*)

2010  CONTINUE
      
c     end of 2010 loop      
      LK(1) = LKJ(N1)
      LK(2) = LKI(N1)
      DO 2050 II = 2, N1
         JJ = N-II
         DO 2020 KK = 1, II
            IF (LKI(JJ).EQ.LK(KK)) GO TO 2030
2020     CONTINUE
         GO TO 2970
2030     DO 2040 IK = KK, II
            KI = II+KK-IK
            LK(KI+1) = LK(KI)
2040     CONTINUE
         LK(KK) = LKJ(JJ)
2050  CONTINUE
      
      
      DO 2100 I = 1, N1
         DO 2060 JI = 1, N
            IF (LK(JI).EQ.LKI(I)) GO TO 2070
2060     CONTINUE
         GO TO 2970
2070     DO 2080 JJ = 1, N
            IF (LK(JJ).EQ.LKJ(I)) GO TO 2090
2080     CONTINUE
         GO TO 2970
2090     LKI(I) = JI
         LKJ(I) = JJ
2100  CONTINUE
      DO 2120 I = 2, N2
         I1 = I-1
         DO 2110 J = 1, I1
            JJ = I-J
            IF (LKJ(I).NE.LKI(JJ)) GO TO 2110
            LKJ(I) = LKJ(JJ)
            GO TO 2120
2110     CONTINUE
2120  CONTINUE
      XX(N) = XX(N1)

      DO 2380 IIXP = 2, N
         I1 = IIXP-1
         DO 2380 IIPJ = 1, I1
            D(IIPJ,IIXP) = 0.0
2380  CONTINUE
      NIC1 = (N*(N-1))/2
      agloop: DO  IIPX1 = 1, N2
          if (XX(IIPX1) .gt. distlim) exit agloop
         JJX = LKJ(IIPX1)
         JX = LKI(IIPX1)
         JX1 = JX-1
         DO 2400 IIXP2 = JJX, JX1
            JJX1 = IIXP2+1
            DO 2390 I1I = JJX1, JX
               JPL1 = MIN0(LK(I1I),LK(IIXP2))
               JPL2 = MAX0(LK(I1I),LK(IIXP2))
               IF (D(JPL1,JPL2).NE.0.0) GO TO 2390
               D(JPL1,JPL2) = XX(IIPX1)
               WD(JPL1,JPL2) = .TRUE.
2390        CONTINUE
2400     CONTINUE
         
c       write out information that might be agglomerations
c         do MM=1,N
c             write(9,'(i3,2x,5i8,2x,f12.6)') MM,LEVEL(MM), IP(MM),
c     &      LK(MM),LKI(MM),LKJ(MM),XX(MM)
c         enddo
c         write(9,*)
c         do MM=1,N
c             write (9,*) (WD(MM,NN),NN=1,N)
c         enddo
c         write(9,*)
c         write(9,'(2I6,f12.6)') JPL1,JPL2,D(JPL1,JPL2)
c         write(9,*)
C
C   Compute Cluster Centroids
C
         IF (IIPX1.LT.LEV2) cycle agloop !GO TO 2670
         IF (IIPX1.GT.LEV1) cycle agloop !GO TO 2670
c         IF (IR.EQ.1.AND.JR.EQ.0) GO TO 2670
         INPX1 = N-IIPX1
c         WRITE (7,2410) INPX1
2410     FORMAT (/,' Conducting analyses of cluster level ',I2)
         DO 2420 I = 2, N
            IP(I) = 0
2420     CONTINUE
         NCR = 1
         II = 1
         J = 2
         IP(1) = 1
2430     DO 2440 JJ = J, N
            IF (.NOT.WD(II,JJ)) GO TO 2440
            IP(JJ) = NCR
            GO TO 2480
2440     CONTINUE
2450     NCR = NCR+1
         IF (NCR.GT.INPX1) GO TO 2490
         DO 2460 JJ = 1, N
            IF (IP(JJ).EQ.0) GO TO 2470
2460     CONTINUE
         GO TO 2970
2470     IP(JJ) = NCR
2480     II = JJ
         J = II+1
         IF (II.GE.N) GO TO 2450
         GO TO 2430
2490     CONTINUE
c         WRITE (9,2500) INPX1,INPX1
2500     FORMAT (//,' Cluster membership of data points for the ',I3,
     *      ' cluster solution',/' (Hierarchy level ',I3,')',/)

c         WRITE (9,2510)
2510     FORMAT (' Point    Assigned to cluster',/,1X,30('-'))
c         DO I = 1, N
c            WRITE (9,'(1X,I5,2X,a30,4X,I3)') I,locname(I),IP(I)
c         enddo
C
c2670     CONTINUE
      enddo agloop
c      write(9,*) 'Exit agloop'


c2910  CONTINUE

2970  CONTINUE
      RETURN
      END
c     ***********************************************************************
C UKC NETLIB DISTRIBUTION COPYRIGHT 1990 RSS
C
      FUNCTION CHISQN(X, DF, IFAULT)
C
C<<<<<  Acquired in machine-readable form from 'Applied Statistics'
C<<<<<  algorithms editor, January 1983.
C
C
C        ALGORITHM AS 170  APPL. STATIST. (1981) VOL.30, NO.3
C
C        The non-central chi-squared distribution.
C
C     Auxiliary routines required: GAMMDS = AS147, ALOGAM = CACM 291.
C     See AS245 for an alternative to ALOGAM.
c     taken from StatLib, lib.stta.cmu.edu
c     If you use an algorithm, dataset, or other information from StatLib, please acknowledge both StatLib and the original contributor of the material.
c     StatLib is hosted by
c     the Department of Statistics at Carnegie Mellon University
C
c     21/10/14 edited by CH to make into central chi-square where FL is always zero
      implicit double precision (a-h,o-z)
      CHISQN = 0.0
      IFAULT = 0
C
C        TEST FOR ADMISSIBILITY OF ARGUMENTS
C
      IF (DF.LE.0.0) IFAULT = 1
      IF (X.LT.0.0) IFAULT = 2
c      IF (FL.LT.0.0) IFAULT = 3
      IF (IFAULT.GT.0.OR.X.EQ.0.0) RETURN
C
      DF2 = 0.5*DF
      X2 = 0.5*X
      FXP = GAMMDS(X2,DF2,IFAULT)
      CHISQN = FXP   !CHI(X2,DF2,FL,FXP)
      RETURN
      END
C

C     *********************************************************************
      
           double precision function gammds (y,p,ifault)
c
c        Algorithm AS 147  Appl. Statist. (1980) Vol. 29, No. 1
c
c        Computes the incomplete gamma integral for positive
c        parameters y,p using an infinite series
c
c        Auxiliary function required: ALNGAM = CACM algorithm 291
c
c	 AS239 should be considered as an alternative to AS147
c
      implicit double precision (a-h,o-z)
      data e/1.0d-9/, zero/0.0d0/, one/1.0d0/, uflo/1.0d-37/
c
c        Checks admissibility of arguments and value of f
c
      ifault = 1
      gammds = zero
      if(y.le.zero .or. p.le.zero) return
      ifault = 2
c
c        alngam is natural log of gamma function
c
      arg = p*log(y)-alngam(p+one,ifault)-y
c      if(arg.lt.log(uflo)) return
      if(arg.lt.log(uflo)) then
          gammds = one
          return
      endif
      f = exp(arg)
      if(f.eq.zero) return
      ifault = 0
c
c          Series begins
c
      c = one
      gammds = one
      a = p
    1 a = a+one
      c = c*y/a
      gammds = gammds+c
      if (c/gammds.gt.e) goto 1
      gammds = gammds*f
      return
      end
           
           
c     ***************************************************************
c
      DOUBLE PRECISION FUNCTION ALNGAM(XVALUE, IFAULT)
C
C     ALGORITHM AS245  APPL. STATIST. (1989) VOL. 38, NO. 2
C
C     Calculation of the logarithm of the gamma function
C
      INTEGER IFAULT
      DOUBLE PRECISION ALR2PI, FOUR, HALF, ONE, ONEP5, R1(9), R2(9),
     +		R3(9), R4(5), TWELVE, X, X1, X2, XLGE, XLGST, XVALUE,
     +		Y, ZERO
C
C     Coefficients of rational functions
C
      DATA R1/-2.66685 51149 5D0, -2.44387 53423 7D1,
     +        -2.19698 95892 8D1,  1.11667 54126 2D1,
     +	       3.13060 54762 3D0,  6.07771 38777 1D-1,
     +	       1.19400 90572 1D1,  3.14690 11574 9D1,
     +	       1.52346 87407 0D1/
      DATA R2/-7.83359 29944 9D1, -1.42046 29668 8D2,
     +         1.37519 41641 6D2,  7.86994 92415 4D1,
     +         4.16438 92222 8D0,  4.70668 76606 0D1,
     +         3.13399 21589 4D2,  2.63505 07472 1D2,
     +         4.33400 02251 4D1/
      DATA R3/-2.12159 57232 3D5,  2.30661 51061 6D5,
     +         2.74647 64470 5D4, -4.02621 11997 5D4,
     +        -2.29660 72978 0D3, -1.16328 49500 4D5,
     +        -1.46025 93751 1D5, -2.42357 40962 9D4,
     +        -5.70691 00932 4D2/
      DATA R4/ 2.79195 31791 8525D-1, 4.91731 76105 05968D-1,
     +         6.92910 59929 1889D-2, 3.35034 38150 22304D0,
     +         6.01245 92597 64103D0/
C
C     Fixed constants
C
      DATA ALR2PI/9.18938 53320 4673D-1/, FOUR/4.D0/, HALF/0.5D0/,
     +     ONE/1.D0/, ONEP5/1.5D0/, TWELVE/12.D0/, ZERO/0.D0/
C
C     Machine-dependant constants.
C     A table of values is given at the top of page 399 of the paper.
C     These values are for the IEEE double-precision format for which
C     B = 2, t = 53 and U = 1023 in the notation of the paper.
C
      DATA XLGE/5.10D6/, XLGST/1.D+305/
C
      X = XVALUE
      ALNGAM = ZERO
C
C     Test for valid function argument
C
      IFAULT = 2
      IF (X .GE. XLGST) RETURN
      IFAULT = 1
      IF (X .LE. ZERO) RETURN
      IFAULT = 0
C
C     Calculation for 0 < X < 0.5 and 0.5 <= X < 1.5 combined
C
      IF (X .LT. ONEP5) THEN
	IF (X .LT. HALF) THEN
	  ALNGAM = -LOG(X)
	  Y = X + ONE
C
C     Test whether X < machine epsilon
C
	  IF (Y .EQ. ONE) RETURN
	ELSE
	  ALNGAM = ZERO
	  Y = X
	  X = (X - HALF) - HALF
	END IF
	ALNGAM = ALNGAM + X * ((((R1(5)*Y + R1(4))*Y + R1(3))*Y
     +                + R1(2))*Y + R1(1)) / ((((Y + R1(9))*Y + R1(8))*Y
     +                + R1(7))*Y + R1(6))
	RETURN
      END IF
C
C     Calculation for 1.5 <= X < 4.0
C
      IF (X .LT. FOUR) THEN
	Y = (X - ONE) - ONE
	ALNGAM = Y * ((((R2(5)*X + R2(4))*X + R2(3))*X + R2(2))*X
     +              + R2(1)) / ((((X + R2(9))*X + R2(8))*X + R2(7))*X
     +              + R2(6))
	RETURN
      END IF
C
C     Calculation for 4.0 <= X < 12.0
C
      IF (X .LT. TWELVE) THEN
	ALNGAM = ((((R3(5)*X + R3(4))*X + R3(3))*X + R3(2))*X + R3(1)) /
     +            ((((X + R3(9))*X + R3(8))*X + R3(7))*X + R3(6))
	RETURN
      END IF
C
C     Calculation for X >= 12.0
C
      Y = LOG(X)
      ALNGAM = X * (Y - ONE) - HALF * Y + ALR2PI
      IF (X .GT. XLGE) RETURN
      X1 = ONE / X
      X2 = X1 * X1
      ALNGAM = ALNGAM + X1 * ((R4(3)*X2 + R4(2))*X2 + R4(1)) /
     +              ((X2 + R4(5))*X2 + R4(4))
      RETURN
      END
