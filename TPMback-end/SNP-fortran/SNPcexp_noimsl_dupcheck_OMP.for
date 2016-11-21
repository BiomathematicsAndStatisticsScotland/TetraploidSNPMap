c     cmap.for, developed from tetrasimain.for --- A Fortran-77 program 
c     to analyse recombination between marker loci using the EM algorithm.

c	cexpsub2 modified 8/2/01 so no double reduction

c	programme modified 25/4/2011 to analyse SNP data
c	adapt on 7/7/2011 to remove duplicate and near duplicate markers (ie different for one individual)
c	NB will not handle missing data yet (this is now fixed)

c     For KP's simulation study I've commented out the duplicate lines and dropped fit threshold to 0.00001 to use all SNPs.
c     14/10/2013 I have changed this back for real data.
c     20/11/2013 I've commented out call of ipem and sprob from lines 166-169 as no longer needed

c     9/12/2013 changed to allocatable
c     9/12/2013 NB current locname length is *20, but is *40 in other programs. Problem? Changed to *40 18/3/2014
c     18/3/2013 loop to check for duplicates has been commented out

c     10/12/2014 IMSL routines excluded
c     No need to change anthing in this part, just in subroutine file

c     16/12/2014 Loop to check for duplicates has been commented out
c     19/12/2014 Input format changed to handle codes
c     19/12/2014 Length of locname changed to 30 like clustering as 40 seems too long

c	parameter(maxind=200,mxloci=500)

	implicit double precision (a-h,o-z)
      
	integer, allocatable,save :: SNPp1(:),SNPp2(:),gtp(:,:)
      integer, allocatable :: markpair(:,:),mp(:,:),o(:)
      integer, allocatable,save :: nband(:),wkSNP(:,:),wkp1(:),wkp2(:)
      integer, allocatable,save :: markSNP(:,:)
      integer, allocatable :: snppwd_int(:,:,:)
      real(8), allocatable :: snppwd_real(:,:,:)
c	integer SNPp1(mxloci),SNPp2(mxloci),gtp(mxloci,8)
c     integer mt(mxloci),cgtp(8),markpair(maxind,2),mp(maxind,2)
c	integer nband(mxloci),wkSNP(maxind,mxloci)
c	integer wkp1(mxloci),wkp2(mxloci)

      real(8), allocatable, save :: prec(:),prob(:),rec(:)     
      character*30, allocatable, save :: locname(:)*30,wkname(:)*30
c      character*20, allocatable :: code(:)


c	dimension prec(mxloci),markSNP(maxind,mxloci),prob(mxloci)
c	character locname(mxloci)*20,wkname(mxloci)*20

      character stem*15,infile*22,outfile1*22,outfile2*22,outfile3*22
      character tch*1, fulloutput*1,lname(2)*30
      logical fulloutputl
      integer g1(2,4),g2(2,4),pt(24,4),p1,p2,cgtp(8),iperm(100)
	real(8) y(10,100),yb(10),yv(10)
	real(8)  pprob(100)  !,ipem(100),sprob(100)
	real(8) chisqr(100),chist(100),lod,lod_diff
      integer tid, CHUNK, nthreads, omp_get_thread_num
      integer numthr, omp_get_num_threads
      character tidnum*3
      integer ioresult, readsize
      character(500) iobuf
    
	common /seed/ idum
	
c	common /gparameter/ prec
c	common /revent/ rec

	data (pt(1,i),i=1,4) /      1, 2, 3, 4/
	data (pt(2,i),i=1,4) /      3, 2, 4, 1/
	data (pt(3,i),i=1,4) /      3, 2, 1, 4/
	data (pt(4,i),i=1,4) /      4, 2, 3, 1/
	data (pt(5,i),i=1,4) /      3, 1, 2, 4/
	data (pt(6,i),i=1,4) /      1, 4, 2, 3/
	data (pt(7,i),i=1,4) /      4, 1, 3, 2/
	data (pt(8,i),i=1,4) /      2, 1, 4, 3/
	data (pt(9,i),i=1,4) /      2, 4, 3, 1/
	data (pt(10,i),i=1,4) /     3, 4, 1, 2/
	data (pt(11,i),i=1,4) /     2, 1, 3, 4/
	data (pt(12,i),i=1,4) /     1, 2, 4, 3/
	data (pt(13,i),i=1,4) /     2, 4, 1, 3/
	data (pt(14,i),i=1,4) /     3, 4, 2, 1/
	data (pt(15,i),i=1,4) /     2, 3, 1, 4/
	data (pt(16,i),i=1,4) /     1, 3, 2, 4/
	data (pt(17,i),i=1,4) /     4, 3, 1, 2/
	data (pt(18,i),i=1,4) /     2, 3, 4, 1/
	data (pt(19,i),i=1,4) /     4, 2, 1, 3/
	data (pt(20,i),i=1,4) /     4, 3, 2, 1/
	data (pt(21,i),i=1,4) /     1, 3, 4, 2/
	data (pt(22,i),i=1,4) /     3, 1, 4, 2/
	data (pt(23,i),i=1,4) /     1, 4, 3, 2/
	data (pt(24,i),i=1,4) /     4, 1, 2, 3/
	
	write (*,*) "Input file name (.SNPloc),max 15 characters"
	read (*,"(a)") stem
140   write (*,*) "Write full output (y/n)?"
	read (*,'(a)') fulloutput
      if (fulloutput .ne.'y' .and. fulloutput .ne. 'Y' 
     & .and. fulloutput .ne. 'n' .and.  fulloutput .ne. 'N') then 
          write(*,*) "Incorrect input!"
          
          goto 140
      endif
      if (fulloutput .eq. 'y' .or. fulloutput .eq. 'Y') then
          fulloutputl = .TRUE.
      else
          fulloutputl = .FALSE.
      end if
      
      
	infile = trim(stem) // ".SNPloc"
	outfile1 = trim(stem) // ".SNPout"
	outfile2 = trim(stem) // ".SNPpwd"
      outfile3 = trim(stem) // ".SNPfullout"

	open(3,file=infile,status='old')
	open(8,file=outfile1,status='unknown')
	open(9,file=outfile2,status='unknown')
      if (fulloutputl) then
          open(10,file=outfile3,status='unknown')
      end if
      
      

	read(3,*) n,nloci
      
      allocate (SNPp1(nloci),SNPp2(nloci),gtp(nloci,8),
     & nband(nloci),wkp1(nloci),wkp2(nloci))
      
      allocate (prec(nloci),prob(nloci),rec(nloci))
      
      allocate (markpair(n,2),mp(n,2),o(n),wkSNP(n,nloci),
     & markSNP(n,nloci))
      
      allocate (locname(nloci),wkname(nloci))
c      ,code(nloci))
      
	do i=1,n
	  do j=1,nloci
	    markSNP(i,j) = 0
	  end do  
	end do

	do i=1,nloci
c	  read(3,*) locname(i),code(i),SNPp1(i),SNPp2(i),(markSNP(k,i),k=1,n)
	  read(3,*) locname(i),SNPp1(i),SNPp2(i),(markSNP(k,i),k=1,n)
      end do

	if(fulloutputl) then   
      	write(10,*) 'Parental phenotypes'
	    write(10,*)
	    do i=1,nloci
	        write(10,'(i3,2x,a30,2x,i1,4x,i1)') i,locname(i),
     &	     SNPp1(i),SNPp2(i)
          end do
      end if
      

c	check for duplicates
	wkname(1)=locname(1)
	wkp1(1)=SNPp1(1)
	wkp2(1)=SNPp2(1)
	wkSNP(1:n,1)=markSNP(1:n,1)
	nl2 = 1	

c     check whether to exclude duplicates
150   write (*,*) "Exclude duplicate and near-duplicate markers (y/n)?"
	read (*,'(a)') tch
      if (tch .ne.'y' .and. tch .ne. 'Y' .and. tch .ne. 'n' .and. 
     & tch .ne. 'N') then 
          write(*,*) "Incorrect input!"
          goto 150
      endif


      loop1: do i=2,nloci

c     comment out duplicate loop for simulation study
c          write(4,'(i4)') i
       if (tch .eq. 'y' .or. tch .eq. 'Y') then
	    idup=0
          loop2:  do k=1,i-1
	      pdiff = abs(SNPp1(i)-wkp1(k)) + abs(SNPp2(i)-wkp2(k))
	      if (pdiff .gt. 0) then
	        cycle loop2
	      else
	        ndiff = 0
	        do l=1,n
	          ndiff=ndiff+abs(markSNP(l,i) - wkSNP(l,k))
	        end do
	        if (ndiff .eq. 0) then
	          write(8,*) 'Locus ',i,locname(i),' is the same as locus ',
     &	 	      k,wkname(k)
	          rf=0.0d0
	          lod = -1.0d0*n*log(0.5)
c		      write(7,'(1x,a,2x,a,2x,f7.4,2x,f10.2)') 
c     &         locname(i),wkname(k),rf,lod
	          cycle loop1
	        elseif (ndiff .eq. 1) then
	          write(8,*) 'Locus ',i,locname(i),' is different for one line 
     &          from locus ',k,wkname(k)
	          rf=1.0d0/(1.0d0*n)
	          lod = (n-1)*log(1.0d0-rf) + log(rf)-n*log(0.5)
c		      write(7,'(1x,a,2x,a,2x,f7.4,2x,f10.2)') 
c     &      locname(i),wkname(k),rf,lod
	          cycle loop1
	        elseif (ndiff .eq. 2) then
	          write(8,*) 'Locus ',i,locname(i),' is different for two lines
     & from locus ',k,wkname(k)
	          rf=2.0d0/(1.0d0*n)
	          lod = (n-2)*log(1.0d0-rf) + 2*log(rf)-n*log(0.5)
c		      write(7,'(1x,a,2x,a,2x,f7.4,2x,f10.2)') 
c     &  locname(i),wkname(k),rf,lod
	          cycle loop1
	        endif
	      endif
          enddo loop2
        endif
	  nl2=nl2+1
	  wkname(nl2)=locname(i)
	  wkp1(nl2)=SNPp1(i)
	  wkp2(nl2)=SNPp2(i)
	  wkSNP(1:n,nl2)=markSNP(1:n,i)
      enddo loop1
      
      allocate (snppwd_int(nl2, nl2, 17))
      allocate (snppwd_real(nl2, nl2, 4))

      
      do i=1,nl2
c	  mt(i)=0 
	  prob(i)=0.0d0
	end do
c      nl3 = 0
	do i=1,nl2
          if(fulloutputl) then
	      write(10,'(a,2x,i4,2x,a,2i2)') 'Locus ',i,wkname(i),wkp1(i),
     &	  wkp2(i)
          end if
		p1=wkp1(i)
		p2=wkp2(i)
          nnl=0
          do j=1,n
              if (wkSNP(j,i) .ne. 9) then
                  nnl=nnl+1
	            o(nnl)=wkSNP(j,i)
	        end if
	    end do
	    call gtypepred(nnl,p1,p2,o,alpha,odds,nt,cgtp,chiprob,fulloutputl)
c	    it=0
c		if (chiprob.ge.0.001d0) then   !usually 0.001 but changed for sim study
c	        it=it+1
c              nl3 = nl3 + 1 ! nl3 is the num loci with chiprob ge 0
c		    mt(i)=mt(i)+1
		    prob(i)=chiprob
	        do k=1,8
			    gtp(i,k)=cgtp(k)
              end do
c          else
c              write(*, *) i, chiprob
c		end if
      end do
c	prec(1)=0.5d0
	
      write(*, *) 'Remaining number of loci: ', nl2

      if(fulloutputl) then
	    write (10,*)
	    write (10,*) 'Pairwise analyses'
      end if
c     nl2 is the remaining number of loci after 'remove duplicates' but there might be 
c     more loci getting removed because of  "if (mt(l1).gt.0.and.mt(l2).gt.0)"
	write(9,*) nl2,nint(nl2*(nl2-1)/2.0d0)
c      write(9,*) nl3,nint(nl3*(nl3-1)/2.0d0)

C PRIVATE(outfile1, outfile2, tid, tidnum, l1, markpair, l2, i, nnl, mp, n, gtp, np, lname, g1, g2, gtk)
      CHUNK = 100
!$OMP  PARALLEL
!$OMP& SHARED(wkSNP,pt,wkname,stem,nl2,gtp,n,prob,numthr,
!$OMP& fulloutputl, snppwd_bool, snppwd_int, snppwd_real,nloci)  
!$OMP& DEFAULT (PRIVATE)
      numthr = omp_get_num_threads()
	np=0
	pf=0.0d0
c      it2 = it
      
      tid = omp_get_thread_num()
      if(tid .ne. 0) then
          write(tidnum, '(I3.3)') tid
          outfile1 = trim(stem) // tidnum // ".SNPout"
c	    outfile2 = trim(stem) // tidnum // ".SNPpwd"
	    open(8 + tid * 3,file=outfile1,status='unknown')
c	    open(9 + tid * 3,file=outfile2,status='unknown')
          if (fulloutputl) then
              outfile3 = trim(stem) // tidnum // ".SNPfullout"
              open(10 + tid * 3,file=outfile3,status='unknown')
          end if
      end if
      

!$OMP DO SCHEDULE(dynamic, 10)
      do l1=1,nl2-1
        write(*, '(a, i4)') 'pairwise ', l1
	  do i=1,n
	    markpair(i,1)=wkSNP(i,l1)
	  end do
	  innerloop: do l2=l1+1,nl2
	    do i=1,n
	      markpair(i,2)=wkSNP(i,l2)
	    end do
	    nnl=0
c	    do i=1,n
c	      do j=1,2
c	        mp(i,j)=0 
c	      end do
c	    end do
	    do i=1,n
	      if (markpair(i,1).ne.9 .and. markpair(i,2).ne.9) then
	        nnl=nnl+1
	        do j=1,2
	          mp(nnl,j)=markpair(i,j) 
	        end do
	      end if
          end do
          
          if(fulloutputl) then
	        write(10+3*tid,*) 
     & '-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+'
	        write(10+3*tid,*)
            write(10+3*tid,'(a,3x,a)') wkname(l1),wkname(l2)
	        write(10+3*tid,*)
            write(10+3*tid,'(a4,38x,a4,2x,a4,2x,a6)') 
     &             'Name','P1','P2','Prob.'
	        write(10+3*tid,'(a30,2x,4i1,2x,4i1,2x,f8.5)') wkname(l1),
     &		  (gtp(l1,k),k=1,8),prob(l1)
	        write(10+3*tid,'(a30,2x,4i1,2x,4i1,2x,f8.5)') wkname(l2),
     &		  (gtp(l2,k),k=1,8),prob(l2)
	        write(10+3*tid,*)
          end if
          if (gtp(l1,1) .eq. 0 .and. gtp(l2,5) .eq. 0) then
              if(fulloutputl) then
	            write(10+3*tid,*) 
     &            'Alleles come from different parents: pair omitted'
		        write(10+3*tid,*)
              end if
              cycle innerloop
          endif
          if (gtp(l2,1) .eq. 0 .and. gtp(l1,5) .eq. 0) then
              if(fulloutputl) then
	            write(10 + 3 * tid,*) 
     &            'Alleles come from different parents: pair omitted'
		        write(10 + 3 * tid,*)
              end if
              cycle innerloop
	    endif

c	    do i=1,nnl
c	      write(8 + 2 * tid,'(2x,8i1,4x,8i1)') ((mp(i,j,k),k=1,8),j=1,2)
c	    end do
c	    if (mt(l1).gt.0.and.mt(l2).gt.0) then
	          np=np+1
	          lname(1)=wkname(l1)
	          lname(2)=wkname(l2)
		      do k=1,4
			    g1(1,k)=gtp(l1,k)
			    g2(1,k)=gtp(l1,4+k)
                end do
c			  it2=it2+1    
			  do k=1,4
			    g1(2,k)=gtp(l2,k)
			    g2(2,k)=gtp(l2,4+k)
			  end do
c			  write(6,'(1x,4i1,2x,4i1)') (g1(1,k),k=1,4),(g2(1,k),k=1,4)
c			  write(6,'(1x,4i1,2x,4i1)') (g1(2,k),k=1,4),(g2(2,k),k=1,4)
		      call linkage(n,nnl,pt,g1,g2,lname,mp,rf,lod, fulloutputl, 
     &           snppwd_int, snppwd_real, nl2, l1, l2)
                
c			  write(8 + 2 * tid,'(1x,i3,2x,i3,2x,a,2x,a,2x,f7.4,2x,3f10.2)') 
c     &			  l1,l2,wkname(l1),wkname(l2),rf,lod
c			  write(7,'(1x,a,2x,2i2,2x,a,2x,2i2,2x,f7.4,2x,f10.2)') 
c     &  wkname(l1),wkp1(l1),wkp2(l1),wkname(l2),wkp1(l2),wkp2(l2),rf,lod
c	    end if
	  end do innerloop
      end do
!$OMP END DO
      if(tid .ne. 0) then
          close(8 + 3 * tid)
          close(9 + 3 * tid)
          if(fulloutputl) close(10 + 3 * tid)
      end if
!$OMP END PARALLEL

c      close(8)
c      open(8, file="twopoint.SNPout", position='append', form='binary', 
c     & status="old")
c      close(9)
c      open(9, file="twopoint.SNPpwd", position='append', form='binary', 
c     & status="old")

      do i=1,numthr-1
          write(tidnum, '(I3.3)') i
          outfile1 = trim(stem) // tidnum // ".SNPout"
	    open(11,file=outfile1,status='old')
          do
              read(11, '(A500)', iostat=ioresult) iobuf
              if(ioresult .ne. 0) exit
              write(8, '(A)')  trim(iobuf)
          end do
          close(11) 
      end do

      do l1=1,nl2

c        write(*, *) ln1
c        write(lname(1), '(i30)') ln1
c        lname(1) = adjustl(lname(1))
        lname(1) = wkname(l1)
        do l2=l1+1,nl2
            if(snppwd_int(l1,l2,17)==1) then
              lname(2) = wkname(l2)
              rf = snppwd_real(l1, l2, 1)
              lod = snppwd_real(l1, l2, 2)
              lod_diff = snppwd_real(l1, l2, 3)
              rf_next = snppwd_real(l1, l2, 4)
              write(9, 
     &'(1x,a,2x,a,2x,f7.4,2x,f10.2,a3,<2>(2x,4i1,4i1),2x,f7.4,2x,f10.2)'
     &      ) lname(1),lname(2),rf,lod,' ; ',
     &      (snppwd_int(l1,l2,k*2+1),k=0,3),
     &      (snppwd_int(l1,l2,k*2+2),k=0,3),
     &      (snppwd_int(l1,l2,8+k*2+1),k=0,3),
     &      (snppwd_int(l1,l2,8+k*2+2),k=0,3),lod_diff,rf_next
          end if
        end do
      end do
      
100	format(1x,4i1,2x,4i1,2x,f8.5)
      

	end
