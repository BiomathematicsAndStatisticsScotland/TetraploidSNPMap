c     Modified version of tetrecon.for, to reconstruct offspring using 
c	a map from a single parent, and to handle missing marker data

c	Trait has missing value code -99

c	This is modified from C:\DevStudioprogs\ZWLeft\QTL2004\realoneparrecon.for, starting on 8/9/2011

c	 20/3/2012 Modified programme to use real recombination frequencies and correct binomial probabilities for transitions
c     added 8/10/2012 calculate average weights over all offspring to check for distorted segregation   

c     11/7/14 only change from SNP_QTLhmmv2.for is to handle longer trait names (20 characters)
c     also change output file name to v3

c     11/12/2014 changes that need to be made
c     need to replace IMSL routine  dcsiez, calculates smoothing spline, poss with Monahan splint
c     needs to have dff internally
c     variables need to be made allocatable
c     to read map data from .maploc file produced by separate readingQTL program.

c     5/1/2015 variable made allocatable except for mxstep
c     non-IMSL smoothing routine added

c	INCLUDE 'link_fnl_shared.h'
c     USE IMSL_LIBRARIES


c	parameter(maxind=400,mxloci=500,mxtrt=400,mxstep=150)

c     19/10/2016 interpolation of QTL probs with spline smoother replaces by linear interpolation
      parameter (mxstep=250)
      
	implicit double precision (a-h,o-z)
			
	integer,allocatable :: p1(:,:),p2(:,:),matpheno(:,:,:),matmatch(:,:)
      integer,allocatable ::  partype(:,:)
	integer,allocatable ::  sortSNP(:,:),id(:)

 	real(8), allocatable :: permmatch(:,:)
	real(8), allocatable :: allct(:),halfct(:),dist(:)
	real(8), allocatable :: mdiff(:),rec(:)
	real(8), allocatable :: trait(:,:)
	real(8), allocatable :: postprob(:,:,:),segrat(:,:)
	character, allocatable :: locname(:)*20,mapname(:)*20,mphase(:)*4
	character, allocatable :: fphase(:)*4,traitlab(:)*20

      real(8) prlin(36,mxstep)

c	integer markSNP(maxind,mxloci),SNPp1(500),SNPp2(500)

	integer g1(2,4),g2(2,4),gelp(36,36)
      integer zpheno(6,6),wkpheno(6,6)
	integer matchchr(36,4),wkgel,imatch(36)
 	integer matchtot(36), form(36,4), iperm(36),dff(36,36),sform(36,4)
	integer sdff(36,36),maxrun(36)
	real(8) rform(36,4),rdff(36,36),rmatchtot(36)
	character cftype(4)*2,cmtype(4)*2
      character fgam(6,2)*2,mgam(6,2)*2,zgeno(6,6,4)*2
	character stem*12,infile1*19,infile2*16,outfile1*34,infile3*19
      character outfile2*19,outfile3*31,stem2*12

	common /diffblock/ dff

      form(1, :) = (/1,2,5,6/)
	form(2, :) = (/1,2,5,7/)
	form(3, :) = (/1,2,5,8/)
	form(4, :) = (/1,2,6,7/)
	form(5, :) = (/1,2,6,8/)
	form(6, :) = (/1,2,7,8/)
	form(7, :) = (/1,3,5,6/)
	form(8, :) = (/1,3,5,7/)
	form(9, :) = (/1,3,5,8/)
	form(10, :) = (/1,3,6,7/)
	form(11, :) = (/1,3,6,8/)
	form(12, :) = (/1,3,7,8/)
	form(13, :) = (/1,4,5,6/)
	form(14, :) = (/1,4,5,7/)
	form(15, :) = (/1,4,5,8/)
	form(16, :) = (/1,4,6,7/)
	form(17, :) = (/1,4,6,8/)
	form(18, :) = (/1,4,7,8/)
	form(19, :) = (/2,3,5,6/)
	form(20, :) = (/2,3,5,7/)
	form(21, :) = (/2,3,5,8/)
	form(22, :) = (/2,3,6,7/)
	form(23, :) = (/2,3,6,8/)
	form(24, :) = (/2,3,7,8/)
	form(25, :) = (/2,4,5,6/)
	form(26, :) = (/2,4,5,7/)
	form(27, :) = (/2,4,5,8/)
	form(28, :) = (/2,4,6,7/)
	form(29, :) = (/2,4,6,8/)
	form(30, :) = (/2,4,7,8/)
	form(31, :) = (/3,4,5,6/)
	form(32, :) = (/3,4,5,7/)
	form(33, :) = (/3,4,5,8/)
	form(34, :) = (/3,4,6,7/)
	form(35, :) = (/3,4,6,8/)
	form(36, :) = (/3,4,7,8/)
      
      dff(1,:)=(/0,1,1,1,1,2,1,2,2,2,2,3,1,2,2,2,2,3,1,2,2,2,2,3,1,2,2,
     & 2,2,3,2,3,3,3,3,4/)
      dff(2,:)=(/1,0,1,1,2,1,2,1,2,2,3,2,2,1,2,2,3,2,2,1,2,2,3,2,2,1,2,
     & 2,3,2,3,2,3,3,4,3/)
      dff(3,:)=(/1,1,0,2,1,1,2,2,1,3,2,2,2,2,1,3,2,2,2,2,1,3,2,2,2,2,1,
     &  3,2,2,3,3,2,4,3,3/)
      dff(4,:)=(/1,1,2,0,1,1,2,2,3,1,2,2,2,2,3,1,2,2,2,2,3,1,2,2,2,2,3,
     &  1,2,2,3,3,4,2,3,3/)
      dff(5,:)=(/1,2,1,1,0,1,2,3,2,2,1,2,2,3,2,2,1,2,2,3,2,2,1,2,2,3,2,
     &  2,1,2,3,4,3,3,2,3/)
      dff(6,:)=(/2,1,1,1,1,0,3,2,2,2,2,1,3,2,2,2,2,1,3,2,2,2,2,1,3,2,2,
     &  2,2,1,4,3,3,3,3,2/)
      dff(7,:)=(/1,2,2,2,2,3,0,1,1,1,1,2,1,2,2,2,2,3,1,2,2,2,2,3,2,3,3,
     &  3,3,4,1,2,2,2,2,3/)
      dff(8,:)=(/2,1,2,2,3,2,1,0,1,1,2,1,2,1,2,2,3,2,2,1,2,2,3,2,3,2,3,
     &  3,4,3,2,1,2,2,3,2/)
      dff(9,:)=(/2,2,1,3,2,2,1,1,0,2,1,1,2,2,1,3,2,2,2,2,1,3,2,2,3,3,2,
     &  4,3,3,2,2,1,3,2,2/)
      dff(10,:)=(/2,2,3,1,2,2,1,1,2,0,1,1,2,2,3,1,2,2,2,2,3,1,2,2,3,3,4,
     & 2,3,3,2,2,3,1,2,2/)
      dff(11,:)=(/2,3,2,2,1,2,1,2,1,1,0,1,2,3,2,2,1,2,2,3,2,2,1,2,3,4,3,
     & 3,2,3,2,3,2,2,1,2/)
      dff(12,:)=(/3,2,2,2,2,1,2,1,1,1,1,0,3,2,2,2,2,1,3,2,2,2,2,1,4,3,3,
     & 3,3,2,3,2,2,2,2,1/)
      dff(13,:)=(/1,2,2,2,2,3,1,2,2,2,2,3,0,1,1,1,1,2,2,3,3,3,3,4,1,2,2,
     & 2,2,3,1,2,2,2,2,3/)
      dff(14,:)=(/2,1,2,2,3,2,2,1,2,2,3,2,1,0,1,1,2,1,3,2,3,3,4,3,2,1,2,
     & 2,3,2,2,1,2,2,3,2/)
      dff(15,:)=(/2,2,1,3,2,2,2,2,1,3,2,2,1,1,0,2,1,1,3,3,2,4,3,3,2,2,1,
     & 3,2,2,2,2,1,3,2,2/)
      dff(16,:)=(/2,2,3,1,2,2,2,2,3,1,2,2,1,1,2,0,1,1,3,3,4,2,3,3,2,2,3,
     & 1,2,2,2,2,3,1,2,2/)
      dff(17,:)=(/2,3,2,2,1,2,2,3,2,2,1,2,1,2,1,1,0,1,3,4,3,3,2,3,2,3,2,
     & 2,1,2,2,3,2,2,1,2/)
      dff(18,:)=(/3,2,2,2,2,1,3,2,2,2,2,1,2,1,1,1,1,0,4,3,3,3,3,2,3,2,2,
     & 2,2,1,3,2,2,2,2,1/)
      dff(19,:)=(/1,2,2,2,2,3,1,2,2,2,2,3,2,3,3,3,3,4,0,1,1,1,1,2,1,2,2,
     & 2,2,3,1,2,2,2,2,3/)
      dff(20,:)=(/2,1,2,2,3,2,2,1,2,2,3,2,3,2,3,3,4,3,1,0,1,1,2,1,2,1,2,
     & 2,3,2,2,1,2,2,3,2/)
      dff(21,:)=(/2,2,1,3,2,2,2,2,1,3,2,2,3,3,2,4,3,3,1,1,0,2,1,1,2,2,1,
     & 3,2,2,2,2,1,3,2,2/)
      dff(22,:)=(/2,2,3,1,2,2,2,2,3,1,2,2,3,3,4,2,3,3,1,1,2,0,1,1,2,2,3,
     & 1,2,2,2,2,3,1,2,2/)
      dff(23,:)=(/2,3,2,2,1,2,2,3,2,2,1,2,3,4,3,3,2,3,1,2,1,1,0,1,2,3,2,
     & 2,1,2,2,3,2,2,1,2/)
      dff(24,:)=(/3,2,2,2,2,1,3,2,2,2,2,1,4,3,3,3,3,2,2,1,1,1,1,0,3,2,2,
     & 2,2,1,3,2,2,2,2,1/)
      dff(25,:)=(/1,2,2,2,2,3,2,3,3,3,3,4,1,2,2,2,2,3,1,2,2,2,2,3,0,1,1,
     & 1,1,2,1,2,2,2,2,3/)
      dff(26,:)=(/2,1,2,2,3,2,3,2,3,3,4,3,2,1,2,2,3,2,2,1,2,2,3,2,1,0,1,
     & 1,2,1,2,1,2,2,3,2/)
      dff(27,:)=(/2,2,1,3,2,2,3,3,2,4,3,3,2,2,1,3,2,2,2,2,1,3,2,2,1,1,0,
     & 2,1,1,2,2,1,3,2,2/)
      dff(28,:)=(/2,2,3,1,2,2,3,3,4,2,3,3,2,2,3,1,2,2,2,2,3,1,2,2,1,1,2,
     & 0,1,1,2,2,3,1,2,2/)
      dff(29,:)=(/2,3,2,2,1,2,3,4,3,3,2,3,2,3,2,2,1,2,2,3,2,2,1,2,1,2,1,
     & 1,0,1,2,3,2,2,1,2/)
      dff(30,:)=(/3,2,2,2,2,1,4,3,3,3,3,2,3,2,2,2,2,1,3,2,2,2,2,1,2,1,1,
     & 1,1,0,3,2,2,2,2,1/)
      dff(31,:)=(/2,3,3,3,3,4,1,2,2,2,2,3,1,2,2,2,2,3,1,2,2,2,2,3,1,2,2,
     & 2,2,3,0,1,1,1,1,2/)
      dff(32,:)=(/3,2,3,3,4,3,2,1,2,2,3,2,2,1,2,2,3,2,2,1,2,2,3,2,2,1,2,
     & 2,3,2,1,0,1,1,2,1/)
      dff(33,:)=(/3,3,2,4,3,3,2,2,1,3,2,2,2,2,1,3,2,2,2,2,1,3,2,2,2,2,1,
     & 3,2,2,1,1,0,2,1,1/)
      dff(34,:)=(/3,3,4,2,3,3,2,2,3,1,2,2,2,2,3,1,2,2,2,2,3,1,2,2,2,2,3,
     & 1,2,2,1,1,2,0,1,1/)
      dff(35,:)=(/3,4,3,3,2,3,2,3,2,2,1,2,2,3,2,2,1,2,2,3,2,2,1,2,2,3,2,
     & 2,1,2,1,2,1,1,0,1/)
      dff(36,:)=(/4,3,3,3,3,2,3,2,2,2,2,1,3,2,2,2,2,1,3,2,2,2,2,1,3,2,2,
     & 2,2,1,2,1,1,1,1,0/)
                                                                          
	                                                                    
	write (*,*) "Input file name (.maploc),max 12 characters"              
	read (*,"(a)") stem                                                 
                                                                          
	write (*,*) "Input trait file name (.qua),max 12 characters"        
	read (*,"(a)") stem2                                                
	                                                                    
	infile1 = trim(stem) // ".maploc"
c	infile2 = trim(stem) // ".map"
	infile3 = trim(stem2) // ".qua"
	outfile1 = trim(stem)//'_'//trim(stem2) // "QTLv5.out"
	outfile2 = trim(stem) // ".rc5"
	outfile3 = trim(stem)//'_'//trim(stem2) // ".qmm"

	open(3,file=infile1,status='old')
c	open(4,file=infile2,status='old')
	open(5,file=infile3,status='old')
	open(7,file=outfile1,status='unknown')
	open(8,file=outfile2,status='unknown')
	open(9,file=outfile3,status='unknown')

	read(3,*) n,nmap
      allocate (p1(nmap,4),p2(nmap,4),matpheno(nmap,6,6),
     & matmatch(36,nmap),partype(nmap,8),sortSNP(n,nmap),id(n))
      allocate (permmatch(36,nmap),allct(n),halfct(n),dist(nmap),
     & mdiff(nmap),rec(nmap))
      allocate (locname(nmap),mapname(nmap),mphase(nmap),fphase(nmap))
	allocate (postprob(n,0:mxstep,36),segrat(0:mxstep,36))

	do i=1,n
	  do j=1,nmap
	    sortSNP(i,j) = 0
	  end do
      end do

 	do j=1,nmap
	  read(3,'(a20,10x,f6.2,2x,4i1,2x,4i1,<n>(1x,i1))') mapname(j),dist(j),
     &     (p1(j,k),k=1,4),(p2(j,k),k=1,4),(sortSNP(k,j),k=1,n)
	end do

	do j=1,nmap
	  write(7,'(a20,7x,f6.2,2x,4i1,2x,4i1,<n>(1x,i1))') mapname(j),dist(j),
     &     (p1(j,k),k=1,4),(p2(j,k),k=1,4),(sortSNP(k,j),k=1,n)
	enddo

      
c	do i=1,nloci
c	  read(3,*) locname(i),SNPp1(i),SNPp2(i),(markSNP(k,i),k=1,n)
c	end do
c	do i=1,36
c	  read(3,*) (dff(i,j),j=1,36)
c	end do

	read(5,*) ntrt
      
      allocate (trait(ntrt,n),traitlab(ntrt))

	read(5,*) (traitlab(j),j=1,ntrt)
      write(7,'(<ntrt>(a20,2x))') (traitlab(j),j=1,ntrt)
	do i=1,n
	  read(5,*) id(i),(trait(j,i),j=1,ntrt)
	write(7,'(<ntrt>(f8.3))') (trait(j,i),j=1,ntrt)
	end do

c	write(7,*) (traitlab(j),j=1,2)
c	do i=1,n
c	write(7,'(2f8.3)') (trait(j,i),j=1,2)
c	enddo

c	not needed when using both parents	   
c	if (ipar .eq. 1) then
c	  partype = ftype
c	else
c	  partype = mtype
c	end if

c	p1, p2 carry the phase information
c     this is now all in .maploc file
c	read(4,*) nmap
c	do j=1,nmap
c	  read(4,'(a20,7x,f6.2,2x,4i1,1x,4i1)') mapname(j),dist(j),
c     &     (p1(j,k),k=1,4),(p2(j,k),k=1,4)
c	end do

c	do j=1,nmap
c	  write(7,'(a20, 7x,f6.2,2x,4i1,2x,4i1)') mapname(j),dist(j),
c     &     (p1(j,k),k=1,4),(p2(j,k),k=1,4)
c	enddo


	mdiff(1) = 0.0d0
	rec(1) = 0.0d0
	do i=2,nmap
	  mdiff(i) = dist(i) - dist(i-1)
        if (mdiff(i) .eq. 0.0d0) mdiff(i) = 0.001d0 
	  rec(i) = (1.0d0 - dexp(-2*mdiff(i)/100.0d0))/2.0d0
	end do

c	read(4,*) nmap
c	do j=1,nmap
c	  read(4,'(a,4i1,2x,4i1)') mapname(j),
c    &     (p1(j,k),k=1,4),(p2(j,k),k=1,4)
c	end do

c	do i=2,nmap
c	  read(4,*) mdiff(i)
c	end do

c	mdiff(1) = 0.0d0
c	rec(1) = 0.0d0
c	dist(1) = 0.0d0
c	do i=2,nmap
c	  rec(i) = (1.0d0 - dexp(-2*mdiff(i)))/2.0d0
c	  dist(i) = dist(i-1)+100.0*mdiff(i) 
c	end do

c	do i=1,nmap
c	  write(7,'(2f8.2,f7.4)') dist(i),mdiff(i),rec(i)
c	end do

c	do i=1,n
c	  do j=1,nmap
c	    sortSNP(i,j) = 0
c	  end do
c	end do

c	maploop: do j=1,nmap
c	write(7,*) trim(mapname(j))
c	locloop:   do k=1,nloci
c	write(7,*) trim(locname(k))
c	             if (trim(mapname(j)) .eq. trim(locname(k))) then
c	               write (7,*) "Match!"
c	               do i=1,n
c	                 sortSNP(i,j) = markSNP(i,k)
c		           end do
c	               exit locloop
c	             end if
c	           write (7,*) "No match for locus", mapname(j)
c	           end do locloop
c	         end do maploop

	do k=1,nmap
		  write(7,'(a20,f6.2,2x,4i1,2x,4i1,<n>(1x,i1))') mapname(k),
     &	dist(k),(p1(k,m),m=1,4),(p2(k,m),m=1,4),
     &    (sortSNP(j,k),j=1,n)
	end do

      do i=1,nmap
	  write(8,*) 'Locus',i
	  do j=1,4
	    if (p1(i,j) .eq. 0) cftype(j)=' O'
          if (p1(i,j) .eq. 1) cftype(j)=' A'
	    if (p1(i,j) .eq. 2) cftype(j)=' B'
	    if (p1(i,j) .eq. 3) cftype(j)=' C'
	    if (p1(i,j) .eq. 4) cftype(j)=' D'
	    if (p1(i,j) .eq. 5) cftype(j)=' E'
	    if (p1(i,j) .eq. 6) cftype(j)=' F'
	    if (p1(i,j) .eq. 7) cftype(j)=' G'
	    if (p1(i,j) .eq. 8) cftype(j)=' H'
	  end do
	  do j=1,4
	    if (p2(i,j) .eq. 0) cmtype(j)=' O'
          if (p2(i,j) .eq. 1) cmtype(j)=' A'
	    if (p2(i,j) .eq. 2) cmtype(j)=' B'
	    if (p2(i,j) .eq. 3) cmtype(j)=' C'
	    if (p2(i,j) .eq. 4) cmtype(j)=' D'
	    if (p2(i,j) .eq. 5) cmtype(j)=' E'
	    if (p2(i,j) .eq. 6) cmtype(j)=' F'
	    if (p2(i,j) .eq. 7) cmtype(j)=' G'
	    if (p2(i,j) .eq. 8) cmtype(j)=' H'
	  end do
	  call rgamete(cftype,fgam)
 	  call rgamete(cmtype,mgam)
	  call rzygote(fgam,mgam,zgeno)
	  call SNPpheno(zgeno,zpheno)
	  do j=1,6
	    do k=1,6
	      matpheno(i,j,k)=zpheno(j,k)
	    end do
	  end do
	end do

      do i=1,n    !number of individuals
        write(8,*) 'Individual',i
        write(6,*) 'Individual',i
 	  do j=1,nmap  !number of loci in linkage group
c	    write(8,*) 'Locus',j
c	what's this for, can't see this defined!
c          do k=1,4
c	      true(j,k)=itype(i,j,k)
c	    end do
	    wkgel=sortSNP(i,j)
	    do k=1,6
	      do l=1,6
          	wkpheno(k,l)=matpheno(j,k,l)
	      end do
	    end do
c		write(8,*) 'Call SNP match'
	    call SNPmatch(wkgel,wkpheno,matchchr,imatch)
	    do k=1,36
	      matmatch(k,j)=imatch(k)
	    end do
	  end do
	  do jj=1,36
	    matchtot(jj)=0
	    iperm(jj)=jj
          do j=1,nmap !number of loci in linkage group
	      matchtot(jj)=matchtot(jj)+matmatch(jj,j)
	    end do
	    write(8,'(4i4,6x,<nmap>i2,6x,i5)') (form(jj,k),k=1,4),
     &		(matmatch(jj,k),k=1,nmap),matchtot(jj)
c	    write(7,'(4i4,6x,<nmap>i2,6x,i5)') (form(jj,k),k=1,4),
c    &		(matmatch(jj,k),k=1,nmap),matchtot(jj)
	  end do
	  call hmmreconstruct(nmap,dist,form,rec,matmatch,nstep,prlin)
c	keep in mind prlin has nstep+1 entries, so set lower limit of postprob to 0
	  do jj=1,36
	    do k=0,nstep
	      postprob(i,k,jj)=prlin(jj,k+1)
	    enddo
	  enddo
c	  do jj=1,36
c          rmatchtot=1.0/real(matchtot)
c	  end do
c	  call dsvrgp(36,rmatchtot,rmatchtot,iperm)
c	  ipath=1
c	  call dperma(36,nmap,1.0d0*matmatch,36,iperm,ipath,permmatch,36) 
c	  call dperma(36,4,1.0d0*form,36,iperm,ipath,rform,36) 
c	  call dperma(36,36,1.0d0*dff,36,iperm,ipath,rdff,36)
c	  ipath=2 
c	  call dperma(36,36,rdff,36,iperm,ipath,rdff,36)
c	  do jj=1,36
c	    do k=1,4
c		  sform(jj,k)=int(rform(jj,k))
c		end do
c	    do k=1,nmap
c		  matmatch(jj,k)=int(permmatch(jj,k))
c		end do
c	    do k=1,36
c		  sdff(jj,k)=int(rdff(jj,k))
c		end do
c	  end do
c	  do jj=1,36
c          matchtot=int(1.0/rmatchtot)
c	  end do

c	calculate length of maximum run
c	  maxrun=1
c	  do jj=1,36
c	    irun = 0
c	    do k=1,nmap
c	      if (matmatch(jj,k) .eq. 1) then
c	        irun=irun+1
c	      else
c	        if (irun .gt. maxrun(jj)) maxrun(jj) = irun
c	        irun=0
c	      endif
c	    enddo
c	    if (irun .gt. maxrun(jj)) maxrun(jj) = irun
c	  enddo


c      write(8,*)
c	write(8,'(10x,<nmap>i2)')   (k,k=1,nmap)
c	  do jj=1,36
c	    write(8,'(4i2,6x,<nmap>i2,6x,i5,6x,i5)') (sform(jj,k),k=1,4),
c     &		(matmatch(jj,k),k=1,nmap),matchtot(jj),maxrun(jj)

c	    write(8,'(4f3.0,6x,10f3.0,6x,i5)') (rform(jj,k),k=1,4),
c    &		(permmatch(jj,k),k=1,10),matchtot(jj)
c	  end do
c	 write(7,'(i4,4i2,6x,<nmap>i2,6x,i5,6x,i5)') i,(sform(1,k),k=1,4),
c     &		(matmatch(1,k),k=1,nmap),matchtot(1),maxrun(1)



      end do
c     added 8/10/2012 calculate average weights over all offspring to check for distorted segregation   
      segrat=sum(postprob,dim=1)/(1.0d0*n)
      write (8,*) 'Average weight for each genotype'
      do jj=0,nstep
      write (8,'(36f7.4)') (segrat(jj,kk),kk=1,36)
      enddo

c     12/1/2015 deallocate unneeded arrays to clear space
      deallocate (p1,p2,matpheno,matmatch,partype,sortSNP,permmatch)

	write (6,*) 'Start QTL analysis - call SNPprob'
	call SNPprob(n,nmap,nstep,form,postprob,                
     &	ntrt,trait,traitlab)
c	call qprob(n,nmap,dist,incc,icstore,icbivboth,itotprob,
c     &	ntrt,trait,traitlab)

c	close(8)
c	logi=makedirqq('tempqtl')
c	open(8,file='tempqtl\test.txt',status='unknown')
c      write(8,*) n,nmap
c	do i=1,nmap
c        write(8,*) dist(i)
c	enddo
c	do i=1,n
c	  write(8,*) incc(i)
c	enddo
c	do l=1,4
c	  do k=1,nmap
c	    do i=1,n
c	      do j=1,incc(i)
c	        write(8,'(i2)') icstore(i,j,k,l)
c	      enddo
c	    enddo
c	  enddo
c	enddo
c	do i=1,n
c	  do j=1,incc(i)
c	    write(8,'(a1,a7,a1,3x,f20.18)') '"',icbivboth(i,j),'"',
c    &     itotprob(i,j)
c	  enddo
c	enddo

      end

c	************************************************
	subroutine rgamete(par,gamtype)

c     to work out the possible gamete types obtained from parental genotype
c      par, at a single locus, no double reduction

      character par(4)*2,gamtype(6,2)*2
      
	write(8,'(4(a2))') (par(i),i=1,4)

      gamtype(1,1)=par(1)
	gamtype(1,2)=par(2)
	gamtype(2,1)=par(1)
	gamtype(2,2)=par(3)
	gamtype(3,1)=par(1)
	gamtype(3,2)=par(4)
	gamtype(4,1)=par(2)
	gamtype(4,2)=par(3)
	gamtype(5,1)=par(2)
	gamtype(5,2)=par(4)
	gamtype(6,1)=par(3)
	gamtype(6,2)=par(4)

      do i=1,6
        write(8,'(2(a2))') (gamtype(i,j),j=1,2)
	end do

	return
	end

c	************************************************
	subroutine rzygote(fgamtype,mgamtype,zgeno)

c     work out genotype possibilities for zygotes by combining the gametes

	character fgamtype(6,2)*2,mgamtype(6,2)*2, zgeno(6,6,4)*2

      do i=1,6
	  do j=1,6
	    zgeno(i,j,1)=fgamtype(i,1)
	    zgeno(i,j,2)=fgamtype(i,2)
	    zgeno(i,j,3)=mgamtype(j,1)
	    zgeno(i,j,4)=mgamtype(j,2)
	  end do
	end do

      do i=1,6
	  write(8,'(6(4a2,3x))') ((zgeno(i,j,k),k=1,4), j=1,6)
	end do

      return
	end

c	************************************************
	subroutine SNPpheno(zgeno,zpheno)

c     convert 6x6 matrix of genotypes to phenotypes

      integer zpheno(6,6),ip
	character zgeno(6,6,4)*2, allele*2

	do i=1,6
	  do j=1,6
	    zpheno(i,j)=0
	  end do
	end do
	
	do i=1,6
	  do j=1,6
	    do k=1,4
	      allele=zgeno(i,j,k)
	      call gtop2(allele,ip)
		  if (ip.eq.2) zpheno(i,j)=zpheno(i,j)+1
	    end do
	  end do
 	end do

      do i=1,6
	  write(8,'(6(i1,3x))') (zpheno(i,j), j=1,6)
	end do

      return
	end


c	***************************
	subroutine gtop2(allele,ip)

c	to turn out allele character with length of 2 into gel
	
	character allele*2

	if (allele.eq.' O') ip=0
	if (allele.eq.' A') ip=1
	if (allele.eq.' B') ip=2
	if (allele.eq.' C') ip=3
	if (allele.eq.' D') ip=4
	if (allele.eq.' E') ip=5
	if (allele.eq.' F') ip=6
	if (allele.eq.' G') ip=7
	if (allele.eq.' H') ip=8

	return
	end
c	***************************
	subroutine SNPmatch(gel,poss,matchchr,imatch)

c     for an individual and a locus, identify the chromosome 
c      combinations which give that individual

      integer gel,poss(6,6),matchchr(36,4),wkchr(36,2),pair
      integer imatch(36)
 
      do i=1,36
	  imatch(i)=0
	end do

c	write(8,'(i1)') gel
c	do i=1,6
c	  write(8,'(6(i1,3x))') (poss(i,j),j=1,6)
c	end do


	nmatch=0
	im=0
	do i=1,6
	  do j=1,6
	    im=im+1
	    idif=0
	    if (gel .eq. 9) then !to handle missing values
	      idif = 0
	    else
	      idif=idif+(gel-poss(i,j))**2
	    endif		      
	    if (idif .eq. 0) then
	      nmatch=nmatch+1
	      wkchr(nmatch,1)=i
	      wkchr(nmatch,2)=j
	      imatch(im)=1
	    end if
	  end do
	end do

	do i=1,nmatch
	  pair=wkchr(i,1)
	  select case (pair)
	    case (1)
	      matchchr(i,1)=1
	      matchchr(i,2)=2
	    case (2)
	      matchchr(i,1)=1
	      matchchr(i,2)=3
	    case (3)
	      matchchr(i,1)=1
	      matchchr(i,2)=4
	    case (4)
	      matchchr(i,1)=2
	      matchchr(i,2)=3
	    case (5)
	      matchchr(i,1)=2
	      matchchr(i,2)=4
	    case (6)
	      matchchr(i,1)=3
	      matchchr(i,2)=4
	  end select
        pair=wkchr(i,2)
	  select case (pair)
	    case (1)
	      matchchr(i,3)=5
	      matchchr(i,4)=6
	    case (2)
	      matchchr(i,3)=5
	      matchchr(i,4)=7
	    case (3)
	      matchchr(i,3)=5
	      matchchr(i,4)=8
	    case (4)
	      matchchr(i,3)=6
	      matchchr(i,4)=7
	    case (5)
	      matchchr(i,3)=6
	      matchchr(i,4)=8
	    case (6)
	      matchchr(i,3)=7
	      matchchr(i,4)=8
	  end select
	end do

c	write (8,*) 'Matching chromosomes'
c	do i=1,nmatch
c	  write(8,'(4i2)') (matchchr(i,j),j=1,4)
c	end do
c	do i=1,6
c	  write(8,'(i2)') imatch(i)
c	end do

	return 
	end



c	**************************************************************
	subroutine cpat(fpat,bpat)

c     subroutine to transform chromosome patterns to a binary type


      integer fpat(4),bpat(8)

      do i=1,8
	  bpat(i)=0
	end do

	do i=1,4
	  j=fpat(i)
	  bpat(j)=1
	end do

c	write(8,'(4i2,4x,8i1)') (fpat(i),i=1,4),(bpat(i),i=1,8)

	return
	end



c	*************************************************************
	subroutine hmmreconstruct(nloci,dist,chrom,rec,match,nstep,
     &	prlin)

c	This subroutine replaces the branch and bound reconstruction in previous programmes
c	Reconstructs for one individual

c	USE Numerical_Libraries

c	parameter(mxloci=500,mxstep=150)
      parameter(mxstep=250)

	implicit double precision (a-h,o-z)

	integer chrom(36,4),bivindi(36),match(36,nloci)
	integer dff(36,36),rdff(16,16),rmatch(16,nloci),rchrom(36,4)

	real(8) postprob(16,nloci)              
	real(8) wtpostprob(36,nloci),tmpprob(36,nloci)
	real(8) ddist(nloci),dwtprob(36,nloci),dist(nloci)
	real(8) xdata(nloci),fdata(nloci),xvec(mxstep),value(mxstep)
	real(8) prlin(36,mxstep),prtot(mxstep),rec(nloci)
c	real(8) summn1(nloci),sumwt1(nloci)
c	real(8) sumwt(nloci,maxind)
      real(8) mn(nloci),h(nloci),fn(mxstep)

	common /diffblock/ dff

c	sort out bivalents

	wtsum=0.0d0
	mnpostprob=0.0d0
	wtpostprob=0.0d0

	do ibiv=1,9

	select case (ibiv)
	case(1)
c	bivalent type 1: 1+2,3+4,5+6,7+8
	bivindi=0
	bivtot=0
	do i=1,36
	  if ((chrom(i,1) .eq. 1 .and. chrom(i,2) .eq. 2) .or. 
     & 	(chrom(i,1) .eq. 3 .and. chrom(i,2) .eq. 4) .or.  
     & 	(chrom(i,3) .eq. 5 .and. chrom(i,4) .eq. 6) .or.  
     & 	(chrom(i,3) .eq. 7 .and. chrom(i,4) .eq. 8)) then
          bivindi(i)=1  
	    bivtot=bivtot+bivindi(i)
	  endif
	enddo
c	write(7,*) bivtot

	case(2)
c	bivalent type 2: 1+2,3+4,5+7,6+8
	bivindi=0
	bivtot=0
	do i=1,36
	  if ((chrom(i,1) .eq. 1 .and. chrom(i,2) .eq. 2) .or. 
     & 	(chrom(i,1) .eq. 3 .and. chrom(i,2) .eq. 4) .or.  
     & 	(chrom(i,3) .eq. 5 .and. chrom(i,4) .eq. 7) .or.  
     & 	(chrom(i,3) .eq. 6 .and. chrom(i,4) .eq. 8)) then
          bivindi(i)=1  
	    bivtot=bivtot+bivindi(i)
	  endif
	enddo
c	write(7,*) bivtot

	case(3)
c	bivalent type 3: 1+2,3+4,5+8,6+7
	bivindi=0
	bivtot=0
	do i=1,36
	  if ((chrom(i,1) .eq. 1 .and. chrom(i,2) .eq. 2) .or. 
     & 	(chrom(i,1) .eq. 3 .and. chrom(i,2) .eq. 4) .or.  
     & 	(chrom(i,3) .eq. 5 .and. chrom(i,4) .eq. 8) .or.  
     & 	(chrom(i,3) .eq. 6 .and. chrom(i,4) .eq. 7)) then
          bivindi(i)=1  
	    bivtot=bivtot+bivindi(i)
	  endif
	enddo
c	write(7,*) bivtot

	case(4)
c	bivalent type 4: 1+3,2+4,5+6,7+8
	bivindi=0
	bivtot=0
	do i=1,36
	  if ((chrom(i,1) .eq. 1 .and. chrom(i,2) .eq. 3) .or. 
     & 	(chrom(i,1) .eq. 2 .and. chrom(i,2) .eq. 4) .or.  
     & 	(chrom(i,3) .eq. 5 .and. chrom(i,4) .eq. 6) .or.  
     & 	(chrom(i,3) .eq. 7 .and. chrom(i,4) .eq. 8)) then
          bivindi(i)=1  
	    bivtot=bivtot+bivindi(i)
	  endif
	enddo
c	write(7,*) bivtot

	case(5)
c	bivalent type 5: 1+3,2+4,5+7,6+8
	bivindi=0
	bivtot=0
	do i=1,36
	  if ((chrom(i,1) .eq. 1 .and. chrom(i,2) .eq. 3) .or. 
     & 	(chrom(i,1) .eq. 2 .and. chrom(i,2) .eq. 4) .or.  
     & 	(chrom(i,3) .eq. 5 .and. chrom(i,4) .eq. 7) .or.  
     & 	(chrom(i,3) .eq. 6 .and. chrom(i,4) .eq. 8)) then
          bivindi(i)=1  
	    bivtot=bivtot+bivindi(i)
	  endif
	enddo
c	write(7,*) bivtot

	case(6)
c	bivalent type 6: 1+3,2+4,5+8,6+7
	bivindi=0
	bivtot=0
	do i=1,36
	  if ((chrom(i,1) .eq. 1 .and. chrom(i,2) .eq. 3) .or. 
     & 	(chrom(i,1) .eq. 2 .and. chrom(i,2) .eq. 4) .or.  
     & 	(chrom(i,3) .eq. 5 .and. chrom(i,4) .eq. 8) .or.  
     & 	(chrom(i,3) .eq. 6 .and. chrom(i,4) .eq. 7)) then
          bivindi(i)=1  
	    bivtot=bivtot+bivindi(i)
	  endif
	enddo
c	write(7,*) bivtot

	case(7)
c	bivalent type 7: 1+4,2+3,5+6,7+8
	bivindi=0
	bivtot=0
	do i=1,36
	  if ((chrom(i,1) .eq. 1 .and. chrom(i,2) .eq. 4) .or. 
     & 	(chrom(i,1) .eq. 2 .and. chrom(i,2) .eq. 3) .or.  
     & 	(chrom(i,3) .eq. 5 .and. chrom(i,4) .eq. 6) .or.  
     & 	(chrom(i,3) .eq. 7 .and. chrom(i,4) .eq. 8)) then
          bivindi(i)=1  
	    bivtot=bivtot+bivindi(i)
	  endif
	enddo
c	write(7,*) bivtot

	case(8)
c	bivalent type 8: 1+4,2+3,5+7,6+8
	bivindi=0
	bivtot=0
	do i=1,36
	  if ((chrom(i,1) .eq. 1 .and. chrom(i,2) .eq. 4) .or. 
     & 	(chrom(i,1) .eq. 2 .and. chrom(i,2) .eq. 3) .or.  
     & 	(chrom(i,3) .eq. 5 .and. chrom(i,4) .eq. 7) .or.  
     & 	(chrom(i,3) .eq. 6 .and. chrom(i,4) .eq. 8)) then
          bivindi(i)=1  
	    bivtot=bivtot+bivindi(i)
	  endif
	enddo
c	write(7,*) bivtot

	case(9)
c	bivalent type 9: 1+4,2+3,5+8,6+7
	bivindi=0
	bivtot=0
	do i=1,36
	  if ((chrom(i,1) .eq. 1 .and. chrom(i,2) .eq. 4) .or. 
     & 	(chrom(i,1) .eq. 2 .and. chrom(i,2) .eq. 3) .or.  
     & 	(chrom(i,3) .eq. 5 .and. chrom(i,4) .eq. 8) .or.  
     & 	(chrom(i,3) .eq. 6 .and. chrom(i,4) .eq. 7)) then
          bivindi(i)=1  
	    bivtot=bivtot+bivindi(i)
	  endif
	enddo
c	write(7,*) bivtot

	end select

c	select right elements of match and dff
	j=0
	do i=1,36	
	  if (bivindi(i) .eq. 0) then
	    j=j+1
		rmatch(j,1:nloci)=match(i,1:nloci)
		rchrom(j,1:4)=chrom(i,1:4)
	  endif
	enddo

	j=0
	do i=1,36
	  k=0
	  if (bivindi(i) .eq. 0) then
	    j=j+1
	    do m=1,36
	      if (bivindi(m) .eq. 0) then
	        k=k+1
	        rdff(j,k)=dff(i,m)
	      endif
	    enddo
	  endif
	enddo
c	do i=1,16
c	  write(7,'(4i2,2x,16i2)') (rchrom(i,j),j=1,4),(rdff(i,j),j=1,16)
c	enddo
c	do i=1,16
c	  write(7,'(4i2,2x,<nloci>(i2))') (rchrom(i,j),j=1,4),
c     &	  (rmatch(i,j),j=1,nloci)
c	enddo

	call hmmtd(nloci,rdff,rec,rmatch,termprob,postprob)
c	write(7,'(a,i3,f20.17)') 'Bivalent type',ibiv,termprob

c	need to unscramble probs back to the right places!
	tmpprob=0.0d0
	kk=0
	do i=1,36
	  if (bivindi(i) .eq. 0) then
	    kk=kk+1
	    tmpprob(i,1:nloci)=postprob(kk,1:nloci)
	  endif
	enddo
c	write(7,*)
c	write(7,*) 'temp matrix'
c	write(7,'(a6,36i10)')  'State',(i,i=1,36)
c	do k=1,4
c	  write(7,'(6x,36i10)') (chrom(i,k),i=1,36)
c	enddo

c	do j=1,nloci
c	  write(7,'(i4,2x,36f10.6)') j,(tmpprob(i,j),i=1,36)
c	enddo
	  	
c	mnpostprob=mnpostprob+tmpprob
	wtpostprob=wtpostprob+termprob*tmpprob
	wtsum=wtsum+termprob
	enddo

c	mnpostprob=mnpostprob/9.0d0
	wtpostprob=wtpostprob/wtsum

c	write(7,*)
c	write(7,*) 'Unweighted mean'
c	write(7,'(a6,36i10)')  'State',(i,i=1,36)
c	do k=1,4
c	  write(7,'(6x,36i10)') (chrom(i,k),i=1,36)
c	enddo

c	do j=1,nloci
c	  write(7,'(i4,2x,36f10.6)') j,(mnpostprob(i,j),i=1,36)
c	enddo
c	summn1=sum(mnpostprob*truemat,dim=1)

c	write(7,*)
c	write(7,*) 'Weighted mean'
c	write(7,'(a6,36i10)')  'State',(i,i=1,36)
c	do k=1,4
c	  write(7,'(6x,36i10)') (chrom(i,k),i=1,36)
c	enddo
c	do j=1,nloci
c	  write(7,'(i4,2x,36f10.6)') j,(wtpostprob(i,j),i=1,36)
c	enddo
c	sumwt1=sum(wtpostprob*truemat,dim=1)
c	do j=1,nloci
c	  write(7,'(i4,2x,2f10.6)') j,summn1(j),sumwt1(j)
c	enddo

c	summn(1:nloci,mm)=summn1(1:nloci)
c	sumwt(1:nloci,mm)=sumwt1(1:nloci)

c	enddo      !don't think we need this enddo??

c	write(7,*)
c	do j=1,n
c	  write(7,'(i4,2x,<nloci>(f6.3))') j,(summn(i,j),i=1,nloci)
c	enddo
c	ssmn=sum(summn)/(1.0d0*n*nloci)
c	write(7,'(f6.3)') ssmn

c	write(7,*)
c	do j=1,n
c	  write(7,'(i4,2x,<nloci>(f6.3))') j,(sumwt(i,j),i=1,nloci)
c	enddo
c	sswt=sum(sumwt)/(1.0d0*n*nloci)
c	write(7,'(f6.3)') sswt

	ndata=1
	imatch=0
	ddist(1)=dist(1)
	dwtprob(1:36,1)=wtpostprob(1:36,1)

	do j=2,nloci
	  if (dist(j) .gt. dist(j-1)) then
	    if (imatch .gt. 0) then
	      imatch=imatch+1
	      dwtprob(1:36,ndata)=dwtprob(1:36,ndata)/(1.0d0*imatch)
	      imatch=0
	    endif
	    ndata=ndata+1
	    ddist(ndata)=dist(j)
	    dwtprob(1:36,ndata)=wtpostprob(1:36,j)
	  else
	    imatch=imatch+1
	    dwtprob(1:36,ndata)=dwtprob(1:36,ndata)+wtpostprob(1:36,j)
	  endif
      enddo

c      write (7,*) 'Unsmoothed prob'
c      do j=1,ndata
c	  write(7,'(i5,2x,f8.2,2x,36f10.6)') j,ddist(j),
c     &	  (dwtprob(i,j),i=1,36)
c	enddo
	nstep=dint(ddist(ndata))
	do ns=0,nstep
	  xvec(ns+1)=ns
      enddo
      
c     call to new smoothing routines
      
      write(8,*)
c      write(8,*) 'New individual'
      do k=1,36
	  fdata(1:ndata)=dwtprob(k,1:ndata)
c	  call dcsiez(ndata,ddist,fdata,nstep+1,xvec,value)
c        call splstn(ddist,fdata,mn,h,ndata)
        do jj=1,nstep+1
c            value(jj)=splev(xvec(jj),ddist,fdata,mn,h,ndata)
            rj=xvec(jj)
            if (rj .gt. 0.00 .and. rj .le. ddist(ndata)) then
            do jf=1,ndata
                if (rj .gt. ddist(jf) .and. rj .le. ddist(jf+1)) then
                exit
                endif
            enddo
            pjj=(rj-ddist(jf))/(ddist(jf+1)-ddist(jf))
            prlin(k,jj)=(1-pjj)*fdata(jf)+pjj*fdata(jf+1)
c            write(8,'(i5,2x,f7.2,2x,i5,2x,f7.2,2x,f7.4,2x,f10.6)') jj,
c     &      rj,jf,ddist(jf),pjj,prlin(k,jj)
            elseif (rj .eq. 0.0) then 
                prlin(k,jj)=fdata(1)
            else
                prlin(k,jj)=fdata(ndata)
            endif
        enddo
c        do jj=1,nstep+1
c          write(8,'(i5,2x,f8.2,2x,f10.6)') jj,xvec(jj),
c     &      prlin(k,jj) 
c        enddo
c	  do ll=1,nstep+1
c	    if (value(ll) .lt. 0.0) then
c		  prspline(k,ll) = 0.0d0
c	    elseif (value(ll) .gt. 1.0) then
c	      prspline(k,ll) = 1.0d0
c	    else
c	      prspline(k,ll)=value(ll)
c	    endif
c	  enddo
	enddo
c	prtot=sum(prspline,dim=1)
	prtot=sum(prlin,dim=1)
c      write(7,*) 'Raw spline'
c      do j=1,nstep+1
c	  write(7,'(i5,2x,f8.2,2x,37f10.6)') j,xvec(j),
c     &	  (prlin(i,j),i=1,36),prtot(j)
c	enddo

	do j=1,nstep+1
	  do k=1,36
	    prlin(k,j)=prlin(k,j)/prtot(j)
	  enddo	
	enddo

	do j=1,nstep+1
	  write(8,'(i5,2x,f8.2,2x,36f10.6)') j,xvec(j),
     &	  (prlin(i,j),i=1,36)
	enddo


	end	 


c	*************************************************************************
	subroutine hmmtd(nloci,dff,rec,match,termvprob,postprob)

c	subroutine to fit hmm for a particular bivalent

c	USE Numerical_Libraries

c	parameter(maxind=400,mxloci=500)

	implicit double precision (a-h,o-z)
			
	integer dff(16,16),match(16,nloci),ptr(16,nloci),optpath(nloci)
	real(8) tdprob(16,16,nloci),eprob(16,nloci),fprob(0:16,0:nloci)
	real(8) sprod(16),s(nloci),bprob(16,nloci)
	real(8) postprob(16,nloci),vsum(16),tvsum(16)
	real(8) logtdprob(16,16,nloci),logeprob(16,nloci)
	real(8) vprob(0:16,0:nloci),rec(nloci)


c	calc matrix of transition probabilities
c	 20/3/2012. Found mistake in calc of transmission probabilities, comment out
c	code from hmm5.for
c	tprob=0.01**dff
c	logtprob=dlog(tprob)
c	totprob=0.0d0
c	do i=2,16
c	  totprob=totprob+tprob(1,i)
c	end do
c	do i=1,16
c	  tprob(i,i)=1.0d0-totprob
c	end do
	do kk=1,nloci
	  tdprob(1:16,1:16,kk)=(rec(kk)**dff(1:16,1:16))*
     &	  ((1.0d0-rec(kk))**(4-dff(1:16,1:16)))
	enddo
	do kk=2,nloci
	  logtdprob(1:16,1:16,kk)=dlog(tdprob(1:16,1:16,kk))
	end do  



c	do i=1,16
c	  write(7,'(16f12.9)') (tprob(i,j),j=1,16)
c	end do

c	calc matrix of emission probs = p(output = match)
	err=0.005d0  !guess at error rate
	eprob=match*(1.0d0-err)+(1-match)*err
	logeprob=dlog(eprob)
c	do i=1,16
c	  write(7,'(<nloci>(f5.2))') (eprob(i,j),j=1,nloci)
c	end do

c	forward algorithm

c	initialise ftilda
	fprob(0,0) = 1.0d0
	do i=1,16
	  fprob(i,0) = 0.0d0
	enddo

c	initialise s
	s = 0.0d0
	do i=1,16
	  s(1) = s(1) + eprob(i,1)
	enddo
	s(1) = s(1)/16.0d0
	do i=1,16
	  fprob(i,1) = eprob(i,1)/(16.0d0*s(1))
	enddo

c	write(7,*) 'Initial probabilities'
c	write(7,'(a6,16i10)')  'State',(i,i=1,16)
c      write(7,'(a4,2x,16f10.6)') '1',(fprob(i,1),i=1,16)
	

c	iterate
c	write(7,*) 
c	write(7,*) 'Iterative probabilities'
	do j=2,nloci
	  sprod = 0.0d0
	  fsum=0.0d0
	  do l=1,16
	    do k=1,16
	      sprod(l)=sprod(l)+fprob(k,j-1)*tdprob(k,l,j)
	    enddo
	    s(j)=s(j)+eprob(l,j)*sprod(l)
	  enddo
	  do l=1,16
	    fprob(l,j)=eprob(l,j)*sprod(l)/s(j)
	    fsum=fsum+fprob(l,j)
	  enddo
c	  write(7,'(i4,2x,17f10.6)') j,(fprob(i,j),i=1,16),fsum
	enddo

c	termination
	termprob=0.0d0
	do l=1,16
	  termprob=termprob+fprob(l,nloci)/16.0d0
	enddo
c	write(7,*) 
c	write(7,'(a,f10.6)') 'Final probability', termprob

c	backwards algorithm

c	write(7,*) 's values'
c	do j=1,nloci
c	  write(7,'(i5,2x,f20.17)') j,s(j)	
c	enddo


c	initialise btilda
	do i=1,16
	  bprob(i,nloci) = 1.0d0/16.0d0
	enddo

c	iterate
c	write(7,*) 
c	write(7,*) 'Iterative backwards probabilities'
c	write(7,'(a6,16i10)')  'State',(i,i=1,16)
	do j=nloci-1,1,-1
	  bsum=0
	  do k=1,16
	    bprob(k,j)=0.0d0
	    do l=1,16
c	    write(7,'(3i4,4f20.17)') j,k,l,bprob(l,j+1),tprob(k,l),
c     &		eprob(l,j+1),s(j)
	     bprob(k,j)=bprob(k,j)+
     &		 tdprob(k,l,j+1)*bprob(l,j+1)*eprob(l,j+1)
	    enddo
	    bprob(k,j)=bprob(k,j)/s(j+1)
	    bsum=bsum+bprob(k,j)
	  enddo
c	  write(7,'(i4,2x,17f10.6)') j,(bprob(i,j),i=1,16),bsum
	enddo

c	posterior probabilities
	
	do j=1,nloci
	  psum=0.0d0
	  do k=1,16
	    postprob(k,j)=fprob(k,j)*bprob(k,j)/termprob
	    psum=psum+postprob(k,j)
	  enddo
	enddo
c	write(7,*) 
c	write(7,*) 'Posterior probabilities'
c	write(7,'(a6,16i10)')  'State',(i,i=1,16)
c	do j=1,nloci
c	  write(7,'(i4,2x,17f10.6)') j,(postprob(i,j),i=1,16),psum
c	enddo

c	Viterbi algorithm

c	initialise vtilda
	do i=1,16
	  vprob(i,1) = logeprob(i,1)+dlog(1.0d0/16.0d0)
	enddo

c	iterate
c	write(7,*) 
c	write(7,*) 'Iterative probabilities'
	do j=2,nloci
	  do l=1,16
	    vsum(1:16)=vprob(1:16,j-1)+logtdprob(1:16,l,j)
	    vprob(l,j)=logeprob(l,j)+maxval(vsum)
		maxcycle: do k=1,16
	      if (vsum(k) .eq. maxval(vsum)) then
	        ptr(l,j)=k
	        exit maxcycle
	      endif
	    enddo maxcycle
	  enddo
c	  write(7,'(i4,2x,16f10.3,2x,16i3)') j,(vprob(l,j),l=1,16),
c     &	  (ptr(l,j),l=1,16)
	enddo
	vsum(1:16)=vprob(1:16,nloci)
c	write(7,*) 'maxterm',maxval(vsum)

c	termination
	tvsum(1:16)=vprob(1:16,nloci)
	termvprob=(maxval(tvsum))/16.0d0
c	write(7,'(a,f10.3)') 'Final log probability', termvprob
	termvprob=exp(termvprob)
c	write(7,'(a,f10.6)') 'Final probability', termvprob
		optcycle: do k=1,16
	      if (tvsum(k) .eq. maxval(tvsum)) then
	        optpath(nloci)=k
	        exit optcycle
	      endif
	    enddo optcycle
c	write(7,*) 
c	write(7,'(a)') 'Optimal path'
c	  write(7,'(2i6)') nloci,optpath(nloci)
	do j=nloci-1,1,-1
	  optpath(j)=ptr(optpath(j+1),j+1)
c	  write(7,'(3i6)') j,optpath(j),match(optpath(j),j)
	enddo

	return
      end
