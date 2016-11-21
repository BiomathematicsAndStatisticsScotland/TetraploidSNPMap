c     Modified version of tetrecon.for, to reconstruct offspring using 
c	a map from a single parent, and to handle missing marker data

c	Trait has missing value code -99

c	This is modified from C:\DevStudioprogs\ZWLeft\QTL2004\realoneparrecon.for, starting on 8/9/2011

c	 20/3/2012 Modified programme to use real recombination frequencies and correct binomial probabilities for transitions
c     added 8/10/2012 calculate average weights over all offspring to check for distorted segregation   

c     11/7/14 only change from SNP_QTLhmmv2.for is to handle longer trait names (20 characters)
c     also change output file name to v3

c     need to replace ISML routine  dcsiez, calculates smoothing spline, poss with Monahan splint

c     10/10/2014 Cut down the first QTL program to read the data and sort into map order, write to .maploc file
c      also need to make allocatable
c     NB Have set lengths of texts to 30 for locname, mapname and 30 for trait labels, check printing
c     The formatting of the .map input files will need to change if the locname changes from this.


c     will need to initialise dff somewhere else - now in QTL mapping program
c     8/1/15 changed to print n, nmap at start of .maploc file
c     9/1/15 changed to read in .SNPloc files that include the separate code column
c	INCLUDE 'link_fnl_shared.h'
c      USE IMSL_LIBRARIES


c	parameter(maxind=400,mxloci=500,mxtrt=400)

	implicit double precision (a-h,o-z)
      
      integer, allocatable, save:: markSNP(:,:),SNPp1(:),SNPp2(:),id(:)
      integer, allocatable, save:: p1(:,:),p2(:,:),sortSNP(:,:)
      integer, allocatable, save:: sortSNPp1(:),sortSNPp2(:)
      real(8), allocatable, save:: trait(:,:),dist(:),mdiff(:),rec(:)
      
      character*30, allocatable, save :: locname(:),mapname(:)
      character*30, allocatable, save :: traitlab(:)
c      character*20, allocatable :: code(:)
      
      character stem*12,stem2*12,infile1*19,infile2*16,infile3*19
	character outfile1*36,outfile2*22
      integer p1count,p2count
     
			
c	integer g1(2,4),g2(2,4)
c	integer gelp(36,36)

c      integer zpheno(6,6),matpheno(mxloci,6,6),wkpheno(6,6)
c	integer matchchr(36,4),wkgel,imatch(36),matmatch(36,mxloci)
c	integer matchtot(36), form(36,4), iperm(36),sform(36,4)
c	real(8) permmatch(36,mxloci),rform(36,4),rmatchtot(36)
c	real(8) allct(maxind),halfct(maxind)
c	real(8) postprob(maxind,0:mxstep,36),segrat(0:mxstep,36)
c	real(8) prspline(36,mxstep)
c	character cftype(4)*2,cmtype(4)*2,fphase(mxloci)*4
c      character fgam(6,2)*2,mgam(6,2)*2,zgeno(6,6,4)*2

c	integer partype(mxloci,8),maxrun(36)

c	character mphase(mxloci)*4
     
c     integer   dff(36,36),sdff(36,36),rdff(36,36)    
c	common /diffblock/ dff

c     error types
c     Q0001   No map file found
c     Q0002   No marker data for mapped locus
c     Q0003   Map phase does not match parental genotype
c     Q0004   Map phase is incomplete
c     Q0005   Incorrect character found in phase data
c     Q0006   Too few offspring in .qua file
c     Q0007   Too many offspring in .qua file

      
	
	write (*,*) "Input file name (.loc),max 12 characters"
	read (*,"(a)") stem

	write (*,*) "Input trait file name (.qua),max 12 characters"
	read (*,"(a)") stem2
	
	infile1 = trim(stem) // ".SNPloc"
	infile2 = trim(stem) // ".map"
	infile3 = trim(stem2) // ".qua"
	outfile1 = trim(stem)//'_'//trim(stem2) // "readQTL.out"
	outfile2 = trim(stem) // ".maploc"

	open(3,file=infile1,status='old')
	open(4,file=infile2,status='old')
	open(5,file=infile3,status='old')
	open(7,file=outfile1,status='unknown')
	open(8,file=outfile2,status='unknown')

	read(3,*) n,nloci
      
      allocate (markSNP(n,nloci),locname(nloci),SNPp1(nloci),
     & SNPp2(nloci))
c      ,code(nloci))
      
	do i=1,n
	  do j=1,nloci
	    markSNP(i,j) = 0
	  end do
	end do


	do i=1,nloci
	  read(3,*) locname(i),SNPp1(i),SNPp2(i),(markSNP(k,i),k=1,n)
c	  read(3,*) locname(i),code(i),SNPp1(i),SNPp2(i),(markSNP(k,i),k=1,n)
	end do
c	do i=1,36
c	  read(3,*) (dff(i,j),j=1,36)
c	end do

	read(5,*) ntrt
      
      allocate (traitlab(ntrt),id(n),trait(ntrt,n))
      
	read(5,*) (traitlab(j),j=1,ntrt)
      write(7,'(<ntrt>(a30,2x))') (traitlab(j),j=1,ntrt)
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
	read(4,*) nmap
      
      allocate (mapname(nmap),dist(nmap),p1(nmap,4),p2(nmap,4),
     & mdiff(nmap),rec(nmap),sortSNP(n,nmap),sortSNPp1(nmap),
     & sortSNPp2(nmap))
      
	do j=1,nmap
	  read(4,'(a20,7x,f6.2,2x,4i1,1x,4i1)') mapname(j),dist(j),
     &     (p1(j,k),k=1,4),(p2(j,k),k=1,4)
        write(*, *) dist(j), p1(j, 1)
	end do

	do j=1,nmap
	  write(7,'(a30, 7x,f6.2,2x,4i1,2x,4i1)') mapname(j),dist(j),
     &     (p1(j,k),k=1,4),(p2(j,k),k=1,4)
	enddo


	mdiff(1) = 0.0d0
	rec(1) = 0.0d0
	do i=2,nmap
	  mdiff(i) = dist(i) - dist(i-1)
        if (mdiff(i) .eq. 0.0d0) mdiff(i) = 0.001d0 
	  rec(i) = (1.0d0 - dexp(-2*mdiff(i)/100.0d0))/2.0d0
	end do

c	do i=1,nmap
c	  write(7,'(2f8.2,f7.4)') dist(i),mdiff(i),rec(i)
c	end do

	do i=1,n
	  do j=1,nmap
	    sortSNP(i,j) = 0
	  end do
	end do

	maploop: do j=1,nmap
          ifault=0
	write(7,*) trim(mapname(j))
	locloop:   do k=1,nloci
c	write(7,*) trim(locname(k))
	             if (trim(mapname(j)) .eq. trim(locname(k))) then
                       ifault=1
	               write (7,*) "Match!"
	               do i=1,n
	                 sortSNP(i,j) = markSNP(i,k)
                     end do
                     sortSNPp1(j)=SNPp1(k)
                     sortSNPp2(j)=SNPp2(k)
	               exit locloop
	             end if
	           end do locloop
             if (ifault .eq.0) write (7,*) "Q0002: No match for locus ",
     &            mapname(j)
      end do maploop
      
c     check phases agree
      maploop2: do j=1,nmap
          p1count = 0
          do k=1,4
              if (p1(j,k) .ne. 1 .and. p1(j,k) .ne. 2) then
                  write (7,*) 
     &   "Q0005 Incorrect character found in p1 phase data for marker ",
     &             mapname(j), k, p1(j,1), p1(j,2), p1(j,3), p1(j,4)
                  exit
              endif
              if (p1(j,k) .eq. 2) p1count =p1count+1
          enddo
          p2count = 0
          do k=1,4
              if (p2(j,k) .ne. 1 .and. p2(j,k) .ne. 2) then
                  write (7,*) 
     &   "Q0005 Incorrect character found in p2 phase data for marker ",
     &             mapname(j), k, p1(j,1), p1(j,2), p1(j,3), p1(j,4)
                  exit
              endif
              if (p2(j,k) .eq. 2) p2count =p2count+1
          enddo
         write(7,'(a30,2x,4i3)') mapname(j),p1count,sortSNPp1(j),
     &    p2count,sortSNPp2(j)
         if (p1count .ne. sortSNPp1(j)) then
             write(7,*)    
     &   "Q0003 Map phase does not match parental"
     &   " genotype for p1, marker ",
     &        mapname(j)
         endif
         if (p2count .ne. sortSNPp2(j)) then
             write(7,*)    
     &   "Q0003 Map phase does not match parental"
     &   " genotype for p2, marker ",
     &        mapname(j)
         endif
         enddo maploop2
         
          
      write(8,'(i5,2x,i5)') n,nmap
	do k=1,nmap
		  write(8,'(a30,f6.2,2x,4i1,2x,4i1,<n>(1x,i1))') mapname(k),
     &	dist(k),(p1(k,m),m=1,4),(p2(k,m),m=1,4),
     &    (sortSNP(j,k),j=1,n)
	end do

    	end
