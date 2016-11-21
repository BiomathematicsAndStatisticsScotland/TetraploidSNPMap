c	reads in real data in JoinMap format and calculates chisquared
c	for independent segregation

c	use after simplex clustering to combine homologous chromosomes

c	30/3/2011 adapted from clusterall.for to handle SNP data

c     17/9/2012 Started editing to bring into a more general form, for new Fortran
c     locname length changed from 20 to 30 for compatibility with other programmes.
c     19/12/2014 input format changed to handle code in .SNPloc files

c     9/6/2016 try the effect of merging catgories 0/1 and 3/4 in double-duplex markers to avoid small expected numbers of observations
c     Also found and fixed bug in calc of chi-squared significance, as it was giving sig=1 for very large chi-square values.

c	INCLUDE 'link_fnl_shared.h'
c      USE IMSL_LIBRARIES

c	parameter(maxind=200,mxloci=4500)

	implicit double precision (a-h,o-z)
      
c     structures no longer needed
c     integer, allocatable :: iclus(:),iclson(:),nkcount(:)
c     integer, allocatable :: icrson(:),nclus(:),ind(:)
c     real(8), allocatable :: prec(:),rec(:),clevel(:),cclevel(:),simplex(:,:)
        
      integer, allocatable :: ftype(:),mtype(:),p1(:,:)
	integer, allocatable :: p2(:,:),IP(:),IDCUM(:,:)
      integer, allocatable :: markSNP(:,:),nkmcount(:)
     
      real(8), allocatable :: chisq(:,:),sigx(:,:)
      real(8), allocatable :: dist(:,:),XX(:)
      
c      character*7, allocatable :: nodenm(:)
      character*30, allocatable :: locname(:)
c      character*20, allocatable :: code(:)
      
	integer gtp(2,10,8),g1(2,4),g2(2,4),gelp(6,6),df,ichoice,kk1,iperm(100)
	real(8) prob(2,10),mt(2),y(10,100),yb(10),yv(10),chist(100)
	
	character stem*12,infile*19,outfile1*19,outfile2*19,ytext

	     
	write (*,*) "Input file name (file.SNPloc, max 12 characters)"
	read (*,"(a)") stem

	
	infile = trim(stem) // ".SNPloc"
	outfile1 = trim(stem) // ".SNPchi"
c	outfile2 = trim(stem) // ".SNPagg"

	open(3,file=infile,status='old')
	open(4,file=outfile1,status='unknown')
c	open(7,file=outfile2,status='unknown')

      read(3,*) n,nloci
      write(*, '(a, i)') 'maximum: ', 2*nloci
      
      allocate (ftype(nloci),mtype(nloci),p1(nloci,4), p2(nloci,4),
     & markSNP(n,nloci),IP(nloci),IDCUM(nloci,2),nkmcount(nloci))
      
      allocate (chisq(nloci,nloci),sigx(nloci,nloci),dist(nloci,nloci),
     & XX(nloci))
      
      allocate (locname(nloci))
c	  allocate code(nloci))
      
	do i=1,n
	  do j=1,nloci
	    markSNP(i,j) = 0
	  end do
	end do

	do i=1,nloci
c	  read(3,*) locname(i),code(i),ftype(i),mtype(i),(markSNP(k,i),k=1,n)
	  read(3,*) locname(i),ftype(i),mtype(i),(markSNP(k,i),k=1,n)

	end do

	write(4,*) 'Parental phenotypes'
	do i=1,nloci
	  write(4,'(i6,2x,a,2x,i1,4x,i1)') i,locname(i),ftype(i),mtype(i)
	end do
c	do i=1,n
c	  write(4,'(<nloci>(1x,i1))') (markSNP(i,j),j=1,nloci)
c	end do


 	call chimatrix(n,nloci,locname,ftype,mtype,markSNP,chisq,sigx)
c     8/12/2014 chimatrix subroutine changed to return sigx as complete matrix rather than upper triangle
c     as transformation causes problem for Milligan approach
c      write(4,*)
c	write(4,*) 'Matrix of chi-squared statistics'
c	do nl=1,nloci
c	  write(4,'(<nloci>(1x,f8.2))') (chisq(nl,j),j=1,nloci)
c	end do

c	write(4,*)
c	write(4,*) 'Matrix of pairwise significances'
c     do nl=1,nloci
c	  write(4,'(<nloci>(1x,f8.5))') (sigx(nl,j),j=1,nloci)
c	end do

      do nl=1,nloci
	  do nm=1,nloci
	    dist(nl,nm)=1.0-10.0**(-2.0*(sigx(nl,nm)))
	  end do
      end do
      
c     write out distance matrix to file .SNPchi
c      write(4,*) 'Matrix of distances'
c      do nl=1,nloci
c         write(4,'(a,1x,<nl>(1x,f7.4))') locname(nl),(dist(j,nl),j=1,nl)
c      write(4,'(a,1x,<nloci>(1x,f7.4))') locname(nl),
c     & (dist(j,nl),j=1,nloci)
c	end do

c      stop   


	write(6,*) 'Input number of groups'
	read *,kk
	write(6,*) 'Number of groups ', kk 
      
      ICLUS1=kk      !1
      ICLUS2=nloci-1 !kk !nloci-1
      imeth=3
      do I=1,nloci
          IDCUM(I,1)=I
          IDCUM(I,2)=I
      enddo
 
      distlim=1.0d0 !thresh

      call Milligan_cluster(nloci,locname,dist,ICLUS1,ICLUS2,imeth,
     & distlim,IP,IDCUM,XX)
      
      do i=1,nloci-1
          XX(i)=max(XX(i),0.0d0)
      enddo
  
      
      write(4,*) 
      write(4,*) 'Average linkage clustering'
      do I=1,nloci-1
          WRITE (4,'(I6,2X,F20.16,2X,2I6)') I, XX(I),(IDCUM(I,J),J=1,2)
      enddo
      
      do kmcount=1,kk
	  nkmcount(kmcount)=0
	  do i=1,nloci
	    if (IP(i) .eq. kmcount) nkmcount(kmcount)=nkmcount(kmcount)+1
	  end do 
	end do

      do kmcount=1,kk
	write(4,*)	
	write(4,*) 'Linkage group ',kmcount
	write(4,'(i4,4x,i4)') n,nkmcount(kmcount)
	  do i=1,nloci
	    if (IP(i) .eq. kmcount) then
      	  write(4,*) locname(i)
	        write(4,'(2i2,<n>(i2))') ftype(i),mtype(i),
     &			(markSNP(k,i),k=1,n)
	    end if
	  end do 
	end do

      write(4,*) 
      write (4,*) 'Final grouping'
      DO I = 1, nloci
            WRITE (4,'(1X,I5,2X,a30,4X,I3)') I,locname(I),IP(I)
      enddo
  

	end

c	************************************************
	subroutine chimatrix(n,nloci,locname,ftype,mtype,wkgel,chisq,sigx)
c     9/6/2016 try the effect of merging catgories 0/1 and 3/4 in double-duplex markers to avoid small expected numbers of observations


c	to work out distribution of offspring gel pattern.

	implicit double precision (a-h,o-z)

c	parameter(maxind=200,mxloci=4500)

	integer gelp(6,6),wkgel(n,nloci),wkgel1(n,nloci),wkgel0(n,nloci)
	integer n,ntype(nloci),gelo(n,2),ftype(nloci),mtype(nloci)
	integer cumct(nloci),cumcttot,df

	character nodenm(nloci)*7,locname(nloci)*30
      
	dimension lcount(nloci,6),lpat(nloci,6),marg1(6),marg2(6)
      dimension expect(6,6),chisq(nloci,nloci),sigx(nloci,nloci)
      dimension lpat0(5),lpat1(3),lct1(3)
      
c     extra loop to change double-duplex to 1,2,3 coding
      do l=1,nloci
          if (ftype(l) .eq. 2 .and. mtype(l) .eq. 2) then
              do kk=1,n
                  if (wkgel(kk,l) .eq. 0) then
                      wkgel0(kk,l) = 1
                  elseif (wkgel(kk,l) .eq. 4) then
                      wkgel0(kk,l) = 3
                  else
                      wkgel0(kk,l)=wkgel(kk,l)
                  endif
              enddo
          else
              wkgel0(1:n,l)=wkgel(1:n,l)
          endif
      enddo
            
      locus_loop: do l=1,nloci
	  do i=1,6
	    lcount(l,i)=0
	    lpat(l,i)=0
	  end do
        do i=1,n 
	        if (wkgel0(i,l) .eq. 9) then
	          wkgel1(i,l) = 99
c	          exit 
	        else
	          wkgel1(i,l)=wkgel0(i,l)
	        end if
	  end do 

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

      end do locus_loop

	
	pair_loop: do l=1,nloci-1
c	write(6,'(i5,2x,a30)') l,locname(l)
	  do m=l+1,nloci
	    do i=1,n
		    if (wkgel0(i,l) .eq. 9 .or. wkgel0(i,m) .eq. 9) then
	          gelo(i,1) = 99
c	          exit 
	        else
		      gelo(i,1)=wkgel0(i,l)
			  gelo(i,2)=wkgel0(i,m)
	        end if
		end do 	
	    do i=1,6
	      do j=1,6
	        gelp(i,j)=0
	      end do
	    end do

	cumcttot=0
	do i=1,ntype(l)
	  do j=1,ntype(m)
	    do k=1,n
	      if (gelo(k,1).ne.99) then
	        idif=0
	        idif=idif+(lpat(l,i)-gelo(k,1))**2
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

      do i=1,6
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
	
      chisq(l,m)=0.0
      do i=1,ntype(l)
	  do j=1,ntype(m)
	  expect(i,j)=marg1(i)*marg2(j)/real(cumcttot)
	  if (expect(i,j) .gt. 0.0d0) then
        chisq(l,m) = chisq(l,m)+((gelp(i,j)-expect(i,j))**2)/expect(i,j)
	  end if
	  end do
	end do


	df=(ntype(l)-1)*(ntype(m)-1)
      ifault=0
	sigx(l,m)=1-CHISQN(1.0d0*chisq(l,m),1.0d0*df,ifault)
	sigx(m,l)=sigx(l,m)
c	write(4,'(1x,a,2x,2a20,2x,2i1,2x,2i1,2x,f8.2,2x,a,f8.5)')  
c     &	'Chi-squared statistic ',locname(l),locname(m),ftype(l),mtype(l),
c     & ftype(m),mtype(m),chisq(l,m),'Significance',sigx(l,m)
	end do
	end do pair_loop


      
	return
      end

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
c      write(4,*) 'arg = ',arg
c      if(arg.lt.log(uflo)) return
      if(arg.lt.log(uflo)) then
          gammds = one
          return
      endif
      f = exp(arg)
c      write(4,*) 'f = ',f
      if(f.eq.zero) return
      ifault = 0
c
c          Series begins
c
      c = one
      gammds = one
      a = p
c      write(4,*) 'Start chi it'
c      write(4,*) a,gammds
    1 a = a+one
c      write(4,*) a,gammds
      c = c*y/a
      gammds = gammds+c
      if (c/gammds.gt.e) goto 1
      gammds = gammds*f
c      write(4,*) a,gammds
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

c     ****************************************************************
c change program to creat subroutine Milligan_cluster and call to it


      subroutine Milligan_cluster(N,locname,D,ICLUS1,ICLUS2,imeth,
     & distlim,IP,IDCUM,XX)
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
C***********************************************************************
C
c     cut down to drop test of influence and plots, now to work on similarity matrix
c     gp_ave file cuts back to only use method 3, group average
C
c      PARAMETER(MAXPTS=8000)
c     program changed 4/12/2014 to just use D and skip E
C
      implicit double precision (a-h,o-z)
      
      REAL*8 D(N,N),XX(N),P(N*2)

      REAL*8 LEVEL(N)

      INTEGER ERR,IP(N),LK(N),LKI(N),LKJ(N),
     & IDCUM(N,2),IDCUM0(N),IDCUM1(N)

      LOGICAL W(N),WD(N,N),TEST

      CHARACTER*30 locname(N)

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
      S = IS
      N1 = N-1
      N2 = N1-1
      XN = N
c      DO  I = 1, N
c         XR(I) = 0.0
c         DR(I) = 0.0
c         E(I,I) = 0.0
c      enddo



      NCUM=N
      IDCUM0(1:N)=IDCUM(1:N,1)
      IDCUM1(1:N)=IDCUM(1:N,1)
     
c      IKOUNT = 1 no longer needed ?
      K = imeth   !method
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
          write(*, '(a, i)'), 'position: ', II
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
c         write(6,*) II, 'distance matrix'
c         do MM=1,N
c             write (6,'(<N>(f8.4))') (D(MM,NN),NN=1,N)
c         enddo
c         write(6,*)

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
      agloop: DO  IIPX1 = 1, N1   !changed from N2 17/11/14
          write(*, '(a, i)'), 'position: ', IIPX1 + N1
          if (XX(IIPX1) .gt. distlim) exit agloop
          NCUM=NCUM+1
          IDCUM0(1:N)=IDCUM1(1:N)
         JJX = LKJ(IIPX1)
         JX = LKI(IIPX1)
         JX1 = JX-1
         JCL1=0
         JCL2=0
         IF (IIPX1 .eq. N1) JJX=1
         DO 2400 IIXP2 = JJX, JX1
            JJX1 = IIXP2+1
            DO 2390 I1I = JJX1, JX
               JPL1 = MIN0(LK(I1I),LK(IIXP2))
               JPL2 = MAX0(LK(I1I),LK(IIXP2))
c              IF (D(JPL1,JPL2).NE.0.0) GO TO 2390     !try omitting this 18/11/14
               if (IDCUM0(JPL2) .NE. IDCUM0(JPL1)) then 
                   JCL1 = JPL1
                   JCL2 = JPL2
               endif
c               write(6,'(6I6)') JPL1,JPL2,IDCUM0(JPL1),IDCUM0(JPL2),
c     &          JCL1,JCL2
              D(JPL1,JPL2) = XX(IIPX1)
               WD(JPL1,JPL2) = .TRUE.
               IDCUM1(JPL1)=NCUM
               IDCUM1(JPL2)=NCUM            
2390        CONTINUE
2400     CONTINUE
         
c       write out information that might be agglomerations
c         do MM=1,N
c             write(6,'(i3,2x,5i8,2x,f12.6)') MM,LEVEL(MM), IP(MM),
c     &      LK(MM),LKI(MM),LKJ(MM),XX(MM)
c         enddo
c         write(6,*)
c         do MM=1,N
c             write (6,*) (WD(MM,NN),NN=1,N)
c         enddo
c         write(6,*)
c         write(6,*) 'distance matrix'
c         do MM=1,N
c             write (6,'(<N>(f8.4))') (D(MM,NN),NN=1,N)
c         enddo
c         write(6,*)
c        write(6,'(2I6,f12.6,2I6)') JPL1,JPL2,D(JPL1,JPL2),JCL1,JCL2
c         write(6,'(2I6,f12.6)') IDCUM0(JCL1),IDCUM0(JCL2),XX(IIPX1)
c        write(6,*)
        IDCUM(IIPX1,1)= MIN0(IDCUM0(JCL1),IDCUM0(JCL2))
        IDCUM(IIPX1,2)= MAX0(IDCUM0(JCL1),IDCUM0(JCL2))
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
c         WRITE (6,2500) INPX1,INPX1
2500     FORMAT (//,' Cluster membership of data points for the ',I3,
     *      ' cluster solution',/' (Hierarchy level ',I3,')',/)

c         WRITE (6,2510)
2510     FORMAT (' Point    Assigned to cluster',/,1X,30('-'))
c         DO I = 1, N
c            WRITE (6,'(1X,I5,2X,a30,4X,I3,4X,I3)') I,locname(I),IP(I),
c     &    IDCUM1(I)
c         enddo
C
c2670     CONTINUE
      enddo agloop
c      write(6,*) 'Exit agloop'


c2910  CONTINUE

2970  CONTINUE
      RETURN
      END

