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
      
      integer, allocatable :: ftype(:),mtype(:)
	integer, allocatable :: markSNP(:,:),nkmcount(:)
     
      real(8), allocatable :: chisq(:,:),sigx(:,:)
      real(8), allocatable :: dist(:,:)
      
      character*30, allocatable :: locname(:)
      
	
	character stem*12,infile*19,outfile1*19,outfile2*19
	     
	write (*,*) "Input file name (file.SNPloc, max 12 characters)"
	read (*,"(a)") stem

	
	infile = trim(stem) // ".SNPloc"
	outfile1 = trim(stem) // ".distmatrix"
      outfile2 = trim(stem) // ".parphen"

	open(3,file=infile,status='old')
	open(4,file=outfile1,status='unknown')
	open(5,file=outfile2,status='unknown')

      read(3,*) n,nloci
c      //write(*, '(a, i)') 'maximum: ', 2*nloci
      
      allocate (ftype(nloci),mtype(nloci),
     & markSNP(n,nloci),nkmcount(nloci))
      
      allocate (chisq(nloci,nloci),sigx(nloci,nloci),dist(nloci,nloci))
      
      allocate (locname(nloci))
      
	do i=1,n
	  do j=1,nloci
	    markSNP(i,j) = 0
	  end do
	end do

	do i=1,nloci
	  read(3,*) locname(i),ftype(i),mtype(i),(markSNP(k,i),k=1,n)

	end do

	write(5,*) 'Parental phenotypes'
	do i=1,nloci
	  write(5,'(i6,2x,a,2x,i1,4x,i1)') i,locname(i),ftype(i),mtype(i)
	end do

 	call chimatrix(n,nloci,ftype,mtype,markSNP,chisq,sigx)

      do nl=1,nloci
	  do nm=1,nloci
	    dist(nl,nm)=1.0-10.0**(-2.0*(sigx(nl,nm)))
	  end do
      end do
      
      write(4, '(a,  <nloci>(1x,i5))') 'MNAME', (j,j=1,nloci)
      write(4, '(a)') trim(locname(1))
      do nl=2,nloci
         nlm1 = nl-1
         write(4,'(a,<nlm1>(1x,f6.4))') trim(locname(nl)),
     & (dist(j,nl),j=1,nlm1)
	end do

	end

c	************************************************
	subroutine chimatrix(n,nloci,ftype,mtype,wkgel,chisq,sigx)
c     9/6/2016 try the effect of merging catgories 0/1 and 3/4 in double-duplex markers to avoid small expected numbers of observations


c	to work out distribution of offspring gel pattern.

	implicit double precision (a-h,o-z)

c	parameter(maxind=200,mxloci=4500)

	integer gelp(6,6),wkgel(n,nloci),wkgel1(n),wkgel0(n,nloci)
	integer n,ntype(nloci),gelo(n,2),ftype(nloci),mtype(nloci)
	integer cumct(nloci),cumcttot,df

	character locname(nloci)*30
      
	dimension lcount(6),lpat(nloci,6),marg1(6),marg2(6)
      dimension expect(6,6),chisq(nloci,nloci),sigx(nloci,nloci)
      dimension lpat0(5),lpat1(3),lct1(3)
      
c     extra loop to change double-duplex to 1,2,3 coding

!$OMP PARALLEL
!$OMP& SHARED (n,nloci,ftype,mtype,wkgel,chisq,sigx,cumct,wkgel0,
!$OMP& ntype,lpat)
!$OMP& DEFAULT (PRIVATE)

!$OMP DO SCHEDULE (dynamic)
      do l=1,nloci
          write(*,*) "pr"
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
c      enddo
c!$OMP END DO
c
c      write(*,*) "1"
c
c!$OMP DO SCHEDULE (dynamic)
c      locus_loop: do l=1,nloci
	  do i=1,6
	    lcount(i)=0
	    lpat(l,i)=0
	  end do
        do i=1,n 
	        if (wkgel0(i,l) .eq. 9) then
	          wkgel1(i) = 99
	        else
	          wkgel1(i)=wkgel0(i,l)
	        end if
	  end do 

	ntype(l)=0
	cumct(l)=0
	do i=1,n
	  do while (wkgel1(i).ne.99) !count patterns in parent l
	    ntype(l)=ntype(l)+1
	    lpat(l,ntype(l))=wkgel1(i)
	    lcount(ntype(l))=lcount(ntype(l))+1
		do j=i+1,n
		  idif=(wkgel1(i)-wkgel1(j))**2
		  if (idif.eq.0) then
			wkgel1(j)=99
			lcount(ntype(l))=lcount(ntype(l))+1
		  end if
		end do
		wkgel1(i)=99
	    cumct(l)=cumct(l)+lcount(ntype(l))
	  end do !while
	end do

c      end do locus_loop
      end do
!$OMP END DO

!$OMP DO SCHEDULE(dynamic)
	pair_loop: do l=1,nloci-1
          write(*, *) "pr", l
          do m=l+1,nloci
c      if (l.gt.1) then
c        do m=1,l-1
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
c	sigx(m,l)=sigx(l,m)
      end do
	end do pair_loop
c      end if
c      end do
      
!$OMP END DO

!$OMP END PARALLEL
      
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

