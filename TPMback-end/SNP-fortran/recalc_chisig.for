      implicit double precision (a-h,o-z)
      
	character*40, allocatable, save :: locname(:)
      integer, allocatable,save :: markSNP(:,:)
	integer, allocatable,save ::  SNPp1(:),SNPp2(:)
      integer p1,p2
      integer, allocatable,save :: o(:),omiss(:)
	double precision chiprob
	character name*40
	character stem*60,infile*64,outfile1*64

	common /seed/ idum

	write (*,*) "Input file stem (file.loc)"
	read (*,"(a)") stem

	infile = trim(stem) // ".loc"
	outfile1 = trim(stem) // ".out"

	open(3,file=infile,status='old')
	open(4,file=outfile1,status='unknown')

	read(3,*) n,nloci
	write(4,*) n,nloci
     
      allocate (locname(nloci))
      allocate (markSNP(n,nloci))
      allocate (SNPp1(nloci),SNPp2(nloci))
      allocate (o(n),omiss(n))
	do i=1,n
	  do j=1,nloci
	    markSNP(i,j) = 0
	  end do
	end do

c	for general data
	do j=1,nloci
	  read(3,*) locname(j),SNPp1(j),SNPp2(j),(markSNP(i,j),i=1,n)
	end do

	loop1: do i= 1,nloci
	  p1=SNPp1(i)
	  p2=SNPp2(i)
	  name=locname(i)
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
 	  nnl=0
	  do j=1,n
	    if (o(j) .ne. 9) then
	      nnl=nnl+1
	      omiss(nnl)=o(j)
	    endif
        enddo
	  call gtypepred(nnl,name,p1,p2,omiss,alpha,odds,nt,chiprob)
      end do loop1
	end


c	*******************************************************
	subroutine gtypepred(n,name,p1,p2,o1,alpha,odds,nt,chiprob)

c	to predict parental genotypes from their phenotypes and phenotypes
c	of their offspring in a tetrasomic inheritance model. 
c	This program takes double reduction into account.

         
	implicit double precision (a-h,o-z)

      real(8) work(n)
	integer p1,p2,g1(4),g2(4),o1(n)
	integer wkb(100),t1(4),t2(4)
	character name*40

	dimension c(100),xlkhd(17),ipem(1)

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

      prob=0.0d0
      odds=0.0d0
      alpha=0.0d0
      iprint = 0
	call SNPbandist(n,name,alpha,iprint,p1,p2,nob,wkb,c,nt,chiprob,prob)
      if (chiprob .ge. 0.0d0) then
          iprint = 1
          call SNPbandista(n,name,alpha,iprint,p1,p2,nob,wkb,c,nt,
     &    chiprob,prob)
c          write(4,'(a,f6.4)') name,chiprob
      else
          do i=1,16
	        xlkhd(i)=0.0d0
	    end do
          chiprob0=chiprob
      
	    do i=1,16
	        alpha=0.01d0*i
	        call SNPbandista(n,name,alpha,iprint,p1,p2,nob,wkb,c,nt,
     &         chiprob,prob)
	        xlkhd(i)=prob
              if (chiprob .gt. chiprob0) chiprob0=chiprob
          end do
      
          if (chiprob0 .ge. 0.0d0) then
              ipem=maxloc(xlkhd)
              alpha=0.01d0*ipem(1)
              iprint = 1
  	        call SNPbandista(n,name,alpha,iprint,p1,p2,nob,wkb,c,nt,
     &         chiprob,prob)
c c             write(4,'(a,f6.4)') name,chiprob
	
c c         else
c c             write(4,'(a,f9.4)') name,chiprob
	    endif
      endif
      write(4,'(a,f9.4)') name,chiprob
	return
	end
c	***************************************************************
	subroutine SNPbandist(n,name,alpha,iprint,p1,p2,nob,wkb,c,nt,chiprob,
     & prob)
	implicit double precision (a-h,o-z)
      integer g1(4),g2(4),p1gt(10,2),p2gt(10,2),zygote(100,4)
	integer p1,p2,bptn(100)
c      ,genotype(8),phenotype(8)
	integer wkb(100)
	character name*40
	dimension p1gtfq(10),p2gtfq(10),freq(100)
c      mtx1(6,6),mtx2(6,6),
c	dimension prior(100),xlkhd(100),
      dimension c(100)
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
	call gametogensis(alpha,g1,ngt1,p1gt,p1gtfq)
	call gametogensis(alpha,g2,ngt2,p2gt,p2gtfq)
	call zygt(p1gt,p2gt,p1gtfq,p2gtfq,ngt1,ngt2,ntype,zygote,freq)
	call SNPgtob(ntype,zygote,freq,np,bptn)
	call SNPptsort(n,np,iprint,bptn,freq,nob,wkb,c,x2,chiprob,prob)
c10	format(1x,a,2x,a)
c20	format(5x,4(i1),2x,4(i1),4x,f15.12,4x,i2,5x,f8.2)

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
      
 	if (ndf.gt.0) then
	  x2=-99.0d0
	  chiprob=-99.0d0
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
	  end do
	df=1.0d0*np-1.0d0
      ifault=0
	chiprob=1.0d0-CHISQN(x2, df,ifault)
      np1=np
	prob=pmltnr(n,np1,ct,f2freq)
	end if

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
