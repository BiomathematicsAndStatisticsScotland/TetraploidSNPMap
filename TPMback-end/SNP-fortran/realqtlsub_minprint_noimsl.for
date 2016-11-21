	subroutine SNPprob(n,nloci,nstep,chrom,postprob,
     &	ntrt,trait)

c     subroutine to assign
c     probabilities, for all individuals and one chromosome
c	adapted from chromprob.for

c     8/10/2012 Adapt to print lod scores for analysis without iteration

c     9/1/2015 cut back to do single run without iteration
c     12/1/2015 add code to work out percentage points
c     12/1/2015 cut back output to same as old TPM
	
c	parameter(maxind=400,mxloci=500,mxstep=150,mxtrt=500)
      parameter(mxstep=250)
	implicit double precision (a-h,o-z)
	
	integer nloci,ntrt,chrom(36,4),jorder(ntrt)
	real(8) postprob(n,0:mxstep,36),percentile(2)
	real(8) allexpl(n,36,9),trait(ntrt,n)
	real(8) SNPexpl(36,8),trtmean(ntrt,36)
c	character traitlab(ntrt)*20
	
	real(8) tprob,mpos(nloci),expl(n,36,8)
	real(8) scoef(7,5),wt(n,36),adr2,bestfadr2(ntrt),ems
c	real(8) bestfems(ntrt),bestfpos(ntrt)
c	real(8) bestscoef(ntrt,7),bestems(ntrt),bestsse(ntrt,7)
	real(8) seltrait(n,36),bestflike(ntrt),lcoef(7,5)
	real(8) floglike,ladr2,lems,lloglike,scaleprob(n*36)
c	real(8) sfadr2(ntrt,mxstep), sfloglike(ntrt,mxstep)
	real(8) exptrait(n,36,ntrt),sflod(ntrt,mxstep)
	real(8) loglike0,lod,flod,bestflod(ntrt),bestlod_sort(ntrt)
c      real(8) sfems(ntrt,mxstep),sfcoef(ntrt,mxstep,7,2)
c	set up explanatory variables
	SNPexpl=0.0d0
	do j=1,36
	  if (chrom(j,1) .eq. 1) SNPexpl(j,1) = 1.0d0
	  if (chrom(j,1) .eq. 2 .or. chrom(j,2) .eq. 2) 
     &	  SNPexpl(j,2) = 1.0d0
	  if (chrom(j,1) .eq. 3 .or. chrom(j,2) .eq. 3) 
     &	  SNPexpl(j,3) = 1.0d0
	  if (chrom(j,2) .eq. 4) SNPexpl(j,4) = 1.0d0
	  if (chrom(j,3) .eq. 5) SNPexpl(j,5) = 1.0d0
	  if (chrom(j,3) .eq. 6 .or. chrom(j,4) .eq. 6) 
     &	  SNPexpl(j,6) = 1.0d0
	  if (chrom(j,3) .eq. 7 .or. chrom(j,4) .eq. 7) 
     &	  SNPexpl(j,7) = 1.0d0
	  if (chrom(j,4) .eq. 8) SNPexpl(j,8) = 1.0d0
      enddo

      do in=1,n
        do k=1,36
	    do j=1,8
	      allexpl(in,k,j) = SNPexpl(k,j)
	    end do
	    do itrt=1,ntrt
	      exptrait(in,k,itrt) = trait(itrt,in)
	    end do
	  end do
	end do

      do itrt=1,ntrt
        write(6,*) 'permutation ',itrt

c        write(*,*) 'start itrt = ', itrt, nstep, n
        do istep=0,nstep
          tpos=real(istep)
          do in=1,n
            do k=1,36
              allexpl(in,k,9) = postprob(in,istep,k)
	        seltrait(in,k)=exptrait(in,k,itrt)
		    if (idnint(exptrait(in,1,itrt)) .eq. -99) then
                allexpl(in,k,9)=0.0d0
		    end if	      
            end do
	    end do
c	    call weighted regression
	    call qitwregr_noit(n,allexpl,seltrait,scoef,
     &	adr2,ems,floglike,flod)
          if(istep .eq. 0 .or. floglike .gt. bestflike(itrt)) then
            bestflod(itrt) = flod
            bestflike(itrt) = floglike
          end if
	  end do
      end do


      


c     added 27/6/2013 to calc means for each genotype combination
c     no need for this part for permutation test
c       do itrt=1,ntrt
c 	  do k=1,36
c          wtpp=0.0d0
c          wtrt=0.0d0
c   	    do in=1,n
c            if (idnint(trait(itrt,in)) .eq. -99) cycle
c            wtpp=wtpp+postprob(in,bestfpos(itrt),k)
c            wtrt=wtrt+postprob(in,bestfpos(itrt),k)*trait(itrt,in)
c          enddo
c          trtmean(itrt,k)=wtrt/wtpp
c        enddo
c        do k=1,36
c          write (7,'(4i2,2x,f10.4)') (chrom(k,j),j=1,4),trtmean(itrt,k)
c        enddo
c       enddo
       

c	write (7,*) 'Position, coefficients, lod and error 
c     &	  mean square for each trait, with iteration'
c	do it=1,ntrt
c	  write(7,'(i4,2x,a12,2x,f5.1,2x,7(f9.4,2x),2x,f7.2,4x,f8.3,
c     &	  4x,f7.2)') it,traitlab(it),bestlpos(it),
c     &	  (bestlcoef(it,j),j=1,7),bestlod(it),bestlems(it),
c     &      bestladr2(it)
c	end do

c	do itrt=1,ntrt
c	write (7,*) 'Profile for trait ',itrt, traitlab(itrt)
c	  do istep=0,nstep
c          tpos=real(istep)
c       write(7,'(f7.2,2x,f12.7,2x,2f7.2,2x,f12.7,2x,2f7.2)') tpos,
c     &    	 sfloglike(itrt,istep+1),sfadr2(itrt,istep+1),
c     &	  sflod(itrt,istep+1),slloglike(itrt,istep+1),
c     &      sladr2(itrt,istep+1),sllod(itrt,istep+1)
c	  end do

c	write(7,*) 'Trait ', itrt, traitlab(itrt)
c	write(7,*) 'Best position without iteration'
c	write(7,'(4f12.6)') bestfpos(itrt),bestfadr2(itrt),bestfems(itrt),
c     & bestflod(itrt)
c	do ib = 1,7
c	  write(7,'(2(4f8.3))') bestscoef(itrt,ib),bestsse(itrt,ib)
c	end do
c	write(7,*) 'Best position with iteration'
c	write(7,'(4(f12.6,2x))') bestlpos(itrt),bestladr2(itrt),
c     &	bestlems(itrt),bestlod(itrt)
c	do ib = 1,7
c	  write(7,'(4f8.3)') bestlcoef(itrt,ib)
c	end do
c	end do

c	write (7,*) 'Position, coefficients, adjusted R2 and error 
c     &	mean square for each trait, without iteration'
c	do it=1,ntrt
c	  write(7,'(i4,2x,a20,2x,f5.1,7f9.4,2x,f7.2,4x,2x,f7.2,4x,f8.3)') it,
c     &	  traitlab(it),bestfpos(it),
c     &	  (bestscoef(it,j),j=1,7),bestfadr2(it),bestflod(it),bestfems(it)
c	end do

c     new brief output
      write(6,*) 'lod'
      do it=1,ntrt
          write(6,'(f8.2)') bestflod(it)
      enddo
      
c	write (7,*) 'Position, coefficients, adjusted R2 and error 
c     &	mean square for each trait, with iteration'
c	do it=1,ntrt
c	  write(7,'(i4,2x,a20,2x,f5.1,7f9.4,2x,f7.2,4x,2x,f7.2,4x,f8.3)') it,
c     &	  traitlab(it),bestlpos(it),
c     &	  (bestlcoef(it,j),j=1,7),bestladr2(it),bestlod(it),bestlems(it)
c      end do

c     write out genotype means at the best position
c     no need for this part for permutation test
c      write (7,*)
c      write (7,*) 'Genotype means at the best position, no iteration'
c      write (7,'(10x,<ntrt>(a20))') (traitlab(itrt),itrt=1,ntrt)
c      do k=1,36
c          write (7,'(4i2,2x,<ntrt>f20.4)') (chrom(k,j),j=1,4),
c     &     (trtmean(itrt,k),itrt=1,ntrt)
c      enddo
 
c     write out lod profiles without iteration
c     no need for this part for permutation test
c      write (7,*)
c     	write (7,*) 'Lod profiles for traits '
c      write (7,'(9x,<ntrt>(a20))') (traitlab(itrt),itrt=1,ntrt)c
c	  do istep=0,nstep
c          tpos=real(istep)
c       write(7,'(f7.2,2x,<ntrt>f20.7)') tpos,	 
c     &	  (sflod(itrt,istep+1),itrt=1,ntrt)
c	  end do

c     calc percentage points of lod score
c     within module RANDOM of Alan Miller 
c     http://www.it.uu.se/edu/course/homepage/algpar1/ht02/Quicksort_examples/nur.f90
c      write (7,*) ' **** Percentage points for lod score ****'
c      bestlod_sort=bestflod
c      call sort_2(ntrt,bestlod_sort,jorder)
c      do i = 1,2
c          ii=85+5*i
c          pos = ii * FLOAT(ntrt+1) / 100.
c          j = int(pos)
c          remainder = pos - j
c          percentile(i) = (1.0d0 - remainder)*bestlod_sort(j) + 
c     &    remainder*bestlod_sort(j+1)
c      write(7,'(i5,a1,f8.2)') ii,'%',percentile(i)
c      enddo

	end
      


c	*********************************************************************
	subroutine qitwregr_noit(n,allexpl,seltrait,scoef,
     &	adr2,ems,floglike,flod)

c	edited Sep 2011
c	does iteratively reweighted regression on a particular position
c	using both parents
c	edit 29/9/2011 to add in material from Zewei/Fortran/QTL2006 to calc lod

c     edited 3/7/2012 to change designation of scpe and sse in IMSL routines, which seems to have changed
c     edited 8/10/2012 to calculate lod without iteration and print coefficients without iteration

c     edited July 2014 to only run single regression without reweighting
c     11/7/2014: adapt to us non IMSL routine for regression

c	INCLUDE 'link_fnl_shared.h'
c      USE IMSL_LIBRARIES

      use lsq
      
c	parameter(maxind=400,mxloci=500,mxtrt=400,ldscpe=1,idep=1)
      parameter(ldscpe=1,idep=1)
	implicit double precision (a-h,o-z)
	real(8) allexpl(n,36,9),loglike,fitted(n*36),prtq(n*36)
	real(8) scoef(7,5),lcoef(7,5),scpe(ldscpe,idep),sse		
	real(8) adr2,floglike,wttot,ems,newprob(n*36),like(n)
	real(8) scaleprob(n*36),seltrait(n,36),ladr2,lems,lloglike
c	integer wkqtlgeno(n*36,4),wkallexpl(n*36,36,9)
	integer indind(6),irbef(7),rid(n*36),inddep(1)
	dimension xmat(n*36,10),b(7),prqm(n*36)
	dimension xmin(7),xmax(7),aov(15),sqss(6,4)

	real(8) b0(1),r0(1,1),d0(1),sqss0(1,4),lcoef0(1,5),fitted0(n*36)
	real(8) ladr20,lems0,loglike0,lod,flod
	real(8) ladr2i,lemsi,lcoefi(7,5)
      REAL(8) DPI
      
c      real(8) tol,r(7,7),d(7)
      
      integer     ifault
      LOGICAL            :: fit_const, lindep(0:6)
      REAL(8)            :: xx(0:6), yy, wt,beta(0:6),xx0(0:1)
      REAL(8)            :: var, covmat(n*36), sterr(0:6), hii,
     &                     cormat(n*36), ycorr(6),t(0:6)

      integer         ii

      PARAMETER (DPI=3.141592653589793238D0)

c	do weighted regression missing first and fifth chromosomes 
c	due to singularities: each set of four must sum to 2.	

c	tol=100.0d0*dmach(4)
	ido = 0
	intcep=1
c	idep = 1
c      ldscpe = 1
	ifrq = 0
	iwt = 9
	wttot=0.0d0

	indind(1)=2
	indind(2)=3
	indind(3)=4
	indind(4)=6
	indind(5)=7
	indind(6)=8
	inddep=10

	itcount = 0
	do i=1,n
	  do j=1,36
	    if (allexpl(i,j,9) .gt. 0.0) then !omit cases with weight = 0
	      itcount=itcount+1
	      wttot=wttot+allexpl(i,j,9)
	      prqm(itcount) = allexpl(i,j,9)
	      rid(itcount) = i
	      xmat(itcount,10) = seltrait(i,j)
	      do k=1,9
	        xmat(itcount,k) = allexpl(i,j,k)
c	        wkallexpl(itcount,j,k)=allexpl(i,j,k)
	      end do
c	      do k=1,4
c	        wkqtlgeno(itcount,k)=qtlgeno(i,1,k)
c	      end do
	    end if
	  end do
	end do

c	write(7,*) 'allexpl'
c	do i=1,n
c	  do j=1,rct(i)
c	   write(7,'(i4,8(f4.1,2x),f9.3)') i,(allexpl(i,j,k),k=1,9)
c	  end do
c	end do

c	write(7,*) 'xmat ',' degrees of freedom ',wttot
c	do i=1,itcount
c	  write(7,'(i4,8(f4.1,2x),2(f9.3,2x))') i,(xmat(i,j),j=1,10) 
c	end do

c     first regression using lsq
c     NB probably need to think about total df, see code below
      fit_const = .true.           ! Change to .false. if fitting a model without
                             ! a constant.
      nvar=6                 ! number of variables
      CALL startup(nvar, fit_const)          ! Initializes the QR-factorization
      lmax=itcount              ! length of variates
c      write(7,*) 'Input to lsq routine'
      do i=1,lmax
        xx(0)=1.0d0
        xx(1:3)=xmat(i,2:4)
        xx(4:6)=xmat(i,6:8)
        yy=xmat(i,10)
        wt=xmat(i,9)
c	  write(7,'(i4,7(f4.1,2x),2(f9.3,2x))') i,(xx(j),j=0,6),yy,wt 
        call includ(wt,xx,yy)
      enddo
      nobs=wttot
c      WRITE(7, *)'No. of observations =', nobs

      CALL tolset()
      CALL sing(lindep, ifault)              ! Checks for singularities

c      IF (ifault == 0) THEN
c        WRITE(7, *)'QR-factorization is not singular'
c      ELSE
c        DO i = 1, nvar
c          IF (lindep(i)) THEN
c            WRITE(7, *) i, 
c     &        ' is exactly linearly related to earlier variables'
c          END IF
c        END DO ! i = 1, nvar
c      END IF ! (ifault == 0)
c      WRITE(7, *)
      
      nreq = nvar +1                             
      CALL regcf(beta, nreq, ifault)
      CALL ss()                   ! Calculate residual sums of squares

!     Calculate covariance matrix of the regression coefficients & their
!     standard errors.

      var = rss(nreq) / (wttot - nreq)   !wttot replaces nobs to try to get df right
      CALL cov(nreq, var, covmat, itcount, sterr, ifault)

      t(0:nreq-1) = beta(0:nreq-1) / sterr(0:nreq-1)
      b(1:nreq)=beta(0:nreq-1)
      scoef(1:nreq,1)=beta(0:nreq-1)
      scoef(1:nreq,2)=sterr(0:nreq-1)

!     Output regression table, residual sums of squares, and R-squared.

c      WRITE(7, *)
c      WRITE(7, *)
c     & 'Regn.coeff.   Std.error  t-value   Res.sum of sq.'
c      DO i = 0, nreq-1
c        WRITE(7, 900) beta(i), sterr(i), t(i), 
c     &    rss(i+1)
c  900 FORMAT('  ', f12.4, '  ', f11.4, ' ', f7.2, '  ', 
c     &   f14.6)
c      END DO
c      WRITE(7, *)

c     	adr2 = aov(12)   !What to do about this?

c	first regression
c	write(7,*) 'First regression'
c	call drgivn(ido,itcount,10,xmat,n*36,intcep,6,indind,idep,inddep,
c     &	ifrq,iwt,1,tol,b,7,r,7,d,irank,dfe,scpe,ldscpe,nrmiss,xmin,xmax)
	dfe = wttot - 7.0 !needed to handle weighted data correctly
c      sse = scpe(1,1)
c	call drstat(intcep,-6,irbef,b,r,7,dfe,sse,'N',aov,sqss,6,
c     &	  scoef,7,r,7)
c	do i=1,7
c	  write(7,'(f9.3,4f8.3)') b(i),(scoef(i,j),j=1,4)
c	end do
		
c	adr2 = aov(12)
c	ems  = aov(8)
	
c	write(7,*) 'Adjusted R2, error m.s. and df for each locus'
c	write(7,'(f7.1,2x,f7.4,2x,f9.4)') adr2,ems,dfe

	ems  = rss(nreq)/dfe
c	write(7,*) 'error m.s. and df for each locus'
c	write(7,'(f7.4,2x,f9.4)') ems,dfe

c	calculate posterior probability
	do i=1,itcount
	  fitted(i) = b(1)+b(2)*xmat(i,2)+b(3)*xmat(i,3)+
     &	  b(4)*xmat(i,4)+b(5)*xmat(i,6)+b(6)*xmat(i,7)+
     &      b(7)*xmat(i,8)
	  prtq(i) = dexp(-0.5*((xmat(i,10) - fitted(i))**2)/ems)/
     &	  dsqrt(2*DPI*ems)
	  newprob(i) = prqm(i)*prtq(i)
	end do

c	calculate log-likelihood
	loglike = 0.0d0
	istart = 1
	iend = itcount
	do j=1,n
	  cttype = 0
	  like(j) = 0.0d0
	  do i=istart,itcount
		if (rid(i) .gt. j) exit
		cttype = cttype+1
	  end do 
	  if (cttype .eq. 0) cycle
	  iend = i - 1
	  do i = istart,iend
	    like(j) = like(j) + prtq(i)*prqm(i)
	  end do
	  istart = istart + cttype
	  loglike = loglike + dlog(like(j))
      end do
c	write(7,*) 'Log-likelihood = ',loglike
	oldll = loglike
	floglike=loglike

c	write(7,*) 'No.  Indi Fitted trt  True trt    pr(trt|qtl) new prob    
c    &	old prob'
c	do i=1,itcount
c	  write(7,'(2i5,2f12.4,3f12.7)') i,rid(i),fitted(i),xmat(i,10),
c    &	  prtq(i),newprob(i),prqm(i)
c	end do

c     calc null likelihood
c     15/7/2014 now fixed
      nvar=0                 ! number of variables

      CALL startup(nvar, fit_const)          ! Initializes the QR-factorization
      lmax=itcount              ! length of variates
      do i=1,lmax
c                write(*,*) 'qitwregr 14', i

        xx0(0)=1.0d0
        yy=xmat(i,10)
        wt=xmat(i,9)
        call includ(wt,xx0,yy)
      enddo
      nobs=wttot
c      WRITE(7, *)'No. of observations =', nobs

c      CALL tolset()
c      CALL sing(lindep, ifault)              ! Checks for singularities

c      IF (ifault == 0) THEN
c        WRITE(7, *)'QR-factorization is not singular'
c      ELSE
c        DO i = 1, nvar
c          IF (lindep(i)) THEN
c            WRITE(7, *) i, 
c     &        ' is exactly linearly related to earlier variables'
c          END IF
c        END DO ! i = 1, nvar
c      END IF ! (ifault == 0)
c      WRITE(7, *)
      
      nreq = nvar+1                              
      CALL regcf(beta, nreq, ifault)
      CALL ss()                   ! Calculate residual sums of squares
!     Calculate covariance matrix of the regression coefficients & their
!     standard errors.

      var = rss(nreq) / (wttot - nreq)
      CALL cov(nreq, var, covmat, itcount, sterr, ifault)
      t(0:nreq-1) = beta(0:nreq-1) / sterr(0:nreq-1)
      b0(1:nreq)=beta(0:nreq-1)
      lcoef0(1:nreq,2)=sterr(0:nreq-1)
!     Output regression table, residual sums of squares, and R-squared.

c      WRITE(7, *)
c      WRITE(7, *)
c     & 'Regn.coeff.   Std.error  t-value   Res.sum of sq.'
c      DO i = 0, nreq-1
c        WRITE(7, 900) beta(i), sterr(i), t(i), 
c     &    rss(i+1)
c        END DO
c      WRITE(7, *)

c     	adr2 = aov(12)   !What to do about this?

c	new part to calc likelihood under null hypothesis 26/1/05
c	call drgivn(ido,itcount,10,xmat,n*36,intcep,0,indind,idep,inddep,
c     &	ifrq,iwt,1,tol,b0,1,r0,1,d0,irank,dfe,scpe,1,nrmiss,xmin,xmax)
	dfe = wttot - 1.0 !needed to handle weighted data correctly
c      sse=scpe(1,1)
c	call drstat(intcep,0,irbef0,b0,r0,1,dfe,sse,'N',aov,sqss0,1,
c     &	  lcoef0,1,r0,1)
c	write(7,'(f9.3,4f8.3)') b0(1),(lcoef0(1,j),j=1,4)
c      write (7,*) 'AOV statistics for null model'
c      do i=1,12
c        write(7,*) i,aov(i)
c      enddo 		
		
c	ladr20 = aov(12)
c	lems0  = aov(8)
	lems0  = rss(1)/dfe     !Check later: found OK
	adr2  = max(100.0*(1.0d0 - (ems/lems0)),0.0d0)

	do i=1,itcount
	  fitted0(i) = b0(1)     
	  prtq(i) = dexp(-0.5*((xmat(i,10) - fitted0(i))**2)/lems0)/
     &	  dsqrt(2*DPI*lems0)
	  newprob(i) = prqm(i)*prtq(i)
	end do

	loglike0 = 0.0d0
	istart = 1
	iend = itcount
	do j=1,n
	  cttype = 0
	  like(j) = 0.0d0
	  do i=istart,itcount
		if (rid(i) .gt. j) exit
		cttype = cttype+1
	  end do 
	  if (cttype .eq. 0) cycle
	  iend = i - 1
	  do i = istart,iend
	    like(j) = like(j) + prtq(i)*prqm(i)
c	    write(7,'(2i4,2x,f10.3,2x,f8.4,2x,f10.3)') j,i,like(j),
c     &		prqm(i),prtq(i)
	  end do
	  istart = istart + cttype
	  loglike0 = loglike0 + dlog(like(j))
	end do
c	lod=(loglike - loglike0)*dlog10(dexp(1.0d0))
      flod=(floglike - loglike0)*dlog10(dexp(1.0d0))
c	write(7,'(a,f10.3,2x,f10.3)') 'Null log-likelihood  = ,
c     & flod = ',	loglike0,flod

c	write(7,*) 'Adjusted R2 and error m.s. for each locus'
c	write(7,'(f7.1,1x,2(f7.4,1x),f7.3,1x,f10.3,1x,f10.3)') ladr2,
c     & lems,lems0,lod,loglike,loglike0 
	return
      end

c	**************************************************************************

	subroutine configsumm(nconfig,ir,allwkexpl,wkwt,nt,sexpl,swt)

c	subroutine to combine possible QTL genotypes from all configurations

	integer nconfig,allwkexpl(1000,36,8),sexpl(36,8),idif,ir(1000)
	real(8) wkwt(1000,36),swt(36)

c	copy possible types and weights for config 1 into sexpl	
	nt = 0          !no. of possible genotypes from all configs
	do j=1,ir(1)
	  swt(j) = 0.0d0
	end do

c	write(7,*) 'Configuration summary'
c	do j=1,ir(1)
c	  write(7,'(8i3,2x,f10.7)') (allwkexpl(1,j,k),k=1,8),wkwt(1,j)
c	end do

	do j=1,ir(1)
	  nt=nt+1
	  swt(j) = wkwt(1,j)
	  do k=1,8
	    sexpl(j,k) = allwkexpl(1,j,k)
	  end do
	end do

	do i=2,nconfig
c	  do j=1,ir(i)
c	    write(7,'(8i3,2x,f10.7)') (allwkexpl(i,j,k),k=1,8),wkwt(i,j)
c	  end do
	  genoloop: do j=1,ir(i)
	    do jj = 1,nt
	      idif = 0
	      do k=1,8
	        idif = idif+abs(allwkexpl(i,j,k) - sexpl(jj,k))
	      end do
	      if (idif .eq. 0) then
	        swt(jj) = swt(jj) + wkwt(i,j)
	        cycle genoloop
	      endif
	    end do
	    nt = nt+1
	    swt(nt) = wkwt(i,j)
	    do k=1,8
	      sexpl(nt,k) = allwkexpl(i,j,k)
	    end do
	  end do genoloop
	end do
c	b do j=1,nt
c	b   write(7,'(8i3,2x,f10.7)') (sexpl(j,k),k=1,8),swt(j)
c	b end do

	return
      end

c     *************************************************************
      subroutine swap(a,b)
      
c     data swapping routine from Brainerd et al. Fortran book p98

      real(8) a,b,temp
      temp=a
      a=b
      b=temp
      
      return
      end
      
c     *************************************************************
      subroutine iswap(ia,ib)
      
c     data swapping routine from Brainerd et al. Fortran book p98, for integers

      integer ia,ib,itemp
      itemp=ia
      ia=ib
      ib=itemp
      
      return
      end
c     *************************************************************
      subroutine sort_2(n,list,jorder)
      
c     small sorting routine from Brainerd et al. Fortran book p153
c     edited to return permutation vector iorder

      real(8) list(n),temp(n)
      integer iorder(n),minplace(1),maxplace(1),jorder(n)
c      write(4,*)
c      write(4,*) 'Sort output'
c      do i=1,n
c          write(4,'(i6,f12.6)') i,list(i)
c      enddo
      
      maxplace=maxloc(list)
      do i=1,n
          jorder(i)=i
      enddo
      
      iorder(n)=maxplace(1)
      
 
      do i=1,n-1
         minplace=minloc(list(i:))
         iorder(i)=minplace(1)
c         write(4,'(2i6)') i,iorder(i)
         call swap(list(i),list(i+iorder(i)-1))
         call iswap(jorder(i),jorder(i+iorder(i)-1))
      enddo
         
c      write(4,*)
c      do i=1,n
c          write(4,'(3i6,f12.6)') i,iorder(i),jorder(i),list(i)
c      enddo
      
      return
      end
      