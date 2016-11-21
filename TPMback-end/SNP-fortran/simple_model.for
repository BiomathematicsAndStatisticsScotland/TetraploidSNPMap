c     program to fit simpler models to means from QTL analysis
c     v2 modified from original by dropping specific call to H1+H5. These now handled by separate program
c     v3 has output of coefficients tidied up for TPM 18/11/15

      USE lsq


	parameter(mxtrt=400,maxvar=7,max_cdim = maxvar*(maxvar+1)/2)

	implicit double precision (a-h,o-z)
			
	character stem*12,infile1*16,outfile1*19
	character traitlab(mxtrt)*20,dum(4)*2,model(81)*8,tch*1,tempmodel(81)*8
      character tterm(7)*8
	integer chr(36,4),jorder(80)
	real(8) trait(36,mxtrt),h(36,8),v(36,6),dm(36,12),s(36,16),tm(36,16)
      real(8) fv(36,6,2),fs(36,16,2)
c	integer g1(2,4),g2(2,4),SNPp1(500),SNPp2(500)
c	integer gelp(36,36),p1(mxloci,4),p2(mxloci,4)

c      integer zpheno(6,6),matpheno(mxloci,6,6),wkpheno(6,6)
c	integer matchchr(36,4),wkgel,imatch(36),matmatch(36,mxloci)
c	integer matchtot(36), form(36,4), iperm(36),dff(36,36),sform(36,4)
c	integer sdff(36,36)
c	real(8) permmatch(36,mxloci),rform(36,4),rdff(36,36),rmatchtot(36)
c	real(8) allct(maxind),halfct(maxind),dist(mxloci)
c	real(8) mdiff(mxloci),rec(mxloci)
c	real(8) trait(mxtrt,maxind)
c	real(8) postprob(maxind,0:mxstep,36),segrat(0:mxstep,36)
c	real(8) prspline(36,mxstep)
c	character cftype(4)*2,cmtype(4)*2
c     character fgam(6,2)*2,mgam(6,2)*2,zgeno(6,6,4)*2

c	integer markSNP(maxind,mxloci),partype(mxloci,8),maxrun(36)
c	integer sortSNP(maxind,mxloci),id(maxind)

c	character locname(mxloci)*20,mapname(mxloci)*20,mphase(mxloci)*4
c	character outfile2*19,fphase(mxloci)*4,traitlab(mxtrt)*20
c     character outfile3*21,stem2*12

      LOGICAL            :: fit_const
      real(8) wt,xx(0:maxvar),yy, beta(0:maxvar),var, covmat(max_cdim),      
     &     sterr(0:maxvar), cormat(max_cdim), ycorr(maxvar),t(0:maxvar)
      real(8) adjr2(81),sic(81),temp(81)
      
	write (*,*) "Input file name (.qmm),max 12 characters"
	read (*,"(a)") stem

		
	infile1 = trim(stem) // ".qmm"
	outfile1 = trim(stem)// "qmm.out"

	open(3,file=infile1,status='old')
	open(7,file=outfile1,status='unknown')

	read(3,*) ntrt
	read(3,*) (dum(j),j=1,4),(traitlab(j),j=1,ntrt)
	do i=1,36
	  read(3,*) (chr(i,j),j=1,4),(trait(i,j),j=1,ntrt)
	write(7,'(4i2,2x,<ntrt>(f8.3))') (chr(i,j),j=1,4),(trait(i,j),j=1,ntrt)
      enddo

      do i=1,ntrt
          write(*,*) i,traitlab(i)
      enddo
 1000 write (*,*) "Which trait for simple model?"
	read (*,'(i3)') itt

c     set up explanatory variables
      fit_const = .true.           ! Change to .false. if fitting a model without
                             ! a constant.

c     simplex
      model(1)='H1'
      model(2)='H2'
      model(3)='H3'
      model(4)='H4'
      model(5)='H5'
      model(6)='H6'
      model(7)='H7'
      model(8)='H8'
      h=0.0d0
      do i=1,36
          if (chr(i,1) .eq. 1) h(i,1) = 1.0d0
          if (chr(i,1) .eq. 2 .or. chr(i,2) .eq. 2) h(i,2) = 1.0d0
          if (chr(i,1) .eq. 3 .or. chr(i,2) .eq. 3) h(i,3) = 1.0d0
          if (chr(i,2) .eq. 4) h(i,4) = 1.0d0
          if (chr(i,3) .eq. 5) h(i,5) = 1.0d0
          if (chr(i,3) .eq. 6 .or. chr(i,4) .eq. 6) h(i,6) = 1.0d0
          if (chr(i,3) .eq. 7 .or. chr(i,4) .eq. 7) h(i,7) = 1.0d0
          if (chr(i,4) .eq. 8) h(i,8) = 1.0d0
      enddo

c      do i=1,36
c          write(7,'(4i2,8f5.1)') (chr(i,j),j=1,4),(h(i,j),j=1,8)      
c      enddo
      
      tterm(1)='Constant'
      do j=1,8                       
      nvar=1
      CALL startup(nvar, fit_const)          ! Initializes the QR-factorization

!     Read in the data, one line at a time, and progressively update the
!     QR-factorization.

      wt = 1.0d0

      DO i= 1,36
        xx(0) = 1.0d0                                              
        xx(1) = h(i,j)                                               
        yy = trait(i,itt)
        CALL includ(wt, xx, yy)
      END DO
c      WRITE(4, *)'No. of observations =', nobs
      nreq = 2                               
      CALL regcf(beta, nreq, ifault)

      CALL ss()
      var = rss(nreq) / (nobs - nreq)
      CALL cov(nreq, var, covmat, max_cdim, sterr, ifault)

!     Calculate t-values

      t(0:nreq-1) = beta(0:nreq-1) / sterr(0:nreq-1)
      adjr2(j) = 100.0*(1.0d0 - ((rss(nreq)/(nobs-nreq)) / 
     & (rss(1)/(nobs-1.0d0))) )
      sic(j) = 36.0*log(var)+2.0*log(36.0)

!     Output regression table, residual sums of squares, and R-squared.
      tterm(2)=model(j)
      WRITE(7, *)
      WRITE(7, '(a,1x,a8)')  'Model coefficients ', model(j)
      WRITE(7, *) 'Term      Reg.coeff.    Std.error     t-value'  
      DO i = 0, nreq-1
        WRITE(7, 900) tterm(i+1),beta(i), sterr(i)   , t(i) !, rss(i+1)
c  900 FORMAT(a8,2x,g12.4, '  ', g11.4, ' ', f7.2, '  ', g14.6)
  900 FORMAT(a8,2x,g012.4, 2x, g011.4, 2x, f7.2)
      END DO
c      WRITE(7, *)
c      WRITE(7, *) 'Model     Adj R^2   SIC'
c      WRITE(7, '(a8,2x,f8.3,2x, f8.3)')  model(j),adjr2(j),sic(j)
      enddo

      
c     duplex as codominant variate
      model(9)='V12'
      model(10)='V13'
      model(11)='V14'
      model(12)='V56'
      model(13)='V57'
      model(14)='V58'
      v(1:36,1)=h(1:36,1)+h(1:36,2)
      v(1:36,2)=h(1:36,1)+h(1:36,3)
      v(1:36,3)=h(1:36,1)+h(1:36,4)
      v(1:36,4)=h(1:36,5)+h(1:36,6)
      v(1:36,5)=h(1:36,5)+h(1:36,7)
      v(1:36,6)=h(1:36,5)+h(1:36,8)
 
c      do i=1,36
c          write(7,'(4i2,8f5.1)') (chr(i,j),j=1,4),(v(i,j),j=1,6)      
c      enddo
      

      do j=9,14                       
      nvar=1
      CALL startup(nvar, fit_const)          ! Initializes the QR-factorization

!     Read in the data, one line at a time, and progressively update the
!     QR-factorization.

      wt = 1.0d0

      DO i= 1,36
        xx(0) = 1.0d0                                              
        xx(1) = v(i,j-8)                                               
        yy = trait(i,itt)
        CALL includ(wt, xx, yy)
      END DO
      nreq = 2                               
      CALL regcf(beta, nreq, ifault)

      CALL ss()
      var = rss(nreq) / (nobs - nreq)
      CALL cov(nreq, var, covmat, max_cdim, sterr, ifault)

!     Calculate t-values

      t(0:nreq-1) = beta(0:nreq-1) / sterr(0:nreq-1)
      adjr2(j) = 100.0*(1.0d0 - ((rss(nreq)/(nobs-nreq)) / 
     & (rss(1)/(nobs-1.0d0))) )
      sic(j) = 36.0*log(var)+2.0*log(36.0)

!     Output regression table, residual sums of squares, and R-squared.
      tterm(2)=model(j)
      WRITE(7, *)
      WRITE(7, '(a,1x,a8)')  'Model coefficients ', model(j)
      WRITE(7, *) 'Term      Reg.coeff.    Std.error     t-value'  
      DO i = 0, nreq-1
        WRITE(7, 900) tterm(i+1),beta(i), sterr(i), t(i)!, rss(i+1)
      END DO
c      WRITE(7, *)
c      WRITE(7, *) 'Model     Adj R^2   SIC'
c      WRITE(7, '(a8,2x,f8.3,2x, f8.3)')  model(j),adjr2(j),sic(j)
      enddo

c     duplex as codominant factor
      model(15)='F12'
      model(16)='F13'
      model(17)='F14'
      model(18)='F56'
      model(19)='F57'
      model(20)='F58'
      
      do i=1,36
          do kk=1,6
            if (v(i,kk) .eq. 1) fv(i,kk,1) = 1.0d0
            if (v(i,kk) .eq. 2) fv(i,kk,2) = 1.0d0
          enddo
      enddo

      do j=15,20                       
      nvar=2
      CALL startup(nvar, fit_const)          ! Initializes the QR-factorization

!     Read in the data, one line at a time, and progressively update the
!     QR-factorization.

      wt = 1.0d0

      DO i= 1,36
        xx(0) = 1.0d0                                              
        xx(1) = fv(i,j-14,1)
        xx(2) = fv(i,j-14,2)
        yy = trait(i,itt)
        CALL includ(wt, xx, yy)
      END DO
      nreq = 3                               
      CALL regcf(beta, nreq, ifault)

      CALL ss()
      var = rss(nreq) / (nobs - nreq)
      CALL cov(nreq, var, covmat, max_cdim, sterr, ifault)

!     Calculate t-values

      t(0:nreq-1) = beta(0:nreq-1) / sterr(0:nreq-1)
      adjr2(j) = 100.0*(1.0d0 - ((rss(nreq)/(nobs-nreq)) / 
     & (rss(1)/(nobs-1.0d0))) )
      sic(j) = 36.0*log(var)+3.0*log(36.0)

!     Output regression table, residual sums of squares, and R-squared.
      tterm(2)=trim(model(j)) // "_1"
      tterm(3)=trim(model(j)) // "_2"

      WRITE(7, *)
      WRITE(7, '(a,1x,a8)')  'Model coefficients ', model(j)
      WRITE(7, *) 'Term      Reg.coeff.    Std.error     t-value'  
      DO i = 0, nreq-1
        WRITE(7, 900) tterm(i+1),beta(i), sterr(i), t(i)!, rss(i+1)
      END DO
c      WRITE(7, *)
c      WRITE(7, *) 'Model     Adj R^2   SIC'
c      WRITE(7, '(a8,2x,f8.3,2x, f8.3)')  model(j),adjr2(j),sic(j)
      enddo

      
c     dominant duplex
      model(21)='D12'
      model(22)='D13'
      model(23)='D14'
      model(24)='D23'
      model(25)='D24'
      model(26)='D34'
      model(27)='D56'
      model(28)='D57'
      model(29)='D58'
      model(30)='D67'
      model(31)='D68'
      model(32)='D78'
      dm=0.0d0
      do i=1,36
          if (h(i,1) .eq. 1.0d0 .or. h(i,2) .eq. 1.0d0) dm(i,1)=1.0d0
          if (h(i,1) .eq. 1.0d0 .or. h(i,3) .eq. 1.0d0) dm(i,2)=1.0d0
          if (h(i,1) .eq. 1.0d0 .or. h(i,4) .eq. 1.0d0) dm(i,3)=1.0d0
          if (h(i,2) .eq. 1.0d0 .or. h(i,3) .eq. 1.0d0) dm(i,4)=1.0d0
          if (h(i,2) .eq. 1.0d0 .or. h(i,4) .eq. 1.0d0) dm(i,5)=1.0d0
          if (h(i,3) .eq. 1.0d0 .or. h(i,4) .eq. 1.0d0) dm(i,6)=1.0d0
          if (h(i,5) .eq. 1.0d0 .or. h(i,6) .eq. 1.0d0) dm(i,7)=1.0d0
          if (h(i,5) .eq. 1.0d0 .or. h(i,7) .eq. 1.0d0) dm(i,8)=1.0d0
          if (h(i,5) .eq. 1.0d0 .or. h(i,8) .eq. 1.0d0) dm(i,9)=1.0d0
          if (h(i,6) .eq. 1.0d0 .or. h(i,7) .eq. 1.0d0) dm(i,10)=1.0d0
          if (h(i,6) .eq. 1.0d0 .or. h(i,8) .eq. 1.0d0) dm(i,11)=1.0d0
          if (h(i,7) .eq. 1.0d0 .or. h(i,8) .eq. 1.0d0) dm(i,12)=1.0d0
      enddo

c      do i=1,36
c          write(7,'(4i2,8f5.1)') (chr(i,j),j=1,4),(h(i,j),j=1,8)      
c      enddo
      

      do j=21,32                       
      nvar=1
      CALL startup(nvar, fit_const)          ! Initializes the QR-factorization

!     Read in the data, one line at a time, and progressively update the
!     QR-factorization.

      wt = 1.0d0

      DO i= 1,36
        xx(0) = 1.0d0                                              
        xx(1) = dm(i,j-20)                                               
        yy = trait(i,itt)
        CALL includ(wt, xx, yy)
      END DO
      nreq = 2                               
      CALL regcf(beta, nreq, ifault)

      CALL ss()
      var = rss(nreq) / (nobs - nreq)
      CALL cov(nreq, var, covmat, max_cdim, sterr, ifault)

!     Calculate t-values

      t(0:nreq-1) = beta(0:nreq-1) / sterr(0:nreq-1)
      adjr2(j) = 100.0*(1.0d0 - ((rss(nreq)/(nobs-nreq)) / 
     & (rss(1)/(nobs-1.0d0))) )
      sic(j) = 36.0*log(var)+2.0*log(36.0)

!     Output regression table, residual sums of squares, and R-squared.
      tterm(2)=model(j) 
      WRITE(7, *)
      WRITE(7, '(a,1x,a8)')  'Model coefficients ', model(j)
      WRITE(7, *) 'Term      Reg.coeff.    Std.error     t-value'  
      DO i = 0, nreq-1
        WRITE(7, 900) tterm(i+1),beta(i), sterr(i), t(i)!, rss(i+1)
      END DO
c      WRITE(7, *)
c      WRITE(7, *) 'Model     Adj R^2   SIC'
c     WRITE(7, '(a8,2x,f8.3,2x, f8.3)')  model(j),adjr2(j),sic(j)
      enddo

c     double simplex as codominant variate
      model(33)='S15'
      model(34)='S16'
      model(35)='S17'
      model(36)='S18'
      model(37)='S25'
      model(38)='S26'
      model(39)='S27'
      model(40)='S28'
      model(41)='S35'
      model(42)='S36'
      model(43)='S37'
      model(44)='S38'
      model(45)='S45'
      model(46)='S46'
      model(47)='S47'
      model(48)='S48'
      s(1:36,1)=h(1:36,1)+h(1:36,5)
      s(1:36,2)=h(1:36,1)+h(1:36,6)
      s(1:36,3)=h(1:36,1)+h(1:36,7)
      s(1:36,4)=h(1:36,1)+h(1:36,8)
      s(1:36,5)=h(1:36,2)+h(1:36,5)
      s(1:36,6)=h(1:36,2)+h(1:36,6)
      s(1:36,7)=h(1:36,2)+h(1:36,7)
      s(1:36,8)=h(1:36,2)+h(1:36,8)
      s(1:36,9)=h(1:36,3)+h(1:36,5)
      s(1:36,10)=h(1:36,3)+h(1:36,6)
      s(1:36,11)=h(1:36,3)+h(1:36,7)
      s(1:36,12)=h(1:36,3)+h(1:36,8)
      s(1:36,13)=h(1:36,4)+h(1:36,5)
      s(1:36,14)=h(1:36,4)+h(1:36,6)
      s(1:36,15)=h(1:36,4)+h(1:36,7)
      s(1:36,16)=h(1:36,4)+h(1:36,8)

c      do i=1,36
c          write(7,'(4i2,16f5.1)') (chr(i,j),j=1,4),(s(i,j),j=1,16)      
c      enddo
      

      do j=33,48                       
      nvar=1
      CALL startup(nvar, fit_const)          ! Initializes the QR-factorization

!     Read in the data, one line at a time, and progressively update the
!     QR-factorization.

      wt = 1.0d0

      DO i= 1,36
        xx(0) = 1.0d0                                              
        xx(1) = s(i,j-32)                                               
        yy = trait(i,itt)
        CALL includ(wt, xx, yy)
      END DO
      nreq = 2                               
      CALL regcf(beta, nreq, ifault)

      CALL ss()
      var = rss(nreq) / (nobs - nreq)
      CALL cov(nreq, var, covmat, max_cdim, sterr, ifault)

!     Calculate t-values

      t(0:nreq-1) = beta(0:nreq-1) / sterr(0:nreq-1)
      adjr2(j) = 100.0*(1.0d0 - ((rss(nreq)/(nobs-nreq)) / 
     & (rss(1)/(nobs-1.0d0))) )
      sic(j) = 36.0*log(var)+2.0*log(36.0)

!     Output regression table, residual sums of squares, and R-squared.
      tterm(2)=model(j)

      WRITE(7, *)
      WRITE(7, '(a,1x,a8)')  'Model coefficients ', model(j)
      WRITE(7, *) 'Term      Reg.coeff.    Std.error     t-value'  
      DO i = 0, nreq-1
        WRITE(7, 900) tterm(i+1),beta(i), sterr(i), t(i)!, rss(i+1)
       END DO
c      WRITE(7, *)
c      WRITE(7, *) 'Model     Adj R^2   SIC'
c      WRITE(7, '(a8,2x,f8.3,2x, f8.3)')  model(j),adjr2(j),sic(j)
      enddo
c     double simplex as codominant factor
      model(49)='FS15'
      model(50)='FS16'
      model(51)='FS17'
      model(52)='FS18'
      model(53)='FS25'
      model(54)='FS26'
      model(55)='FS27'
      model(56)='FS28'
      model(57)='FS35'
      model(58)='FS36'
      model(59)='FS37'
      model(60)='FS38'
      model(61)='FS45'
      model(62)='FS46'
      model(63)='FS47'
      model(64)='FS48'

       do i=1,36
          do kk=1,16
            if (s(i,kk) .eq. 1) fs(i,kk,1) = 1.0d0
            if (s(i,kk) .eq. 2) fs(i,kk,2) = 1.0d0
          enddo
      enddo

      do j=49,64
        nvar=2
      CALL startup(nvar, fit_const)          ! Initializes the QR-factorization

!     Read in the data, one line at a time, and progressively update the
!     QR-factorization.

      wt = 1.0d0

      DO i= 1,36
        xx(0) = 1.0d0                                              
        xx(1) = fs(i,j-48,1)
        xx(2) = fs(i,j-48,2)
        yy = trait(i,itt)
        CALL includ(wt, xx, yy)
      END DO
      nreq = 3                               
      CALL regcf(beta, nreq, ifault)

      CALL ss()
      var = rss(nreq) / (nobs - nreq)
      CALL cov(nreq, var, covmat, max_cdim, sterr, ifault)

!     Calculate t-values

      t(0:nreq-1) = beta(0:nreq-1) / sterr(0:nreq-1)
      adjr2(j) = 100.0*(1.0d0 - ((rss(nreq)/(nobs-nreq)) / 
     & (rss(1)/(nobs-1.0d0))) )
      sic(j) = 36.0*log(var)+3.0*log(36.0)

!     Output regression table, residual sums of squares, and R-squared.
      tterm(2)=trim(model(j)) // "_1"
      tterm(3)=trim(model(j)) // "_2"

      WRITE(7, *)
      WRITE(7, '(a,1x,a8)')  'Model coefficients ', model(j)
      WRITE(7, *) 'Term      Reg.coeff.    Std.error     t-value'  
      DO i = 0, nreq-1
        WRITE(7, 900) tterm(i+1),beta(i), sterr(i), t(i)!, rss(i+1)
      END DO
c      WRITE(7, *)
c      WRITE(7, *) 'Model     Adj R^2   SIC'
c      WRITE(7, '(a8,2x,f8.3,2x, f8.3)')  model(j),adjr2(j),sic(j)
      enddo
      
c     double simplex as dominant 
      model(65)='T15'
      model(66)='T16'
      model(67)='T17'
      model(68)='T18'
      model(69)='T25'
      model(70)='T26'
      model(71)='T27'
      model(72)='T28'
      model(73)='T35'
      model(74)='T36'
      model(75)='T37'
      model(76)='T38'
      model(77)='T45'
      model(78)='T46'
      model(79)='T47'
      model(80)='T48'
      tm=0.0d0
      do i=1,36
          if (h(i,1) .eq. 1.0d0 .or. h(i,5) .eq. 1.0d0) tm(i,1)=1.0d0
          if (h(i,1) .eq. 1.0d0 .or. h(i,6) .eq. 1.0d0) tm(i,2)=1.0d0
          if (h(i,1) .eq. 1.0d0 .or. h(i,7) .eq. 1.0d0) tm(i,3)=1.0d0
          if (h(i,1) .eq. 1.0d0 .or. h(i,8) .eq. 1.0d0) tm(i,4)=1.0d0
          if (h(i,2) .eq. 1.0d0 .or. h(i,5) .eq. 1.0d0) tm(i,5)=1.0d0
          if (h(i,2) .eq. 1.0d0 .or. h(i,6) .eq. 1.0d0) tm(i,6)=1.0d0
          if (h(i,2) .eq. 1.0d0 .or. h(i,7) .eq. 1.0d0) tm(i,7)=1.0d0
          if (h(i,2) .eq. 1.0d0 .or. h(i,8) .eq. 1.0d0) tm(i,8)=1.0d0
          if (h(i,3) .eq. 1.0d0 .or. h(i,5) .eq. 1.0d0) tm(i,9)=1.0d0
          if (h(i,3) .eq. 1.0d0 .or. h(i,6) .eq. 1.0d0) tm(i,10)=1.0d0
          if (h(i,3) .eq. 1.0d0 .or. h(i,7) .eq. 1.0d0) tm(i,11)=1.0d0
          if (h(i,3) .eq. 1.0d0 .or. h(i,8) .eq. 1.0d0) tm(i,12)=1.0d0
          if (h(i,4) .eq. 1.0d0 .or. h(i,5) .eq. 1.0d0) tm(i,13)=1.0d0
          if (h(i,4) .eq. 1.0d0 .or. h(i,6) .eq. 1.0d0) tm(i,14)=1.0d0
          if (h(i,4) .eq. 1.0d0 .or. h(i,7) .eq. 1.0d0) tm(i,15)=1.0d0
          if (h(i,4) .eq. 1.0d0 .or. h(i,8) .eq. 1.0d0) tm(i,16)=1.0d0
      enddo

c      do i=1,36
c          write(7,'(4i2,8f5.1)') (chr(i,j),j=1,4),(h(i,j),j=1,8)      
c      enddo
      

      do j=65,80                       
      nvar=1
      CALL startup(nvar, fit_const)          ! Initializes the QR-factorization

!     Read in the data, one line at a time, and progressively update the
!     QR-factorization.

      wt = 1.0d0

      DO i= 1,36
        xx(0) = 1.0d0                                              
        xx(1) = tm(i,j-64)                                               
        yy = trait(i,itt)
        CALL includ(wt, xx, yy)
      END DO
      nreq = 2                               
      CALL regcf(beta, nreq, ifault)

      CALL ss()
      var = rss(nreq) / (nobs - nreq)
      CALL cov(nreq, var, covmat, max_cdim, sterr, ifault)

!     Calculate t-values

      t(0:nreq-1) = beta(0:nreq-1) / sterr(0:nreq-1)
      adjr2(j) = 100.0*(1.0d0 - ((rss(nreq)/(nobs-nreq)) / 
     & (rss(1)/(nobs-1.0d0))) )
      sic(j) = 36.0*log(var)+2.0*log(36.0)

!     Output regression table, residual sums of squares, and R-squared.
      tterm(2)=model(j)

      WRITE(7, *)
      WRITE(7, '(a,1x,a8)')  'Model coefficients ', model(j)
      WRITE(7, *) 'Term      Reg.coeff.    Std.error     t-value'  
      DO i = 0, nreq-1
        WRITE(7, 900) tterm(i+1),beta(i), sterr(i), t(i)!, rss(i+1)
      END DO
c      WRITE(7, *)
c      WRITE(7, *) 'Model     Adj R^2   SIC'
c      WRITE(7, '(a8,2x,f8.3,2x, f8.3)')  model(j),adjr2(j),sic(j)
      enddo
      
c     h1+h5 for maturity loc
c      do j=81,81
c      model(81)='H1+H5'
c      nvar=2
c      CALL startup(nvar, fit_const)          ! Initializes the QR-factorization

!     Read in the data, one line at a time, and progressively update the
!     QR-factorization.

c      wt = 1.0d0
c      DO i= 1,36
c        xx(0) = 1.0d0                                              
c        xx(1) = h(i,1)                                               
c        xx(2) = h(i,5)                                               
c        yy = trait(i,itt)
c        CALL includ(wt, xx, yy)
c      END DO
c      WRITE(7, *)'No. of observations =', nobs
c      nreq = 3                               
c      CALL regcf(beta, nreq, ifault)

c      CALL ss()
c      var = rss(nreq) / (nobs - nreq)
c      CALL cov(nreq, var, covmat, max_cdim, sterr, ifault)

!     Calculate t-values

c      t(0:nreq-1) = beta(0:nreq-1) / sterr(0:nreq-1)
c      adjr2(j) = 100.0*(1.0d0 - ((rss(nreq)/(nobs-nreq)) / 
c     & (rss(1)/(nobs-1.0d0))) )
c      sic(j) = 36.0*log(var)+3.0*log(36.0)

!     Output regression table, residual sums of squares, and R-squared.
c      tterm(2)="h1"
c      tterm(3)="h5"


c      WRITE(7, *)
c      WRITE(7, '(a8)')  model(j)
c      WRITE(7, *) 'Term      Regn.coeff.   Std.error  t-value   
c     & Res.sum of sq.'
c      DO i = 0, nreq-1
c        WRITE(7, 900) tterm(i+1),beta(i), sterr(i), t(i), rss(i+1)
c      END DO
c      WRITE(7, *)
c      WRITE(7, *) 'Model     Adj R^2   SIC'
c      WRITE(7, '(a8,2x,f8.3,2x, f8.3)')  model(j),adjr2(j),sic(j)
c      enddo
     
      
c     full model
      do j=81,81
      model(81)='Full'
      nvar=6
      CALL startup(nvar, fit_const)          ! Initializes the QR-factorization

!     Read in the data, one line at a time, and progressively update the
!     QR-factorization.

      wt = 1.0d0

      DO i= 1,36
        xx(0) = 1.0d0                                              
        xx(1) = h(i,2)                                               
        xx(2) = h(i,3)                                               
        xx(3) = h(i,4)                                               
        xx(4) = h(i,6)                                               
        xx(5) = h(i,7)                                               
        xx(6) = h(i,8)                                               
        yy = trait(i,itt)
        CALL includ(wt, xx, yy)
      END DO
c      WRITE(7, *)'No. of observations =', nobs
      nreq = 7                               
      CALL regcf(beta, nreq, ifault)

      CALL ss()
      var = rss(nreq) / (nobs - nreq)
      CALL cov(nreq, var, covmat, max_cdim, sterr, ifault)

!     Calculate t-values

      t(0:nreq-1) = beta(0:nreq-1) / sterr(0:nreq-1)
      adjr2(j) = 100.0*(1.0d0 - ((rss(nreq)/(nobs-nreq)) / 
     & (rss(1)/(nobs-1.0d0))) )
      sic(j) = 36.0*log(var)+7.0*log(36.0)

!     Output regression table, residual sums of squares, and R-squared.
      tterm(2)="M2"
      tterm(3)="M3"
      tterm(4)="M4"
      tterm(5)="M6"
      tterm(6)="M7"
      tterm(7)="M8"

      WRITE(7, *)
      WRITE(7, '(a,1x,a8)')  'Model coefficients ', model(j)
      WRITE(7, *) 'Term      Reg.coeff.    Std.error     t-value'  
      DO i = 0, nreq-1
        WRITE(7, 900) tterm(i+1),beta(i), sterr(i), t(i)!, rss(i+1)
      END DO
c      WRITE(7, *)
c      WRITE(7, *) 'Model     Adj R^2   SIC'
c      WRITE(7, '(a8,2x,f8.3,2x, f8.3)')  model(j),adjr2(j),sic(j)
      enddo
       do j=1,80
        jorder(j)=j
      enddo
     
   
      call sort_2(80,sic,jorder)
      temp(1:80)=adjr2(1:80)  
 	do i=1,80
     	  adjr2(i) = temp(jorder(i))
	end do
      tempmodel(1:80)=model(1:80)  
 	do i=1,80
     	  model(i) = tempmodel(jorder(i))
      end do
      write(7,*)
      write(7,*) 'Sorted data'
      do j=1,81
       WRITE(7, '(a8,2x,f8.3,2x, f8.3)')  model(j),adjr2(j),sic(j)
      enddo

      write(7,*)
      write(7,*) 'For display  ',traitlab(itt)
      WRITE(7, *) 'Model     Adj R^2   SIC'
      do j=1,6
       WRITE(7, '(a8,2x,f8.3,2x, f8.3)')  model(j),adjr2(j),sic(j)
      enddo
      do j=81,81
       WRITE(7, '(a8,2x,f8.3,2x, f8.3)')  model(j),adjr2(j),sic(j)
      enddo

      write(7,*)
      write (*,*) "Analyse another trait (y/n)?"
	read (*,'(a)') tch
      if (tch .eq. 'y') then
         write (7,*) tch
         go to 1000
      endif
      

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
      
      
      
