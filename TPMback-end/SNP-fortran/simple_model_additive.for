c     program to fit simpler models to means from QTL analysis
c     4/11/15 This is adaptation of simplemodel.for to let user choose and fit additive pairs such as H1 + H5      

      USE lsq


	parameter(mxtrt=400,maxvar=7,max_cdim = maxvar*(maxvar+1)/2)

	implicit double precision (a-h,o-z)
			
	character stem*12,infile1*16,outfile1*23
	character traitlab(mxtrt)*20,dum(4)*2,model(2)*8,tch*1
      character tterm(7)*8,hh(2)*1
	integer chr(36,4)
	real(8) trait(36,mxtrt),h(36,8),v(36,6),dm(36,12),s(36,16),tm(36,16)
      real(8) fv(36,6,2),fs(36,16,2)

      LOGICAL            :: fit_const
      real(8) wt,xx(0:maxvar),yy, beta(0:maxvar),var, covmat(max_cdim),      
     &     sterr(0:maxvar), cormat(max_cdim), ycorr(maxvar),t(0:maxvar)
      real(8) adjr2(82),sic(82),temp(82)
      
	write (*,*) "Input file name (.qmm),max 12 characters"
	read (*,"(a)") stem

		
	infile1 = trim(stem) // ".qmm"
	outfile1 = trim(stem)// "qmm_add.out"

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

      write (*,*) "Choose first allele (1-8)"
	read (*,'(i3)') iall1
2000  write (*,*) "Choose second allele (1-8)"
	read (*,'(i3)') iall2
      if (iall1 .eq. iall2) then
          write(*,*) "Second allele must be different from first"
          goto 2000
      endif
 
      select case (iall1)
      case(1)
          hh(1)="1"
      case(2)
          hh(1)="2"
      case(3)
          hh(1)="3"
      case(4)
          hh(1)="4"
      case(5)
          hh(1)="5"
      case(6)
          hh(1)="6"
      case(7)
          hh(1)="7"
      case(8)
          hh(1)="8"
      end select   
      
      select case (iall2)
      case(1)
          hh(2)="1"
      case(2)
          hh(2)="2"
      case(3)
          hh(2)="3"
      case(4)
          hh(2)="4"
      case(5)
          hh(2)="5"
      case(6)
          hh(2)="6"
      case(7)
          hh(2)="7"
      case(8)
          hh(2)="8"
      end select   
        
      tterm(1)='Constant'
      model(1)='H'// hh(1) //'+H'// hh(2)
      nvar=2
      CALL startup(nvar, fit_const)          ! Initializes the QR-factorization

!     Read in the data, one line at a time, and progressively update the
!     QR-factorization.

      wt = 1.0d0
      DO i= 1,36
        xx(0) = 1.0d0                                              
        xx(1) = h(i,iall1)                                               
        xx(2) = h(i,iall2)                                               
        yy = trait(i,itt)
        CALL includ(wt, xx, yy)
      END DO
c      WRITE(7, *)'No. of observations =', nobs
      nreq = 3                               
      CALL regcf(beta, nreq, ifault)

      CALL ss()
      var = rss(nreq) / (nobs - nreq)
      CALL cov(nreq, var, covmat, max_cdim, sterr, ifault)

!     Calculate t-values

      t(0:nreq-1) = beta(0:nreq-1) / sterr(0:nreq-1)
      adjr2(1) = 100.0*(1.0d0 - ((rss(nreq)/(nobs-nreq)) / 
     & (rss(1)/(nobs-1.0d0))) )
      sic(1) = 36.0*log(var)+3.0*log(36.0)

!     Output regression table, residual sums of squares, and R-squared.
      tterm(2)='H'// hh(1)
      tterm(3)='H'// hh(2)


      WRITE(7, *)
      WRITE(7, '(a,1x,a8)')  'Model coefficients ',model(1)
      WRITE(7, *) 'Term      Reg.coeff.    Std.error     t-value'
      DO i = 0, nreq-1
        WRITE(7, 900) tterm(i+1),beta(i), sterr(i), t(i)!, rss(i+1)
  900 FORMAT(a8,2x,g12.4, 2x, g11.4, 2x, f7.2)
      END DO
      WRITE(7, *)
      write(7,*) 'For display  ',traitlab(itt)
      WRITE(7, *) 'Model     Adj R^2   SIC'
      WRITE(7, '(a8,2x,f8.3,2x, f8.3)')  model(1),adjr2(1),sic(1)
     
      end



      
