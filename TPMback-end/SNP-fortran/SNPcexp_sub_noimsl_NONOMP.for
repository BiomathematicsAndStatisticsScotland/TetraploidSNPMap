c	*************************************************
c     cmapsub2.for ---- subroutines used for cmap.for.
c	*******************************************************
	subroutine gtypepred(n,p1,p2,o,alpha,odds,nt,cgtp,prob,fulloutputl)

c	to predict parental genotypes from their phenotypes and phenotypes
c	of their offspring in a tetrasomic inheritance model. 
c	This program takes double reduction into account.

	implicit double precision (a-h,o-z)
      logical fulloutputl
	integer p1,p2,o(n),work(n),g1(4),g2(4)
	integer wkb(100),t1(4),t2(4),cgtp(8)

	dimension c(100),xlkhd(17),slkhd(17),ipem(17)

	do i=1,n
	  work(i)=o(i)
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

	alpha=0.0d0
	call SNPbandist(n,alpha,p1,p2,nob,wkb,c,nt,cgtp,prob,fulloutputl)

	return
      end


      
c	***************************************************************
	subroutine SNPbandist(n,alpha,p1,p2,nob,wkb,c,nt,cgtp,chiprob,
     & fulloutputl)

	implicit double precision (a-h,o-z)
      logical fulloutputl

	integer g1(4),g2(4),p1gt(10,2),p2gt(10,2),zygote(100,4)
	integer p1,p2,bptn(100),genotype(8),phenotype(8)
	integer o(n),cgtp(8),nptn(100),wkb(100)

	dimension mtx1(6,6),mtx2(6,6),p1gtfq(10),p2gtfq(10),freq(100)
	dimension prior(100),xlkhd(100),chisqr(100),probt(100),c(100)

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

	ic=1
	do k=1,4
	  cgtp(k)=g1(k)
	  cgtp(4+k)=g2(k)
	end do
	   call gametogensis(alpha,g1,ngt1,p1gt,p1gtfq)
	   call gametogensis(alpha,g2,ngt2,p2gt,p2gtfq)
	   call zygt(p1gt,p2gt,p1gtfq,p2gtfq,ngt1,ngt2,ntype,zygote,freq)
	   call SNPgtob(ntype,zygote,freq,np,bptn)
	   nptn(ic)=np
	   call SNPptsort(n,np,bptn,freq,nob,wkb,c,x2,chiprob,fulloutputl)
	   chisqr(ic)=x2

10	format(1x,a,2x,a)
20	format(5x,4(i1),2x,4(i1),4x,f15.12,4x,i2,5x,f8.2)

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

	dimension ffg(10),fmg(10),fwk(100),freq(100)

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
	  
	return
	end
c	*****************************************
	subroutine SNPgtob(n,gtype,freq,ntype,ptype)

c	to turn the genotypes into bands

	implicit double precision (a-h,o-z)

	integer gtype(100,4),ptype(100),tempg(4),tempp(8),work(100)

	dimension freq(100),fwk(100)

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

	return
	end
c	***************************************************
	subroutine SNPptsort(n,np,bptn,freq,nob,wkb,c,x2,chiprob,fulloutputl)

	implicit double precision (a-h,o-z)
      logical fulloutputl

	integer o(n,8),bptn(100),wkb(100)

	dimension freq(100),c(100),ct(100)
      
      integer omp_get_thread_num, tnum
      
      tnum = 0
c      tnum = omp_get_thread_num()
	
	ndf=0
	do i=1,nob !I replaced np by nob on the 1st of Feb
	  idf=1
	  do j=1,np
		dif=1.0d0*abs(wkb(i)-bptn(j))
		if (dif.eq.0.0d0) idf=0
	  end do
	  ndf=ndf+idf
	end do

	if (ndf.gt.0) then
	  x2=-99.0d0 	    
	else
	  x2=0.0d0
	  do i=1,np
	    ct(i)=0
	    do j=1,nob
		  dif=dabs(1.0d0*(bptn(i)-wkb(j)))
		  if (dif.eq.0.0d0) ct(i)=c(j)
	    end do
	    x2=x2+(n*freq(i)-ct(i))**2/(n*freq(i))
	  end do
        df=1.0d0*np-1.0d0
        ifault=0
	  chiprob=1.0d0-CHISQN(x2, df,ifault)
        if(fulloutputl) then
      	write(10 + 3 * tnum,
     &      '(f5.2,2x,i4,2x,f5.2,2x,f6.4)') 
     &     x2,np,df,chiprob
        end if
      
c	  prob=pmltnr(n,np,c,freq)
	end if

	return
      end	  
      
c	*****************************************************
	subroutine linkage(n,nn,pt,g1,g2,lname,mloci,rf,lod, fulloutputl)

	implicit double precision (a-h,o-z)

      logical fulloutputl
	integer g1(2,4),g2(2,4),gp1(24,4,2),gp2(24,4,2),pt(24,4)
	integer o(nn,2),stgel(2000,2),ocount(nn)
	integer gelp(nn,2),wkgel(nn,2),optype
	integer mloci(n,2) !,p1gtp(mxloci,4),p2gtp(mxloci,4)
	integer iperm(576),vi(576),vj(576),vvi(576),vvj(576)	
	double precision :: lod,vr(576),vyL1(576),vyLd(576),vyL0(576)
	double precision ::vchi(576),lod_next,lod_diff,temp(576)
	dimension ncount(2000),nfreq(2000,6),g(6)
	character lname(2)*30
      
      integer omp_get_thread_num, tnum
      
c      tnum = omp_get_thread_num()
      tnum = 0
      
      mloc1=1 !pid(loc1)
	mloc2=2 !pid(loc2)          

	do i=1,nn
	  o(i,1)=mloci(i,mloc1)
	  o(i,2)=mloci(i,mloc2)
      end do
	    
	call phasen(g1,g2,pt,n1,n2,gp1,gp2)

	do i=1,nn
	  do j=1,2
		wkgel(i,j)=o(i,j)
	  end do
      end do

	call paternsort(nn,wkgel,optype,gelp,ocount)
      if(fulloutputl) then
	    write(10 + 3 * tnum,*) 'Observed counts'
	    do i1=1,optype
	     write(10 + 3 * tnum,'(1x,i2,2x,i2,2x,i3)') 
     &     (gelp(i1,k),k=1,2),ocount(i1)
          end do
      end if
      


	icc=0
	it=1
	ips=0
	yL1old=-1.0d5
	do i=1,n1
	  do k=1,4
	    do l=1,2
	      g1(l,k)=gp1(i,k,l)
		end do
	  end do
	  do j=1,n2
		do k=1,4
		  do l=1,2
			g2(l,k)=gp2(j,k,l)
		  end do
		end do
		ips=ips+1
	    call offspring(g1,g2,pt,ntp,stgel,nfreq)
	    call patterncomp(nn,ntp,stgel,optype,gelp,ocount,ncount,ic)
	    if (ic.eq.1) then
	      call emalg(nn,ntp,ncount,nfreq,rnew,yL1new,yL0new,
     &	  yLdnew,chinew)
            iperm(it) = it
	      vi(it) = i
	      vj(it) = j
	      vr(it) = rnew
	      vyL1(it) = -1.0*yL1new
	      vyLd(it) = yLdnew
	      vyL0(it) = yL0new
	      vchi(it) = chinew
		  it=it+1
		end if
	  end do
      end do	      

      n12=n1*n2
      call sort_2(n12,vyL1,iperm)
      temp(1:n12)=vr(1:n12)  
 	do i=1,n12
     	  vr(i) = temp(iperm(i))
	end do
      temp(1:n12)=vyLd(1:n12)  
 	do i=1,n12
     	  vyLd(i) = temp(iperm(i))
	end do
      temp(1:n12)=vyL0(1:n12)  
 	do i=1,n12
     	  vyL0(i) = temp(iperm(i))
	end do
      temp(1:n12)=vchi(1:n12)  
 	do i=1,n12
     	  vchi(i) = temp(iperm(i))
	end do
    
      
	do i=1,n1*n2
	  vvi(i) = vi(iperm(i))
	  vvj(i) = vj(iperm(i))
	end do
       
	do i=1,n1*n2
	  vyL1(i) = -1.0*vyL1(i)
      end do
      if(fulloutputl) then
	    write(10 + 3 * tnum,*) 'Summary of configurations'
	    write(10 + 3 * tnum,'(1x,a)') 
     &	    'Configuration MLE      L1      L0      LOD   Chi^2'
	    do i=1,min(3,n1*n2)
	        write(10+3*tnum,'(10x,i3,2x,f5.3,3f8.2,f7.2)') 
     &        iperm(i),vr(i),
     &	    vyL1(i),vyL0(i),vyLd(i),vchi(i)
          end do
      end if
      

	do i=1,n1*n2
	  if (vr(i) .le. 0.5) then
	    ii = i
	    exit
	  end if
	end do

	ij=0
	do i=ii+1,n1*n2
	  if (vr(i) .le. 0.5) then
	    ij = i
	    exit
	  end if
	end do


	rf = vr(ii)
	lod = vyLd(ii)
	if (ij .gt. 0) then
	  rf_next = vr(ij)
	  lod_next = vyLd(ij)
	  lod_diff=lod-lod_next
	else
	  rf_next = 0.9
	  lod_next = 0.0
	  lod_diff=0.0
	endif

	write(8 + 3 * tnum,*)
	write(8 + 3 * tnum,'(1x,a,i4)') 
     &	'The most likely phase with r < 0.5: configuration ',iperm(ii)
      if(fulloutputl) then
      	write(10+ 3 * tnum,*)
	    write(10+ 3 * tnum,'(1x,a,i4)') 
     &	'The most likely phase with r < 0.5: configuration ',iperm(ii)
      end if
	do l=1,2
	  write(8 + 3 * tnum,'(a,1x,4i1,2x,4i1)')
     &   lname(l),(gp1(vvi(ii),k,l),k=1,4),  (gp2(vvj(ii),k,l),k=1,4)
        if(fulloutputl) then
	    write(10+ 3 * tnum,'(a,1x,4i1,2x,4i1)')
     &    lname(l),(gp1(vvi(ii),k,l),k=1,4),  (gp2(vvj(ii),k,l),k=1,4)
        end if
        
      end do
      if(fulloutputl) then
	    write(10+ 3 * tnum,*)
      end if
      
	write(8 + 3 * tnum,*)
	write(9 + 3 * tnum,
     & '(1x,a,2x,a,2x,f7.4,2x,f10.2,a3,<2>(2x,4i1,4i1),2x,f7.4,
     &  2x,f10.2)') 
     &  lname(1),lname(2),rf,lod,' ; ',(gp1(vvi(ii),k,1),k=1,4),
     &	  (gp2(vvj(ii),k,1),k=1,4),(gp1(vvi(ii),k,2),k=1,4),
     &	  (gp2(vvj(ii),k,2),k=1,4),lod_diff,rf_next


10	format(1x,4i2,2x,4i2)
20	format(1x,8i2,2x,8i2,2x,i3)
30	format(1x,a)
40	format(1x,f6.4,3f8.2,i10,f15.2)

	return
	end

c	*****************************************
	subroutine phasen(g1,g2,pt,n1,n2,gp1,gp2)

c	to work out the number of all possible parental configurations
c	with various linkage phases.

	integer g1(2,4),g2(2,4),wk(24,4,2),gp1(24,4,2),gp2(24,4,2)
	integer pt(24,4),p1(4,2),p2(4,2)

	do i=1,24
	  do j=1,4
		wk(i,j,1)=g1(1,j)
	    wk(i,j,2)=g1(2,pt(i,j))
	  end do
	end do

	n1=0
	
	do i=1,24
	  do while (wk(i,1,1).ne.99)
	    n1=n1+1
		do j=1,4
	      do k=1,2
			gp1(n1,j,k)=wk(i,j,k)
			p1(j,k)=wk(i,j,k)
		  end do
	    end do
		do j=i+1,24
	      do k=1,4
			do l=1,2
			  p2(k,l)=wk(j,k,l)
			end do
	      end do
		  call comp(p1,p2,pt,ic)
		  if (ic.eq.0) wk(j,1,1)=99
		end do
	    wk(i,1,1)=99
	  end do !while
	end do
		  
	do i=1,24
	  do j=1,4
		wk(i,j,1)=g2(1,j)
	    wk(i,j,2)=g2(2,pt(i,j))
	  end do
	end do

	n2=0
	
	do i=1,24
	  do while (wk(i,1,1).ne.99)
	    n2=n2+1
		do j=1,4
	      do k=1,2
			gp2(n2,j,k)=wk(i,j,k)
	        p1(j,k)=wk(i,j,k)
		  end do
	    end do
		do j=i+1,24
	      do k=1,4
			do l=1,2
			  p2(k,l)=wk(j,k,l)
			end do
	      end do
		  call comp(p1,p2,pt,ic)
		  if (ic.eq.0) wk(j,1,1)=99
		end do
	    wk(i,1,1)=99
	  end do !while
	end do

	return
	end
c	****************************
	subroutine comp(g1,g2,pt,ic)

	integer g1(4,2),g2(4,2),pt(24,4)

	do i=1,24
	  idif=0
	  do j=1,4
	    do k=1,2
	      idif=idif+(g1(j,k)-g2(pt(i,j),k))**2
	    end do
	  end do
	  if (idif.eq.0) then
		ic=0
	    return
	  else
	    ic=1
	  end if
	end do

	return
	end
c	******************************************************
	subroutine emalg(n,notp,ncount,npfqt,mle,maxlike,likeul,lod,chisq)

c	to work out the recombination frequency using the EM algorithm 

      implicit double precision (a-h,o-z)

	integer ncount(notp),npfqt(2000,6)
	double precision :: tol=0.00001,ul=0.5
	double precision :: theta1,mle,maxlike,likeul,lod,chisq,theta0
	dimension cprob(notp),update(notp),cmprob(notp),cuprob(notp)

	do i=1,notp
	  cprob(i)=0.0
	end do
       
	theta0 = 0.25
      emstage: do 
	 theta1=0.0
	 call catprob(notp,npfqt,theta0,cprob)
	 do i=1,notp
	    update(i)=0.0
	    update(i)=(theta0**4)*npfqt(i,3)/36.0 +
     &	(theta0**3)*(1.0-theta0)*npfqt(i,6)/48.0 + 
     &   2.0*(theta0**2)*((1.0-theta0)**2)*(npfqt(i,5)+npfqt(i,2))/144.0
     &   + theta0*((1.0-theta0)**3)*npfqt(i,4)/144.0
          theta1=theta1+update(i)*ncount(i)/cprob(i)
    	  end do
	  theta1=theta1/(4.0d0*n)
        if (abs(theta1-theta0) .lt. tol) then
	  mle=theta1
	  exit emstage
	  else
	  theta0=theta1
	  end if
	end do emstage
      
	maxlike=0.0
	likeul=0.0
	chisq=0.0
	call catprob(notp,npfqt,mle,cmprob)
	call catprob(notp,npfqt,ul,cuprob)
	do i=1,notp
	    maxlike=maxlike+ncount(i)*log10(cmprob(i))
	    likeul=likeul+ncount(i)*log10(cuprob(i))
	    chisq=chisq+((ncount(i)-n*cmprob(i))**2)/(n*cmprob(i))
      end do

	lod=maxlike - likeul

10	format(1x,f12.4)

	return
	end
c    ***********************************************************
      subroutine catprob(ncat,npfqt,rf,cprob)

c     calculates the category probabilities for a given recombination freq
      implicit double precision (a-h,o-z)

	integer ncat,npfqt(2000,6)
	double precision :: rf
	dimension cprob(ncat)

	do i=1,ncat
	  cprob(i)=0.0
	end do
       
      do i=1,ncat
	    cprob(i)=0.0
	    cprob(i)=cprob(i)+npfqt(i,1)*((1.0-rf)**4)
          cprob(i)=cprob(i)+npfqt(i,2)*((rf**2)*((1.0-rf)**2))
          cprob(i)=cprob(i)+npfqt(i,3)*(rf**4)
          cprob(i)=cprob(i)+npfqt(i,4)*(rf*((1.0-rf)**3))
          cprob(i)=cprob(i)+npfqt(i,5)*((rf**2)*((1.0-rf)**2))
          cprob(i)=cprob(i)+npfqt(i,6)*((rf**3)*(1.0-rf))
	    cprob(i)=cprob(i)/144.0
     	end do
	 
      return
	end
c	******************************************************
	subroutine patterncomp(n,ntype,stgel,optype,gelp,ocount,ncount,ic)

c	to work out the observed distribution of offspring gel patterns
c	in comparison with a given expected distribution.

	implicit double precision (a-h,o-z)

	integer otype,wkgel(n,2),stgel(2000,2)
	integer ocount(n),gelp(n,2),optype

	dimension ncount(2000)

	do i=1,ntype
	  ncount(i)=0
	end do

	do i=1,optype
	  do j=1,2
	    wkgel(i,j)=gelp(i,j)
	  end do
	end do

      otype=0
	do i=1,ntype
	  do j=1,optype
	    if (wkgel(j,1).ne.99) then
	      idif=0
		  do k=1,2
			  idif=idif+(stgel(i,k)-wkgel(j,k))**2
		  end do
		  if (idif.eq.0) then
			ncount(i)=ncount(i)+ocount(j)
		    wkgel(j,1)=99
		  end if
		end if
	  end do
        otype=otype+ncount(i)
	end do

	if (otype .eq. n) then
	  ic=1
      else
	  ic=0
      end if

	return
	end

c	************************************************
	subroutine paternsort(n,wkgel,ntype,gelp,ncount)

c	to work out distribution of offspring gel pattern.

	implicit double precision (a-h,o-z)

	integer gelp(n,2),wkgel(n,2)

	dimension ncount(n)

	do i=1,n
	  ncount(i)=0
	  gelp(i,1)=0
	  gelp(i,2)=0
	end do

	ntype=0
	do i=1,n
	  do while (wkgel(i,1).ne.99)
	    ntype=ntype+1
	    do j=1,2
	        gelp(ntype,j)=wkgel(i,j)
		end do
	    ncount(ntype)=ncount(ntype)+1
		do j=i+1,n
	      idif=0
		  do k=1,2
			  idif=idif+(wkgel(i,k)-wkgel(j,k))**2
	      end do
		  if (idif.eq.0) then
			wkgel(j,1)=99
			ncount(ntype)=ncount(ntype)+1
		  end if
		end do
		wkgel(i,1)=99
	  end do !while
	end do

	return
      end

c	**************************************************
	subroutine offspring(g1,g2,nperm,ntp,noffpt,nfreq)

c	tetralink.for --- To calculate distribution of offspring 
c	genotypes given their parental genotypes in a two-locus
c	tetrasomic inheritance model.

c	parameter(maxind=200)

	implicit double precision (a-h,o-z)

	integer g1(2,4),g2(2,4)

	character*2 allele,p1(2,4),p2(2,4)

	dimension nperm(24,4),noffpt(2000,2),nfreq(2000,6)
	
	do i=1,2
	  do j=1,4
	    ip=g1(i,j)
	    call ptog2(ip,allele)
		p1(i,j)=allele
	    ip=g2(i,j)
	    call ptog2(ip,allele)
	    p2(i,j)=allele
	  end do
	end do

	call zygote(p1,p2,nperm,ntp,noffpt,nfreq)

	return
	end
c	**********************************************
	subroutine zygote(p1,p2,nperm,notp,nopt,npfqt)

c	to turn out distribution of the offspring genotypes 
c	for given genotypes of two parents.

c	parameter(maxind=200)

	implicit double precision (a-h,o-z)
	
	character*4 gtp1(48,2),gtp2(48,2),gt1(4),gt2(4)
	character*4 wtp(2304,4),oftp(2304,4),gmt(4)
	character*2 p1(2,4),p2(2,4),allele

	dimension nfqt1(48,3),nfqt2(48,3),nwt(2304,6),nftp(2304,6)
	dimension nperm(24,4),npfqt(2000,6),npt(2),nntp(8)
	dimension nopt(2000,2),nwtp(1000,2)
	
	call gamete(p1,nt1,gtp1,nfqt1)
	call gamete(p2,nt2,gtp2,nfqt2)

	nt=0
	do i=1,nt1
	  do j=1,nt2
		nt=nt+1
		do k=1,2
		  wtp(nt,k)=gtp1(i,k)
		  wtp(nt,2+k)=gtp2(j,k)
		end do
		nwt(nt,1)=nfqt1(i,1)*nfqt2(j,1)
		nwt(nt,2)=nfqt1(i,2)*nfqt2(j,2)
		nwt(nt,3)=nfqt1(i,3)*nfqt2(j,3)
	    nwt(nt,4)=nfqt1(i,1)*nfqt2(j,2)+nfqt1(i,2)*nfqt2(j,1)
		nwt(nt,5)=nfqt1(i,1)*nfqt2(j,3)+nfqt1(i,3)*nfqt2(j,1)
		nwt(nt,6)=nfqt1(i,2)*nfqt2(j,3)+nfqt1(i,3)*nfqt2(j,2)
	  end do
	end do

	ntype=0
	do i=1,nt
	  do while (wtp(i,1).ne.'KKKK') 
	    ntype=ntype+1
		do j=1,4
		  oftp(ntype,j)=wtp(i,j)
	      gt1(j)=wtp(i,j)
		end do
		do j=1,6
		  nftp(ntype,j)=nwt(i,j)
		end do
		do j=i+1,nt
		  do k=1,4
	        gt2(k)=wtp(j,k)
	      end do
		  call comparsion(gt1,gt2,nperm,ip)
		  if (ip.eq.0) then
			wtp(j,1)='KKKK'
			do k=1,6
			  nftp(ntype,k)=nftp(ntype,k)+nwt(j,k)
			end do
	      end if
		end do 
	    wtp(i,1)='KKKK'
	  end do !while
      end do


10    format(1x,i3,3(a4,' /'),a4,4x,6i5)


	do i=1,ntype
	    nwtp(i,1)=0
	    nwtp(i,2)=0
	  do j=1,4
	    gmt(j)=oftp(i,j)
	  end do
	  call gtop4(gmt,npt)
	  nwtp(i,1)=npt(1)
	  nwtp(i,2)=npt(2)
	end do

20	format(1x,a,i1,2x,8i2)
	
	notp=0
	do i=1,ntype
	  do while (nwtp(i,1).ne.99) 
	    notp=notp+1
		  nopt(notp,1)=nwtp(i,1)
		  nopt(notp,2)=nwtp(i,2)
		do j=1,6
		  npfqt(notp,j)=nftp(i,j)    
	    end do
	    do j=i+1,ntype
		  idif=0
	      do k=1,2
	        idif=idif+(nwtp(i,k)-nwtp(j,k))**2
	      end do
		  if (idif.eq.0) then
		    nwtp(j,1)=99
	        do k=1,6
			  npfqt(notp,k)=npfqt(notp,k)+nftp(j,k)
			end do
		  end if
		end do
		nwtp(i,1)=99
	  end do !while
	end do


30	format(1x,i2,i2,1x,i2,2x,6i5)

	return
	end
c	***************************
	subroutine ptog2(ip,allele)

	character*2 allele

	if (ip.eq.0) allele=' O'
	if (ip.eq.1) allele=' A'
	if (ip.eq.2) allele=' B'
	if (ip.eq.3) allele=' C'
	if (ip.eq.4) allele=' D'
	if (ip.eq.5) allele=' E'
	if (ip.eq.6) allele=' F'
	if (ip.eq.7) allele=' G'
	if (ip.eq.8) allele=' H'

	return
	end

c	*************************
	subroutine gtop4(gmt,npt)

	character*4 gmt(4)
	character*2 allele

	dimension npt(2)

	npt=0

	do i=1,4
	  allele=gmt(i)(1:2)
	  if (allele.eq.' B') npt(1)=npt(1)+1
	  allele=gmt(i)(3:4)
	  if (allele.eq.' B') npt(2)=npt(2)+1
	end do

	return
	end
c	*****************************************
	subroutine comparsion(gtp1,gtp2,nperm,ip)

	implicit double precision (a-h,o-z)

	integer dif

	character*4 gtp1(4),gtp2(4)

	dimension nperm(24,4)

	i=1
	ip=1
	do while (ip.ne.0.and.i.lt.24)
	  i1=nperm(i,1)
	  i2=nperm(i,2)
	  i3=nperm(i,3)
	  i4=nperm(i,4)
	  if (gtp1(1).eq.gtp2(i1).and.gtp1(2).eq.gtp2(i2).and.
     &      gtp1(3).eq.gtp2(i3).and.gtp1(4).eq.gtp2(i4)) then
		ip=0
	  end if
        i=i+1
	end do !while

	return
	end
c	******************************************
	subroutine gamete(ptype,ntype,gtype,mfreq)

c	to turn out distribution of gamete genotypes for
c	given parental genotype.

	implicit double precision (a-h,o-z)

	character*4 gtp(16,2),wtp(48,2),gtype(48,2)
	character*2 ptype(2,4),btype(2,4)
	
	dimension ifq(16,3),nfreq(48,3),mfreq(48,3)

	ntp=0	
	do i=1,3
	  if (i.eq.1) then
	    do i1=1,2
	      do i2=1,4
	        btype(i1,i2)=ptype(i1,i2)
		  end do
		end do
	    call gametogensisc(btype,n1,gtp,ifq)
		do j=1,n1
		  ntp=ntp+1
		  do k=1,2
	        wtp(ntp,k)=gtp(j,k)
	      end do
	      do k=1,3
	        nfreq(ntp,k)=ifq(j,k)
		  end do
	    end do
	  else if (i.eq.2) then
	    do i1=1,2
	      btype(i1,1)=ptype(i1,1)
	      btype(i1,2)=ptype(i1,3)
		  btype(i1,3)=ptype(i1,2)
		  btype(i1,4)=ptype(i1,4)
		end do
		call gametogensisc(btype,n2,gtp,ifq)
		do j=1,n2
		  ntp=ntp+1
		  do k=1,2
	        wtp(ntp,k)=gtp(j,k)
	      end do
	      do k=1,3
	        nfreq(ntp,k)=ifq(j,k)
		  end do
	    end do
	  else
	    do i1=1,2
	      btype(i1,1)=ptype(i1,1)
	      btype(i1,2)=ptype(i1,4)
		  btype(i1,3)=ptype(i1,2)
		  btype(i1,4)=ptype(i1,3)
		end do
		call gametogensisc(btype,n3,gtp,ifq)
		do j=1,n3
		  ntp=ntp+1
		  do k=1,2
	        wtp(ntp,k)=gtp(j,k)
	      end do
	      do k=1,3
	        nfreq(ntp,k)=ifq(j,k)
		  end do
	    end do
	  end if
	end do

	n=n1+n2+n3
	ntype=0

	do i=1,n
	  do while (wtp(i,1).ne.'KKKK')
		ntype=ntype+1
		gtype(ntype,1)=wtp(i,1)
		gtype(ntype,2)=wtp(i,2)
	    do j=1,3
		  mfreq(ntype,j)=nfreq(i,j)
		end do
	    do j=i+1,n
	      if (wtp(i,1).eq.wtp(j,1).and.wtp(i,2).eq.wtp(j,2).or.
     &		  wtp(i,1).eq.wtp(j,2).and.wtp(i,2).eq.wtp(j,1)) then
	        wtp(j,1)='KKKK'
			do k=1,3
			  mfreq(ntype,k)=mfreq(ntype,k)+nfreq(j,k)
			end do
		  end if
		end do
		wtp(i,1)='KKKK'
	  end do !while
	end do

10	format(1x,i2,2x,a4,a,a4,2x,3i3)

	return
	end
c	********************************************
	subroutine gametogensisc(ptype,n,gtype,nfreq)

c	to turn out gametes and their frequencies from a 
c	given parental genotype in a two-locus tetrasomic 
c	inheritance	model without double reduction.

	implicit double precision (a-h,o-z)

	character*4 wtp(16,2),gtype(16,2)
	character*2 A1,A2,A3,A4,B1,B2,B3,B4,ptype(2,4)

	dimension nfreq(16,3)

	do i=1,16
	  do j=1,3
		nfreq(i,j)=0
	  end do
	end do

	A1=ptype(1,1)
	A2=ptype(1,2)
	A3=ptype(1,3)
	A4=ptype(1,4)
	B1=ptype(2,1)
	B2=ptype(2,2)
	B3=ptype(2,3)
	B4=ptype(2,4)

	wtp(1,1)=A1//B1
	wtp(1,2)=A3//B3
	wtp(2,1)=A2//B2
	wtp(2,2)=A3//B3
	wtp(3,1)=A1//B1
	wtp(3,2)=A4//B4
	wtp(4,1)=A2//B2
	wtp(4,2)=A4//B4

	wtp(5,1)=A1//B2
	wtp(5,2)=A3//B3
	wtp(6,1)=A2//B1
	wtp(6,2)=A3//B3
	wtp(7,1)=A1//B1
	wtp(7,2)=A3//B4
	wtp(8,1)=A2//B2
	wtp(8,2)=A3//B4
	wtp(9,1)=A1//B1
	wtp(9,2)=A4//B3
	wtp(10,1)=A2//B2
	wtp(10,2)=A4//B3
	wtp(11,1)=A1//B2
	wtp(11,2)=A4//B4
	wtp(12,1)=A2//B1
	wtp(12,2)=A4//B4

	wtp(13,1)=A1//B2
	wtp(13,2)=A3//B4
	wtp(14,1)=A2//B1
	wtp(14,2)=A3//B4
	wtp(15,1)=A1//B2
	wtp(15,2)=A4//B3
	wtp(16,1)=A2//B1
	wtp(16,2)=A4//B3

	ntype=0
	do i=1,16
	  do while (wtp(i,1).ne.'KKKK')
		ntype=ntype+1
		gtype(ntype,1)=wtp(i,1)
		gtype(ntype,2)=wtp(i,2)
		if (i.le.4) then
	      nfreq(ntype,1)=nfreq(ntype,1)+1
		else if (i.gt.4.and.i.le.12) then
		  nfreq(ntype,2)=nfreq(ntype,2)+1
		else if (i.gt.12.and.i.le.16) then
	      nfreq(ntype,3)=nfreq(ntype,3)+1
		end if
	    do j=i+1,16
	      if (wtp(i,1).eq.wtp(j,1).and.wtp(i,2).eq.wtp(j,2).or.
     &		  wtp(i,1).eq.wtp(j,2).and.wtp(i,2).eq.wtp(j,1)) then
	        wtp(j,1)='KKKK'
	        if (j.le.4) then
			  nfreq(ntype,1)=nfreq(ntype,1)+1
			else if (j.gt.4.and.j.le.12) then
			  nfreq(ntype,2)=nfreq(ntype,2)+1
			else if (j.gt.12.and.j.le.16) then
			  nfreq(ntype,3)=nfreq(ntype,3)+1
			end if
		  end if
		end do
		wtp(i,1)='KKKK'
	  end do !while
	end do
	n=ntype


10	format(1x,a4,a,a4,2x,3i3)

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
c COMMENTED 16/06/2016  
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
      
      maxplace=maxloc(list)
      do i=1,n
          jorder(i)=i
      enddo
      
      iorder(n)=maxplace(1)
      
 
      do i=1,n-1
         minplace=minloc(list(i:))
         iorder(i)=minplace(1)
         call swap(list(i),list(i+iorder(i)-1))
         call iswap(jorder(i),jorder(i+iorder(i)-1))
      enddo
      
      return
      end
      
    
          

      