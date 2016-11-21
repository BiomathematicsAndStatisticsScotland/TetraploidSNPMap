c	programme to sort out phase information in SNP tetraploid markers
c     programme edited on 26/2/2014 from v3 to cut out dealing with unmapped and duplicate markers
c     17/12/2014 change length of ngp1 etc from 100 to 500 to agree with version on laptop

c     24/9/2015 Edit clustering of simplex markers to separate code for the two parents so there's a test for eitehr having no simpelx markers
c     10/11/2016 add in check for no markers from each parent among the mapped markers

	MODULE WORK_ARRAYS
	character*20, allocatable, save :: name1(:),name2(:),unmapped(:)
	character*20, allocatable, save :: mapped(:),X(:),nameunsort(:)
	character*8, allocatable :: geno1(:),geno2(:)
	character*8, allocatable:: phase(:),phaset(:),phased(:)
	character*5, allocatable:: septext(:)
	character*20, allocatable :: tname(:),Xd(:)
	real(8), allocatable :: tlod(:),trf(:)
	real(8), allocatable :: rf(:),lod(:),dist(:),distd(:),sim(:,:)
	real(8), allocatable :: isource(:),iisource(:),idsource(:)
	integer, allocatable :: indi(:),par1ct(:),par2ct(:),par1ctu(:)
	integer, allocatable :: par2ctu(:),iclP1(:),iclP2(:)
	integer, allocatable :: par2ctd(:),par1ctd(:)
	END MODULE WORK_ARRAYS

	program phasesort
c	INCLUDE 'link_fnl_shared.h'
c      USE IMSL_LIBRARIES
c      USE SVIGP_INT

	USE WORK_ARRAYS

c	parameter (maxpair=32000,maxloc=250,NEPS=4)
c      parameter (lodlim=5.0d0)
	implicit double precision (a-h,o-z)
	
	INTEGER N,np,par,ngp1(500),ngp2(500),ipermP1(500),ipermP2(500)
      integer simpclusP1(500),simpclusP2(500),simpcls(4)
	character stem*15,infile1*22,infile2*27,outfile1*22,outfile2*25
	character*13 simpcode(8),tcode,simpcodeP1(16),simpcodeP2(16)
      character*13 ctrimP1(16),ctrimP2(16)
	character*20 maxname
	character*8 maxgeno1,maxgeno2
	real(8) maxlod,minrf,lodlim,rsimpclusP1(500),rsimpclusP2(500),temp(500)


	integer, allocatable :: iperm(:),iclson(:),icrson(:),iclusP1(:),idum(:)
	integer, allocatable :: nclus(:),ipermd(:),iclusP2(:),fmclus(:)
	real(8), allocatable:: clevel(:)
	character*20, allocatable :: named1(:),named2(:),tnamed(:)
	real(8), allocatable:: rfd(:),lodd(:)
	write (*,*) "Input file name (file.SNPpwd, max 15 characters)"
	read (*,"(a)") stem

	infile1 = trim(stem) // ".SNPpwd"
      infile2 = trim(stem) // "_nophase.map"
	outfile1 = trim(stem) // "qTV.out"
      outfile2 = trim(stem) // "_qhase.map"
	
	open(3,file=infile1,status='old')
	open(5,file=infile2,status='old')
      open(4,file=outfile1,status='unknown')
      open(6,file=outfile2,status='unknown')
	
	read(3,*) N,np !,ndup
	
      ndup=0
	allocate (rf(np))
	allocate (lod(np))
	allocate (name1(np),name2(np),geno1(np),geno2(np),septext(np))
	allocate (phase(N+ndup),phased(N+ndup),phaset(N))
	allocate (dist(N),distd(N+ndup),tlod(N),trf(N))
	allocate (unmapped(N),mapped(N),X(N),Xd(N+ndup),tname(N),
     &	nameunsort(N),idum(N))
	allocate (iperm(N),iclusP1(N),iclusP2(N),nclus(N),iclP1(N),iclP2(N))
	allocate (indi(N),par1ct(N+ndup),par2ct(N+ndup),par1ctu(N))
	allocate (isource(N+ndup),iisource(N+ndup),idsource(N+ndup))
	allocate (sim(N,N))
	allocate (iclson(N-1),icrson(N-1),par2ctu(N),fmclus(N))
	allocate (clevel(N-1))
	allocate (named1(ndup),named2(ndup),rfd(ndup),lodd(ndup))
	allocate (tnamed(N+ndup),ipermd(N+ndup),par2ctd(N+ndup),
     &	par1ctd(N+ndup))

	do l=1,np
	  read (3,*) name1(l),name2(l),rf(l),lod(l),septext(l),geno1(l),
     &	  geno2(l)  
	end do
c	read(3,*) nunmap
c	do i=1,nunmap
c	  read(3,*) unmapped(i)
c	end do
	read(5,*) nmap
	do i=1,nmap
	  read(5,*) mapped(i),dist(i)  !idum(i),
	end do
c	do l=1,ndup
c	  read (3,*) named1(l),named2(l),rfd(l),lodd(l)
c	end do

c	do l=1,10
c	write(4,'(a20,2x,a20,2x,f8.4,2x,f8.2,2x,a8,2x,a8)') name1(l),
c     &	  name2(l),rf(l),lod(l),geno1(l),geno2(l)  
c	enddo

c	cluster analysis of phases - actually working with distance, not similarity
c     drop this, not useful
 
	phaset(1)=geno1(1)
	nameunsort(1) = name1(1)
	do i=1,N-1
	phaset(i+1)=geno2(i)
	nameunsort(i+1)=name2(i)
      enddo

c	count number of times 2 occurs for each parent
	par1ctu=0
	par2ctu=0
	do i=1,N
	  do j=1,4
	    if (phaset(i)(j:j) .eq. "2") par1ctu(i) = par1ctu(i)+1
	    if (phaset(i)(4+j:4+j) .eq. "2") par2ctu(i) = par2ctu(i)+1
	  enddo
      enddo

c     cluster using P1 genotype      
      lodlim=3.0d0
	sim = 1.0d0
	do i=1,N
	  sim(i,i) = 0.0d0
	enddo
	npct=0
      nass1=0
	do i=1,N-1
	  do j=i+1,N
	    npct=npct+1
	   if (par1ctu(i) .eq. 1 .and. par1ctu(j) .eq. 1 .and. 
     &        geno1(npct)(1:4) .eq. geno2(npct)(1:4) .and. 
     &        geno1(npct)(5:8) .eq. "1111" .and. 
     &        geno2(npct)(5:8) .eq. "1111" .and. 
     &     lod(npct) .ge. lodlim) then
    	 	 sim(i,j) = rf(npct)
	     sim(j,i) = rf(npct)
           nass1=nass1+1
	   endif
	  enddo
c	  write (4,'(<N-i>(f4.1))') (sim(i,j),j=i+1,N)
      enddo

c     12/7/2016 print this to test
c	do i=1,N
c	  write (4,'(a20,2x,a8,2x<N>(f4.1))') nameunsort(i),phaset(i),
c     &    (sim(i,j),j=1,N)
c	enddo

c	write(*, *) 'after loop1'

d	imeth=0 
d	idist=0

d	call dclink(N,imeth,idist,sim,N,clevel,iclson,icrson)
c	do lev=1,N-1
c	  write(4,*) lev,clevel(lev)            !,iclson,icrson
c	end do

d	kk = N
d	thresh = 0.30d0  
d	do lev=1,N-1
d	  if (clevel(lev) .le. thresh) kk=kk-1
d	end do
d	call cnumb(N,iclson,icrson,kk,iclusP1,nclus)

d	do kcount=1,kk
d	write(4,*)	
d	write(4,*) 'Phase type P1',kcount
d	write(4,'(i4,4x,i4)') kcount,nclus(kcount)
d	  do i=1,N
d	    if (iclusP1(i) .eq. kcount) then
d      	  write(4,'(a20,2x,a8)') nameunsort(i),phaset(i)
d	    end if
d	  end do 
d     end do
      
c     replacement clustering
      iclus1=2
      iclus2=N-1
      iMill=1   !single linkage method
c      D=sim
      thresh=0.30d0
      if (nass1 .eq. 0) then
          write (4,*) 'No simplex markers segregating for P1'
          iclusP1(1:N)=1
      else
      call Milligan_cluster(N,nameunsort,sim,iclus1,iclus2,iMill,
     & thresh,iclusP1)
      
      maxip=maxval(iclusP1)
d	write(4,*)
d	write(4,'(a,f5.2,a,i4)') 'Number of groups at threshold ',
d     &	thresh, ' is ',maxip
d      write(4,*) 'no. Milligan group is ',maxip
      fmclus = 0
c      write(*, *) 'before loop2'
      locloop1: do jj=1,N
      do kkk=1,maxip
          if (iclusP1(jj) .eq. kkk) then
              fmclus(kkk)=fmclus(kkk)+1
              cycle locloop1
          endif
      enddo
      enddo locloop1
c      do kkk=1,maxip
c          write(4,'(2i5)') kkk,fmclus(kkk)
c      enddo
c      write(*, *) 'after loop2'
      do kcount=1,maxip
	write(4,*)	
	write(4,*) 'Phase type P1',kcount
	write(4,'(i4,4x,i4)') kcount,fmclus(kcount)
	  do i=1,N
	    if (iclusP1(i) .eq. kcount) then
      	  write(4,'(a20,2x,a8)') nameunsort(i),phaset(i)
	    end if
	  end do 
      end do
      endif
c     cluster using P2 genotype      
c      write(*, *) 'after loop3'    
      sim = 1.0d0
	do i=1,N
	  sim(i,i) = 0.0d0
	enddo
	npct=0
      nass2=0
	do i=1,N-1
	  do j=i+1,N
	    npct=npct+1
	   if (par2ctu(i) .eq. 1 .and. par2ctu(j) .eq. 1 .and. 
     &	    geno1(npct)(5:8) .eq. geno2(npct)(5:8) .and. 
     &        geno1(npct)(1:4) .eq. "1111" .and. 
     &        geno2(npct)(1:4) .eq. "1111" .and. 
     &     lod(npct) .ge. lodlim) then
    	 	 sim(i,j) = rf(npct)
	     sim(j,i) = rf(npct)
           nass2=nass2+1
    	   endif
	  enddo
c	  write (4,'(<N-i>(f4.1))') (sim(i,j),j=i+1,N)
      enddo
c	do i=1,N
c	  write (4,'(a20,<N>(f4.1))') nameunsort(i),(sim(i,j),j=1,N)
c	enddo

c      write(*, *) 'after loop4'

d	call dclink(N,imeth,idist,sim,N,clevel,iclson,icrson)
c	do lev=1,N-1
c	  write(4,*) lev,clevel(lev),iclson,icrson
c	end do

d	kk = N
d	do lev=1,N-1
d	  if (clevel(lev) .le. thresh) kk=kk-1
d	end do
d	call cnumb(N,iclson,icrson,kk,iclusP2,nclus)

d	do kcount=1,kk
d	write(4,*)	
d	write(4,*) 'Phase type P2',kcount
d	write(4,'(i4,4x,i4)') kcount,nclus(kcount)
d	  do i=1,N
d	    if (iclusP2(i) .eq. kcount) then
d      	  write(4,'(a20,2x,a8)') nameunsort(i),phaset(i)
d	    end if
d	  end do 
d	end do
    
c     replacement clustering
      if (nass2 .eq. 0) then
          write (4,*) 'No simplex markers segregating for P2'
           iclusP2(1:N)=1
      else
      call Milligan_cluster(N,nameunsort,sim,iclus1,iclus2,iMill,
     & thresh,iclusP2)
      maxip=maxval(iclusP2)
c	write(4,*)
c	write(4,'(a,f5.2,a,i4)') 'Number of groups at threshold ',
c     &	thresh, ' is ',maxip
c      write(4,*) 'no. Milligan group is ',maxip
      fmclus = 0
      locloop2: do jj=1,N
      do kkk=1,maxip
          if (iclusP2(jj) .eq. kkk) then
              fmclus(kkk)=fmclus(kkk)+1
              cycle locloop2
          endif
      enddo
      enddo locloop2
c      write(*, *) 'after loop5'
c      do kkk=1,maxip
c          write(4,'(2i5)') kkk,fmclus(kkk)
c      enddo

	do kcount=1,maxip
	write(4,*)	
	write(4,*) 'Phase type P2',kcount
	write(4,'(i4,4x,i4)') kcount,fmclus(kcount)
	  do i=1,N
	    if (iclusP2(i) .eq. kcount) then
      	  write(4,'(a20,2x,a8)') nameunsort(i),phaset(i)
	    end if
	  end do 
      end do
      endif
c      write(*, *) 'after loop6'      
      write(4,*) "Section 1: all markers"
      write(4,'(a4,2x,a20,2x,a8,2x,2a10)') "Id ","Name ","Genotype",
     & "P1 dosage ","P2 dosage"
	do i=1,N
	  write(4,'(i4,2x,a20,2x,a8,2x,2i10)') i,nameunsort(i),phaset(i),
     &	  par1ctu(i),par2ctu(i)
	enddo

    
c	place unmapped loci near mapped markers with highest lod
c     comment out this loop
c	iisource=0.0d0

c	unmaploop: do i=1,nunmap
c	nn=0
c	tlod=0.0d0
c	trf=0.5
c	tname="-"
c	    pairloop: do k=1,np
c	      if (unmapped(i) .ne. name1(k) .and. 
c     &		  unmapped(i) .ne. name2(k)) then
c	        cycle pairloop
c	      else
c	        nn=nn+1
c	        if (unmapped(i) .eq. name1(k)) then
c	          tname(nn)=name2(k)
c	          tlod(nn)=lod(k)
c	          trf(nn)=rf(k)
c	        elseif (unmapped(i) .eq. name2(k)) then
c	          tname(nn)=name1(k)
c	          tlod(nn)=lod(k)
c	          trf(nn)=rf(k)
c	        endif
c			if (tlod(nn) .ge. 5.0) then
c	          write(4,'(a,2x,a,2x,f8.4,2x,f10.2)') unmapped(i),
c     &			  tname(nn),trf(nn),tlod(nn)
c	        endif
c	      endif
c	    end do pairloop
c	  maxlod=0.0d0
c	  minrf=0.5d0
c	  maxno=0
c	  maxname="-"
c	  maploop: do j=1,nmap
c	    pairloop2: do k=1,nn
c	      if (mapped(j) .ne. tname(k)) then
c	        cycle pairloop2
c	      else
c	        if (tlod(k) .gt. maxlod .and. trf(k) .le. 0.05) then
c	          maxlod = tlod(k)
c	          maxno=j
c	          maxname=mapped(j)
c	          minrf = trf(k)
c		    endif
c	      exit pairloop2
c	      endif
c	    enddo pairloop2
c	  enddo maploop
c	  if (maxlod .ge. 5.0) then
c	    dist(nmap+i)=dist(maxno)+0.01   !add small number so that unmapped loci are after mapped
c	    iisource(nmap+i)=minrf
c	  else
c	    dist(nmap+i) = -100.0d0
c	    iisource(nmap+i) = 3.0d0
c       endif
c	  write(4,'(a,2x,a,2x,i4,2x,f10.4,2x,f10.2,2x,f10.2)') unmapped(i)
c     &	  ,maxname,maxno,minrf,maxlod,dist(nmap+i)
c	enddo unmaploop  

	X(1:nmap)=mapped(1:nmap)
c	X(nmap+1:nmap+nunmap)=unmapped(1:nunmap)
c	do i=1,nmap
c	  write(4,'(i4,2x,a,2x,f10.2)') i,X(i),dist(i)
c	  iperm(i)=i
c	enddo
c	call dsvrgp(N,dist,dist,iperm)
c	tname=X
c	do i=1,N
c	  X(i) = tname(iperm(i))
c	  isource(i)=iisource(iperm(i))
c	enddo
c	do i=1,N
c	  write(4,'(i4,2x,a,2x,f10.2,2x,f8.4)') i,X(i),dist(i),isource(i)
c	enddo

c	isource(N+1:N+ndup)=2.0d0
c	Xd(1:N)=X(1:N)

c	sort phaset and par1ct, par2ct into map order
c     26/2/2014: now cutting out unmapped markers, so index of outer loop changes from N to nmap
	do i=1,nmap
	  do j=1,N
	    if (X(i) .eq. nameunsort(j)) then
	      par1ct(i) = par1ctu(j)
	      par2ct(i) = par2ctu(j)
	      exit
	    endif
	  enddo
      enddo
c      write(*, *) 'after loop7'
      write(4,*)
      write(4,*) "Section 2: marker map"
      write(4,'(a4,2x,a20,2x,a10,2x,2a10)') "Id ","Name ","Position ",
     & "P1 dosage ","P2 dosage"
	do i=1,nmap
	  write(4,'(i4,2x,a20,2x,f10.2,2x,2i10)') i,X(i),dist(i),par1ct(i),
     &   par2ct(i)
c	  iperm(i)=i
	e nddo

c     count up codes in map
      do i=1,nmap
         do j=1,N
	     if (X(i) .eq. nameunsort(j)) then
	       iclP1(i)=iclusP1(j)
             iclP2(i)=iclusP2(j)
	       exit
	     endif
         enddo
      enddo
c     write(*, *) 'after loop8'      
      ip1=1
      ip2=1
      ngp1=0
      ngp2=0
      codeloop: do i=1,nmap
c          write(4,*) "C ",par1ct(i),par2ct(i)
          if (par1ct(i) .eq. 1) then
               if (ip1 .eq. 1) then 
               simpclusP1(ip1) = iclP1(i)
                 ngp1(ip1)=ngp1(ip1)+1
                 ipermP1(ip1)=ip1
c            write(4,*) "A ",ip1,simpcodeP1(ip1),mapped(i),ngp1(ip1)
c            write(4,*) "IP ",ipermP1(ip1)
                 ip1=ip1+1
              else
                 do j=1,ip1
                     if (iclP1(i) .eq. simpclusP1(j)) then
                         ngp1(j) = ngp1(j)+1
                         cycle codeloop
                     endif
                 enddo    
                 simpclusP1(ip1) = iclP1(i)
                 ngp1(ip1)=ngp1(ip1)+1
                 ipermP1(ip1)=ip1
                 ip1=ip1+1
c            write(4,*) "B ",ip1,simpcodeP1(ip1),mapped(i),ngp1(ip1),
c     &         ipermP1(ip1)
              endif   
          elseif (par2ct(i) .eq. 1) then 
             if (ip2 .eq. 1) then 
                 simpclusP2(ip2) = iclP2(i)
                 ngp2(ip2)=ngp2(ip2)+1
                 ipermP2(ip2)=ip2
                 ip2=ip2+1
             else
                 do j=1,ip2
                     if (iclP2(i) .eq. simpclusP2(j)) then
                         ngp2(j) = ngp2(j)+1
                         cycle codeloop
                     endif
                 enddo    
                 simpclusP2(ip2) = iclP2(i)
                 ngp2(ip2)=ngp2(ip2)+1
                 ipermP2(ip2)=ip2
                ip2=ip2+1
             endif 
          endif 
      enddo codeloop
      
c      write(*, *) 'after loop9'
      nassm1=0
	do i=1,nmap
	   if (par1ct(i) .ge. 1) then
           nassm1=nassm1+1
	   endif
      enddo

      nassm2=0
	do i=1,nmap
	   if (par2ct(i) .ge. 1) then
           nassm2=nassm2+1
	   endif
      enddo

      ip1=ip1-1
      ip2=ip2-1
      write(4,*)
      write(4,*) "Section 3a: count of simplex groups for P1"
      if (nass1 .eq. 0 .or. nassm1 .le. 1) then
          write(4,*) 'No simplex groups for P1'
      else
      write(4,'(a4,2x,a20,2x,a6)') "Id ","P1_Code ","Count "
	do j=1,ip1
          write(4,'(i4,2x,i4,2x,i6)') j,simpclusP1(j),ngp1(j)
      enddo
      ngp1=(-1)*ngp1
      call isort_2(ip1,ngp1,ipermP1)
      ngp1=(-1)*ngp1
      rsimpclusP1=1.0*simpclusP1
      temp(1:ip1)=rsimpclusP1(1:ip1)
      do j=1,ip1
          rsimpclusP1(j)=temp(ipermP1(j))
      enddo
      np1=min(ip1,4)
      simpclusP1=aint(rsimpclusP1)
      write(4,*)
      write(4,*) "Section 3b: ordered simplex groups"
      write(4,'(a4,2x,a20)') "Id ","P1_Code "
   	do j=1,np1
          write(4,'(i4,2x,i4,2x,i4)') j,simpclusP1(j),ngp1(j)
      enddo
      endif
      write(4,*)
      write(4,*) "Section 3a: count of simplex groups for P2"
      if (nass2 .eq. 0 .or. nassm2 .le. 1) then
          write(4,*) 'No simplex groups for P2'
      else
      write(4,'(a4,2x,a20,2x,a6)') "Id ","P2_Code ","Count "
	do j=1,ip2
          write(4,'(i4,2x,i4,2x,i6)') j,simpclusP2(j),ngp2(j)
      enddo
      ngp2=(-1)*ngp2

      call isort_2(ip2,ngp2,ipermP2)
      ngp2=(-1)*ngp2
      rsimpclusP2=1.0*simpclusP2
      temp(1:ip2)=rsimpclusP2(1:ip2)
      
      do j=1,ip2
          rsimpclusP2(j)=temp(ipermP2(j))
      enddo
      np2=min(ip2,4)
      simpclusP2=aint(rsimpclusP2)   

      write(4,*)
      write(4,*) "Section 3b: ordered simplex groups for P2"
      write(4,'(a4,2x,a20)') "Id ","P2_Code "
   	do j=1,np2
          write(4,'(i4,2x,i4,2x,i4)') j,simpclusP2(j),ngp2(j)
      enddo
      endif
     
c      do j=1,ip1
c          llen=index(simpcodeP1(j),"_")
c          lfin=len_trim(simpcodeP1(j))
c          ctrimP1(j)=simpcodeP1(j)(llen:lfin)
c      enddo
c      do j=1,ip2
c          llen=index(simpcodeP2(j),"_")
c          lfin=len_trim(simpcodeP2(j))
c          ctrimP2(j)=simpcodeP2(j)(llen:lfin)
c      enddo
 
c      simpcode(1)=simpcodeP1(ipermP1(1))
c      iictP1=1
c      mmloopP1: do j=2,ip1
c          do k=1,j-1
c              if (ctrimP1(ipermP1(j)) .eq. ctrimP1(ipermP1(k))) then 
c                  cycle mmloopP1
c              endif
c         enddo
c          iictP1=iictP1+1
c          simpcode(iictP1)=simpcodeP1(ipermP1(iictP1))
c      enddo mmloopP1  
 
c      simpcode(5)=simpcodeP2(ipermP2(1))
c      iictP2=1
c      mmloopP2: do j=2,np2
c          do k=1,j-1
c              if (ctrimP2(ipermP2(j)) .eq. ctrimP2(ipermP2(k))) then 
c                  cycle mmloopP2
c              endif
c          enddo
c          iictP2=iictP2+1
c          simpcode(4+iictP2)=simpcodeP2(ipermP2(iictP2))
c      enddo mmloopP2  

              
              
c     do j=1,np1
c          simpcode(j)=simpcodeP1(ipermP1(j))
c     enddo
c      do j=1,np2
c          simpcode(4+j)=simpcodeP2(ipermP2(j))
c      enddo
      

            

c	write (*,*) "How many linkage groups for P1?"
c	read (*,*) np1

c	do i=1,np1
c	  write(*,'(a,i2,a)') "Input code for P1 group ", i," :"
c	  read (*,'(a)') simpcode(i)
c	enddo

c	write (*,*) "How many linkage groups for P2?"
c	read (*,*) np2

c	do i=1,np2
c	  write(*,'(a,i2,a)') "Input code for P2 group ", i," :"
c	  read (*,'(a)') simpcode(4+i)
c	enddo

	phase="--------"
	indi=0

c	look at first parent
c     26/2/2014: now cutting out unmapped markers, so first parameter changes from N to nmap
      if (nass1 .gt. 0 .and. nassm1 .gt. 1) then
      write(4,*)
      write(4,*) "Section 4: comparison to simplex markers for P1"
	par=1
      simpcls(1:4)=simpclusP1(1:4)
      write (4,*) (simpcls(jj),jj=1,4)
	do lg=1,np1
          write(*, '(a, i)') 'position: ' , lg
          if (ngp1(lg) .ge. 4) then
	  call nchomolog(nmap,np,par,lg,simpcls)
        endif
      enddo

      endif
c     26/2/2014: now cutting out unmapped markers, so index of loops changes from N to nmap
c     change condition to avoid printing error messages
	do i=1,nmap
	itemp=0
	  do j=1,4
	    if (phase(i)(j:j) .eq. "2") itemp = itemp+1
	  enddo
c	  if (itemp .gt. par1ct(i)) then
c	    write (4,'(i4,2x,a)') i, 'Too many 2s for parent 1!'
c	  elseif (itemp .lt. par1ct(i)) then
c	    write (4,'(i4,2x,a)') i, 'Too few 2s for parent 1!'
c	  else
c	    do j=1,4
c	      if(phase(i)(j:j) .eq. "-") phase(i)(j:j) = "a"
c	    enddo
c	  endif
	  if (itemp .eq. par1ct(i)) then
	    do j=1,4
	      if(phase(i)(j:j) .eq. "-") phase(i)(j:j) = "a"
	    enddo
	  endif
	enddo


      

c	look at second parent
      if (nass2 .gt. 0 .and. nassm2 .gt. 1) then
      write(4,*)
      write(4,*) "Section 4: comparison to simplex markers for P2"
	par=2
      simpcls(1:4)=simpclusP2(1:4)

c     26/2/2014: now cutting out unmapped markers, so first parameter changes from N to nmap

c      write(*, *) 'before loop10'
	do lg=1,np2
        write(*, '(a, i)') 'position: ' , lg + 4
        if (ngp2(lg) .ge. 4) then
	  call nchomolog(nmap,np,par,lg,simpcls)
        endif
      enddo
      endif
c     26/2/2014: now cutting out unmapped markers, so index of loops changes from N to nmap
	do i=1,nmap
	itemp=0
	  do j=5,8
	    if (phase(i)(j:j) .eq. "2") itemp = itemp+1
	  enddo
c	  if (itemp .gt. par2ct(i)) then
c	    write (4,'(i4,2x,a)') i, 'Too many 2s for parent 2!'
c	  elseif (itemp .lt. par2ct(i)) then
c	    write (4,'(i4,2x,a)') i, 'Too few 2s for parent 2!'
c	  else
c	    do j=5,8
c	      if(phase(i)(j:j) .eq. "-") phase(i)(j:j) = "a"
c	    enddo
c	  endif
	  if (itemp .eq. par2ct(i)) then
	    do j=5,8
	      if(phase(i)(j:j) .eq. "-") phase(i)(j:j) = "a"
	    enddo
	  endif
	enddo





c     26/2/2014: now cutting out unmapped markers, so index of loops changes from N to nmap
      write(4,*)
      write(4,*) "Section 5: marker map with phase information"
      write(4,'(a4,2x,a20,2x,a10,2x,a8,2x,2a10)') "Id ","Name ",
     & "Position ","Phase ","P1 dosage ","P2 dosage"
	do i=1,nmap
	  distd(i)=dist(i)
	  write(4,'(i4,2x,a20,2x,f10.2,2x,a8,2x,2i10)') i,X(i),dist(i),
     &	  phase(i),par1ct(i),par2ct(i)
      enddo
      write(6,'(i5)') nmap
	do i=1,nmap
	  write(6,'(a20,7x,f6.2,2x,a4,1x,a4,2x,2i10)') X(i),dist(i),
     &	  phase(i)(1:4),phase(i)(5:8),par1ct(i),par2ct(i)
      enddo

	
c	place near-duplicate loci near close
c	nduploop: do i=1,ndup
c	  dmaploop: do j=1,N
c	    if (named2(i) .ne. X(j)) then
c	      cycle dmaploop
c	    else
c	      distd(N+i)=dist(j)+0.005
c		  phase(N+i)=phase(j)
c	      par1ct(N+i)=par1ct(j)
c	      par2ct(N+i)=par2ct(j)
c	      Xd(N+i)=named1(i)
c		  exit dmaploop
c		endif
c	  enddo dmaploop
c	enddo nduploop  

c	do i=1,N+ndup
c	  ipermd(i)=i
c	enddo

c	call dsvrgp(N+ndup,distd,distd,ipermd)
c	tnamed=Xd
c	iisource=isource
c	par1ctd=par1ct
c	par2ctd=par2ct
c	phased=phase
c	do i=1,N+ndup
c	  Xd(i) = tnamed(ipermd(i))
c	  idsource(i)=iisource(ipermd(i))
c	  par1ct(i)=par1ctd(ipermd(i))
c	  par2ct(i)=par2ctd(ipermd(i))
c	  phase(i)=phased(ipermd(i))
c	enddo

c	do i=1,N+ndup
c	 write(4,'(i4,2x,a,2x,f10.2,2x,a8,2i4,2x,f8.4)') i,Xd(i),distd(i),
c    &	  phase(i),par1ct(i),par2ct(i),idsource(i)
c	enddo

c	do i=1,N+ndup
c	if (idsource(i) .eq. 3) cycle
c	if (idsource(i) .eq. 0) then
c	 write(4,'(i4,2x,f8.4,2x,a20,42x,f10.2,2x,a8)') i,idsource(i),Xd(i),
c     &	  distd(i),phase(i)
c	elseif (idsource(i) .eq. 2) then
c	 write(4,'(i4,2x,f8.4,22x,a20,22x,f10.2)') i,idsource(i),Xd(i),
c     &	  distd(i)
c	else
c	 write(4,'(i4,2x,f8.4,42x,a20,2x,f10.2)') i,idsource(i),Xd(i),
c     &	  distd(i)
c	endif
c	enddo

c      phaset="Unmapped"
c     clustercheck: do i=1,N
c          do j=1,nmap
c              if (trim(nameunsort(i)) .eq. trim(X(j))) then
c                  phaset(i)=phase(j)
c                  cycle clustercheck
c              endif
c          enddo
c      enddo clustercheck
                    
	write(4,*)	
      write(4,*) "Section 6: clustering of markers with same phase"
c     cluster using P1 genotype      
      lodlim=4.0d0
	sim = 1.0d0
	do i=1,N
	  sim(i,i) = 0.0d0
	enddo
	npct=0
	do i=1,N-1
	  do j=i+1,N
	    npct=npct+1
	   if (geno1(npct)(1:4) .eq. geno2(npct)(1:4) .and. 
     &     lod(npct) .ge. lodlim) then
    	 	 sim(i,j) = 0.0d0
	     sim(j,i) = 0.0d0
	   endif
	  enddo
c	  write (4,'(<N-i>(f4.1))') (sim(i,j),j=i+1,N)
      enddo

c	do i=1,N
c	  write (4,'(a20,<N>(f4.1))') nameunsort(i),(sim(i,j),j=1,N)
c	enddo


d	imeth=3 
d	idist=0

d	call dclink(N,imeth,idist,sim,N,clevel,iclson,icrson)
c	do lev=1,N-1
c	  write(4,*) lev,clevel(lev),iclson,icrson
c	end do

d	kk = N
d	thresh = 0.90d0  
d	do lev=1,N-1
d	  if (clevel(lev) .le. thresh) kk=kk-1
d	end do
d	call cnumb(N,iclson,icrson,kk,iclusP1,nclus)

d	do kcount=1,kk
d	write(4,*)	
d	write(4,*) 'Phase type P1',kcount
d	write(4,'(i4,4x,i4)') kcount,nclus(kcount)
d	  do i=1,N
d	    if (iclusP1(i) .eq. kcount) then
d      	  write(4,'(a20,2x,a8)') nameunsort(i),phaset(i)
d	    end if
d	  end do 
d      end do

c     replacement clustering
      iclus1=2
      iclus2=N-1
      iMill=3   !average linkage method
c      D=sim
      thresh=0.90d0
      call Milligan_cluster(N,nameunsort,sim,iclus1,iclus2,iMill,
     & thresh,iclusP1)
      
      maxip=maxval(iclusP1)
d	write(4,*)
d	write(4,'(a,f5.2,a,i4)') 'Number of groups at threshold ',
d     &	thresh, ' is ',maxip
d      write(4,*) 'no. Milligan group is ',maxip
      fmclus = 0
      locloop3: do jj=1,N
      do kkk=1,maxip
          if (iclusP1(jj) .eq. kkk) then
              fmclus(kkk)=fmclus(kkk)+1
              cycle locloop3
          endif
      enddo
      enddo locloop3
d      do kkk=1,maxip
d          write(4,'(2i5)') kkk,fmclus(kkk)
d      enddo

	do kcount=1,maxip
	write(4,*)	
	write(4,*) 'Phase type P1',kcount
	write(4,'(i4,4x,i4)') kcount,fmclus(kcount)
	  do i=1,N
	    if (iclusP1(i) .eq. kcount) then
      	  write(4,'(a20,2x,a8)') nameunsort(i),phaset(i)
	    end if
	  end do 
      end do

c     cluster using P2 genotype      
     
      sim = 1.0d0
	do i=1,N
	  sim(i,i) = 0.0d0
	enddo
	npct=0
	do i=1,N-1
	  do j=i+1,N
	    npct=npct+1
	   if (geno1(npct)(5:8) .eq. geno2(npct)(5:8) .and. 
     &     lod(npct) .ge. lodlim) then
    	 	 sim(i,j) = 0.0d0
	     sim(j,i) = 0.0d0
    	   endif
	  enddo
c	  write (4,'(<N-i>(f4.1))') (sim(i,j),j=i+1,N)
      enddo

c	do i=1,N
c	  write (4,'(a20,<N>(f4.1))') nameunsort(i),(sim(i,j),j=1,N)
c	enddo


d	imeth=3
d	idist=0

d	call dclink(N,imeth,idist,sim,N,clevel,iclson,icrson)
c	do lev=1,N-1
c	  write(4,*) lev,clevel(lev),iclson,icrson
c	end do

d	kk = N
d	thresh = 0.90d0  
d	do lev=1,N-1
d	  if (clevel(lev) .le. thresh) kk=kk-1
d	end do
d	call cnumb(N,iclson,icrson,kk,iclusP2,nclus)

d	do kcount=1,kk
d	write(4,*)	
d	write(4,*) 'Phase type P2',kcount
d	write(4,'(i4,4x,i4)') kcount,nclus(kcount)
d	  do i=1,N
d	    if (iclusP2(i) .eq. kcount) then
d      	  write(4,'(a20,2x,a8)') nameunsort(i),phaset(i)
d	    end if
d	  end do 
d	end do

c     replacement clustering
      call Milligan_cluster(N,nameunsort,sim,iclus1,iclus2,iMill,
     & thresh,iclusP2)
      
      maxip=maxval(iclusP2)
d	write(4,*)
d	write(4,'(a,f5.2,a,i4)') 'Number of groups at threshold ',
d     &	thresh, ' is ',maxip
d      write(4,*) 'no. Milligan group is ',maxip
      fmclus = 0
      locloop4: do jj=1,N
      do kkk=1,maxip
          if (iclusP2(jj) .eq. kkk) then
              fmclus(kkk)=fmclus(kkk)+1
              cycle locloop4
          endif
      enddo
      enddo locloop4
d      do kkk=1,maxip
d          write(4,'(2i5)') kkk,fmclus(kkk)
d      enddo

	do kcount=1,maxip
	write(4,*)	
	write(4,*) 'Phase type P2',kcount
	write(4,'(i4,4x,i4)') kcount,fmclus(kcount)
	  do i=1,N
	    if (iclusP2(i) .eq. kcount) then
      	  write(4,'(a20,2x,a8)') nameunsort(i),phaset(i)
	    end if
	  end do 
      end do

      STOP
      END program phasesort


c	*********************************************************************
	subroutine nchomolog(N,np,par,lg,simpcls)

	USE WORK_ARRAYS

	implicit double precision (a-h,o-z)

     
	character*8, allocatable :: tgeno1(:),tgeno2(:)
	real(8), allocatable :: trlod(:)
      integer, allocatable :: icl(:)
	

	character*13 simpcode(8),tcode 
	character*20 maxname
	character*8 maxgeno1,maxgeno2

	integer par,tcls,simpcls(4)
	real(8) maxlod,lodlin
	allocate (tgeno1(N),tgeno2(N))
	allocate (trlod(N),icl(N))
	

      lodlim=4.0d0
	if (par .eq. 1) then
          ig=0
c	  lg1=lg
        icl=iclP1
      else
          ig=4
c	  lg1=lg+4
        icl=iclP2
	  endif  

	tcls=simpcls(lg)
	write(4,*) 'cluster = ',tcls
	locloop: do i=1,N
 	    nn=0
	    tlod=0.0d0
	    tname="-"
           do j=1,N
                if (j .eq. i .or. icl(j) .ne. tcls) then
                    cycle
                else                  
	    pairloop3: do k=1,np
	      if (X(i) .ne. name1(k) .and. X(i) .ne. name2(k)) then
	        cycle pairloop3
	      elseif (X(j) .ne. name1(k) .and. X(j) .ne. name2(k)) then
	        cycle pairloop3
	      else
	        nn=nn+1
c              write(4,*) 'pair no. ',k
	    if (X(i) .eq. name1(k) .and. X(j) .eq. name2(k)) then
	          tname(nn)=name2(k)
	          trlod(nn)=lod(k)
	          tgeno1(nn)=geno2(k)
	          tgeno2(nn)=geno1(k)
	    elseif (X(i) .eq. name2(k).and. X(j) .eq. name1(k)) then
	          tname(nn)=name1(k)
	          trlod(nn)=lod(k)
	          tgeno1(nn)=geno1(k)
	          tgeno2(nn)=geno2(k) !simplex is always the first code
	    endif
	       write(4,'(a,2x,a,2x,f10.2,2x,a,2x,a)') X(i),tname(nn),
     &		   trlod(nn),tgeno1(nn),tgeno2(nn)
	      endif
          end do pairloop3
          endif
          enddo
	    if (par .eq. 1 .and. index(tgeno2(1),"2") .gt. 4) then
		  phase(i)(1:4)="1111"
	      write (4,'(i4,2x,a8)') i,phase(i)
	    elseif (par .eq. 2 .and. 
     &		index(tgeno2(1),"2",back = .true.) .le. 4) then
	         phase(i)(5:8)="1111"
	       write (4,'(i4,2x,a8)') i,phase(i)
	    else
	  maxlod=lodlim
	  maxno=0
	  maxname="-"
	    pairloop4: do k=1,nn
	        if (trlod(k) .gt. maxlod) then
	          maxlod = trlod(k)
	          maxno=k
	          maxgeno1=tgeno1(k)
	          maxgeno2=tgeno2(k)
		    endif
	    enddo pairloop4
	    if (maxlod .gt. lodlim) then
	  	  iindex=index(maxgeno1,"2")
	      phase(i)(lg+ig:lg+ig)=maxgeno2(iindex:iindex)
	      write (4,'(i4,2x,f8.2,2x,a8)') i,maxlod,phase(i)
	    else
	      phase(i)(lg+ig:lg+ig)="-"
	      write (4,'(i4,2x,f8.2,2x,a8)') i,maxlod,phase(i)
	    endif
	    endif
	enddo locloop

	return
	end
c      *********************************************************************

c change program to create subroutine Milligan_cluster and call to it


      subroutine Milligan_cluster(N,locname,D,ICLUS1,ICLUS2,imeth,
     & distlim,IP)
      
c     N =  number of points to be clustered
c     locname = names of points
c     D = distance matrix
c     ICLUS1 = smallest number of clusters, min 2
c     ICLUS2 = largest number of clusters, max N-1
c     imeth=clustering method. imeth=1 for single linkage, 3 for group average
c     distlim = distance at which to stop clustering, overrules ICLUS1
c     IP=group identifier
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
C
c     cut down to drop test of influence and plots, now to work on similarity matrix
C
c      PARAMETER(MAXPTS=8000)
C
      implicit double precision (a-h,o-z)
      
      REAL*8 D(N,N),XX(N),P(N*2)

      REAL*8 LEVEL(N)

      INTEGER ERR,IP(N),LK(N),LKI(N),LKJ(N)

      LOGICAL W(N),WD(N,N),TEST

      CHARACTER*40 locname(N)

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
c      S = IS
      N1 = N-1
      N2 = N1-1
      XN = N
c      DO  I = 1, N
c         XR(I) = 0.0
c         DR(I) = 0.0
c         E(I,I) = 0.0
c      enddo




      IKOUNT = 1
      K = imeth   !Single link method
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
c         write(9,*) II, 'distance matrix'
c         do MM=1,N
c             write (9,'(<N>(f8.4))') (D(MM,NN),NN=1,N)
c         enddo
c         write(9,*)

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
      agloop: DO  IIPX1 = 1, N2
          if (XX(IIPX1) .gt. distlim) exit agloop
         JJX = LKJ(IIPX1)
         JX = LKI(IIPX1)
         JX1 = JX-1
         DO 2400 IIXP2 = JJX, JX1
            JJX1 = IIXP2+1
            DO 2390 I1I = JJX1, JX
               JPL1 = MIN0(LK(I1I),LK(IIXP2))
               JPL2 = MAX0(LK(I1I),LK(IIXP2))
               IF (D(JPL1,JPL2).NE.0.0) GO TO 2390
               D(JPL1,JPL2) = XX(IIPX1)
               WD(JPL1,JPL2) = .TRUE.
2390        CONTINUE
2400     CONTINUE
         
c       write out information that might be agglomerations
c         do MM=1,N
c             write(9,'(i3,2x,5i8,2x,f12.6)') MM,LEVEL(MM), IP(MM),
c     &      LK(MM),LKI(MM),LKJ(MM),XX(MM)
c         enddo
c         write(9,*)
c         do MM=1,N
c             write (9,*) (WD(MM,NN),NN=1,N)
c         enddo
c         write(9,*)
c         write(9,'(2I6,f12.6)') JPL1,JPL2,D(JPL1,JPL2)
c         write(9,*)
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
c         WRITE (9,2500) INPX1,INPX1
2500     FORMAT (//,' Cluster membership of data points for the ',I3,
     *      ' cluster solution',/' (Hierarchy level ',I3,')',/)

c         WRITE (9,2510)
2510     FORMAT (' Point    Assigned to cluster',/,1X,30('-'))
c         DO I = 1, N
c            WRITE (9,'(1X,I5,2X,a30,4X,I3)') I,locname(I),IP(I)
c         enddo
C
c2670     CONTINUE
      enddo agloop
c      write(9,*) 'Exit agloop'


c2910  CONTINUE

2970  CONTINUE
      RETURN
      END
      
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
      subroutine isort_2(n,ilist,jorder)
      
c     small sorting routine from Brainerd et al. Fortran book p153
c     edited to return permutation vector iorder
c     integer sort

      integer ilist(n),itemp(n)
      integer iorder(n),minplace(1),maxplace(1),jorder(n)
c      write(4,*)
c      write(4,*) 'Sort output'
c      do i=1,n
c          write(4,'(i6,f12.6)') i,list(i)
c      enddo
      
      maxplace=maxloc(ilist)
      do i=1,n
          jorder(i)=i
      enddo
      
      iorder(n)=maxplace(1)
      
 
      do i=1,n-1
         minplace=minloc(ilist(i:))
         iorder(i)=minplace(1)
c         write(4,'(2i6)') i,iorder(i)
         call iswap(ilist(i),ilist(i+iorder(i)-1))
         call iswap(jorder(i),jorder(i+iorder(i)-1))
      enddo
         
c      write(4,*)
c      do i=1,n
c          write(4,'(3i6,f12.6)') i,iorder(i),jorder(i),list(i)
c      enddo
      
      return
      end     
    
          

    