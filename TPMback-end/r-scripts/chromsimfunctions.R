library(smacof) #loads the required wMDS functions
library(reshape)
library(princurve)
library(rgl)

calc.pair.rf.lod<-function(stem,weightfn='lod'){
  nloci<-scan(paste(stem,'.txt',sep=""),what=integer(),nmax=1, quiet=TRUE)
  d<-read.table(paste(stem,'.txt',sep=""),skip=1,header=FALSE)
  names(d)<-c("name1","name2","rfreq","lodscore")
  if(weightfn=='lod2') d$lodscore<-d$lodscore^2 else
    if(weightfn!='lod') {write('Warning the weight function has been wrongly set it must be \"lod\" or \"lod2\"',""); return(0)}
  
  dd<-d
  missing1<-with(d,unique(as.character(name1[!name1%in%name2])))
  missing2<-with(d,as.character(unique(name2[!name2%in%name1])))
  if(length(missing1)>1){
    dd$name1<-as.character(dd$name1);dd$name2<-as.character(dd$name2)
    write(missing1,"")
    for(i in 2:length(missing1))dd<-rbind(dd,list(missing1[1],missing1[i],0,0))
  }
  if(length(missing2)>1){
    dd$name1<-as.character(dd$name1);dd$name2<-as.character(dd$name2)
        write(missing2,"")
    for(i in 2:length(missing2))dd<-rbind(dd,list(missing2[i],missing2[1],0,0))
      }
 dd$name1<-factor(dd$name1,unique(as.character(dd$name1)))
  dd$name1<-relevel(dd$name1,missing1[1])
  dd$name2<-factor(dd$name2,levels=c(as.character(levels(dd$name1)[2:length(levels(dd$name1))]),as.character(missing2[1])))

  d<-dd
  b<-matrix(0,ncol=nloci,nrow=nloci)
  temp<-cast(name1~name2,data=d,value="rfreq",add.missing=TRUE,fill=0)
  tt<-as.matrix(temp[,2:(nloci)])
  colnames(tt)<-names(temp)[2:nloci]
  rownames(tt)<-temp$name1
  b[upper.tri(b)]<-tt[upper.tri(tt,diag=TRUE)]
  rfmat<-b+t(b)
  colnames(rfmat)<-c(rownames(tt)[1],colnames(tt))
  rownames(rfmat)<-c(rownames(tt),colnames(tt)[nloci-1])
  rm(temp,tt,b)
  b<-matrix(0,ncol=nloci,nrow=nloci)  
  temp<-cast(name1~name2,data=d,value="lodscore",add.missing=TRUE,fill=0)
  tt<-as.matrix(temp[,2:(nloci)])  
  colnames(tt)<-names(temp)[2:nloci]
  rownames(tt)<-temp$name1
  b[upper.tri(b)]<-tt[upper.tri(tt,diag=TRUE)]
  lodmat<-b+t(b)  
  colnames(lodmat)<-c(rownames(tt)[1],colnames(tt))
  rownames(lodmat)<-c(rownames(tt),colnames(tt)[nloci-1])
  rfmat[rfmat>0.499999]<-0.499999
  rm(temp,tt,b)
  diag(rfmat)<-NA
  diag(lodmat)<-NA
  lodmat[lodmat<0]<-0  
  list(rf=rfmat,lod=lodmat,nloci=nloci,locinames=rownames(rfmat))
}


#takes an argument rf: a matrix of pairwise genetic distances where the diagonal is NAs 
#output: m x m matrix of pairwise genetic distances
dmap<-function(rf){return(-0.5*log(1-2*rf))} 


#creates a data frame of eestimated pairwise distances between the of the loci
#arguments: estpos vector of estimated positions
#output: a data frame of eestimated pairwise distances between the of the loci
dist.check.vector<-function(estpos){
  l<-length(estpos)
  d<-data.frame(outer(1:l,1:l,function(i,j)abs(estpos[i]-estpos[j])))
  write('step 3', "")
  estd<-reshape(d,varying=list(names(d)),direction="long")$X1
  return(estd)
}

#reorders the original distance map by the estimated locus order
#2 arguments: realdmap - the original distance map, order - the estimated order of the loci
dmap.check<-function(realdmap,order){
  o<-sapply(1:length(order),function(i)which(order==i))  
  return(outer(o,o,Vectorize(function(i,j)realdmap[i,j]) ))
}


get.nearest.informative<-function(loci,lodmap){
  #split matrix by loci
  neighbours<-NULL
  
  if(loci>1) {
    locileft<-lodmap[loci,(loci-1):1]
    if(length(which(locileft!=0))>0)    neighbours<-loci-min(which(locileft!=0))
      }
  if(loci<dim(lodmap)[2]){
    lociright<-lodmap[loci,(loci+1):dim(lodmap)[2]]
    if(length(which(lociright!=0))>0)  neighbours<-c(neighbours,loci+min(which(lociright!=0)))
  }
  neighbours
}
  
calc.nnfit.loci<-function(loci,distmap,lodmap,estmap){
  nns<-get.nearest.informative(loci,lodmap)
  obs<-distmap[loci,nns]
  est<-estmap[loci]-estmap[nns]
  nn.fit<-sum(abs(obs-est))
  nn.fit
}

calc.nnfit<-function(distmap,lodmap,estmap){
  pointfits<-unlist(lapply(1:dim(distmap)[2],calc.nnfit.loci,distmap=distmap,lodmap=lodmap,estmap=estmap))
  fit<-sum(pointfits)
  meanfit<-mean(pointfits)
  list(fit=fit,pointfits=pointfits,meanfit=meanfit)
}


#kendall's tau b=2(C-D)/n(n-1), and C=N-D where N=nC2, the total no. of pairs so D=0.5nC2(1-tau)
calc.nswaps<-function(estpos, realpos){
  n<-length(estpos)
  N<-n*(n-1)/2
  nswaps<-0.5*N*(1-cor(estpos,realpos,method="kendall"))
  return(round(min(nswaps,N-nswaps)))
}

#loads data, estimates loci position using Principal curves and writes outputs to comma separated text files 
#takes 3 arguments:
#stem: text string the name of the file of recombination fractions and scores it should not contain any suffices (the file should be a .txt file as described above)

#optional parameters
#spar: integer - the smoothing parameter for the principal curve. If NULL this will be done using leave one out cross validation.
#n: vector of integers or strings containing loci to be omitted from the analysis

#Output: map object consisting of:
#M: the input distance map
# smacofsym: the unconstrained wMDS results

#pc: the principal curve results
#distmap: a matrix of pairwise distances between loci where the columns are in the estimated order
#locimap: a data frame of the loci containing the name and position of each locus in order of increasing distance
#length: integer giving the total length of the segment
#removed: a vector of the names of loci removed from the analysis
#scale: the scaling factor from the MDS
#locikey: a data frame showing the number associated with each locus name for interpreting the MDS configuration plot
#confplotno: a data frame showing locus name associated with each number on the MDS configuration plots

#creates files containing the final configuration, , the matrix of intermarker distances, the estimated map, the key to map from number on the configuration plot to loci name, and a list of the removed loci.

calc.maps.load.and.trim.prin.curve<-function(stem,spar=NULL,n=NULL,ndim=2,weightfn='lod'){
  write('step 1', "")
  lodrf<-calc.pair.rf.lod(stem,weightfn)
  confplotno<-1:lodrf$nloci
  if(!is.null(n)){
    if(!is.numeric(n))n<-which(lodrf$locinames%in%n)    
    r<-lodrf$rf[-n,-n]
    lod<-lodrf$lod[-n,-n]
    confplotno<-confplotno[-n]
    locinames<-lodrf$locinames[-n]
	}  
	else 
	{
		r<-lodrf$rf
        lod<-lodrf$lod
        locinames<-lodrf$locinames}
  M<-dmap(r)
  nloci=length(confplotno)
  write('step 2', "")
  smacofsym<-smacofSym(M,ndim=ndim,weightmat=lod,itmax=100000)
  pc1<-principal.curve(smacofsym$conf,maxit=150,spar=spar)
 
  if(ndim==2) conf<-data.frame(plotno=confplotno, name=locinames,MDS1=smacofsym$conf[,1],MDS2=smacofsym$conf[,2], Smooth1=pc1$s[,"D1"],Smooth2=pc1$s[,"D2"]) else conf<-data.frame(plotno=confplotno, name=locinames,MDS1=smacofsym$conf[,1],MDS2=smacofsym$conf[,2],MDS3=smacofsym$conf[,3], Smooth1=pc1$s[,"D1"],Smooth2=pc1$s[,"D2"],Smooth3=pc1$s[,"D3"])
    
  
  #gives the projection of points onto the line
  scale<-sum(smacofsym$delta)/sum(smacofsym$dhat) #configuration dissim are basedon the normalized observed distances - dhat. True observed dissimilarities are delta
 maporder<-pc1$tag
  estpos<-pc1$lambda[maporder]*scale*100                #gives the estimated length from the beginning of the line
  distmap<-outer(maporder,maporder,Vectorize(function(i,j)M[i,j]))
  lodmap<-outer(maporder,maporder, Vectorize(function(i,j)lod[i,j]))
  
   if(!is.null(n))  locikey<-data.frame(locus=lodrf$locinames[-n],confplotno=confplotno)  else  locikey<-data.frame(locus=lodrf$locinames,confplotno=confplotno)
    if(!is.null(n))removedloci<-data.frame(n,lodrf$locinames[n],row.names=NULL) else removedloci<-n
  l<-dim(smacofsym$conf)[1]
 nnfit<-calc.nnfit(distmap,lodmap,estpos)
 locimap<-data.frame(confplotno=confplotno[maporder],name=locikey$locus[maporder],position=estpos,nnfit=nnfit$pointfits)
 phasemap<-data.frame(locikey$locus[maporder],estpos)
 
 
 write.table(nloci,file=paste(stem,'_phasemap.txt',sep=""),sep=",",row.names=FALSE,quote=FALSE,col.names=FALSE)
 write.table(phasemap,file=paste(stem,'_phasemap.txt',sep=""),sep=",",row.names=FALSE,quote=FALSE,col.names=FALSE,append=TRUE)
  write.table(conf,file=paste(stem,'_conf.txt',sep=""),sep=",",row.names=FALSE)
  write.table(locimap,file=paste(stem,'_estimatedmap.txt',sep=""),sep=",",row.names=FALSE,quote=FALSE)
 list(M=M,lod=lod,smacofsym=smacofsym,pc=pc1,locimap=locimap,length=max(estpos),removed=n,locikey=locikey,scale=scale,confplotno=confplotno,nnfit=nnfit)
  
}



#.png file
#MDS plot on left               NN vs position on right (can this be labelled by same number as MDS plot?)
#Plots to be larger but labels stay small so that as many as possible are legible.


#plots the diagnostic figures for the principal components method
#displaytext lets you choose whether to display marker identifiers (TRUE) programme generated numbers (defualt) (FALSE)

plot.diag.pc<-function(mappc,picname,D1lim=NULL,D2lim=NULL,displaytext=TRUE){

  with(mappc,{
    if (displaytext==TRUE) labels=locikey$locus else labels=locikey$confplotno
    png(paste(stem,'_diagplot.png',sep=""),width=960,height=480)
    par(mfrow=c(1,2))
    plot(smacofsym$conf,type="n",main='MDS with principal curve',xlim=D1lim,ylim=D2lim,xlab='Dim 1',ylab='Dim 2')
    text(smacofsym$conf,labels=labels,cex=0.8)
    lines(pc)
    if (displaytext==TRUE) labels1=locimap$name else labels1=locimap$confplotno
    plot(locimap$position,locimap$nnfit,type='n',xlab='Position',ylab='nnfit')
    text(locimap$position,locimap$nnfit,labels1)
    dev.off()})
}


#diagnostic plot for the 3d PC, to change the scale of the axis for dimension1 put in D1lim=c(lower, upper) etc.
#displaytext lets you choose whether to display marker identifiers (TRUE) programme generated numbers (defualt) (FALSE)

plot.diag.pc.3d<-function(mappc,picname,D1lim=NULL,D2lim=NULL,D3lim=NULL,displaytext=TRUE){
	numscale = 1.0
  with(mappc,{
    if (displaytext==TRUE) labels=locikey$locus else labels=locikey$confplotno
    png(paste(stem,'_diagplot.png',sep=""),height=1200,width=1200)
    par(mfrow=c(2,2))
    plot(smacofsym$conf[,'D1'],smacofsym$conf[,'D2'],type="n",main='MDS with principal curve',xlab='Dimension 1',ylab='Dimension 2',xlim=D1lim,ylim=D2lim)
    text(smacofsym$conf[,'D1'],smacofsym$conf[,'D2'],labels=labels,cex=numscale)
    lines(pc$s[,'D1'][pc$tag],pc$s[,'D2'][pc$tag])
    plot(smacofsym$conf[,'D1'],smacofsym$conf[,'D3'],type="n",main='MDS with principal curve',xlab='Dimension 1',ylab='Dimension 3',xlim=D1lim,ylim=D3lim)
    text(smacofsym$conf[,'D1'],smacofsym$conf[,'D3'],labels=labels,cex=numscale)
    lines(pc$s[,'D1'][pc$tag],pc$s[,'D3'][pc$tag])
    plot(smacofsym$conf[,'D2'],smacofsym$conf[,'D3'],type="n",main='MDS with principal curve',xlab='Dimension 2',ylab='Dimension 3',xlim=D2lim,ylim=D3lim)
    text(smacofsym$conf[,'D2'],smacofsym$conf[,'D3'],labels=labels,cex=numscale)
    lines(pc$s[,'D2'][pc$tag],pc$s[,'D3'][pc$tag])
    if (displaytext==TRUE) 
	labels1=locimap$name 
	else 
	labels1=locimap$confplotno
    plot(locimap$position,locimap$nnfit,type='n',xlab='Position',ylab='nnfit')
    text(locimap$position,locimap$nnfit,labels1)
    dev.off()
	write.table(smacofsym$conf, "smacofsym.conf")
	write.table(locikey, "locikey")
 	write.table(pc$s[pc$tag,], "pc")
    #plot3d(smacofsym$conf,type="n")
    #text3d(smacofsym$conf,text=labels)
    #lines3d(pc$s[pc$tag,])
  }
  )
}





#loads data, calculates wMDS and writes outputs into text files and outputs diagnostic plots
#1 compulsory arguments and 3 optional ones
#stem: text string the name of the file from which the data should be read - should contain the complete file name excluding any suffix (the file should be a .txt file as described in calc.pair.rf.lod)

#optional picname: text string for the title to be printed on diagnostic plots
#         p: penalty for deviation from the sphere in constrained smacof/smoothing parameter for pc method (if pc method is used then p=NULL will invoke use of leave one out cross validation to determine the best smoothing parameter)
#         n: vector of integers or text strings containing the name or position in the input list of loci to be excluded from the analysis
#ispc:  If FALSE then the constrained MDS method will be used, if true then the PC method will be used.
#ndim: 2 or 3 the number of dimensions to use for the principal curvesanalysis
load.and.trim<-function(stem,p=NULL,n=NULL,ndim=2,D1lim=NULL,D2lim=NULL,D3lim=NULL,displaytext=TRUE,weightfn='lod'){
  if(!is.null(n)){
    t<-table(n)[(table(n)>1)]
    if(length(t)>0){
      write("Warning: the list of ommitted loci is not unique, please remove duplicates of the loci listed below","")
      write(names(t),"")
      return(0)
    }
  }
    map<-calc.maps.load.and.trim.prin.curve(stem,spar=p,n,ndim=ndim,weightfn=weightfn)
    #  png(paste(stem,'pc.png',sep=""),width=595, height=300)
    if(ndim==2)
	plot.diag.pc(map,picname=stem,displaytext=displaytext)   
	else 
	plot.diag.pc.3d(map,picname=stem,D1lim,D2lim,D3lim,displaytext=displaytext)
    #   dev.off()
  write('step 4', "")
  write(paste('Stress:',map$smacofsym$stress),"")
  write(paste('Nearest Neighbour Fit:',sum(map$locimap$nnfit)),"")
  write(paste('Mean Nearest Neighbour Fit:',mean(map$locimap$nnfit)),"")
  write('Markers omitted:',"")
  write(omitvec,"")
  list(map=map)
}


#calculates a new nnfit based on a new order after estimating a map.
#args:
#estmap - a .csv file with no header, the first column being loci names and the second being positions
#mapobject - an R object the output from the function 'load.and.trim()'
#fname- optional argument specifying a file to write to

#value:
# fit - the sum of all the pointfits
#pointfits - the nnfit for each point
#meanfit - the mean nnfit for the points

recalc.nnfit.from.map<-function(estmap,mapobject,fname=NULL){
  estmap<-read.csv(estmap,header=FALSE)
  M<-mapobject$map$M
  lod<-mapobject$map$lod
  lnames<-colnames(M)
  names<-estmap[,1]
  maporder<- sapply(1:length(names),function(i)which(lnames==names[i]))
  distmap<-outer(maporder,maporder,Vectorize(function(i,j)M[i,j]))
  lodmap<-outer(maporder,maporder,Vectorize(function(i,j)lod[i,j]))
  nnfit<-calc.nnfit(distmap,lodmap,estmap[,2])
  newmap<-data.frame(name=estmap[,1],position=estmap[,2],nnfit=nnfit$pointfits)
  if(!is.null(fname)) write.csv(newmap,file=fname)
nnfit
}

#Calculates the nearest neighbour fit from an estimated map and the original pairwise recombination fraction/lod score file

#args:
#estmap - The estimated map. a csv file with no header, the first column being loci names and the second being positions
#stem - the stem of a .txt file specifying the pairwise rf and lod scores (as for load.and.trim)
#weightfn - the weight function to appy to the lod scores - default is 'lod', can also be  'lod2'
#fname- optional argument specifying a file to write to
#n - vector of loci to omit

#value:
# fit - the sum of all the pointfits
#pointfits - the nnfit for each point
#meanfit - the mean nnfit for the points
calc.nnfit.from.file<-function(estmap,stem,fname=NULL,weightfn='lod',n=NULL){
  estmap<-read.csv(estmap,header=FALSE)
  lodrf<-calc.pair.rf.lod(stem,weightfn)
  if(!is.null(n)){
    if(!is.numeric(n))n<-which(lodrf$locinames%in%n)    
    r<-lodrf$rf[-n,-n]
    lod<-lodrf$lod[-n,-n]}  else {r<-lodrf$rf
    lod<-lodrf$lod}
  M<-dmap(r)
  lnames<-colnames(M)
  names<-estmap[,1]
  maporder<- sapply(1:length(names),function(i)which(lnames==names[i]))
  distmap<-outer(maporder,maporder,Vectorize(function(i,j)M[i,j]))
  lodmap<-outer(maporder,maporder,Vectorize(function(i,j)lod[i,j]))
  nnfit<-calc.nnfit(distmap,lodmap,estmap[,2])
  newmap<-data.frame(name=estmap[,1],position=estmap[,2],nnfit=nnfit$pointfits)
  if(!is.null(fname)) write.table(newmap,file=fname,sep=',')
  nnfit
}


get.dist.loci<-function(loci,estmap,realmap){
  l<-estmap$name[loci]
  dist<-estmap[estmap$name==l,]$position-realmap[which(realmap[,1]==l),2]
  dist
}

#args:
#estmap - the estimated map should 2 columns 'name' and 'position' containing the loci names and estimated positions.
#realmap - the assumed real map - should have 2 columns - the first containing loci names and the second their positions - no column names are necessary for this
#values
# poindist - the distance of each loci from it's "real position"
# meansquaredist - the mean of the square distances of points from their real position.
mean.dist.from.truth<-function(estmap,realmap){
  dist<-unlist(lapply(1:dim(estmap)[1],get.dist.loci,estmap=estmap,realmap=realmap))
  meansquaredist<-mean(dist^2)
  pointdist<-cbind(name=estmap$name,dist=dist)
  list(pointdist=pointdist, meansquaredist=meansquaredist)
}