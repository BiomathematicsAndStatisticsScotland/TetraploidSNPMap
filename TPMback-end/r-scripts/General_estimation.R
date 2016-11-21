source('chromsimfunctions.R')
stem<-"twopoint"
source('omitvec.R')

#ispc=TRUE will use the principal curves method; Changing p changes spar - the smoothing parameter. leaving is set to NULL wil use leave one out cross validation to determine the best one.

#ispc=FALSE will use the constrained MDS method;  change p if you need to reduce the penalty imposed for deviations from the sphere

#ndim =2 or 3 will change the number of dimensions used when using the PC method

#lsphere<-load.and.trim(stem,p=100,n=omitvec,ispc=FALSE)
ndim = as.integer(commandArgs(trailingOnly=TRUE)[2])
lpc<-load.and.trim(stem,p=NULL,n=omitvec,ndim=ndim,weightfn='lod2')
