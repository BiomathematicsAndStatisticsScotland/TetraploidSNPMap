library(fastcluster, quietly=TRUE, warn.conflicts=FALSE) #loads the required wMDS functions

outfile = "cluster.parphen"

nloci = as.integer(commandArgs(trailingOnly=TRUE)[1])
nlocip1 = nloci + 1
ngroups = as.integer(commandArgs(trailingOnly=TRUE)[2])
nindividuals = as.integer(commandArgs(trailingOnly=TRUE)[3])
cat("step: 10\n")
lm<-read.table("cluster.distmatrix", header=TRUE, fill=TRUE)
cat("step: 90\n")
lm_v <- lm[1:nloci,2:nlocip1]
lm_names <- lm[1:nloci,1]
lm_d <- as.dist(lm_v)
cat("step: 93\n")
lm_cl <- hclust(lm_d, "ave")
cat("step: 96\n")
lm_cut <- cutree(lm_cl, k=ngroups)
cat("step: 98\n")
cat("\n Average linkage clustering\n", file=outfile, append=TRUE)
lmmerge = lm_cl$merge
lmmerge[lmmerge > 0] = lmmerge[lmmerge > 0] + nloci
lmmerge[lmmerge < 0] = lmmerge[lmmerge < 0] * -1
write.table(data.frame(lm_cl$height, lmmerge), outfile, append=TRUE, quote=FALSE, col.names=FALSE)
for(group in seq(ngroups)) {
	cat("\n Linkage group ",group,"\n ", nindividuals, " ", length(lm_names[lm_cut==group]), "\n", file=outfile, append=TRUE)
	write.table(as.matrix(lm_names[lm_cut==group]), file=outfile, append=TRUE,col.names=FALSE,row.names=FALSE,quote=FALSE)	
}
cat("step: 100\n")
