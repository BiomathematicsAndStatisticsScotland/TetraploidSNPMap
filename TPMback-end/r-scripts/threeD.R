library(rgl)
conf<-read.table("smacofsym.conf")
locikey<-read.table("locikey")
map<-read.table("pc")
plot3d(conf,type="n")
text3d(conf,text=locikey$locus)
lines3d(map)
while(rgl.cur()) {
Sys.sleep(0.0005)
}