# TetraploidSNPMap

fortran sources are in TPMback-end
java sources are in TPMfront-end
users manual and example data sets are in docdata

the build directory contains some very basic batch files to build everything. 'maketpm.bat' is the main one.
it requires:
* ant
* launch4j
* intel composer (fortran)
* WIX (to create the MSI)
* JDK

It also requires an R installation with the following libraries. All these libraries must be installed within the main R library folder.

Formula Hmisc MASS Matrix R6 RColorBrewer Rcpp  acepack base chron cluster colorspace data.table datasets digest fastcluster foreign gdata ggplot2 grDevices graphics grid gridExtra gtable gtools  htmltools htmlwidgets httpuv jsonlite knitr lattice latticeExtra magrittr  methods mice mime munsell nnet nnls plyr polynom princurve reshape rgl rpart scales shiny smacof splines stats survival tools utils weights xtable
