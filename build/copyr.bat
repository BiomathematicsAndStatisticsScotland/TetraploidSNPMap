@echo off
mkdir "%MYR%\bin\x64"
mkdir "%MYR%\library"
mkdir "%MYR%\modules\x64"

set LIBS=Formula Hmisc MASS Matrix R6 RColorBrewer Rcpp  acepack base chron cluster colorspace data.table datasets digest fastcluster foreign gdata ggplot2 grDevices graphics grid gridExtra gtable gtools  htmltools htmlwidgets httpuv jsonlite knitr lattice latticeExtra magrittr  methods mice mime munsell nnet nnls plyr polynom princurve reshape rgl rpart scales shiny smacof splines stats survival tools utils weights xtable

copy "%RFROM%\bin\x64\R.dll"  "%MYR%\bin\x64\"
copy "%RFROM%\bin\x64\Rblas.dll" "%MYR%\bin\x64\"
copy "%RFROM%\bin\x64\Rgraphapp.dll" "%MYR%\bin\x64\"
copy "%RFROM%\bin\x64\Riconv.dll" "%MYR%\bin\x64\"
copy "%RFROM%\bin\x64\Rlapack.dll" "%MYR%\bin\x64\"
copy "%RFROM%\bin\x64\Rscript.exe" "%MYR%\bin\x64\"
copy "%RFROM%\bin\x64\Rblas.dll" "%MYR%\bin\x64\"
copy "%RFROM%\modules\x64\lapack.dll" "%MYR%\modules\x64\lapack.dll"

FOR %%L IN ( %LIBS% ) DO (
ROBOCOPY "%RFROM%\library\%%L" "%MYR%\library\%%L" /S /XD html doc help i386 include data tests demo examples staticdocs www www-dir po misc themes prompt skeleton unitTests discovery perl xls fonts htmlwidgets
)
