@echo off
set VERSION=1.0.1

set ANT=C:\Program Files\apache-ant-1.9.7\bin\ant
set LAUNCH4J=C:\Program Files (x86)\Launch4j\launch4jc.exe
set IFORT=C:\Program Files (x86)\Intel\Composer XE 2013\bin\ifortvars.bat
set BACKEND=..\TPMback-end
set fortran=..\TPMback-end\SNP-fortran
rem FRONTEND is the folder containing 'lib' and  'res'
set FRONTEND=..\TPMfront-end
rem WORKSPACE is the folder containing buildfileant.xml and src.
set WORKSPACE=..\TPMfront-end
set DOCDATA=..\docdata

pushd %fortran%
call  "%IFORT%" intel64
call compile.bat
popd

rem compile the java:
pushd %WORKSPACE%
call "%ANT%" -f buildfileant.xml
popd

rem make the jar file: 
call "%ANT%" -Ddir.workspace="%WORKSPACE%" -f build-tpmsnp.xml
"%LAUNCH4J%" tpmsnp-launch4j.xml

rem MAKE THE R COPY:

set RFROM=C:\Program Files\R\R-3.3.1
set MYR=%FRONTEND%\lib\R
call copyr.bat


"%WIX%\bin\heat.exe" dir %MYR% -cg MyRstuff -dr lib -sreg -sfrag -ag -var var.RSOURCE -o Rstuff.wxs
"%WIX%\bin\candle.exe" -wx -arch x86 -dBuildTarget="TPMSNP" -dRSOURCE=%MYR% -o Rstuff.wixobj Rstuff.wxs
"%WIX%\bin\candle.exe" -wx -arch x86 -dBuildTarget="TPMSNP" -dVERSION=%VERSION% -dfortran="%fortran%" -dBACKEND="%BACKEND%" -dDOCDATA="%DOCDATA%" -o tpm64.wixobj tpm64.wxs
"%WIX%\bin\light.exe" -ext WixUIExtension -dBuildTarget="TPMSNP" -out tpmsnp-%VERSION%.msi tpm64.wixobj Rstuff.wixobj

