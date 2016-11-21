@echo off
rem call "C:\Program Files (x86)\Intel\Composer XE 2013\bin\ifortvars.bat" intel64
SET FFLAGS=/fast /assume:buffered_io /heap-arrays0 /Qdiag-disable:8290  /c
SET LFLAGS=/SUBSYSTEM:CONSOLE /STACK:16777216,16384
SET ILINKER=xilink.exe
SET FC=ifort.exe
SET SRC=SNP_QTLperm_noimsl.for SNPcluster_noimsl.for SNPmatch_noimsl.for phasev6_noimsl.for read_QTLdata.for realqtlsub_minprint_noimsl.for recalc_chisig.for simple_model.for simple_model_additive.for 
SET QIPOSRC=lsq_ch.f90 realqtlsub_noit_noimsl_alloc.for SNP_QTL_newinput.for 
SET OMPSRC=SNPcexp_sub_noimsl_OMP.for SNPcexp_noimsl_dupcheck_OMP.for SNPmatch_noimsl_OMP.for cluster_chimatrixonly.for
SET LINKOBJ=SNPcluster_noimsl SNPmatch_noimsl phasev6_noimsl read_QTLdata recalc_chisig SNPmatch_noimsl_OMP cluster_chimatrixonly

FOR %%F IN ( %QIPOSRC% ) DO (
%FC% /Qipo %FFLAGS% %%F
)

FOR %%F IN ( %SRC% ) DO (
%FC% %FFLAGS% %%F
)


FOR %%F IN ( %OMPSRC% ) DO (
%FC% /openmp %FFLAGS% %%F
)

FOR %%F IN ( %LINKOBJ% ) DO (
%ILINKER% %LFLAGS% /OUT:%%F.exe %%F.obj
)


%ILINKER% %LFLAGS% /OUT:SNPcexp_noimsl_dupcheck.exe SNPcexp_sub_noimsl_OMP.obj SNPcexp_noimsl_dupcheck_OMP.obj
%ILINKER% %LFLAGS% /OUT:SNP_QTL_newinput.exe SNP_QTL_newinput.obj realqtlsub_noit_noimsl_alloc.obj lsq_ch.obj
%ILINKER% %LFLAGS% /OUT:SNP_QTLperm_noimsl.exe SNP_QTLperm_noimsl.obj realqtlsub_minprint_noimsl.obj lsq_ch.obj
%ILINKER% %LFLAGS% /OUT:simple_model.exe simple_model.obj lsq_ch.obj
%ILINKER% %LFLAGS% /OUT:simple_model_additive.exe simple_model_additive.obj lsq_ch.obj


