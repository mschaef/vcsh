@echo off

SETLOCAL

SET SCC0_BASE=%~dp0%

echo %SCC0_BASE%

: Fixup the SCANPATH to point to the build hierarchies scheme directory. This
: catches a some of the packages that are 'required' by scheme before the
: scansh0 command line parser loads them.
SET SCANPATH=%SCC0_BASE%\..\base-scheme:%SCANPATH%

SET LAUNCH_FILES=
SET LAUNCH_FILES=%LAUNCH_FILES% %SCC0_BASE%\scheme.scf
SET LAUNCH_FILES=%LAUNCH_FILES% %SCC0_BASE%\fasl-compiler-run.scf
SET LAUNCH_FILES=%LAUNCH_FILES% %SCC0_BASE%\fasl-compiler0.scf

%SCC0_BASE%\..\vm\scansh0 %LAUNCH_FILES% %1 %2 %3 %4 %5 %6 %7 %8 %9
