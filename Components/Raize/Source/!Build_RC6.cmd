@echo off
echo ***************************************************
echo Building Raize Components 6
echo ***************************************************

rem ****************************************************************************
rem **** IMPORTANT NOTES *******************************************************
rem ****************************************************************************

rem     DO NOT MOVE THIS FILE!
rem     THIS COMMAND FILE MUST BE LOCATED IN THE SOURCE DIRECTORY FOR YOUR
rem     INSTALLATION OF RAIZE COMPONENTS 6.
rem
rem     e.g. C:\Program Files\Raize\RC6\Source
                         

rem     ALL APPLICATIONS THAT USE THE RAIZE COMPONENTS 6 RUNTIME PACKAGES
rem     (INCLUDING DELPHI AND RAD STUDIO) MUST BE SHUT DOWN BEFORE REBUILDING 
rem     THE COMPONENTS.


rem ****************************************************************************
rem **** SET CONFIGURATION VARIABLES *******************************************
rem ****************************************************************************

rem     Uncomment the following goto statement after you have initialized the
rem     Configuration Variables.

rem goto InitComplete

echo.
echo Build Configuration Variables have not been initialized.  
echo.
echo Before you can execute this command file to rebuild Raize Components 6,
echo you must initialize a few configuration variables.  Simply edit the
echo !Build_RC6.cmd file with a text editor and follow the instructions in the
echo SET CONFIGURATION VARIABLES section. Also, please read the IMPORTANT NOTES
echo section.
echo.
echo Once the configuration variables have been initialized and the
echo !Build_RC6.cmd saved, you can simply run !Build_RC6.cmd file to rebuild
echo Raize Components 6.
echo.
echo IMPORTANT: It is recommended that you run this file as an administrator.
echo.
pause
exit

:InitComplete



rem     Set the SysPath32 variable to the path of your Windows System folder
rem     for 32-bit DLLs.
rem
rem     32-bit Windows    Usually C:\Windows\System32
rem
rem     64-bit Windows    Usually C:\Windows\SysWOW64

set SysPath32="C:\Windows\SysWOW64"


rem     Set VCLVersion to match version of Delphi/RAD Studio you are using:
rem
rem     RAD Studio RX10.2 (Delphi RX10.2)   VCLVersion="25"
rem     RAD Studio RX10.1 (Delphi RX10.1)   VCLVersion="24"
rem     RAD Studio RX10   (Delphi RX10)     VCLVersion="23"
rem     RAD Studio XE8    (Delphi XE8)      VCLVersion="22"
rem     RAD Studio XE7    (Delphi XE7)      VCLVersion="21"
rem     RAD Studio XE6    (Delphi XE6)      VCLVersion="20"
rem     RAD Studio XE5    (Delphi XE5)      VCLVersion="19"
rem     RAD Studio XE4    (Delphi XE4)      VCLVersion="18"
rem     RAD Studio XE3    (Delphi XE3)      VCLVersion="17"
rem     RAD Studio XE2    (Delphi XE2)      VCLVersion="16"
rem     RAD Studio XE     (Delphi XE)       VCLVersion="15"
rem     RAD Studio 2010   (Delphi 2010)     VCLVersion="14"
rem     RAD Studio 2009   (Delphi 2009)     VCLVersion="12"

set VCLVersion="25"


rem     Set the DCC32EXE variable to the full path of the 32-bit command line
rem     compiler (DCC32.exe) located in your Delphi/RAD Studio Bin directory.

set DCC32EXE="C:\Program Files (x86)\Embarcadero\Studio\19.0\Bin\DCC32.exe"


rem     If you are using RAD Studio XE2 or greater, set the DCC64EXE 
rem     variable to the full path of the 64-bit command line compiler 
rem     (DCC64.exe) located in your RAD Studio Bin directory.

set DCC64EXE="C:\Program Files (x86)\Embarcadero\Studio\19.0\Bin\DCC64.exe"


rem ****************************************************************************
rem **** DO NOT CHANGE ANYTHING BELOW THIS POINT *******************************
rem ****************************************************************************

if %VCLVersion% == "12" goto Version12
if %VCLVersion% == "14" goto Version14
if %VCLVersion% == "15" goto Version15
if %VCLVersion% == "16" goto Version16
if %VCLVersion% == "17" goto Version17
if %VCLVersion% == "18" goto Version18
if %VCLVersion% == "19" goto Version19
if %VCLVersion% == "20" goto Version20
if %VCLVersion% == "21" goto Version21
if %VCLVersion% == "22" goto Version22
if %VCLVersion% == "23" goto Version23
if %VCLVersion% == "24" goto Version24
if %VCLVersion% == "25" goto Version25
echo Invalid VCL Version %VCLVersion%
goto Error


rem ============================================================================
:Version12

set IDE_Name=RAD Studio 2009
set PkgSuffix=120

set LibDir32=RS2009
set LibDir64=
set Compile64bit=False
set UnitScopeNames=

goto Init

rem ============================================================================
:Version14

set IDE_Name=RAD Studio 2010
set PkgSuffix=140

set LibDir32=RS2010
set LibDir64=
set Compile64bit=False
set UnitScopeNames=

goto Init


rem ============================================================================
:Version15

set IDE_Name=RAD Studio XE
set PkgSuffix=150

set LibDir32=RS-XE
set LibDir64=
set Compile64bit=False
set UnitScopeNames=

goto Init


rem ============================================================================
:Version16

set IDE_Name=RAD Studio XE2
set PkgSuffix=160

set LibDir32=RS-XE2\Win32
set LibDir64=RS-XE2\Win64
set Compile64bit=True
set UnitScopeNames=-NSSystem;System.Win;Winapi;Vcl;Vcl.Imaging;Data;

goto Init


rem ============================================================================
:Version17

set IDE_Name=RAD Studio XE3
set PkgSuffix=170

set LibDir32=RS-XE3\Win32
set LibDir64=RS-XE3\Win64
set Compile64bit=True
set UnitScopeNames=-NSSystem;System.Win;Winapi;Vcl;Vcl.Imaging;Data;

goto Init


rem ============================================================================
:Version18

set IDE_Name=RAD Studio XE4
set PkgSuffix=180

set LibDir32=RS-XE4\Win32
set LibDir64=RS-XE4\Win64
set Compile64bit=True
set UnitScopeNames=-NSSystem;System.Win;Winapi;Vcl;Vcl.Imaging;Data;

goto Init


rem ============================================================================
:Version19

set IDE_Name=RAD Studio XE5
set PkgSuffix=190

set LibDir32=RS-XE5\Win32
set LibDir64=RS-XE5\Win64
set Compile64bit=True
set UnitScopeNames=-NSSystem;System.Win;Winapi;Vcl;Vcl.Imaging;Data;

goto Init


rem ============================================================================
:Version20

set IDE_Name=RAD Studio XE6
set PkgSuffix=200

set LibDir32=RS-XE6\Win32
set LibDir64=RS-XE6\Win64
set Compile64bit=True
set UnitScopeNames=-NSSystem;System.Win;Winapi;Vcl;Vcl.Imaging;Data;

goto Init


rem ============================================================================
:Version21

set IDE_Name=RAD Studio XE7
set PkgSuffix=210

set LibDir32=RS-XE7\Win32
set LibDir64=RS-XE7\Win64
set Compile64bit=True
set UnitScopeNames=-NSSystem;System.Win;Winapi;Vcl;Vcl.Imaging;Data;

goto Init


rem ============================================================================
:Version22

set IDE_Name=RAD Studio XE8
set PkgSuffix=220

set LibDir32=RS-XE8\Win32
set LibDir64=RS-XE8\Win64
set Compile64bit=True
set UnitScopeNames=-NSSystem;System.Win;Winapi;Vcl;Vcl.Imaging;Data;

goto Init


rem ============================================================================
:Version23

set IDE_Name=RAD Studio RX10
set PkgSuffix=230

set LibDir32=RX10\Win32
set LibDir64=RX10\Win64
set Compile64bit=True
set UnitScopeNames=-NSSystem;System.Win;Winapi;Vcl;Vcl.Imaging;Data;

goto Init


rem ============================================================================
:Version24

set IDE_Name=RAD Studio RX10.1
set PkgSuffix=240

set LibDir32=RX10.1\Win32
set LibDir64=RX10.1\Win64
set Compile64bit=True
set UnitScopeNames=-NSSystem;System.Win;Winapi;Vcl;Vcl.Imaging;Data;

goto Init


rem ============================================================================
:Version25

set IDE_Name=RAD Studio RX10.2
set PkgSuffix=250

set LibDir32=RX10.2\Win32
set LibDir64=RX10.2\Win64
set Compile64bit=True
set UnitScopeNames=-NSSystem;System.Win;Winapi;Vcl;Vcl.Imaging;Data;

goto Init


rem ============================================================================
:Init

set Options=-LUDclStd
set DBOptions=-LUDclDB

set DCC32=%DCC32EXE% -Q -W -H %UnitScopeNames% -$D- -$L- -$Y-
set DCC64=%DCC64EXE% -Q -W -H %UnitScopeNames% -$D- -$L- -$Y-

set ND_RTP=RaizeComponentsVcl
set ND_RTP_BPL=RaizeComponentsVcl%PkgSuffix%.bpl
set DB_RTP=RaizeComponentsVclDb
set DB_RTP_BPL=RaizeComponentsVclDb%PkgSuffix%.bpl
set ND_DP=RaizeComponentsVcl_Design
set ND_DP_BPL=RaizeComponentsVcl_Design%PkgSuffix%.bpl
set DB_DP=RaizeComponentsVclDb_Design
set DB_DP_BPL=RaizeComponentsVclDb_Design%PkgSuffix%.bpl


set ND_RegFile=RaizeComponentsVcl_Reg.pas
set DB_RegFile=RaizeComponentsVclDb_Reg.pas

rem ============================================================================
:PathSetup

set LibPath32=..\Lib\%LibDir32%
set LibPath64=..\Lib\%LibDir64%
set BinPath=..\Bin
set DeployPath32=..\Deploy\Win32
set DeployPath64=..\Deploy\Win64
goto Build


rem ============================================================================
rem ==== Build Processing Section ==============================================
rem ============================================================================

:Build

echo.
echo #### %IDE_Name% : 32-bit ############
echo.
echo Compiling %ND_RegFile% File...
echo.
%DCC32% -B %Options% %ND_RegFile%
if errorlevel 1 goto error

echo.
echo Compiling %DB_RegFile% File...
echo.
%DCC32% -B %DBOptions% %DB_RegFile%
if errorlevel 1 goto error

echo.
echo Compiling %ND_RTP%.dpk Package...
echo.
%DCC32% -B -jl -LN. %ND_RTP%.dpk
if errorlevel 1 goto error
echo.

echo.
echo Compiling %DB_RTP%.dpk Package...
echo.
%DCC32% -B -jl -LN. %DB_RTP%.dpk
if errorlevel 1 goto error
echo.

echo.
echo Compiling %ND_DP%.dpk Package...
echo.
%DCC32% -jl -LN. %ND_DP%.dpk
if errorlevel 1 goto error
echo.

echo.
echo Compiling %DB_DP%.dpk Package...
echo.
%DCC32% -jl -LN. %DB_DP%.dpk
if errorlevel 1 goto error
echo.

echo.
echo Deleting Unnecessary Package DCU, HPP, and LIB files...
if %VCLVersion% GEQ 16 goto SkipDeleting32bitPkgDcus
del %ND_RTP%.dcu > nul
del %DB_RTP%.dcu > nul
del %ND_DP%.dcu > nul
del %DB_DP%.dcu > nul
:SkipDeleting32bitPkgDcus

del %ND_DP%.hpp > nul
del %ND_DP%.lib > nul
del %DB_DP%.hpp > nul
del %DB_DP%.lib > nul


echo.
echo Copying Build Files to %LibPath32%...

copy "*.dcu" %LibPath32% > nul
copy "*.dfm" %LibPath32% > nul
copy "*.res" %LibPath32% > nul
copy "*.hpp" %LibPath32% > nul
copy "*.lib" %LibPath32% > nul

copy %ND_RTP%.dcp %LibPath32% > nul
copy %ND_RTP%.bpi %LibPath32% > nul
copy %ND_RTP%.hpp %LibPath32% > nul
copy %ND_RTP_BPL% %DeployPath32% > nul
copy %ND_RTP_BPL% %SysPath32% > nul

copy %DB_RTP%.dcp %LibPath32% > nul
copy %DB_RTP%.bpi %LibPath32% > nul
copy %DB_RTP%.hpp %LibPath32% > nul
copy %DB_RTP_BPL% %DeployPath32% > nul
copy %DB_RTP_BPL% %SysPath32% > nul

copy %ND_DP_BPL% %BinPath% > nul
copy %DB_DP_BPL% %BinPath% > nul


if "%Compile64bit%" == "False" goto SkipCompile64bit
echo.
echo #### %IDE_Name% : 64-bit ############
echo.
echo Compiling %ND_RegFile% File...
echo.
rem %DCC64% -B %Options% %ND_RegFile%
%DCC64% -B -jl %ND_RegFile%
if errorlevel 1 goto error

echo.
echo Compiling %DB_RegFile% File...
echo.
%DCC64% -B -jl %DB_RegFile%
if errorlevel 1 goto error

echo.
echo Compiling %ND_RTP%.dpk Package...
echo.
%DCC64% -B -jl -LN. %ND_RTP%.dpk
if errorlevel 1 goto error
echo.

echo.
echo Compiling %DB_RTP%.dpk Package...
echo.
%DCC64% -B -jl -LN. %DB_RTP%.dpk
if errorlevel 1 goto error
echo.

echo.
if %VCLVersion% GEQ 16 goto SkipDeleting64bitPkgDcus
echo Deleting Unnecessary Package DCU files...
del %ND_RTP%.dcu > nul
del %DB_RTP%.dcu > nul
:SkipDeleting64bitPkgDcus


echo.
echo Copying Build Files to %LibPath64%...

copy "*.dcu" %LibPath64% > nul
copy "*.dfm" %LibPath64% > nul
copy "*.res" %LibPath64% > nul

if %VCLVersion% LEQ 16 goto SkipCopy64bitHppAFiles
rem XE2 does not support 64-bit C++, so only copy *.hpp and *.a files for XE3 or later
copy "*.hpp" %LibPath64% > nul
copy "*.a" %LibPath64% > nul
:SkipCopy64bitHppAFiles

copy %ND_RTP%.dcp %LibPath64% > nul
copy %ND_RTP%.bpi %LibPath64% > nul
copy %ND_RTP%.hpp %LibPath64% > nul
copy %ND_RTP_BPL% %DeployPath64% > nul

copy %DB_RTP%.dcp %LibPath64% > nul
copy %DB_RTP%.bpi %LibPath64% > nul
copy %DB_RTP%.hpp %LibPath64% > nul
copy %DB_RTP_BPL% %DeployPath64% > nul

:SkipCompile64bit


goto Success


rem ============================================================================
:Success
echo.
echo Build was Successful.
goto end


rem ============================================================================
:error
echo.
echo **ERROR**

rem ============================================================================
:end
pause
