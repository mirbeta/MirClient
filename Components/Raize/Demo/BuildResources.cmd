@echo off

set ResComp="C:\Program Files (x86)\Embarcadero\RAD Studio\9.0\Bin\RC.exe"

echo.
echo Compiler: %ResComp%
echo.
echo.


echo ## Compiling RCDemoResources.rc...
echo.
%ResComp% -foRCDemoResources.res RCDemoResources.rc
echo.


if "%1" == "Auto" goto Done
pause
                                
:Done

