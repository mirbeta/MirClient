@echo off

del __history\*.* /Q
rd __history

cd RemoteFn_Wizard
call _clear
cd ..

cd WebServer_LogReader
call _clear
cd ..

cd WebStress_Client
call _clear
cd ..

cd UnitTesting
call _clear
cd ..

cd RouterCheck
call _clear
cd ..

del *.rsm
del *.map
del *.local
del *.identcache
del *.tgs
del *.tgw
del *.dcu
del *.~*
del *.log
del *.stat
del *.mps
del *.mpt

del *.obj
del *.hpp
del *.tds

del *.exe

