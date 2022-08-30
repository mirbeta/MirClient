rd ModelSupport /S /Q

rd __history /S /Q

rd download /S /Q

rd LOG /S /Q

rd xcode /S /Q

del *.plist
del *.o
del *.so

del *_Icon.ico
del *.tvsconfig

del *.deploy*

del *.txt
del *.cfg
del *.dof
del *.ddp
del *.bdsproj
del *.rsm
del *.m*
del *.local
del *.identcache
del *.tgs
del *.tgw
del *.dcu
del *.~*
del *.log
del *.stat
del *.drc
del *.obj
del *.hpp
del *.tds
del *.dsk

call _Pack
copy *.exe ..\..\..\Bin\RTCSDK
del *.exe
del *.
