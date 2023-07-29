@echo off
set BCC=c:\bc7\binb\bc.exe
set LINKER=c:\bc7\binb\link.exe

%BCC% /E/X/O/Ot/Fs/Lr/FPi/T/C:512 escape1.bas;
REM objs,exefile,mapfile,libs,deffile
%LINKER% /EX /NOE /NOD:BRT71EFR.LIB escape1.obj,escape.exe,,kbd.lib+c:\bc7\lib\bcl71efr.lib,;

deltree /Y dist

mkdir dist
mkdir dist\gfx
mkdir dist\levels

copy escape.exe dist\
copy README.TXT dist\
copy STSG.EXE dist\
copy gfx\*.* dist\gfx\
copy levels\*.* dist\levels\

rem del dist.zip
rem cd dist
rem pkzip -r -p ..\dist.zip *.*
rem cd ..
