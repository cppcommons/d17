@echo off
set PATH=C:\Windows\System32
::set PATH=%PATH%;E:\opt\node-v9.2.1-win-x86
::set PATH=%PATH%;E:\opt\node-v8.9.3-win-x86
::set PATH=%PATH%;E:\opt\node-v7.10.1-win-x86
set PATH=%PATH%;E:\opt\node-v7.9.0-win-x86
set PATH=%PATH%;C:\python27
set PATH=%PATH%;C:\Program Files\Microsoft VS Code\bin
set PATH=%PATH%;C:\D\dmd2\windows\bin
set PATH=%PATH%;E:\opt\bin32
set PATH=%PATH%;E:\opt\opt.m32\mingw32\qt5-static\bin
set PATH=%PATH%;E:\opt\opt.m32\mingw32\bin
set PATH=%PATH%;E:\opt\opt.m32\usr\bin
set PATH=%PATH%;E:\opt\cmder-v1.3.3\vendor\git-for-windows\bin
set PATH=%PATH%;E:\opt\cmake-3.9.4-win64-x64\bin
set PATH=%PATH%;E:\opt\codeblocks-16.01mingw-nosetup
set PATH=%PATH%;C:\ProgramData\chocolatey\bin
@echo on
::chcp 65001
::call "C:\Program Files (x86)\Microsoft Visual Studio\VC98\Bin\VCVARS32.BAT"
call "C:\Program Files (x86)\Microsoft Visual Studio 14.0\VC\vcvarsall.bat" x86
@echo on
