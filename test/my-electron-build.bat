del package.json
rmdir /s /q node_modules

@setlocal
set PATH=C:\Windows\System32
set PATH=%PATH%;E:\opt\node-v8.9.3-win-x86
set PATH=%PATH%;C:\python27
call "C:\Program Files (x86)\Microsoft Visual Studio 14.0\VC\vcvarsall.bat" x86
@echo on

call npm init -y
call npm install electron ffi ref
call npm config set msvs_version 2015
call npm config set python c:\python27\python.exe
call npm install -g node-gyp
call npm rebuild -g node-gyp

call npm install electron-rebuild --save-dev
call .\node_modules\.bin\electron-rebuild.cmd --module-dir .
@endlocal
