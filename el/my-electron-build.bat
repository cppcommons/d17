@setlocal
set PATH=C:\Windows\System32
::set PATH=%PATH%;E:\opt\node-v9.2.1-win-x86
::set PATH=%PATH%;E:\opt\node-v8.9.3-win-x86
::set PATH=%PATH%;E:\opt\node-v7.10.1-win-x86
set PATH=%PATH%;E:\opt\node-v7.9.0-win-x86
set PATH=%PATH%;C:\python27
call "C:\Program Files (x86)\Microsoft Visual Studio 14.0\VC\vcvarsall.bat" x86
@echo on

rmdir /s /q node_modules
::del package.json
::call npm init -y
::call npm install --save electron ffi ref
::call npm install --save electron-packager
call npm install

call npm config set msvs_version 2015
call npm config set python c:\python27\python.exe
call npm install -g node-gyp
call npm rebuild -g node-gyp

call npm install electron-rebuild --save-dev
call .\node_modules\.bin\electron-rebuild.cmd --module-dir .
@endlocal
