rmdir /s /q node_modules
del package.json
call npm init -y
call npm install electron@1.7.9
call npm install asar
::call npm install electron-packager
cd electron-api-demos-1.3.0
rmdir /s /q node_modules
call npm install
::electron-packager . sample --platform=win32 --arch=ia32 --version=1.7.9 --overwrite
::asar pack . ..\sample.asar
node.exe ..\node_modules\asar\bin\asar.js pack . ..\sample.asar
cd ..
node_modules\electron\dist\electron.exe sample.asar
