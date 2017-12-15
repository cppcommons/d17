;@echo %~f0 %* & exit /b %errorlevel%
chcp 65001
g++ -shared -static -o ltest.dll -Ilua-5.3.4/include luatest.cpp -Llua-5.3.4 -llua53
lua53.exe luatest.lua a b c
echo local sub=require('ltest'); print(sub.getclip()) | lua53 -
echo print(#arg) | lua53 - a b c
echo print( require('ltest').getclip() ) | lua53 -
::pause
C:\dm\bin\implib /system lua53-dm32.lib lua53.dll
pexports lua53.dll > lua53.def
lib /def:lua53.def /machine:x86 /out:lua53-ms32.lib
::edub ltest2.dll build ltest2.d arch=ms32 inc=lua lua/macros.d lua53-ms32.lib libcmt.lib 
edub ltest2.dll build ltest2.d arch=dm32 inc=lua lua/macros.d lua53-dm32.lib phobos.lib 
echo print( require('ltest2').getclip() ) | lua53 -
::calldll ltest2.dll EntryA@4 "test1 test2"
Rundll32-ng.exe ltest2.dll EntryA@4
