chcp 65001
g++ -shared -static -o ltest.dll -Ilua-5.3.4/include luatest.cpp -Llua-5.3.4 -llua53
lua53.exe luatest.lua a b c
echo local sub=require('ltest'); print(sub.getclip()) | lua53 -
echo print(#arg) | lua53 - a b c
echo print( require('ltest').getclip() ) | lua53 -
::pause