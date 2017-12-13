g++ -shared -static -o ltest.dll -Ilua-5.3.4/include luatest.cpp -Llua-5.3.4 -llua53
lua53.exe luatest.lua a b c
