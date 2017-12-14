#! bash -uvx
edub ltest2.dll build ltest2.d arch=dm32 inc=lua lua/macros.d lua53-dm32.lib phobos.lib 
#echo "print( require('ltest2').getclip() )" | lua53 -
./lua53.exe luatest.lua
