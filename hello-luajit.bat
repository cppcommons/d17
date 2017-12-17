pexports e:\opt\bin32\lua51.dll > lua51.def
dlltool --input-def lua51.def --output-lib liblua51.a
gcc -o a.exe -IE:\d-dev\d17\LuaJIT-2.0.5\src hello-luajit.c -L. -llua51
a.exe luatest.lua
