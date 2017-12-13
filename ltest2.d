import lua.types;
import lua.functions;
import lua.macros;

private extern (C) int l_getclip(lua_State* L) nothrow
{
    const char* s = "<getclip2>";
    lua_pushstring(L, s);
    return 1;
}

private luaL_Reg reg[] = [{"getclip", &l_getclip}, {null, null}];

extern (C) export int luaopen_ltest2(lua_State* L)
{
    import std.stdio;
    writeln("luaopen_ltest2");
    luaL_newlib(L, reg);
    return 1;
}
