import lua.types;
import lua.functions;
import lua.macros;

import core.sys.windows.dll : SimpleDllMain; // file:///C:\D\dmd2\src\druntime\import\core\sys\windows\dll.d
mixin SimpleDllMain;

extern (Windows) export int EntryA(const char *arg)
{
    import std.stdio : writeln, readln;
	import core.stdc.stdio; // : freopen, stderr, stdin, stdout;
	import core.sys.windows.windows;
	import core.sys.windows.winbase;

    /+
	//AllocConsole();
	if (!AttachConsole(ATTACH_PARENT_PROCESS))
		AllocConsole();
	freopen("CONIN$", "r", stdin);
	freopen("CONOUT$", "w", stdout);
	freopen("CONOUT$", "w", stderr);
    +/
    writeln("[EntryA()]");
    //writeln(arg);
    //readln();
    return 1234;
}

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
