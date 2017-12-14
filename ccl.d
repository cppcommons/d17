import core.runtime;

extern (C) export int my_add2(int a, int b)
{
    Runtime.initialize();
    /+
    import std.stdio;
    try {
        writeln(`a=`, a);
    } catch(Exception ex)
    {
    }
    +/
    import core.stdc.stdio; // : printf;
	freopen("CONIN$", "r", stdin);
	freopen("CONOUT$", "w", stdout);
	freopen("CONOUT$", "w", stderr);
    printf("a=%d\n", a);
    //writeln(`b=`, b);
    return a + b;
}
