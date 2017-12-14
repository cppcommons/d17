import core.sys.windows.windows;
import core.sys.windows.winbase;

import core.sys.windows.dll : SimpleDllMain; // file:///C:\D\dmd2\src\druntime\import\core\sys\windows\dll.d
mixin SimpleDllMain;

extern (C) export int my_add2(int a, int b)
{
    import std.stdio;
    writeln(`a=`, a);
    writeln(`b=`, b);
    /+
     import core.stdc.stdio; // : printf;
     freopen("CONIN$", "r", stdin);
     freopen("CONOUT$", "w", stdout);
     freopen("CONOUT$", "w", stderr);
     printf("a=%d\n", a);
     +/
    return a + b;
}
