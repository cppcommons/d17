/+ 1>nul 2>nul
@chcp 65001
copy %~dpnx0 tmp___.d
edub tmp___.exe run tmp___.d arch=dm32 inc=. ^
def=EnableReal ^
[vibe-d:data] [my-msgpack-d#@my-msgpack-d]
@if %errorlevel% neq 0 (exit /b)
@goto :eof
+/

int main()
{
	import std.stdio;
	writeln(`hello!`);
	return 0;
}
