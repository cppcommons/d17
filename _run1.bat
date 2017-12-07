/+ 1>nul 2>nul
@chcp 65001
copy %~dpnx0 ___tmp.d
edub ___tmp.exe run ___tmp.d arch=dm32 inc=.
@if %errorlevel% neq 0 (exit /b)
@goto :eof
+/

int main()
{
	import std.stdio;
	writeln(`hello!`);
	return 0;
}
