module runtime;

int rt_add2(int a, int b)
{
    return a + b;
}

long[string] var_tbl;

void rt_def_var(string var_name, long var_value)
{
    var_tbl[var_name] = var_value;

}

void rt_dump()
{
    import std.stdio;
    writeln(var_tbl);
}

