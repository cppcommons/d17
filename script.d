import std.conv;
import std.stdio;

void main(string[] args)
{
    import var2mod;

    var2 x1 = 123.45;
    writeln(x1.get!long);
    writeln(x1.get!real);
    writeln(x1.get!string);
    var2 x2 = 123;
    writeln(x2.get!long);
    writeln(x2.get!real);
    writeln(x2.get!string);
    var2 x3 = &add2;
    writeln(x3.get!long);
    writeln(x3.get!real);
    writeln(x3.get!string);
    var2 answer = x3(11, 22.5);
    writeln(`answer=`, answer);
    var2 v;
    v = `abc`;
    writeln("[v(1)]");
    writeln(v.get!long);
    writeln(v.get!real);
    writeln(v.get!string);
    var2[string] obj;
    obj[`a`] = `abc`;
    obj[`b`] = 123.45;
    v = obj;
    writeln("[v(2)]");
    writeln(v.get!long);
    writeln(v.get!real);
    writeln(v.get!string);
    writeln(v.get!(var2[string]));
    //A _a = new A;
    //v = _a;
    //B _b;
    //v = _b;
    int[] i;
    i ~= 1;
    i ~= 2;
    v = i;
    writeln(v);
    v = true;
    writeln(v);
    writeln(v.get!bool);
    var2[] fargs;
    var2 arg1 = 22;
    var2 arg2 = 33;
    fargs ~= arg1;
    fargs ~= var2(44);
    var2 answer2 = x3.apply(fargs);
    writeln(answer2);
    answer2 = x3.apply([var2(1), var2(22)]);
    writeln(answer2);

    var2[string] _aa;
    var2 aa = _aa;
    aa[`x`] = 123;
    writeln(aa.get!(var2[string]));
    writeln(aa);
    writeln(aa[`x`]);
    aa.yyy = 777;
    writeln(aa);
    writeln(aa.yyy);
}

int add2(int a, int b)
{
    return a + b;
}
