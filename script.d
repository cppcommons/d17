import std.conv;
import std.stdio;

private void exit(int code)
{
    import std.c.stdlib;

    std.c.stdlib.exit(code);
}

void main(string[] args)
{
    import var2mod;

    var2 x0 = true;
    writeln(x0);
    writeln(`x0.get!string=`, x0.get!string);
    writeln(`x0.get!long=`, x0.get!long);
    writeln(`x0.get!int=`, x0.get!int);
    writeln(`x0.get!real=`, x0.get!real);
    writeln();

    var2 x1 = 123.45;
    writeln(`x1.get!long=`, x1.get!long);
    writeln(`x1.get!real=`, x1.get!real);
    writeln(`x1.get!string=`, x1.get!string);
    writeln();

    var2 x2 = 123;
    writeln(`x2.get!long=`, x2.get!long);
    writeln(`x2.get!real=`, x2.get!real);
    writeln(`x2.get!string=`, x2.get!string);
    writeln();

    var2 x3 = &add2;
    writeln(`x3.get!long=`, x3.get!long);
    writeln(`x3.get!real=`, x3.get!real);
    writeln(`x3.get!string=`, x3.get!string);
    var2 answer = x3(11, 22.5);
    writeln(`answer=`, answer);
    writeln();
    //exit(0);

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
    string[] slist = [`aaa`, `bbb`];
    v = slist;
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
    writeln(`answer2(A)=`, answer2);
    answer2 = x3.apply([var2(1), var2(22)]);
    writeln(`answer2(B)=`, answer2);

    var2[string] _aa;
    var2 aa = _aa;
    aa[`x`] = 123;
    ////writeln(aa.get!(var2[string]));
    writeln(aa);
    writeln(aa[`x`]);
    aa.yyy = 777;
    writeln(aa);
    writeln(aa.yyy);
    writeln(`kanji=漢字`);
}

int add2(int a, int b)
{
    return a + b;
}
