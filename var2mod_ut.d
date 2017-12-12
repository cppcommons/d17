module var2mod_ut;

import var2mod;

version (unittest)
{
    import fluent.asserts;
    import std.stdio;
    import std.math: isNaN;
}

unittest
{
    scope (success)
        writeln("[unittest(@", __FILE__, ":", __LINE__, ") succeeded]");
    //true.should.equal(false).because("this is a failing assert");
    var2 x0 = true;
    writeln(__LINE__, "==>", x0);
    (x0.get!string).should.equal("true");
    (x0.toString).should.equal("Boolean(true)");
    (x0.get!long).should.equal(1);
    (x0.get!int).should.equal(1);
    (x0.get!real).should.equal(1);
}

unittest
{
    scope (success)
        writeln("[unittest(@", __FILE__, ":", __LINE__, ") succeeded]");
    var2 x1 = 123.45;
    writeln(__LINE__, "==>", x1);
    (x1.get!string).should.equal("123.45");
    (x1.toString).should.equal("Floating(123.45)");
    (x1.get!long).should.equal(123);
    (x1.get!real).should.be.approximately(123.45, 0.01);
}

unittest
{
    scope (success)
        writeln("[unittest(@", __FILE__, ":", __LINE__, ") succeeded]");
    var2 x2 = 123;
    writeln(__LINE__, "==>", x2);
    (x2.get!string).should.equal("123");
    (x2.toString).should.equal("Integral(123)");
    (x2.get!long).should.equal(123);
    (x2.get!real).should.equal(123);
    (x2.get!string).should.equal("123");
}

unittest
{
    scope (success)
        writeln("[unittest(@", __FILE__, ":", __LINE__, ") succeeded]");
    int add2(int a, int b)
    {
        return a + b;
    }
    var2 x3 = &add2;
    writeln(__LINE__, "==>", x3);
    var2 answer = x3(11, 22.5);
    answer.toString.should.equal("Integral(33)");
    (x3.toString).should.equal("Function(<function>)");
    (x3.get!long).should.equal(0);
    Assert.equal(true, isNaN(x3.get!real));
    (x3.get!string).should.equal("<function>");
}

unittest
{
    scope (success)
        writeln("[unittest(@", __FILE__, ":", __LINE__, ") succeeded]");
    var2 x4;
    writeln(__LINE__, "==>", x4);
    (x4.get!string).should.equal("null");
    (x4.toString).should.equal("Null");
    (x4.get!long).should.equal(0);
    Assert.equal(true, isNaN(x4.get!real));
    (x4.get!string).should.equal("null");
}

unittest
{
    scope (success)
        writeln("[unittest(@", __FILE__, ":", __LINE__, ") succeeded]");
    var2 x5 = "kanji=漢字";
    writeln(__LINE__, "==>", x5);
    (x5.toString).should.equal("String(kanji=漢字)");
    (x5.get!long).should.equal(0);
    Assert.equal(true, isNaN(x5.get!real));
    (x5.get!string).should.equal("kanji=漢字");
}

unittest
{
    scope (success)
        writeln("[unittest(@", __FILE__, ":", __LINE__, ") succeeded]");
    var2[string] obj;
    obj[`a`] = `abc`;
    obj[`b`] = 123.45;
    var2 x6 = obj;
    writeln(__LINE__, "==>", x6);
    (x6.toString).should.equal(`Object["b":Floating(123.45), "a":String(abc)]`);
    (x6.get!long).should.equal(0);
    Assert.equal(true, isNaN(x6.get!real));
    (x6.get!string).should.equal(`["b":Floating(123.45), "a":String(abc)]`);
}

