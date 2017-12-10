import pegged.grammar;
import pegged.cutter;
import std.conv; // バージョン3で追加
import std.stdio;

enum Lang1Grammar = `
Lang1:
# Entry Point
_TopLevel       <  _EvalUnit+ eoi  # バージョン4で変更
# Keyword List
Keywords        <  "dump" / "var"
# Grammars
DumpStatement   < "dump" ";"
_EvalUnit       <  VarDeclaration / DumpStatement  # バージョン4で変更
Identifier      <  (!Keywords identifier)
Integer         <~ digit+
VarDeclaration  <  "var" Identifier "=" Integer ";"
# Spacing
Comment1        <~ "/*" (!"*/" .)* "*/"
Comment2        <~ "//" (!endOfLine .)* :endOfLine
Spacing         <- (blank / Comment1 / Comment2)*
`;
mixin(grammar(Lang1Grammar));

enum Lang1Source = `
var x = 1234;
var y = 5678;
dump;
`;

class A
{
    public int a;
    string b;
}

struct B
{
    public int a;
    string b;
}

void main(string[] args)
{
    ParseTree pt = Lang1(Lang1Source);
    // pt.cutNodes([`Lang1.TopLevel`, `Lang1.EvalUnit`]); // バージョン2で追加⇒バージョン4で削除
    pt.cutNodes(); // バージョン4で追加
    writeln(pt);
    // これ以降を追加(バージョン3)
    enum string code = compile(Lang1Source);
    pragma(msg, code);
    writeln(code);
    run!code();
    writeln("kanji=漢字");
    import runtime;

    writeln(rt_add2(11, 22));
    register(typeid(A));
    register(typeid(B));
}

void register(TypeInfo t)
{
    import std.traits;

    writeln(t.toString);
    Object o = Object.factory(t.toString);
    writeln(o);
    //const(OffsetTypeInfo)[] tiList = t.classinfo.offTi();
    const(OffsetTypeInfo)[] tiList = t.offTi();
    writeln(tiList);

    //import os1.lang.jsvar;
    //import os1.lang;
    import arsd.jsvar;

    var a = 10;
    var b = a - 5;
    a = [1, 2, 3];
    a[1] = "two";
    writeln(a);

    int xxx = 123;

    a = json!q{
		"foo":{"bar":100},
		"joe":"joesph"
	};

    a.xxx = long.max;
    a.yyy = cast(real) long.max;
    writeln(a.zzz);
    assert(a.zzz == var(null));
    a.zzz = var.emptyObject;
    a.zzz.z = `😀😬😁😂😃😄😅😆😇😉😊🙂☺️😋😌😍😘😗😙😚😜😝😛😎😏😶😐😑😒😳😞😟😠😡😔😕🙁☹️😣😖😫😩😤😮😱😨😰😯😦😧😢😥😪😓😭😵😲😷😴💤💩😈👿👹👺💀👻👽`;

    writeln(a.joe);
    a.foo.bar += 55;
    b = a;
    writeln(b.foo.bar);

    var c = (var d) { writeln("hello, ", d, "!"); };
    c("adr");
    c(100);
    writeln(a);
    writeln(a.yyy);
    writeln(cast(long) a.yyy);
    test_script();

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
    var v2;
    writeln(v2);
    var2[] args;
    var2 arg1 = 22;
    var2 arg2 = 33;
    args ~= arg1;
    args ~= var2(44);
    var2 answer2 = x3.apply(args);
    writeln(answer2);
    answer2 = x3.apply([var2(1), var2(22)]);
    writeln(answer2);

    var2[string] _aa;
    var2 aa = _aa;
    //aa[`x`] = 123;
    writeln(aa.get!(var2[string]));
    writeln(aa);
    //writeln(aa[`x`]);
    aa.yyy = 777;
    writeln(aa);
    writeln(aa.yyy);
}

int add2(int a, int b)
{
    return a + b;
}

void test_script()
{
    //import os1.lang.script;
    //import os1.lang;
    import arsd.script;

    writeln(interpret("x*x + 3*x;", var(["x" : 3])));
    var env = var.emptyObject;
    env["x"] = long.max;
    env["y"] = `abc`;
    env["a"] = 11;
    env["b"] = 22;
    env[`add2`] = &add2;
    writeln(env);
    writeln(interpret("y", env));
    writeln(interpret("var z=x;", env));
    writeln(env);
    writeln(interpret("var a=add2(a,b);", env));
    writeln(env);
}

string compile(string src)
{
    import std.format;

    ParseTree pt = Lang1(src);
    pt.cutNodes();
    string result = "import runtime;\n";
    for (size_t i = 0; i < pt.children.length; i++)
    {
        ParseTree* unit = &(pt.children[i]);
        switch (unit.name)
        {
        case `Lang1.VarDeclaration`:
            {
                string var_name = unit.children[0].matches[0];
                long var_value = to!long(unit.children[1].matches[0]);
                result ~= format!"rt_def_var(`%s`, %d);\n"(var_name, var_value);
            }
            break;
        case `Lang1.DumpStatement`:
            {
                result ~= "rt_dump();\n";
            }
            break;
        case `Lang1.StatementBlock`:
            break;
        default:
            break;
        }
    }
    return result;
}

void run(string code)()
{
    mixin(code);
}
