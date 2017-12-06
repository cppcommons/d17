import pegged.grammar; // https://github.com/PhilippeSigaud/Pegged/wiki
import dparser; //import pegged.examples.dgrammar; mixin(grammar(Dgrammar));

private void exit(int code)
{
	import std.c.stdlib;

	std.c.stdlib.exit(code);
}

string pkgs = `
/*before*/
 //xyz
 /*123*/
/+
first doc
 doc
 doc.
+/

// comment

 handle archive_t;
 handle handle_t;
 function test() : char*;
 function test(...):real32;
 function test(a char*):int64*;
 procedure test(a: int32 dual, b: int64);
 proc test(a: int32 dual, b: int64 out);
 func test(h: handle archive_t, a: int32, b: char * out) int32;
 func test(json) json;
 func test(msgpack) msgpack;
 func test(a: int32* dual) int32*;
[doc]
doc doc doc
[/doc]
 func test(a: ucstring8 dual, /*ttt*/ b: mbstring in) ucstring32;
 /*
 this function is ...abc!
 this function is ...abc!
 */
[eof]
aaa bbb
xxx
this is end of file.
`;

//
// "EasyIDL.Type"
mixin(grammar(`
EasyIDL:
    _Idl            < (_Def+ "[eof]"i) / (_Def+ eoi)
    EndOfFile       <- "[eof]"i / eoi
    Keywords        < FunctionHead / ProcedureHead / Direction / _Type
    _Def            < Handle / _Prototype / EasyDoc
    Ident           < (!Keywords identifier)
    Handle          < "handle" Name ";"
    _Prototype      < Function / Procedure
    Function        < ;FunctionHead Name Parameters ":"? ReturnValue ";"
    FunctionHead    < ("function" / "func")
    Procedure       < ;ProcedureHead Name Parameters ";"
    ProcedureHead   < ("procedure" / "proc")
    ReturnValue     < _Type
    Parameters      < "(" _ParameterList? ")"
    _ParameterList  < VarArgs / JsonType / MsgpackType / Parameter (',' Parameter)*
    Parameter       < Name ":"? _Type Direction?
    Name            < identifier
    VarArgs         < "..."
    JsonType        < "json"
    MsgpackType     < "msgpack"
    Direction       < "in" / "out" / "dual"
    _Type           < Primitive / ManagedType / MsgpackType / JsonType / HandleType
    Primitive       < ("int32" / "int64" / "byte" / "char" / "wchar" / "real32" / "real64") PointerMark?
    #Pointer        < Primitive ;PointerMark
    PointerMark     < "*"
    ManagedType     < "mbstring" / "ansistring" / "ucstring8" / "ucstring16" / "ucstring32" / "array8" / "array16" / "array32" / "array64" / "object" / "service"
    HandleType      < :"handle" identifier
    EasyDoc         <~ (:"/+" (!"+/" .)* :"+/") / (:"[doc]"i (!"[/doc]"i .)* :"[/doc]"i)
    Comment1        <~ "/*" (!"*/" .)* "*/"
    Comment2        <~ "//" (!endOfLine .)* :endOfLine
    #Spacing        <- (blank / Comment1 / Comment2 / Comment3)*
    Spacing         <- (blank / Comment1 / Comment2)*
`));

private string get_def_type(ref ParseTree p)
{
	import std.string : split;

	return p.name.split(".")[1];
}

private ParseTree[] find_named_children(ref ParseTree p, string def_type)
{
	import std.stdio : writefln, writeln;
	import std.string : split;

	ParseTree[] result;
	foreach (ref child; p.children)
	{
		string child_def_type = child.get_def_type();
		writefln("child_def_type=%s", child_def_type);
		if (child_def_type == def_type)
			result ~= child;
	}
	return result;
}

private void cut_unnecessary_nodes(ref ParseTree p, string[] names = null, string[] names2 = null)
{
	import std.algorithm : canFind, endsWith, startsWith;

	//import std.stdio : writeln;
	import std.string : indexOf;

	if (p.name.endsWith(`_`))
	{
		p.matches.length = 0;
	}
	else if (names2 !is null && names2.canFind(p.name))
	{
		p.matches.length = 0;
	}

	if (p.children.length == 0)
		return;

	bool processed = true;
	while (processed)
	{
		processed = false;
		ParseTree[] new_children;
		foreach (ref child; p.children)
		{
			if (names !is null && names.canFind(child.name))
			{
			}
			else if (child.name.canFind('!'))
			{
			}
			else if (child.name.indexOf("._") == -1)
			{
				new_children ~= child;
				continue;
			}
			foreach (ref grand_child; child.children)
			{
				new_children ~= grand_child;
			}
			processed = true;
		}
		p.children = new_children;
	}
	foreach (ref child; p.children)
	{
		cut_unnecessary_nodes(child, names, names2);
	}
}

mixin(grammar(`
Lang1:
    _TopLevel       < (_Def+ "[eof]"i) / (_Def+ eoi)
    Keywords        < VarKeyword / LetKeyword / FunctionHead / ProcedureHead / Direction / _Type
    VarKeyword      < "var"
    VarAssign       < Ident "=" DecimalInteger
    VarStatement    < VarKeyword VarAssign ";"
    LetKeyword      < "let"
    LetDecl         < LetKeyword VarAssign ";"
    StatementBlock_ < "{" LetDecl* (Statement / StatementBlock_)* "}"
    Statement       < VarStatement / "print" Ident ";"
    DecimalInteger  <- Integer IntegerSuffix?
    Integer         <- digit (digit/"_")*
    IntegerSuffix   <- "Lu" / "LU" / "uL" / "UL"
                     / "L" / "u" / "U"
    _Def            < Statement / StatementBlock_ / _Prototype / EasyDoc
    Ident           < (!Keywords identifier)
    _Prototype      < Function / Procedure
    Function        < ;FunctionHead Name Parameters ":"? ReturnValue ";"
    FunctionHead    < ("function" / "func")
    Procedure       < ;ProcedureHead Name Parameters ";"
    ProcedureHead   < ("procedure" / "proc")
    ReturnValue     < _Type
    Parameters      < "(" _ParameterList? ")"
    _ParameterList  < VarArgs / JsonType / MsgpackType / Parameter (',' Parameter)*
    Parameter       < Name ":"? _Type Direction?
    Name            < identifier
    VarArgs         < "..."
    JsonType        < "json"
    MsgpackType     < "msgpack"
    Direction       < "in" / "out" / "dual"
    _Type           < Primitive / ManagedType / MsgpackType / JsonType / HandleType
    Primitive       < ("int32" / "int64" / "byte" / "char" / "wchar" / "real32" / "real64") PointerMark?
    PointerMark     < "*"
    ManagedType     < "mbstring" / "ansistring" / "ucstring8" / "ucstring16" / "ucstring32" / "array8" / "array16" / "array32" / "array64" / "object" / "service"
    HandleType      < :"handle" identifier
    EasyDoc         <~ (:"/+" (!"+/" .)* :"+/") / (:"[doc]"i (!"[/doc]"i .)* :"[/doc]"i)
    Comment1        <~ "/*" (!"*/" .)* "*/"
    Comment2        <~ "//" (!endOfLine .)* :endOfLine
    Spacing         <- (blank / Comment1 / Comment2)*
`));

string src = `
var x = 1;
{ let x = 2; let y = 3;
  print x;
  print y;
}
print x;
print y;
`;

void main(string[] args)
{
	import std.stdio;
	import std.array : join;

	version (none)
	{
		import dgrammar;

		asModule("dparser", "temp_dparser", Dgrammar);
		exit(0);
	}

	//GenericD!(ParseTree) D;
	//auto mod = D.D.Module(src);
	//cut_unnecessary_nodes(mod, [`D.DeclDefs`, `D.DeclDef`, `D.Declaration`,
	//		`D.BasicTypeX`, `D.Type`, `D.Declarators`]);
	auto mod = Lang1(src);
	cut_unnecessary_nodes(mod, null, [`Lang1`]);
	writeln(mod);
	writeln(mod.children[0].name);
	exit(0);

	foreach (arg; args)
	{
		writeln(arg);
	}

	{
		import std.string : strip;

		writefln("pkgs.length=%d", pkgs.length);
		writefln("pkgs=%s", pkgs);
		if (strip(pkgs) == "")
		{
			writeln("empty pkgs");
			return;
		}
		auto p = EasyIDL(pkgs);
		writeln("(0)", p.name);
		writeln("(1)", p.children[0].name);
		writeln(p);
		string[] unnecessary = [ //"EasyIDL.Idl", "EasyIDL.Def", "EasyIDL.Prototype", /*"EasyIDL.Parameters",*/
		//"EasyIDL.ParameterList", "EasyIDL.FunctionHead", "EasyIDL.ProcedureHead", "EasyIDL.Type"
		];
		/*p =*/
		cut_unnecessary_nodes(p);
		writeln("(2)", p.name);
		writeln("(3)", p.children[0].name);
		writeln(p);
		if (!p.successful)
		{
			writeln("not success!");
			return;
		}
		//cut_unnecessary_nodes(p, unnecessary);
		//writeln(p);
		////writeln(p.matches.length);
		for (int i = 0; i < p.children.length; i++)
		{
			import std.string : replace, splitLines, strip;

			auto child = p.children[i];
			auto description = child.input[child.begin .. child.end];
			description = description.strip();
			foreach (line; description.splitLines)
			{
				if (line.strip().length != 0)
					writeln("//EasyIDL: ", line);
			}
			writefln("%d: %s ==> %s", i, child.name, child.matches.join(" ").strip());
			//gen_cpp_code("mymodule_", child);
			writeln();
		}
	}

	//import pegged.examples.dgrammar;
	//asModule("dparser", "temp_dparser", Dgrammar);
	writeln("kanji=漢字");
}

/+
struct ParseTree
{
    string name; /// The node name
    bool successful; /// Indicates whether a parsing was successful or not
    string[] matches; /// The matched input's parts. Some expressions match at more than one place, hence matches is an array.

    string input; /// The input string that generated the parse tree. Stored here for the parse tree to be passed to other expressions, as input.
    size_t begin, end; /// Indices for the matched part (from the very beginning of the first match to the last char of the last match.

    ParseTree[] children; /// The sub-trees created by sub-rules parsing.

    /**
    Basic toString for easy pretty-printing.
    */
    string toString(string tabs = "") const
    {
        string result = name;

        string childrenString;
        bool allChildrenSuccessful = true;

        foreach(i,child; children)
        {
            childrenString ~= tabs ~ " +-" ~ child.toString(tabs ~ ((i < children.length -1 ) ? " | " : "   "));
            if (!child.successful)
                allChildrenSuccessful = false;
        }

        if (successful)
        {
            result ~= " " ~ to!string([begin, end]) ~ to!string(matches) ~ "\n";
        }
        else // some failure info is needed
        {
            if (allChildrenSuccessful) // no one calculated the position yet
            {
                Position pos = position(this);
                string left, right;

                if (pos.index < 10)
                    left = input[0 .. pos.index];
                else
                    left = input[pos.index-10 .. pos.index];
                //left = strip(left);

                if (pos.index + 10 < input.length)
                    right = input[pos.index .. pos.index + 10];
                else
                    right = input[pos.index .. $];
                //right = strip(right);

                result ~= " failure at line " ~ to!string(pos.line) ~ ", col " ~ to!string(pos.col) ~ ", "
                       ~ (left.length > 0 ? "after " ~ left.stringified ~ " " : "")
                       ~ "expected "~ (matches.length > 0 ? matches[$-1].stringified : "NO MATCH")
                       ~ ", but got " ~ right.stringified ~ "\n";
            }
            else
            {
                result ~= " (failure)\n";
            }
        }

        return result ~ childrenString;
    }

    @property string failMsg()
    {
        foreach(i, child; children)
        {
            if (!child.successful)
                return child.failMsg;
        }

        if (!successful)
        {
            Position pos = position(this);
            string left, right;

            if (pos.index < 10)
                left = input[0 .. pos.index];
            else
                left = input[pos.index - 10 .. pos.index];

            if (pos.index + 10 < input.length)
                right = input[pos.index .. pos.index + 10];
            else
                right = input[pos.index .. $];

            return "Failure at line " ~ to!string(pos.line) ~ ", col " ~ to!string(pos.col) ~ ", "
                ~ (left.length > 0 ? "after " ~ left.stringified ~ " " : "")
                ~ "expected " ~ (matches.length > 0 ? matches[$ - 1].stringified : "NO MATCH")
                ~ `, but got ` ~ right.stringified;
        }

        return "Success";
    }

    ParseTree dup() @property
    {
        ParseTree result = this;
        result.matches = result.matches.dup;
        result.children = map!(p => p.dup)(result.children).array();
        return result;
    }
}
+/
