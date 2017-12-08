import pegged.grammar; // https://github.com/PhilippeSigaud/Pegged/wiki
//import dparser; //import pegged.examples.dgrammar; mixin(grammar(Dgrammar));
import lang1;

import std.conv;
import std.datetime.stopwatch;
import std.variant;

enum Lang1Grammar = `
Lang1:
    _TopLevel       < (_Def+ "[eof]"i) / (_Def+ eoi)
    Keywords        < "var" / "let" / PrintKeyword / FunctionHead / ProcedureHead / Direction / _Type
    _VarAssign      < Identifier "=" Integer
    VarStatement    < "var" _VarAssign ";"
    LetDecl         < "let" _VarAssign ";"
    StatementBlock_ < "{" LetDecl* (_Statement / StatementBlock_)* "}"
    PrintKeyword    < "print"
    PrintStatement  < PrintKeyword Identifier ";"
    _Statement      < VarStatement / PrintStatement
    DecimalInteger  <- Integer IntegerSuffix?
    Integer         <~ digit (digit/"_")*
    IntegerSuffix   <- "Lu" / "LU" / "uL" / "UL"
                     / "L" / "u" / "U"
    _Def            < _Statement / StatementBlock_ / _Prototype / EasyDoc
    Identifier      < (!Keywords identifier)
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
    _Type           < MsgpackType / JsonType
    EasyDoc         <~ (:"[doc]"i (!"[/doc]"i .)* :"[/doc]"i)
    Comment1        <~ "/*" (!"*/" .)* "*/"
    Comment2        <~ "//" (!endOfLine .)* :endOfLine
    Spacing         <- (blank / Comment1 / Comment2)*
`;

mixin(grammar(Lang1Grammar));

string src = `
var x = 1234;
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
	//import std.array : join;

	asModule(`lang1`, `lang1`, Lang1Grammar);
	auto mod0 = lang1.Lang1(src);
	//writeln(`mod0=`, mod0);

	StopWatch sw;
	sw.start();

	sw.reset();
	auto mod = Lang1(src);
	writeln(`(A)`, sw.peek());

	writeln(mod);

	sw.reset();
	//cut_nodes(mod, null, true, null);
	cut_nodes(mod, null, false, null);
	writeln(`(B)`, sw.peek());

	writeln(mod);
	writeln(mod.children[0].name);

	sw.reset();
	long[string] var_tbl;
	for (size_t i = 0; i < mod.children.length; i++)
	{
		//writeln(`  `, mod.children[i].name);
		ParseTree* stmt = &(mod.children[i]);
		writeln(`  `, stmt.name);
		switch (stmt.name)
		{
		case `Lang1.VarStatement`:
			{
				string var_name = stmt.children[0].matches[0];
				writeln(`    `, var_name);
				//long var_value = std.conv.to!long(stmt.children[1].matches[0]);
				long var_value = to!long(stmt.children[1].matches[0]);
				writeln(`    `, var_value);
				var_tbl[var_name] = var_value;
				writeln(`    `, var_tbl);
			}
			break;
		case `Lang1.PrintStatement`:
			{
				string var_name = stmt.children[1].matches[0];
				writeln(`    `, var_name);
				long* found = var_name in var_tbl;
				if (found)
					writeln(`    `, *found);
				else
					writeln(`    null`);
				//writeln(`    `, var_tbl[var_name]);
			}
			break;
		case `Lang1.StatementBlock`:
			break;
		default:
			break;
		}
	}
	writeln("kanji=漢字");
	writeln(`(C)`, sw.peek());

	Variant v;
	//writeln(sw.peek());
}

void cut_nodes(TParseTree)(ref TParseTree p, string[] names1 = null,
		bool clear_auto_nodes = false, string[] names2 = null)
{
	import std.algorithm : canFind, endsWith, startsWith;
	import std.string : indexOf;

	if (p.name.endsWith(`_`))
	{
		p.matches.length = 0;
		p.name = p.name[0 .. $ - 1];
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
			if (child.name.canFind('!'))
			{
			}
			else if (names1 !is null && names1.canFind(child.name))
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
		cut_nodes!TParseTree(child, names1, clear_auto_nodes, names2);
	}
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
