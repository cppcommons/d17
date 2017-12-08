/++
This module was automatically generated from the following grammar:


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
    #PointerMark     < "*"
    EasyDoc         <~ (:"[doc]"i (!"[/doc]"i .)* :"[/doc]"i)
    Comment1        <~ "/*" (!"*/" .)* "*/"
    Comment2        <~ "//" (!endOfLine .)* :endOfLine
    Spacing         <- (blank / Comment1 / Comment2)*


+/
module lang1;

public import pegged.peg;
import std.algorithm: startsWith;
import std.functional: toDelegate;

struct GenericLang1(TParseTree)
{
    import std.functional : toDelegate;
    import pegged.dynamic.grammar;
    static import pegged.peg;
    struct Lang1
    {
    enum name = "Lang1";
    static ParseTree delegate(ParseTree)[string] before;
    static ParseTree delegate(ParseTree)[string] after;
    static ParseTree delegate(ParseTree)[string] rules;
    import std.typecons:Tuple, tuple;
    static TParseTree[Tuple!(string, size_t)] memo;
    static this()
    {
        rules["_TopLevel"] = toDelegate(&_TopLevel);
        rules["Keywords"] = toDelegate(&Keywords);
        rules["_VarAssign"] = toDelegate(&_VarAssign);
        rules["VarStatement"] = toDelegate(&VarStatement);
        rules["LetDecl"] = toDelegate(&LetDecl);
        rules["StatementBlock_"] = toDelegate(&StatementBlock_);
        rules["PrintKeyword"] = toDelegate(&PrintKeyword);
        rules["PrintStatement"] = toDelegate(&PrintStatement);
        rules["_Statement"] = toDelegate(&_Statement);
        rules["DecimalInteger"] = toDelegate(&DecimalInteger);
        rules["Integer"] = toDelegate(&Integer);
        rules["IntegerSuffix"] = toDelegate(&IntegerSuffix);
        rules["_Def"] = toDelegate(&_Def);
        rules["Identifier"] = toDelegate(&Identifier);
        rules["_Prototype"] = toDelegate(&_Prototype);
        rules["Function"] = toDelegate(&Function);
        rules["FunctionHead"] = toDelegate(&FunctionHead);
        rules["Procedure"] = toDelegate(&Procedure);
        rules["ProcedureHead"] = toDelegate(&ProcedureHead);
        rules["ReturnValue"] = toDelegate(&ReturnValue);
        rules["Parameters"] = toDelegate(&Parameters);
        rules["_ParameterList"] = toDelegate(&_ParameterList);
        rules["Parameter"] = toDelegate(&Parameter);
        rules["Name"] = toDelegate(&Name);
        rules["VarArgs"] = toDelegate(&VarArgs);
        rules["JsonType"] = toDelegate(&JsonType);
        rules["MsgpackType"] = toDelegate(&MsgpackType);
        rules["Direction"] = toDelegate(&Direction);
        rules["_Type"] = toDelegate(&_Type);
        rules["EasyDoc"] = toDelegate(&EasyDoc);
        rules["Comment1"] = toDelegate(&Comment1);
        rules["Comment2"] = toDelegate(&Comment2);
        rules["Spacing"] = toDelegate(&Spacing);
    }

    template hooked(alias r, string name)
    {
        static ParseTree hooked(ParseTree p)
        {
            ParseTree result;

            if (name in before)
            {
                result = before[name](p);
                if (result.successful)
                    return result;
            }

            result = r(p);
            if (result.successful || name !in after)
                return result;

            result = after[name](p);
            return result;
        }

        static ParseTree hooked(string input)
        {
            return hooked!(r, name)(ParseTree("",false,[],input));
        }
    }

    static void addRuleBefore(string parentRule, string ruleSyntax)
    {
        // enum name is the current grammar name
        DynamicGrammar dg = pegged.dynamic.grammar.grammar(name ~ ": " ~ ruleSyntax, rules);
        foreach(ruleName,rule; dg.rules)
            if (ruleName != "Spacing") // Keep the local Spacing rule, do not overwrite it
                rules[ruleName] = rule;
        before[parentRule] = rules[dg.startingRule];
    }

    static void addRuleAfter(string parentRule, string ruleSyntax)
    {
        // enum name is the current grammar named
        DynamicGrammar dg = pegged.dynamic.grammar.grammar(name ~ ": " ~ ruleSyntax, rules);
        foreach(name,rule; dg.rules)
        {
            if (name != "Spacing")
                rules[name] = rule;
        }
        after[parentRule] = rules[dg.startingRule];
    }

    static bool isRule(string s)
    {
		import std.algorithm : startsWith;
        return s.startsWith("Lang1.");
    }
    mixin decimateTree;

    static TParseTree _TopLevel(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, _Def, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.caseInsensitiveLiteral!("[eof]"), Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, _Def, Spacing)), pegged.peg.wrapAround!(Spacing, eoi, Spacing)), Spacing)), "Lang1._TopLevel")(p);
        }
        else
        {
            if (auto m = tuple(`_TopLevel`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, _Def, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.caseInsensitiveLiteral!("[eof]"), Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, _Def, Spacing)), pegged.peg.wrapAround!(Spacing, eoi, Spacing)), Spacing)), "Lang1._TopLevel"), "_TopLevel")(p);
                memo[tuple(`_TopLevel`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree _TopLevel(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, _Def, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.caseInsensitiveLiteral!("[eof]"), Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, _Def, Spacing)), pegged.peg.wrapAround!(Spacing, eoi, Spacing)), Spacing)), "Lang1._TopLevel")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, _Def, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.caseInsensitiveLiteral!("[eof]"), Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, _Def, Spacing)), pegged.peg.wrapAround!(Spacing, eoi, Spacing)), Spacing)), "Lang1._TopLevel"), "_TopLevel")(TParseTree("", false,[], s));
        }
    }
    static string _TopLevel(GetName g)
    {
        return "Lang1._TopLevel";
    }

    static TParseTree Keywords(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("var"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("let"), Spacing), pegged.peg.wrapAround!(Spacing, PrintKeyword, Spacing), pegged.peg.wrapAround!(Spacing, FunctionHead, Spacing), pegged.peg.wrapAround!(Spacing, ProcedureHead, Spacing), pegged.peg.wrapAround!(Spacing, Direction, Spacing), pegged.peg.wrapAround!(Spacing, _Type, Spacing)), "Lang1.Keywords")(p);
        }
        else
        {
            if (auto m = tuple(`Keywords`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("var"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("let"), Spacing), pegged.peg.wrapAround!(Spacing, PrintKeyword, Spacing), pegged.peg.wrapAround!(Spacing, FunctionHead, Spacing), pegged.peg.wrapAround!(Spacing, ProcedureHead, Spacing), pegged.peg.wrapAround!(Spacing, Direction, Spacing), pegged.peg.wrapAround!(Spacing, _Type, Spacing)), "Lang1.Keywords"), "Keywords")(p);
                memo[tuple(`Keywords`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Keywords(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("var"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("let"), Spacing), pegged.peg.wrapAround!(Spacing, PrintKeyword, Spacing), pegged.peg.wrapAround!(Spacing, FunctionHead, Spacing), pegged.peg.wrapAround!(Spacing, ProcedureHead, Spacing), pegged.peg.wrapAround!(Spacing, Direction, Spacing), pegged.peg.wrapAround!(Spacing, _Type, Spacing)), "Lang1.Keywords")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("var"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("let"), Spacing), pegged.peg.wrapAround!(Spacing, PrintKeyword, Spacing), pegged.peg.wrapAround!(Spacing, FunctionHead, Spacing), pegged.peg.wrapAround!(Spacing, ProcedureHead, Spacing), pegged.peg.wrapAround!(Spacing, Direction, Spacing), pegged.peg.wrapAround!(Spacing, _Type, Spacing)), "Lang1.Keywords"), "Keywords")(TParseTree("", false,[], s));
        }
    }
    static string Keywords(GetName g)
    {
        return "Lang1.Keywords";
    }

    static TParseTree _VarAssign(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, Integer, Spacing)), "Lang1._VarAssign")(p);
        }
        else
        {
            if (auto m = tuple(`_VarAssign`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, Integer, Spacing)), "Lang1._VarAssign"), "_VarAssign")(p);
                memo[tuple(`_VarAssign`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree _VarAssign(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, Integer, Spacing)), "Lang1._VarAssign")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, Integer, Spacing)), "Lang1._VarAssign"), "_VarAssign")(TParseTree("", false,[], s));
        }
    }
    static string _VarAssign(GetName g)
    {
        return "Lang1._VarAssign";
    }

    static TParseTree VarStatement(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("var"), Spacing), pegged.peg.wrapAround!(Spacing, _VarAssign, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "Lang1.VarStatement")(p);
        }
        else
        {
            if (auto m = tuple(`VarStatement`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("var"), Spacing), pegged.peg.wrapAround!(Spacing, _VarAssign, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "Lang1.VarStatement"), "VarStatement")(p);
                memo[tuple(`VarStatement`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree VarStatement(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("var"), Spacing), pegged.peg.wrapAround!(Spacing, _VarAssign, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "Lang1.VarStatement")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("var"), Spacing), pegged.peg.wrapAround!(Spacing, _VarAssign, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "Lang1.VarStatement"), "VarStatement")(TParseTree("", false,[], s));
        }
    }
    static string VarStatement(GetName g)
    {
        return "Lang1.VarStatement";
    }

    static TParseTree LetDecl(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("let"), Spacing), pegged.peg.wrapAround!(Spacing, _VarAssign, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "Lang1.LetDecl")(p);
        }
        else
        {
            if (auto m = tuple(`LetDecl`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("let"), Spacing), pegged.peg.wrapAround!(Spacing, _VarAssign, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "Lang1.LetDecl"), "LetDecl")(p);
                memo[tuple(`LetDecl`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree LetDecl(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("let"), Spacing), pegged.peg.wrapAround!(Spacing, _VarAssign, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "Lang1.LetDecl")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("let"), Spacing), pegged.peg.wrapAround!(Spacing, _VarAssign, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "Lang1.LetDecl"), "LetDecl")(TParseTree("", false,[], s));
        }
    }
    static string LetDecl(GetName g)
    {
        return "Lang1.LetDecl";
    }

    static TParseTree StatementBlock_(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, LetDecl, Spacing)), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, _Statement, Spacing), pegged.peg.wrapAround!(Spacing, StatementBlock_, Spacing)), Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing)), "Lang1.StatementBlock_")(p);
        }
        else
        {
            if (auto m = tuple(`StatementBlock_`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, LetDecl, Spacing)), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, _Statement, Spacing), pegged.peg.wrapAround!(Spacing, StatementBlock_, Spacing)), Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing)), "Lang1.StatementBlock_"), "StatementBlock_")(p);
                memo[tuple(`StatementBlock_`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree StatementBlock_(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, LetDecl, Spacing)), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, _Statement, Spacing), pegged.peg.wrapAround!(Spacing, StatementBlock_, Spacing)), Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing)), "Lang1.StatementBlock_")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, LetDecl, Spacing)), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, _Statement, Spacing), pegged.peg.wrapAround!(Spacing, StatementBlock_, Spacing)), Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing)), "Lang1.StatementBlock_"), "StatementBlock_")(TParseTree("", false,[], s));
        }
    }
    static string StatementBlock_(GetName g)
    {
        return "Lang1.StatementBlock_";
    }

    static TParseTree PrintKeyword(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("print"), Spacing), "Lang1.PrintKeyword")(p);
        }
        else
        {
            if (auto m = tuple(`PrintKeyword`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("print"), Spacing), "Lang1.PrintKeyword"), "PrintKeyword")(p);
                memo[tuple(`PrintKeyword`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree PrintKeyword(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("print"), Spacing), "Lang1.PrintKeyword")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("print"), Spacing), "Lang1.PrintKeyword"), "PrintKeyword")(TParseTree("", false,[], s));
        }
    }
    static string PrintKeyword(GetName g)
    {
        return "Lang1.PrintKeyword";
    }

    static TParseTree PrintStatement(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, PrintKeyword, Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "Lang1.PrintStatement")(p);
        }
        else
        {
            if (auto m = tuple(`PrintStatement`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, PrintKeyword, Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "Lang1.PrintStatement"), "PrintStatement")(p);
                memo[tuple(`PrintStatement`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree PrintStatement(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, PrintKeyword, Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "Lang1.PrintStatement")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, PrintKeyword, Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "Lang1.PrintStatement"), "PrintStatement")(TParseTree("", false,[], s));
        }
    }
    static string PrintStatement(GetName g)
    {
        return "Lang1.PrintStatement";
    }

    static TParseTree _Statement(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, VarStatement, Spacing), pegged.peg.wrapAround!(Spacing, PrintStatement, Spacing)), "Lang1._Statement")(p);
        }
        else
        {
            if (auto m = tuple(`_Statement`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, VarStatement, Spacing), pegged.peg.wrapAround!(Spacing, PrintStatement, Spacing)), "Lang1._Statement"), "_Statement")(p);
                memo[tuple(`_Statement`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree _Statement(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, VarStatement, Spacing), pegged.peg.wrapAround!(Spacing, PrintStatement, Spacing)), "Lang1._Statement")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, VarStatement, Spacing), pegged.peg.wrapAround!(Spacing, PrintStatement, Spacing)), "Lang1._Statement"), "_Statement")(TParseTree("", false,[], s));
        }
    }
    static string _Statement(GetName g)
    {
        return "Lang1._Statement";
    }

    static TParseTree DecimalInteger(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(Integer, pegged.peg.option!(IntegerSuffix)), "Lang1.DecimalInteger")(p);
        }
        else
        {
            if (auto m = tuple(`DecimalInteger`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(Integer, pegged.peg.option!(IntegerSuffix)), "Lang1.DecimalInteger"), "DecimalInteger")(p);
                memo[tuple(`DecimalInteger`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree DecimalInteger(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(Integer, pegged.peg.option!(IntegerSuffix)), "Lang1.DecimalInteger")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(Integer, pegged.peg.option!(IntegerSuffix)), "Lang1.DecimalInteger"), "DecimalInteger")(TParseTree("", false,[], s));
        }
    }
    static string DecimalInteger(GetName g)
    {
        return "Lang1.DecimalInteger";
    }

    static TParseTree Integer(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(digit, pegged.peg.zeroOrMore!(pegged.peg.or!(digit, pegged.peg.literal!("_"))))), "Lang1.Integer")(p);
        }
        else
        {
            if (auto m = tuple(`Integer`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(digit, pegged.peg.zeroOrMore!(pegged.peg.or!(digit, pegged.peg.literal!("_"))))), "Lang1.Integer"), "Integer")(p);
                memo[tuple(`Integer`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Integer(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(digit, pegged.peg.zeroOrMore!(pegged.peg.or!(digit, pegged.peg.literal!("_"))))), "Lang1.Integer")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(digit, pegged.peg.zeroOrMore!(pegged.peg.or!(digit, pegged.peg.literal!("_"))))), "Lang1.Integer"), "Integer")(TParseTree("", false,[], s));
        }
    }
    static string Integer(GetName g)
    {
        return "Lang1.Integer";
    }

    static TParseTree IntegerSuffix(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.keywords!("Lu", "LU", "uL", "UL", "L", "u", "U"), "Lang1.IntegerSuffix")(p);
        }
        else
        {
            if (auto m = tuple(`IntegerSuffix`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.keywords!("Lu", "LU", "uL", "UL", "L", "u", "U"), "Lang1.IntegerSuffix"), "IntegerSuffix")(p);
                memo[tuple(`IntegerSuffix`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree IntegerSuffix(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.keywords!("Lu", "LU", "uL", "UL", "L", "u", "U"), "Lang1.IntegerSuffix")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.keywords!("Lu", "LU", "uL", "UL", "L", "u", "U"), "Lang1.IntegerSuffix"), "IntegerSuffix")(TParseTree("", false,[], s));
        }
    }
    static string IntegerSuffix(GetName g)
    {
        return "Lang1.IntegerSuffix";
    }

    static TParseTree _Def(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, _Statement, Spacing), pegged.peg.wrapAround!(Spacing, StatementBlock_, Spacing), pegged.peg.wrapAround!(Spacing, _Prototype, Spacing), pegged.peg.wrapAround!(Spacing, EasyDoc, Spacing)), "Lang1._Def")(p);
        }
        else
        {
            if (auto m = tuple(`_Def`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, _Statement, Spacing), pegged.peg.wrapAround!(Spacing, StatementBlock_, Spacing), pegged.peg.wrapAround!(Spacing, _Prototype, Spacing), pegged.peg.wrapAround!(Spacing, EasyDoc, Spacing)), "Lang1._Def"), "_Def")(p);
                memo[tuple(`_Def`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree _Def(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, _Statement, Spacing), pegged.peg.wrapAround!(Spacing, StatementBlock_, Spacing), pegged.peg.wrapAround!(Spacing, _Prototype, Spacing), pegged.peg.wrapAround!(Spacing, EasyDoc, Spacing)), "Lang1._Def")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, _Statement, Spacing), pegged.peg.wrapAround!(Spacing, StatementBlock_, Spacing), pegged.peg.wrapAround!(Spacing, _Prototype, Spacing), pegged.peg.wrapAround!(Spacing, EasyDoc, Spacing)), "Lang1._Def"), "_Def")(TParseTree("", false,[], s));
        }
    }
    static string _Def(GetName g)
    {
        return "Lang1._Def";
    }

    static TParseTree Identifier(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.wrapAround!(Spacing, Keywords, Spacing)), pegged.peg.wrapAround!(Spacing, identifier, Spacing)), Spacing), "Lang1.Identifier")(p);
        }
        else
        {
            if (auto m = tuple(`Identifier`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.wrapAround!(Spacing, Keywords, Spacing)), pegged.peg.wrapAround!(Spacing, identifier, Spacing)), Spacing), "Lang1.Identifier"), "Identifier")(p);
                memo[tuple(`Identifier`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Identifier(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.wrapAround!(Spacing, Keywords, Spacing)), pegged.peg.wrapAround!(Spacing, identifier, Spacing)), Spacing), "Lang1.Identifier")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.wrapAround!(Spacing, Keywords, Spacing)), pegged.peg.wrapAround!(Spacing, identifier, Spacing)), Spacing), "Lang1.Identifier"), "Identifier")(TParseTree("", false,[], s));
        }
    }
    static string Identifier(GetName g)
    {
        return "Lang1.Identifier";
    }

    static TParseTree _Prototype(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, Function, Spacing), pegged.peg.wrapAround!(Spacing, Procedure, Spacing)), "Lang1._Prototype")(p);
        }
        else
        {
            if (auto m = tuple(`_Prototype`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, Function, Spacing), pegged.peg.wrapAround!(Spacing, Procedure, Spacing)), "Lang1._Prototype"), "_Prototype")(p);
                memo[tuple(`_Prototype`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree _Prototype(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, Function, Spacing), pegged.peg.wrapAround!(Spacing, Procedure, Spacing)), "Lang1._Prototype")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, Function, Spacing), pegged.peg.wrapAround!(Spacing, Procedure, Spacing)), "Lang1._Prototype"), "_Prototype")(TParseTree("", false,[], s));
        }
    }
    static string _Prototype(GetName g)
    {
        return "Lang1._Prototype";
    }

    static TParseTree Function(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.drop!(pegged.peg.wrapAround!(Spacing, FunctionHead, Spacing)), pegged.peg.wrapAround!(Spacing, Name, Spacing), pegged.peg.wrapAround!(Spacing, Parameters, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing)), pegged.peg.wrapAround!(Spacing, ReturnValue, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "Lang1.Function")(p);
        }
        else
        {
            if (auto m = tuple(`Function`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.drop!(pegged.peg.wrapAround!(Spacing, FunctionHead, Spacing)), pegged.peg.wrapAround!(Spacing, Name, Spacing), pegged.peg.wrapAround!(Spacing, Parameters, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing)), pegged.peg.wrapAround!(Spacing, ReturnValue, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "Lang1.Function"), "Function")(p);
                memo[tuple(`Function`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Function(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.drop!(pegged.peg.wrapAround!(Spacing, FunctionHead, Spacing)), pegged.peg.wrapAround!(Spacing, Name, Spacing), pegged.peg.wrapAround!(Spacing, Parameters, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing)), pegged.peg.wrapAround!(Spacing, ReturnValue, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "Lang1.Function")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.drop!(pegged.peg.wrapAround!(Spacing, FunctionHead, Spacing)), pegged.peg.wrapAround!(Spacing, Name, Spacing), pegged.peg.wrapAround!(Spacing, Parameters, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing)), pegged.peg.wrapAround!(Spacing, ReturnValue, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "Lang1.Function"), "Function")(TParseTree("", false,[], s));
        }
    }
    static string Function(GetName g)
    {
        return "Lang1.Function";
    }

    static TParseTree FunctionHead(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("function"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("func"), Spacing)), Spacing), "Lang1.FunctionHead")(p);
        }
        else
        {
            if (auto m = tuple(`FunctionHead`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("function"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("func"), Spacing)), Spacing), "Lang1.FunctionHead"), "FunctionHead")(p);
                memo[tuple(`FunctionHead`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree FunctionHead(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("function"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("func"), Spacing)), Spacing), "Lang1.FunctionHead")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("function"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("func"), Spacing)), Spacing), "Lang1.FunctionHead"), "FunctionHead")(TParseTree("", false,[], s));
        }
    }
    static string FunctionHead(GetName g)
    {
        return "Lang1.FunctionHead";
    }

    static TParseTree Procedure(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.drop!(pegged.peg.wrapAround!(Spacing, ProcedureHead, Spacing)), pegged.peg.wrapAround!(Spacing, Name, Spacing), pegged.peg.wrapAround!(Spacing, Parameters, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "Lang1.Procedure")(p);
        }
        else
        {
            if (auto m = tuple(`Procedure`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.drop!(pegged.peg.wrapAround!(Spacing, ProcedureHead, Spacing)), pegged.peg.wrapAround!(Spacing, Name, Spacing), pegged.peg.wrapAround!(Spacing, Parameters, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "Lang1.Procedure"), "Procedure")(p);
                memo[tuple(`Procedure`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Procedure(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.drop!(pegged.peg.wrapAround!(Spacing, ProcedureHead, Spacing)), pegged.peg.wrapAround!(Spacing, Name, Spacing), pegged.peg.wrapAround!(Spacing, Parameters, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "Lang1.Procedure")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.drop!(pegged.peg.wrapAround!(Spacing, ProcedureHead, Spacing)), pegged.peg.wrapAround!(Spacing, Name, Spacing), pegged.peg.wrapAround!(Spacing, Parameters, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "Lang1.Procedure"), "Procedure")(TParseTree("", false,[], s));
        }
    }
    static string Procedure(GetName g)
    {
        return "Lang1.Procedure";
    }

    static TParseTree ProcedureHead(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("procedure"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("proc"), Spacing)), Spacing), "Lang1.ProcedureHead")(p);
        }
        else
        {
            if (auto m = tuple(`ProcedureHead`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("procedure"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("proc"), Spacing)), Spacing), "Lang1.ProcedureHead"), "ProcedureHead")(p);
                memo[tuple(`ProcedureHead`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ProcedureHead(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("procedure"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("proc"), Spacing)), Spacing), "Lang1.ProcedureHead")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("procedure"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("proc"), Spacing)), Spacing), "Lang1.ProcedureHead"), "ProcedureHead")(TParseTree("", false,[], s));
        }
    }
    static string ProcedureHead(GetName g)
    {
        return "Lang1.ProcedureHead";
    }

    static TParseTree ReturnValue(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, _Type, Spacing), "Lang1.ReturnValue")(p);
        }
        else
        {
            if (auto m = tuple(`ReturnValue`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, _Type, Spacing), "Lang1.ReturnValue"), "ReturnValue")(p);
                memo[tuple(`ReturnValue`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ReturnValue(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, _Type, Spacing), "Lang1.ReturnValue")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, _Type, Spacing), "Lang1.ReturnValue"), "ReturnValue")(TParseTree("", false,[], s));
        }
    }
    static string ReturnValue(GetName g)
    {
        return "Lang1.ReturnValue";
    }

    static TParseTree Parameters(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, _ParameterList, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), "Lang1.Parameters")(p);
        }
        else
        {
            if (auto m = tuple(`Parameters`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, _ParameterList, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), "Lang1.Parameters"), "Parameters")(p);
                memo[tuple(`Parameters`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Parameters(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, _ParameterList, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), "Lang1.Parameters")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, _ParameterList, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), "Lang1.Parameters"), "Parameters")(TParseTree("", false,[], s));
        }
    }
    static string Parameters(GetName g)
    {
        return "Lang1.Parameters";
    }

    static TParseTree _ParameterList(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, VarArgs, Spacing), pegged.peg.wrapAround!(Spacing, JsonType, Spacing), pegged.peg.wrapAround!(Spacing, MsgpackType, Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Parameter, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, Parameter, Spacing)), Spacing)))), "Lang1._ParameterList")(p);
        }
        else
        {
            if (auto m = tuple(`_ParameterList`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, VarArgs, Spacing), pegged.peg.wrapAround!(Spacing, JsonType, Spacing), pegged.peg.wrapAround!(Spacing, MsgpackType, Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Parameter, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, Parameter, Spacing)), Spacing)))), "Lang1._ParameterList"), "_ParameterList")(p);
                memo[tuple(`_ParameterList`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree _ParameterList(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, VarArgs, Spacing), pegged.peg.wrapAround!(Spacing, JsonType, Spacing), pegged.peg.wrapAround!(Spacing, MsgpackType, Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Parameter, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, Parameter, Spacing)), Spacing)))), "Lang1._ParameterList")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, VarArgs, Spacing), pegged.peg.wrapAround!(Spacing, JsonType, Spacing), pegged.peg.wrapAround!(Spacing, MsgpackType, Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Parameter, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, Parameter, Spacing)), Spacing)))), "Lang1._ParameterList"), "_ParameterList")(TParseTree("", false,[], s));
        }
    }
    static string _ParameterList(GetName g)
    {
        return "Lang1._ParameterList";
    }

    static TParseTree Parameter(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Name, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing)), pegged.peg.wrapAround!(Spacing, _Type, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Direction, Spacing))), "Lang1.Parameter")(p);
        }
        else
        {
            if (auto m = tuple(`Parameter`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Name, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing)), pegged.peg.wrapAround!(Spacing, _Type, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Direction, Spacing))), "Lang1.Parameter"), "Parameter")(p);
                memo[tuple(`Parameter`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Parameter(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Name, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing)), pegged.peg.wrapAround!(Spacing, _Type, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Direction, Spacing))), "Lang1.Parameter")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Name, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing)), pegged.peg.wrapAround!(Spacing, _Type, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Direction, Spacing))), "Lang1.Parameter"), "Parameter")(TParseTree("", false,[], s));
        }
    }
    static string Parameter(GetName g)
    {
        return "Lang1.Parameter";
    }

    static TParseTree Name(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, identifier, Spacing), "Lang1.Name")(p);
        }
        else
        {
            if (auto m = tuple(`Name`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, identifier, Spacing), "Lang1.Name"), "Name")(p);
                memo[tuple(`Name`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Name(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, identifier, Spacing), "Lang1.Name")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, identifier, Spacing), "Lang1.Name"), "Name")(TParseTree("", false,[], s));
        }
    }
    static string Name(GetName g)
    {
        return "Lang1.Name";
    }

    static TParseTree VarArgs(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("..."), Spacing), "Lang1.VarArgs")(p);
        }
        else
        {
            if (auto m = tuple(`VarArgs`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("..."), Spacing), "Lang1.VarArgs"), "VarArgs")(p);
                memo[tuple(`VarArgs`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree VarArgs(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("..."), Spacing), "Lang1.VarArgs")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("..."), Spacing), "Lang1.VarArgs"), "VarArgs")(TParseTree("", false,[], s));
        }
    }
    static string VarArgs(GetName g)
    {
        return "Lang1.VarArgs";
    }

    static TParseTree JsonType(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("json"), Spacing), "Lang1.JsonType")(p);
        }
        else
        {
            if (auto m = tuple(`JsonType`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("json"), Spacing), "Lang1.JsonType"), "JsonType")(p);
                memo[tuple(`JsonType`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree JsonType(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("json"), Spacing), "Lang1.JsonType")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("json"), Spacing), "Lang1.JsonType"), "JsonType")(TParseTree("", false,[], s));
        }
    }
    static string JsonType(GetName g)
    {
        return "Lang1.JsonType";
    }

    static TParseTree MsgpackType(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("msgpack"), Spacing), "Lang1.MsgpackType")(p);
        }
        else
        {
            if (auto m = tuple(`MsgpackType`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("msgpack"), Spacing), "Lang1.MsgpackType"), "MsgpackType")(p);
                memo[tuple(`MsgpackType`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree MsgpackType(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("msgpack"), Spacing), "Lang1.MsgpackType")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("msgpack"), Spacing), "Lang1.MsgpackType"), "MsgpackType")(TParseTree("", false,[], s));
        }
    }
    static string MsgpackType(GetName g)
    {
        return "Lang1.MsgpackType";
    }

    static TParseTree Direction(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("in"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("out"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("dual"), Spacing)), "Lang1.Direction")(p);
        }
        else
        {
            if (auto m = tuple(`Direction`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("in"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("out"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("dual"), Spacing)), "Lang1.Direction"), "Direction")(p);
                memo[tuple(`Direction`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Direction(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("in"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("out"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("dual"), Spacing)), "Lang1.Direction")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("in"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("out"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("dual"), Spacing)), "Lang1.Direction"), "Direction")(TParseTree("", false,[], s));
        }
    }
    static string Direction(GetName g)
    {
        return "Lang1.Direction";
    }

    static TParseTree _Type(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, MsgpackType, Spacing), pegged.peg.wrapAround!(Spacing, JsonType, Spacing)), "Lang1._Type")(p);
        }
        else
        {
            if (auto m = tuple(`_Type`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, MsgpackType, Spacing), pegged.peg.wrapAround!(Spacing, JsonType, Spacing)), "Lang1._Type"), "_Type")(p);
                memo[tuple(`_Type`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree _Type(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, MsgpackType, Spacing), pegged.peg.wrapAround!(Spacing, JsonType, Spacing)), "Lang1._Type")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, MsgpackType, Spacing), pegged.peg.wrapAround!(Spacing, JsonType, Spacing)), "Lang1._Type"), "_Type")(TParseTree("", false,[], s));
        }
    }
    static string _Type(GetName g)
    {
        return "Lang1._Type";
    }

    static TParseTree EasyDoc(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.caseInsensitiveLiteral!("[doc]")), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.caseInsensitiveLiteral!("[/doc]")), pegged.peg.any)), pegged.peg.discard!(pegged.peg.caseInsensitiveLiteral!("[/doc]")))), "Lang1.EasyDoc")(p);
        }
        else
        {
            if (auto m = tuple(`EasyDoc`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.caseInsensitiveLiteral!("[doc]")), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.caseInsensitiveLiteral!("[/doc]")), pegged.peg.any)), pegged.peg.discard!(pegged.peg.caseInsensitiveLiteral!("[/doc]")))), "Lang1.EasyDoc"), "EasyDoc")(p);
                memo[tuple(`EasyDoc`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree EasyDoc(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.caseInsensitiveLiteral!("[doc]")), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.caseInsensitiveLiteral!("[/doc]")), pegged.peg.any)), pegged.peg.discard!(pegged.peg.caseInsensitiveLiteral!("[/doc]")))), "Lang1.EasyDoc")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.caseInsensitiveLiteral!("[doc]")), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.caseInsensitiveLiteral!("[/doc]")), pegged.peg.any)), pegged.peg.discard!(pegged.peg.caseInsensitiveLiteral!("[/doc]")))), "Lang1.EasyDoc"), "EasyDoc")(TParseTree("", false,[], s));
        }
    }
    static string EasyDoc(GetName g)
    {
        return "Lang1.EasyDoc";
    }

    static TParseTree Comment1(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.literal!("/*"), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.literal!("*/")), pegged.peg.any)), pegged.peg.literal!("*/"))), "Lang1.Comment1")(p);
        }
        else
        {
            if (auto m = tuple(`Comment1`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.literal!("/*"), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.literal!("*/")), pegged.peg.any)), pegged.peg.literal!("*/"))), "Lang1.Comment1"), "Comment1")(p);
                memo[tuple(`Comment1`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Comment1(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.literal!("/*"), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.literal!("*/")), pegged.peg.any)), pegged.peg.literal!("*/"))), "Lang1.Comment1")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.literal!("/*"), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.literal!("*/")), pegged.peg.any)), pegged.peg.literal!("*/"))), "Lang1.Comment1"), "Comment1")(TParseTree("", false,[], s));
        }
    }
    static string Comment1(GetName g)
    {
        return "Lang1.Comment1";
    }

    static TParseTree Comment2(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.literal!("//"), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(endOfLine), pegged.peg.any)), pegged.peg.discard!(endOfLine))), "Lang1.Comment2")(p);
        }
        else
        {
            if (auto m = tuple(`Comment2`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.literal!("//"), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(endOfLine), pegged.peg.any)), pegged.peg.discard!(endOfLine))), "Lang1.Comment2"), "Comment2")(p);
                memo[tuple(`Comment2`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Comment2(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.literal!("//"), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(endOfLine), pegged.peg.any)), pegged.peg.discard!(endOfLine))), "Lang1.Comment2")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.literal!("//"), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(endOfLine), pegged.peg.any)), pegged.peg.discard!(endOfLine))), "Lang1.Comment2"), "Comment2")(TParseTree("", false,[], s));
        }
    }
    static string Comment2(GetName g)
    {
        return "Lang1.Comment2";
    }

    static TParseTree Spacing(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.zeroOrMore!(pegged.peg.or!(blank, Comment1, Comment2)), "Lang1.Spacing")(p);
        }
        else
        {
            if (auto m = tuple(`Spacing`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.zeroOrMore!(pegged.peg.or!(blank, Comment1, Comment2)), "Lang1.Spacing"), "Spacing")(p);
                memo[tuple(`Spacing`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Spacing(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.zeroOrMore!(pegged.peg.or!(blank, Comment1, Comment2)), "Lang1.Spacing")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.zeroOrMore!(pegged.peg.or!(blank, Comment1, Comment2)), "Lang1.Spacing"), "Spacing")(TParseTree("", false,[], s));
        }
    }
    static string Spacing(GetName g)
    {
        return "Lang1.Spacing";
    }

    static TParseTree opCall(TParseTree p)
    {
        TParseTree result = decimateTree(_TopLevel(p));
        result.children = [result];
        result.name = "Lang1";
        return result;
    }

    static TParseTree opCall(string input)
    {
        if(__ctfe)
        {
            return Lang1(TParseTree(``, false, [], input, 0, 0));
        }
        else
        {
            forgetMemo();
            return Lang1(TParseTree(``, false, [], input, 0, 0));
        }
    }
    static string opCall(GetName g)
    {
        return "Lang1";
    }


    static void forgetMemo()
    {
        memo = null;
    }
    }
}

alias GenericLang1!(ParseTree).Lang1 Lang1;

