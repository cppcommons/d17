/++
This module was automatically generated from the following grammar:


Lang1:
    _TopLevel       < (_Def+ "[eof]"i) / (_Def+ eoi)
    _Def            < VarDecl / _Statement / StatementBlock_
    _Keywords        < "var" / "let" / PrintKeyword
    Identifier      < (!_Keywords identifier)
    _VarAssign      < Identifier "=" Integer
    VarDecl         < "var" _VarAssign ";"
    LetDecl         < "let" _VarAssign ";"
    StatementBlock_ < "{" LetDecl* (_Statement / StatementBlock_)* "}"
    AssignStatement < _VarAssign ";"
    PrintKeyword    < "print"
    PrintStatement  < PrintKeyword Identifier ";"
    _Statement      < AssignStatement / PrintStatement
    Integer         <~ digit (digit/"_")*
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
        rules["_Def"] = toDelegate(&_Def);
        rules["_Keywords"] = toDelegate(&_Keywords);
        rules["Identifier"] = toDelegate(&Identifier);
        rules["_VarAssign"] = toDelegate(&_VarAssign);
        rules["VarDecl"] = toDelegate(&VarDecl);
        rules["LetDecl"] = toDelegate(&LetDecl);
        rules["StatementBlock_"] = toDelegate(&StatementBlock_);
        rules["AssignStatement"] = toDelegate(&AssignStatement);
        rules["PrintKeyword"] = toDelegate(&PrintKeyword);
        rules["PrintStatement"] = toDelegate(&PrintStatement);
        rules["_Statement"] = toDelegate(&_Statement);
        rules["Integer"] = toDelegate(&Integer);
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

    static TParseTree _Def(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, VarDecl, Spacing), pegged.peg.wrapAround!(Spacing, _Statement, Spacing), pegged.peg.wrapAround!(Spacing, StatementBlock_, Spacing)), "Lang1._Def")(p);
        }
        else
        {
            if (auto m = tuple(`_Def`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, VarDecl, Spacing), pegged.peg.wrapAround!(Spacing, _Statement, Spacing), pegged.peg.wrapAround!(Spacing, StatementBlock_, Spacing)), "Lang1._Def"), "_Def")(p);
                memo[tuple(`_Def`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree _Def(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, VarDecl, Spacing), pegged.peg.wrapAround!(Spacing, _Statement, Spacing), pegged.peg.wrapAround!(Spacing, StatementBlock_, Spacing)), "Lang1._Def")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, VarDecl, Spacing), pegged.peg.wrapAround!(Spacing, _Statement, Spacing), pegged.peg.wrapAround!(Spacing, StatementBlock_, Spacing)), "Lang1._Def"), "_Def")(TParseTree("", false,[], s));
        }
    }
    static string _Def(GetName g)
    {
        return "Lang1._Def";
    }

    static TParseTree _Keywords(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("var"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("let"), Spacing), pegged.peg.wrapAround!(Spacing, PrintKeyword, Spacing)), "Lang1._Keywords")(p);
        }
        else
        {
            if (auto m = tuple(`_Keywords`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("var"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("let"), Spacing), pegged.peg.wrapAround!(Spacing, PrintKeyword, Spacing)), "Lang1._Keywords"), "_Keywords")(p);
                memo[tuple(`_Keywords`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree _Keywords(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("var"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("let"), Spacing), pegged.peg.wrapAround!(Spacing, PrintKeyword, Spacing)), "Lang1._Keywords")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("var"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("let"), Spacing), pegged.peg.wrapAround!(Spacing, PrintKeyword, Spacing)), "Lang1._Keywords"), "_Keywords")(TParseTree("", false,[], s));
        }
    }
    static string _Keywords(GetName g)
    {
        return "Lang1._Keywords";
    }

    static TParseTree Identifier(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.wrapAround!(Spacing, _Keywords, Spacing)), pegged.peg.wrapAround!(Spacing, identifier, Spacing)), Spacing), "Lang1.Identifier")(p);
        }
        else
        {
            if (auto m = tuple(`Identifier`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.wrapAround!(Spacing, _Keywords, Spacing)), pegged.peg.wrapAround!(Spacing, identifier, Spacing)), Spacing), "Lang1.Identifier"), "Identifier")(p);
                memo[tuple(`Identifier`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Identifier(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.wrapAround!(Spacing, _Keywords, Spacing)), pegged.peg.wrapAround!(Spacing, identifier, Spacing)), Spacing), "Lang1.Identifier")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.wrapAround!(Spacing, _Keywords, Spacing)), pegged.peg.wrapAround!(Spacing, identifier, Spacing)), Spacing), "Lang1.Identifier"), "Identifier")(TParseTree("", false,[], s));
        }
    }
    static string Identifier(GetName g)
    {
        return "Lang1.Identifier";
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

    static TParseTree VarDecl(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("var"), Spacing), pegged.peg.wrapAround!(Spacing, _VarAssign, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "Lang1.VarDecl")(p);
        }
        else
        {
            if (auto m = tuple(`VarDecl`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("var"), Spacing), pegged.peg.wrapAround!(Spacing, _VarAssign, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "Lang1.VarDecl"), "VarDecl")(p);
                memo[tuple(`VarDecl`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree VarDecl(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("var"), Spacing), pegged.peg.wrapAround!(Spacing, _VarAssign, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "Lang1.VarDecl")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("var"), Spacing), pegged.peg.wrapAround!(Spacing, _VarAssign, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "Lang1.VarDecl"), "VarDecl")(TParseTree("", false,[], s));
        }
    }
    static string VarDecl(GetName g)
    {
        return "Lang1.VarDecl";
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

    static TParseTree AssignStatement(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, _VarAssign, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "Lang1.AssignStatement")(p);
        }
        else
        {
            if (auto m = tuple(`AssignStatement`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, _VarAssign, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "Lang1.AssignStatement"), "AssignStatement")(p);
                memo[tuple(`AssignStatement`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree AssignStatement(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, _VarAssign, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "Lang1.AssignStatement")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, _VarAssign, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "Lang1.AssignStatement"), "AssignStatement")(TParseTree("", false,[], s));
        }
    }
    static string AssignStatement(GetName g)
    {
        return "Lang1.AssignStatement";
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
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, AssignStatement, Spacing), pegged.peg.wrapAround!(Spacing, PrintStatement, Spacing)), "Lang1._Statement")(p);
        }
        else
        {
            if (auto m = tuple(`_Statement`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, AssignStatement, Spacing), pegged.peg.wrapAround!(Spacing, PrintStatement, Spacing)), "Lang1._Statement"), "_Statement")(p);
                memo[tuple(`_Statement`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree _Statement(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, AssignStatement, Spacing), pegged.peg.wrapAround!(Spacing, PrintStatement, Spacing)), "Lang1._Statement")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, AssignStatement, Spacing), pegged.peg.wrapAround!(Spacing, PrintStatement, Spacing)), "Lang1._Statement"), "_Statement")(TParseTree("", false,[], s));
        }
    }
    static string _Statement(GetName g)
    {
        return "Lang1._Statement";
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

