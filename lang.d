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

void main(string[] args)
{
    ParseTree pt = Lang1(Lang1Source);
    // pt.cutNodes([`Lang1.TopLevel`, `Lang1.EvalUnit`]); // バージョン2で追加⇒バージョン4で削除
    pt.cutNodes(); // バージョン4で追加
    writeln(pt);
    // これ以降を追加(バージョン3)
    long[string] var_tbl;
    for (size_t i = 0; i < pt.children.length; i++)
    {
        ParseTree* unit = &(pt.children[i]);
        writeln(`[EXECUTE] `, unit.name);
        switch (unit.name)
        {
        case `Lang1.VarDeclaration`:
            {
                string var_name = unit.children[0].matches[0];
                writeln(`    `, var_name);
                long var_value = to!long(unit.children[1].matches[0]);
                writeln(`    `, var_value);
                var_tbl[var_name] = var_value;
            }
            break;
        case `Lang1.DumpStatement`:
            {
                writeln(`    `, var_tbl);
            }
            break;
        case `Lang1.StatementBlock`:
            break;
        default:
            break;
        }
    }
}
