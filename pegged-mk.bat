chcp 65001

edub pegged-dm32.lib build arch=dm32 --build=release ^
pegged/peg.d pegged/grammar.d pegged/parser.d pegged/introspection.d ^
pegged/dynamic/grammar.d pegged/dynamic/peg.d
if %errorlevel% neq 0 (exit /b)
