chcp 65001
edub lang-ms64.exe run arch=ms64 inc=. lang.d dgrammar.d dparser.d pegged-ms64.lib
if %errorlevel% neq 0 (exit /b)
if exist temp_dparser.d mv temp_dparser.d dparser.d
