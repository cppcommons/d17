chcp 65001
edub lang-dm32.exe run arch=dm32 inc=. lang.d pegged-dm32.lib
if %errorlevel% neq 0 (exit /b)
