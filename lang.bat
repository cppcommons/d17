chcp 65001
edub test.exe.json generate visuald
edub test.exe run arch=dm32 lang.d runtime.d jsvar.d script.d ^
 [pegged#"~>0.4.2"] [pegged-cutter#"~>1.0.0"] --build=debug cflag=-g
if %errorlevel% neq 0 (exit /b)
