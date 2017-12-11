@chcp 65001
edub test-script.exe test arch=dm32 script.d runtime.d var2mod.d ^
 [pegged#"~>0.4.2"] [pegged-cutter#"~>1.0.0"] ^
 ["fluent-asserts"#"~>0.8.4"] ^
 --build=unittest inc=. main=script.d
@if %errorlevel% neq 0 (exit /b)
