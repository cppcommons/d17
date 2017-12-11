@chcp 65001
edub test-script.exe run arch=dm32 script.d runtime.d var2mod.d ^
 [pegged#"~>0.4.2"] [pegged-cutter#"~>1.0.0"] ^
 ["arsd-clone:jsvar"#"~>1.2.2"] ["arsd-clone:script"#"~>1.2.2"] ^
 --build=debug cflag=-g
@if %errorlevel% neq 0 (exit /b)
