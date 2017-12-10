chcp 65001
edub test.exe.json generate visuald
edub test.exe run arch=dm32 lang.d runtime.d var2mod.d ^
 [pegged#"~>0.4.2"] [pegged-cutter#"~>1.0.0"] ^
 ["arsd-clone:jsvar"#"~>1.2.2"] ["arsd-clone:script"#"~>1.2.2"] ^
 --build=debug cflag=-g
if %errorlevel% neq 0 (exit /b)
