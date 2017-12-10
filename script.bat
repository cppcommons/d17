chcp 65001
edub test-script.exe run arch=dm32 jsvar.d script.d ^
 --build=debug define=test_script cflag=-wi ^
 [pegged#"~>0.4.2"] [pegged-cutter#"~>1.0.0"]
if %errorlevel% neq 0 (exit /b)
