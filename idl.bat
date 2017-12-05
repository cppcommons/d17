chcp 65001

edub idl-dm32.exe run arch=dm32 inc=. idl.d pegged-dm32.lib ^
[my_common-lib#@E:\d-dev\d-sample\common\my_common.lib.bin] ^
[vibe-d:data]
if %errorlevel% neq 0 (exit /b)
