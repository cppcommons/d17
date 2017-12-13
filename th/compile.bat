@setlocal
set PATH=C:\Windows\System32
::set PATH=%PATH%;E:\opt\node-v9.2.1-win-x86
set PATH=%PATH%;E:\opt\node-v8.9.3-win-x86
::set PATH=%PATH%;E:\opt\node-v7.10.1-win-x86
::set PATH=%PATH%;E:\opt\node-v7.9.0-win-x86
set PATH=%PATH%;C:\python27
pkg . --targets node8-win-x86
@endlocal