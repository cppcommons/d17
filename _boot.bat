@if(0)==(0) echo off
setlocal

set CYG_NAME=project1
set CYG_BITS=32
::set CYG_CATS=Archive,Python
set CYG_PKGS=procps,psmisc,tmux,vim,emacs-w32,git,dos2unix,gcc-g++,make
::set DT_ICONS=1
set CYG_HOME=.$

set CYG_DEBUG=0
set CYG_SITE=http://mirrors.kernel.org/sourceware/cygwin/
set CYG_LANG=ja
set CYG_FONT=MS Gothic
set CYG_FONT_HEIGHT=12
set CYG_CURSOR_TYPE=block
set CYG_CONFIRM_EXIT=no

cscript.exe //nologo //E:JScript "%~f0"
::call cyginst.bat SUBPROC
endlocal
pause
exit /b
goto :EOF
@end
var fso = new ActiveXObject("Scripting.FileSystemObject");
var SCRIPT_CURRENT_DIR = fso.getParentFolderName(WScript.ScriptFullName);
var url = "https://raw.githubusercontent.com/cyginst/cyginst-v1/master/cyginst.bat";
var fileName = SCRIPT_CURRENT_DIR + "\\cyginst.bat";
try {
  downloadFile(url, fileName);
} catch (e) {
  WScript.Echo("cyginst.bat could not be downloaded.");
}
WScript.Quit();
function downloadFile(url, fileName) {
  var http = WScript.CreateObject("MSXML2.XMLHTTP");
  http.Open("GET", url, false);
  http.Send();
  WScript.Echo(http.responseText);
}
