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

wscript.exe //nologo //E:JScript "%~f0"
endlocal
::pause
exit /b
goto :EOF
@end
var fso = new ActiveXObject("Scripting.FileSystemObject");
var SCRIPT_CURRENT_DIR = fso.getParentFolderName(WScript.ScriptFullName);
var path = SCRIPT_CURRENT_DIR + "\\cyginst-v2\\boot.js";
var url = "https://raw.githubusercontent.com/cyginst/cyginst-v2/master/boot.js";
try {
  var http = WScript.CreateObject("MSXML2.XMLHTTP");
  http.Open("GET", url, false);
  http.Send();
  WScript.Echo(http.responseText);
  var content = LoadUtf8Text(path);
  WScript.Echo(content);
  eval(content);
} catch (e) {
  WScript.Echo("boot.js could not be downloaded.");
}
WScript.Quit();
function LoadUtf8Text(path)
{
  var StreamTypeEnum  = { adTypeBinary: 1, adTypeText: 2 };
  var StreamReadEnum    = { adReadAll: -1, adReadLine: -2 };
  var sr = new ActiveXObject("ADODB.Stream");
  sr.Type = StreamTypeEnum.adTypeText;
  sr.charset = "utf-8";
  sr.Open();
  sr.LoadFromFile(path);
  var content = sr.ReadText(StreamReadEnum.adReadAll);
  sr.Close();
  return content;
}
