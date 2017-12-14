#! bash -uvx
edub ccl.dll build ccl.d arch=dm32 phobos.lib
#g++.exe -shared -static -o ccl.dll ccl-cpp.cpp
echo "" | ccl -b -n -Q -l ccl.lisp -- a b c 漢字
