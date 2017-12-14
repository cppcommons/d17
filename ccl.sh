#! bash -uvx
edub ccl.dll build ccl.d arch=dm32 phobos.lib
#g++.exe -shared -static -o ccl.dll ccl-cpp.cpp
echo "" | ccl -b -Q -S 1000000 -l ccl.lisp -- a b c
