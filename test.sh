#! bash -uvx
chicken -ignore-repository test.scm
dules.c test.c -o test.exe -lm -lWs2_32
#gcc -Os -fomit-frame-pointer -DHAVE_CHICKEN_CONFIG_H -I/usr/local/include/chicken test.c -o test.exe -lm -lWs2_32 -L/usr/local/lib -lchicken -lWs2_32 -static
gcc -Os -fomit-frame-pointer -DHAVE_CHICKEN_CONFIG_H -I/usr/local/include/chicken test.c -o test.exe -lm -lWs2_32 -L/usr/local/lib -lchicken -lWs2_32
./test.exe
gcc -Os -fomit-frame-pointer -DHAVE_CHICKEN_CONFIG_H -I/usr/local/include/chicken x.c -o x.exe -lm -lWs2_32 -L/usr/local/lib -lchicken -lWs2_32
./x.exe
