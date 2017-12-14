#! bash -uvx
chicken -ignore-repository test.scm
dules.c test.c -o test.exe -lm -lWs2_32
gcc -static -Os -fomit-frame-pointer -DHAVE_CHICKEN_CONFIG_H -I/usr/local/include/chicken test.c -o test.exe -lm -lWs2_32 -L/usr/local/lib -lchicken -lWs2_32
