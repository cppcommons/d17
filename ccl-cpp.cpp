#include <windows.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

extern "C" __declspec(dllexport) int my_add2(int a, int b)
{
    printf("my_add2()\n");
    return (a + b) * 10;
}
