#include <windows.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
extern "C"
{
//#define LUA_COMPAT_MODULE
#include "lua.h"     /* lua_*  */
#include "lauxlib.h" /* luaL_* */
}

static int l_getclip(lua_State *L)
{
    const char *s = "<getclip>";
    lua_pushstring(L, s);
    return 1;
}

static int l_process_json(lua_State *L)
{
    const char *arg1 = luaL_checkstring(L, 1);
    printf("[process_json()]arg1: %s\n", arg1);
    return 0;
}

static luaL_Reg reg[] = {
    {"getclip", l_getclip},
    {"process_json", l_process_json},
    
    {NULL, NULL}};

extern "C" __declspec(dllexport) int luaopen_ltest(lua_State *L)
{
    printf("luaopen_ltest\n");
    //luaL_register(L, "ltest", reg);
    luaL_newlib(L, reg);
    return 1;
}