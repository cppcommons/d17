#include <windows.h>
#include <stdlib.h>
#include <string.h>
#include "lua.h"     /* lua_*  */
#include "lauxlib.h" /* luaL_* */

__declspec(dllexport) int luaopen_natal(lua_State *L);
