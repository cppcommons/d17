print( 'start!' )
local json = loadfile("json.lua")()
print( json.encode({ 1, 2, 3, { x = 10 } }) )
--print( arg )
--print( arg[0] )
--print (#arg)
--print( arg[1] )
--print( arg[2] )
--print( arg[3] )

local ffi = require("ffi")
ffi.cdef[[
void Sleep(int ms);
int poll(struct pollfd *fds, unsigned long nfds, int timeout);
]]

local sleep
if ffi.os == "Windows" then
  function sleep(s)
    ffi.C.Sleep(s*1000)
  end
else
  function sleep(s)
    ffi.C.poll(nil, 0, s*1000)
  end
end

for i=1,160 do
  io.write("."); io.flush()
  sleep(0.01)
end
io.write("\n")

--local sub2 = require( 'ltest2' )
--print( sub2.getclip() )

print( 'eof' )