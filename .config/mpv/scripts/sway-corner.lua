-- Place only our floating mpv launches; never act on another app's focus.
local mp = require "mp"
local utils = require "mp.utils"
local placed = false

mp.register_event("file-loaded", function()
    if placed or not os.getenv("SWAYSOCK") or mp.get_property("wayland-app-id") ~= "mpv-float" then
        return
    end
    placed = true
    mp.command_native_async({
        name = "subprocess",
        args = {os.getenv("HOME") .. "/local/bin/sway-place-corner", "right", "screen", tostring(utils.getpid())},
        capture_stderr = true,
    }, function(success, result, err)
        if not success or not result or result.status ~= 0 then
            mp.msg.error("Corner placement failed: " .. (err or (result and result.stderr) or "unknown error"))
        end
    end)
end)
