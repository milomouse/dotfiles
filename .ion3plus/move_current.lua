-- Move current window in a frame to another frame in specified direction

move_current={}

function move_current.move(ws, dir)
    local frame=ws:current()
    local cwin=frame:current()
    local frame2=ioncore.navi_next(frame,dir)
    
    if frame2 then
        frame2:attach(cwin, { switchto=true })
    end
    cwin:goto()
end

defbindings("WTiling", {
--    submap(META.."Control", {
        kpress(META.."Shift+k", function(ws) move_current.move(ws, "up") end),
        kpress(META.."Shift+j", function(ws) move_current.move(ws, "down") end),
        kpress(META.."Shift+h", function(ws) move_current.move(ws, "left") end),
        kpress(META.."Shift+l", function(ws) move_current.move(ws, "right") end),
    })
--})
