------------------------------------------------------
-- Session saving / loading functions               --
-- © 2010 Mason Larobina <mason.larobina@gmail.com> --
-- © 2012 fsckd              <fsckdaemon@gmail.com> --
------------------------------------------------------

--
-- Library to use.
--

require("luarocks.loader")
local json = require("json")

--
-- Utility functions and constants.
--

local function file(path,fname)
    return path .. "/" .. fname
end

path = luakit.data_dir .. "/sessions.d"
os.execute("test -d '"..path.."' || mkdir '"..path.."'") -- make sure the path exists

--
-- Session functions.
--

session = {
    -- The directory where sessions are stored
    path = path,

    -- Save all tabs of current window to session file (if it exists).
    save = function (w, session_name, force)
        local name = session_name or w.session
        if name then
            -- create a structure, { {if current, history}, ... }
            local current = w.tabs:current()
            local tabs = {}
            for ti, tab in ipairs(w.tabs.children) do
                table.insert(tabs, { current == ti, tab.history })
            end
            -- do the saving
            if #tabs > 0 then
                if session_name then
                    if session.write(name,tabs,force) then
                        w:notify("\"" .. name .. "\" written")
                        if not w.session then session.setname(w,name,true) end
                    else
                        w:error("\"" .. name .. "\" exists in session directory (add ! to override)")
                    end
                else
                    if "old" == session.write(name,tabs,true) then
                        w:notify("\"" .. name .. "\" written")
                    else
                        w:notify("\"" .. name .. "\" [New] written")
                    end
                end
            else -- normally never reach here
                w:error("But why are the tabs gone?")
                -- optionally remove session
                --os.remove(file(session.path,name)) -- delete file from system
                --w:warning("\"" .. name .. "\" removed")
            end
        else
            w:error("No session name")
        end
    end,

    -- Write tab data.
    write = function (name, tabs, force)
        local sfile = file(session.path,name) -- will save to path/name
        local age = os.exists(sfile) and "old" or "new"
        if age == "old" and not force then return false end
        local fh = io.open(sfile, "w")
        fh:write(json.encode(tabs))
        io.close(fh)
        return age
    end,

    -- Set the name of the session for the window.
    setname = function (w, name, force)
        if os.exists(file(session.path,name)) and not force then return false end
        w.session = name
        w.view:emit_signal("property::session-name")
        return true
    end,

    -- Load new session from file; optionally replace existing session.
    sload = function (w, name, replace)
        if name then
            local tabs = session.read(name)
            if tabs then
                if replace then
                    -- clear tabs from current window
                    while w.tabs:count() ~= 0 do
                        w:close_tab(nil, false)
                    end
                end
                session.open(w,tabs)
                if replace then
                    session.setname(w,name,true)
                end
                if replace then
                    w:notify("\"" .. name .. "\" loaded")
                else
                    w:notify("\"" .. name .. "\" merged")
                end
            else
                w:error("\"" .. name .. "\" does not exist")
            end
        else
            w:error("No session name")
        end
    end,

    -- Read urls from session file.
    read = function (name)
        local path = session.path
        local sfile = file(path,name)
        if not os.exists(sfile) then return end
        local fh = io.open(sfile, "r")
        local tabs = json.decode(fh:read("*all"))
        io.close(fh)
        return tabs
    end,

    -- Open new tabs from table of tab data.
    open = function (w,tabs)
        if tabs and w then -- load new tabs
            for _, tab in ipairs(tabs) do
                w:new_tab(tab[2], tab[1])
            end
        end
    end,

    -- Vestigial.
    restore = function (d)
        return false
    end,
}

--
-- Session interface functions.
--

window.methods.write_session = function (w,s,f)
    session.save(w,s,f)
end

window.methods.set_session = function (w,s)
    if session.setname(w,s) then
        w:notify("\"" .. s .. "\" new session")
    else
        w:error("\"" .. s .. "\" exists in session directory")
    end
end

window.methods.load_session = function (w,s)
    session.sload(w,s,true)
end

window.methods.merge_session = function (w,s)
    session.sload(w,s,false)
end

--
-- Status bar stuff. Shamelessly copied from proxy.lua.
--

window.init_funcs.build_session_indicator = function (w)
    local r = w.sbar.r
    r.session = widget{type="label"}
    r.layout:pack(r.session)
    r.layout:reorder(r.session, 2)
    r.session.fg = theme.session_fg
    r.session.font = theme.buf_sbar_font
    w:update_session_indicator()
end

window.methods.update_session_indicator = function (w)
    local name = w.session
    local s = w.sbar.r.session
    if name then
        local text = string.format("%s", name)
        if s.text ~= text then s.text = text end
        s:show()
    else
        s:hide()
    end
end

webview.init_funcs.session_indicator_update = function (view, w)
    view:add_signal("property::session-name", function (v)
        w:update_session_indicator()
    end)
end

--
-- Bindings.
--

-- Binding aliases.
local key, buf, but = lousy.bind.key, lousy.bind.buf, lousy.bind.but
local cmd, any = lousy.bind.cmd, lousy.bind.any

add_binds("normal", {
    buf("^ZZ$", "Quit and save the session.",
        function (w) w:write_session() w:close_win() end),
},true)

add_cmds({
    cmd("w[rite]", "Save current session.",
        function (w, a, o) w:write_session(a, o.bang) end),

    cmd({"wq"}, "Save the session and quit.",
        function (w, a, o) w:write_session(a, o.bang) w:close_win(o.bang) end),

    cmd("l[oad]", "Load new session.",
        function (w, a) w:load_session(a) end),

    cmd("m[erge]", "Merge session into current window.",
        function (w, a) w:merge_session(a) end),

    cmd("s[etsession]", "Set window as session.",
        function (w, a) w:set_session(a) end),

    cmd("sname", "Show session name.",
        function (w)
            local name = w.session
            if name then
                w:notify("\"" .. name .. "\" is the current session")
            end
        end),

    cmd({"winload","wl","nl"}, "Load session in new window",
        function (w, a) -- copying window.new
            local m = window.build()

            -- Set window metatable
            setmetatable(m, {
                __index = function (_, k)
                    -- Check widget structure first
                    local v = rawget(m, k)
                    if v then return v end
                    -- Call each window index function
                    for _, index in ipairs(window.indexes) do
                        v = index(m, k)
                        if v then return v end
                    end
                end,
            })

            -- Setup window widget for signals
            lousy.signal.setup(m)

            -- Call window init functions
            for _, func in pairs(window.init_funcs) do
                func(m)
            end

            -- Populate notebook with tabs
            m:load_session(a)

            -- Make sure something is loaded
            if m.tabs:count() == 0 then
                m:new_tab(m:search_open(globals.homepage), false)
            end

            -- Set initial mode
            m:set_mode()

            -- Show window
            m.win:show()

            return m
        end),
},true)

-- vim: et:sw=4:ts=8:sts=4:tw=80
