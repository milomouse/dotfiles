----------------------------------------------------------------
-- Switch tabs using a menu widget                            --
-- © 2012 Alexander Clare <alexander.clare@gmail.com>         --
----------------------------------------------------------------
-- Additional modifications provided by milomouse@github.com  --
----------------------------------------------------------------

local ipairs = ipairs
local table = table

local lousy = require "lousy"
local add_binds, add_cmds = add_binds, add_cmds
local new_mode, menu_binds = new_mode, menu_binds

hide_box = false

local cmd = lousy.bind.cmd
add_cmds({
    cmd("buffers", "View or close opened tabs from list.", function (w) w:set_mode("buffers") end),
})

local escape = lousy.util.escape
new_mode("buffers", {
    enter = function (w)
        hide_box = not w.sbar.ebox.visible
        --local rows = {}
        local rows = {{"Title", "URI", title = true},}
        for _, view in ipairs(w.tabs.children) do
            --table.insert(rows, {escape(view.uri), escape(view.title), v = view })
            table.insert(rows, {escape(view.title), escape(view.uri), v = view })
            --table.insert(rows, 2, {escape(view.uri), escape(view.title), v = view })
        end
        w.menu:build(rows)
        local cur = w.tabs:current()
        local ind = 0
        repeat w.menu:move_down(); ind = ind + 1 until ind == cur
        w.sbar.ebox:show()
        w:notify("Use j/k to move, d close, Return switch.", false)
    end,

    leave = function (w)
        if hide_box == true then
            w.sbar.ebox:hide()
        end
        w.menu:hide()
    end,
})

local key = lousy.bind.key
add_binds("buffers", lousy.util.table.join({
    -- Close tab
    key({}, "d", function (w)
        local row = w.menu:get()
        if row and row.v then
            local cur = w.view
            w:close_tab(w.tabs[w.tabs:indexof(row.v)])
            if cur ~= row.v then
                w.menu:del()
            else
                w:set_mode()
            end
        end
    end),

    -- Switch to tab
    key({}, "Return", function (w)
        local row = w.menu:get()
        if row and row.v then
            local cur = w.view
            if cur ~= row.v then
                w.tabs:switch((w.tabs:indexof(row.v)))
            else
                w:set_mode()
            end
        end
    end),

    -- Exit menu
    key({}, "`", function (w) w:set_mode() end),

}, menu_binds))

-- Add key binds.
local buf = lousy.bind.buf
add_binds("normal", {
    key({},     "m",    function (w)
        w:set_mode("buffers")
    end),
})
