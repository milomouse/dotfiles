-- a few custom keybindings:
add_binds("normal", {
  lousy.bind.key({}, "S", "Stop loading the current tab.",
    function (w) w.view:stop() end),
  lousy.bind.key({}, "s", "Toggle statusbar.",
    function (w)
    if true == w.sbar.hidden then
      w.sbar.ebox:show() w.sbar.hidden = false
    else
      w.sbar.ebox:hide() w.sbar.hidden = true
    end
  end),
  lousy.bind.buf("^gp$", "Open history in current tab.",
    function(w)
      w:navigate("luakit://history")
    end),

  lousy.bind.buf("^gP$", "Open history in a new tab.",
    function(w)
      w:new_tab("luakit://history")
    end),
})

-- mailto (mutt):
webview.init_funcs.mailto_hook = function (view, w)
    view:add_signal("navigation-request", function (v, uri)
        if string.match(string.lower(uri), "^mailto:") then
            luakit.spawn(string.format("%s %q", "urxvt -title mutt -e mutt -F /howl/conf/mutt/muttrc", uri))
            return false
        end
    end)
end

-- magnet:
webview.init_funcs.magnet_hook = function (view, w)
    view:add_signal("navigation-request", function (v, uri)
        if string.match(string.lower(uri), "^magnet:") then
            luakit.spawn(string.format("%s %q", "urxvt -title transmission-remote-cli -e transmission-remote-cli", uri))
            return false
        end
    end)
end
