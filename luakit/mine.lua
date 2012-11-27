add_binds("normal", {
  lousy.bind.key({}, "S", "Stop loading the current tab.",
    function (w) w.view:stop() end),
  lousy.bind.buf("^gp$", "Open history in current tab.",
    function(w)
      w:navigate("luakit://history")
    end),

  lousy.bind.buf("^gP$", "Open history in a new tab.",
    function(w)
      w:new_tab("luakit://history")
    end),

  lousy.bind.key({}, "v", "Send hovered or current url to quvi/mplayer.",
  function (w) 
    local view = w.view
    local uri = view.hovered_uri or view.uri
    if uri then
      local cmd = string.format("quvi -f best %q --exec 'mplayer -prefer-ipv4 %%u'", uri)
      luakit.spawn(cmd)
    end
  end),
})
