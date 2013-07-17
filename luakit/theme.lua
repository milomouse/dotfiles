------------------------------
-- cottonmouse luakit theme --
------------------------------

local theme = {}

-- >> start of CUSTOM
theme.sbar_l_ebox_bg  = "#887b8b"
theme.sbar_r_ebox_bg  = "#191919"
-- end of CUSTOM <<

-- Default settings
theme.font = "EnvyCodeR normal 10"
--theme.font = "Liberation normal 8"
--theme.font = "monospace normal 8"
--theme.font = "normal 8"
theme.fg   = "#191919"
theme.bg   = "#eeeeee"

-- General colors
theme.sbar_bg        = "#191919"
theme.success_fg     = "#d680c5"
theme.loaded_fg      = "#98a7b2"
theme.sbar_loaded_fg = "#232323"
theme.sbar_loaded_bg = "#eeeeee"
theme.error_fg       = "#39d3c9"
theme.error_bg       = "#232323"
theme.scroll_sbar_fg = "#191919"

-- Warning colors
theme.warning_fg = "#c5bb5f"
theme.warning_bg = "#191919"

-- Notification colors
theme.notif_fg = "#393939"
theme.notif_bg = "#b4aeba"

-- Menu colors
theme.menu_fg                 = "#848484"
theme.menu_bg                 = "#191919"
theme.menu_selected_fg        = "#191919"
theme.menu_selected_bg        = "#887b8b"
theme.menu_title_bg           = "#313131"
theme.menu_primary_title_fg   = "#c5c5c5"
theme.menu_secondary_title_fg = "#98a7b2"

-- Proxy manager
theme.proxy_active_menu_fg    = '#232323'
theme.proxy_active_menu_bg    = '#98a7b2'
theme.proxy_inactive_menu_fg  = '#787878'
theme.proxy_inactive_menu_bg  = '#191919'

-- Statusbar specific
theme.sbar_fg         = "#aaaaaa"
theme.sbar_bg         = "#191919"
theme.hist_sbar_fg    = "#8fb676"

-- Downloadbar specific
theme.dbar_fg         = "#989898"
theme.dbar_bg         = "#191919"
theme.dbar_error_fg   = "#8eb8c3"

-- Input bar specific
theme.ibar_fg         = "#232323"
theme.ibar_bg         = "#ccc8bf"

-- Tab label
theme.tab_fg          = "#72727d"
theme.tab_bg          = "#232323"
theme.tab_ntheme      = "#8d8d8d"
theme.selected_fg     = "#fefefe"
theme.selected_bg     = "#333333"
theme.selected_ntheme = "#77b6c5"
theme.loading_fg      = "#98a7b2"
theme.loading_bg      = "#898989"

-- Trusted/untrusted ssl colors
theme.trust_fg        = "#98a7b2"
theme.notrust_fg      = "#d680c5"

return theme
-- vim: et:sw=4:ts=8:sts=4:tw=80
