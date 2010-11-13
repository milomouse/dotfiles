--------------------------
-- Default luakit theme --
--------------------------

local theme = {}

-- Default settings
theme.font = "monospace normal 9"
theme.fg   = "#fff"
theme.bg   = "#000"

-- Error colours
theme.error_fg = "#FFF"
theme.error_bg = "#F00"

-- Notification colours
theme.notif_fg = "#444"
theme.notif_bg = "#FFF"

-- Menu colours
theme.menu_fg                   = "#000"
theme.menu_bg                   = "#fff"
theme.menu_selected_fg          = "#000"
theme.menu_selected_bg          = "#FF0"
theme.menu_title_bg             = "#fff"
theme.menu_primary_title_fg     = "#f00"
theme.menu_secondary_title_fg   = "#666"

-- Statusbar specific
theme.sbar_fg           = "#fff"
theme.sbar_bg           = "#000"
theme.loaded_sbar_fg    = "#33AADD"

-- Input bar specific
theme.ibar_fg           = "#000"
theme.ibar_bg           = "#fff"

-- Proxy manager
theme.proxy_active_fg   = '#000'
theme.proxy_inactive_fg = '#888'

-- Tab label
theme.tab_fg            = "#888"
theme.tab_bg            = "#222"
theme.selected_fg       = "#fff"
theme.selected_bg       = "#000"
theme.loading_fg        = "#33AADD"
theme.loading_bg        = "#000"

-- Trusted/untrusted ssl colours
theme.trust_fg          = "#0F0"
theme.notrust_fg        = "#F00"

return theme
-- vim: et:sw=4:ts=8:sts=4:tw=80