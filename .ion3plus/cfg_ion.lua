--## ~/.ion3/cfg_ion.lua
--## milomouse


XTERM="urxvt"

ioncore.set{
    -- Maximum delay between clicks in milliseconds to be considered a
    -- double click.
    dblclick_delay=250,

    -- For keyboard resize, time (in milliseconds) to wait after latest
    -- key press before automatically leaving resize mode (and doing
    -- the resize in case of non-opaque move).
    kbresize_delay=1500,

    -- Opaque resize?
    opaque_resize=false,

    -- Movement commands warp the pointer to frames instead of just
    -- changing focus. Enabled by default.
    warp=false,
    
    -- Switch frames to display newly mapped windows
    switchto=true,
    
    -- Default index for windows in frames: one of 'last', 'next' (for
    -- after current), or 'next-act' (for after current and anything with
    -- activity right after it).
    frame_default_index='next-act',
    
    -- Auto-unsqueeze transients/menus/queries.
    unsqueeze=true,
    
    -- Display notification tooltips for activity on hidden workspace.
    screen_notify=true,
}

-- Load default settings.
dopath("cfg_defaults")
dopath("cfg_ioncore")
dopath("cfg_kludges")
dopath("cfg_layouts")

-- Load some modules. Bindings and other configuration specific to modules
-- are in the files cfg_modulename.lua.
dopath("mod_query")
dopath("mod_menu")
dopath("mod_tiling")
--dopath("mod_statusbar")
--dopath("mod_dock")
--dopath("mod_sp")
