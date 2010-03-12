--
-- Ion core configuration file
--

-- 
-- Bindings. This includes global bindings and bindings common to
-- screens and all types of frames only. See modules' configuration 
-- files for other bindings.
--

-- WScreen context bindings
--
-- The bindings in this context are available all the time.
--
-- The variable META should contain a string of the form 'Mod1+'
-- where Mod1 maybe replaced with the modifier you want to use for most
-- of the bindings. Similarly ALTMETA may be redefined to add a 
-- modifier to some of the F-key bindings.
--META="Mod4+"
--ALTMETA=""
LEFT="h" ; RIGHT="l" ; UP="k" ; DOWN="j"

defbindings("WScreen", {
    bdoc("Switch to n:th object (workspace, full screen client window) "..
         "within current screen."),
    kpress(META.."1", "WScreen.switch_nth(_, 0)"),
    kpress(META.."2", "WScreen.switch_nth(_, 1)"),
    kpress(META.."3", "WScreen.switch_nth(_, 2)"),
    kpress(META.."4", "WScreen.switch_nth(_, 3)"),
    kpress(META.."5", "WScreen.switch_nth(_, 4)"),
    kpress(META.."6", "WScreen.switch_nth(_, 5)"),
    kpress(META.."7", "WScreen.switch_nth(_, 6)"),
    kpress(META.."8", "WScreen.switch_nth(_, 7)"),
    kpress(META.."9", "WScreen.switch_nth(_, 8)"),
    kpress(META.."0", "WScreen.switch_nth(_, 9)"),
    
    bdoc("Switch to next/previous object within current screen."),
    kpress(META.."comma", "WScreen.switch_prev(_)"),
    kpress(META.."period", "WScreen.switch_next(_)"),

    submap(META.."semicolon", {
        bdoc("Go to first region demanding attention or previously active one."),
        kpress("semicolon", "mod_menu.grabmenu(_, _sub, 'focuslist')"),
        -- Alternative without (cyclable) menu
        --kpress("semicolon", "ioncore.goto_activity() or ioncore.goto_previous()"),

        --bdoc("Go to previous active object."),
        --kpress("semicolon", "ioncore.goto_previous()"),
        
        --bdoc("Go to first object on activity/urgency list."),
        --kpress("semicolon", "ioncore.goto_activity()"),
        
        bdoc("Clear all tags."),
        kpress("t", "ioncore.tagged_clear()"),
        
	      bdoc("Lock the screen."),
        kpress("Delete", "ioncore.exec_on(_, 'alock -bg shade:shade=30 -cursor glyph -auth pam')"),

        bdoc("Take a screenshot of current workspace."),
        kpress("End", "ioncore.exec_on(_, 'import -window root ~/foto/shot/$(date +%Y_%m_%d-%H%M).png')"),

        bdoc("Banish the mouse (and then return it to it's previous state)."),
        kpress("b", "ioncore.exec_on(_, 'synap')"),
    }),

    bdoc("Go to n:th screen on multihead setup."),
    kpress(META.."Control+1", "ioncore.goto_nth_screen(0)"),
    kpress(META.."Control+2", "ioncore.goto_nth_screen(1)"),
    
    bdoc("Go to next/previous screen on multihead setup."),
    kpress(META.."Control+comma", "ioncore.goto_prev_screen()"),
    kpress(META.."Control+period", "ioncore.goto_next_screen()"),
    
    bdoc("Create a new workspace of chosen default type."),
    kpress(META.."F9", "ioncore.create_ws(_)"),
    
    bdoc("Display the main menu."),
    kpress(ALTMETA.."F12", "mod_query.query_menu(_, _sub, 'mainmenu', 'Main menu:')"),
    --kpress(ALTMETA.."F12", "mod_menu.menu(_, _sub, 'mainmenu', {big=true})"),
    mpress("Button3", "mod_menu.pmenu(_, _sub, 'mainmenu')"),
    
    bdoc("Display the window list menu."),
    mpress("Button2", "mod_menu.pmenu(_, _sub, 'windowlist')"),

    bdoc("Forward-circulate focus."),
    -- '_chld' used here stands to for an actual child window that may not
    -- be managed by the screen itself, unlike '_sub', that is likely to be
    -- the managing group of that window. The right/left directions are
    -- used instead of next/prev, because they work better in conjunction
    -- with tilings.
    kpress(META..LEFT, "ioncore.goto_next(_chld, 'left')", 
           "_chld:non-nil"),
    kpress(META..RIGHT, "ioncore.goto_next(_chld, 'right')", 
           "_chld:non-nil"),
    submap(META.."semicolon", { 
        bdoc("Backward-circulate focus."),
        kpress("AnyModifier+Tab", "ioncore.goto_next(_chld, 'left')", 
               "_chld:non-nil"),
        
        bdoc("Raise focused object, if possible."),
        kpress("AnyModifier+R", "WRegion.rqorder(_chld, 'front')",
               "_chld:non-nil"),
    }),

})


-- Client window bindings
--
-- These bindings affect client windows directly.

defbindings("WClientWin", {
    bdoc("Nudge the client window. This might help with some "..
         "programs' resizing problems."),
    kpress_wait(META.."e", "WClientWin.nudge(_)"),
    
    submap(META.."semicolon", {
       bdoc("Kill client owning the client window."),
       kpress("x", "WClientWin.kill(_)"),
       
       bdoc("Send next key press to the client window. "..
            "Some programs may not allow this by default."),
       kpress("q", "WClientWin.quote_next(_)"),
    }),
})


-- Client window group bindings

defbindings("WGroupCW", {
    bdoc("Toggle client window group full-screen mode"),
    kpress_wait(META.."Return", "WGroup.set_fullscreen(_, 'toggle')"),
})


-- WMPlex context bindings
--
-- These bindings work in frames and on screens. The innermost of such
-- contexts/objects always gets to handle the key press. 

defbindings("WMPlex", {
    bdoc("Close current object."),
    kpress_wait(META.."x", "WRegion.rqclose_propagate(_, _sub)"),
})

-- Frames for transient windows ignore this bindmap
defbindings("WMPlex.toplevel", {
    bdoc("Toggle tag of current object."),
    kpress(META.."t", "WRegion.set_tagged(_sub, 'toggle')", "_sub:non-nil"),

    bdoc("Query for manual page to be displayed."),
    kpress(ALTMETA.."F1", "mod_query.query_man(_, ':man')"),

    bdoc("Query for Lua code to execute."),
    kpress(META.."F1", "mod_query.query_lua(_)"),
    
    bdoc("Run a terminal emulator with tmux instance."),
    kpress(ALTMETA.."F2", "ioncore.exec_on(_, URXVT or 'urxvt -e tmux')"),

    bdoc("Run a terminal emulator."),
    kpress(META.."F2", "ioncore.exec_on(_, URXVT or 'urxvt')"),

    bdoc("Query for command line to execute."),
    kpress(META.."F3", "mod_query.query_exec(_)"),
    
    bdoc("Query for command line to execute in current frame."),
    kpress(ALTMETA.."F3", "run_here(_)"),

    bdoc("Query for host to connect to with SSH."),
    kpress(ALTMETA.."F4", "mod_query.query_ssh(_, ':ssh')"),

    bdoc("Query for file to edit."),
    kpress(ALTMETA.."F5", "mod_query.query_editfile(_, ':vim $1')"),

    bdoc("Query for file to view."),
    kpress(ALTMETA.."F6", "mod_query.query_runfile(_, ':3view $1')"),

    bdoc("Query for workspace to go to or create a new one."),
    kpress(ALTMETA.."F9", "mod_query.query_workspace(_)"),
    
    bdoc("Query for a client window to go to."),
    kpress(META.."g", "mod_query.query_gotoclient(_)"),
    
    bdoc("Display context menu."),
    --kpress(META.."M", "mod_menu.menu(_, _sub, 'ctxmenu')"),
    kpress(META.."m", "mod_query.query_menu(_, _sub, 'ctxmenu', 'Context menu:')"),
    
    submap(META.."semicolon", {
        bdoc("Detach (float) or reattach an object to its previous location."),
        -- By using _chld instead of _sub, we can detach/reattach queries
        -- attached to a group. The detach code checks if the parameter 
        -- (_chld) is a group 'bottom' and detaches the whole group in that
        -- case.
        kpress("d", "ioncore.detach(_chld, 'toggle')", "_chld:non-nil"),
    }),
})


-- WFrame context bindings
--
-- These bindings are common to all types of frames. Some additional
-- frame bindings are found in some modules' configuration files.

defbindings("WFrame", {
    submap(META.."semicolon", {
        bdoc("Maximize the frame horizontally/vertically."),
        kpress("h", "WFrame.maximize_horiz(_)"),
        kpress("v", "WFrame.maximize_vert(_)"),
    }),
    
    kpress(META..LEFT.."+Control", "WFrame.dec_index(_, _sub)", "_sub:non-nil"),
    kpress(META..RIGHT.."+Control", "WFrame.inc_index(_, _sub)", "_sub:non-nil"),
    kpress(META.."p", "WFrame.switch_prev(_)"),
    kpress(META.."n", "WFrame.switch_next(_)"),
    --kpress(META.."Tab", "WFrame.switch_next(_)"),

    bdoc("Display context menu."),
    mpress("Button3", "mod_menu.pmenu(_, _sub, 'ctxmenu')"),
    
    bdoc("Begin move/resize mode."),
    kpress(META.."r", "WFrame.begin_kbresize(_)"),
    
    bdoc("Switch the frame to display the object indicated by the tab."),
    mclick("Button1@tab", "WFrame.p_switch_tab(_)"),
    mclick("Button2@tab", "WFrame.p_switch_tab(_)"),
    
    bdoc("Resize the frame."),
    mdrag("Button1@border", "WFrame.p_resize(_)"),
    mdrag(META.."Button3", "WFrame.p_resize(_)"),
    
    bdoc("Move the frame."),
    mdrag(META.."Button1", "WFrame.p_move(_)"),

    bdoc("Move objects between frames by dragging and dropping the tab."),
    mdrag("Button1@tab", "WFrame.p_tabdrag(_)"),
    mdrag("Button2@tab", "WFrame.p_tabdrag(_)"),
})

-- Frames for transient windows ignore this bindmap

defbindings("WFrame.toplevel", {
    bdoc("Query for a client window to attach."),
    kpress(META.."a", "mod_query.query_attachclient(_)"),
    
    submap(META.."semicolon", {
        -- Display tab numbers when modifiers are released
        submap_wait("ioncore.tabnum.show(_)"),
        
        bdoc("Switch to n:th object within the frame."),
        kpress("1", "WFrame.switch_nth(_, 0)"),
        kpress("2", "WFrame.switch_nth(_, 1)"),
        kpress("3", "WFrame.switch_nth(_, 2)"),
        kpress("4", "WFrame.switch_nth(_, 3)"),
        kpress("5", "WFrame.switch_nth(_, 4)"),
        kpress("6", "WFrame.switch_nth(_, 5)"),
        kpress("7", "WFrame.switch_nth(_, 6)"),
        kpress("8", "WFrame.switch_nth(_, 7)"),
        kpress("9", "WFrame.switch_nth(_, 8)"),
        kpress("0", "WFrame.switch_nth(_, 9)"),
        
        bdoc("Switch to next/previous object within the frame."),
        kpress("n", "WFrame.switch_next(_)"),
        kpress("p", "WFrame.switch_prev(_)"),
        
        bdoc("Move current object within the frame left/right."),
        kpress("comma", "WFrame.dec_index(_, _sub)", "_sub:non-nil"),
        kpress("period", "WFrame.inc_index(_, _sub)", "_sub:non-nil"),
               
        bdoc("Maximize the frame horizontally/vertically."),
        kpress("h", "WFrame.maximize_horiz(_)"),
        kpress("v", "WFrame.maximize_vert(_)"),

        bdoc("Attach tagged objects to this frame."),
        kpress("a", "ioncore.tagged_attach(_)"),
    }),
})

-- Bindings for floating frames.

defbindings("WFrame.floating", {
    bdoc("Toggle shade mode"),
    mdblclick("Button1@tab", "WFrame.set_shaded(_, 'toggle')"),
    
    bdoc("Raise the frame."),
    mpress("Button1@tab", "WRegion.rqorder(_, 'front')"),
    mpress("Button1@border", "WRegion.rqorder(_, 'front')"),
    mclick(META.."Button1", "WRegion.rqorder(_, 'front')"),
    
    bdoc("Lower the frame."),
    mclick(META.."Button3", "WRegion.rqorder(_, 'back')"),
    
    bdoc("Move the frame."),
    mdrag("Button1@tab", "WFrame.p_move(_)"),
})


-- WMoveresMode context bindings
-- 
-- These bindings are available keyboard move/resize mode. The mode
-- is activated on frames with the command begin_kbresize (bound to
-- META.."R" above by default).

defbindings("WMoveresMode", {
    bdoc("Cancel the resize mode."),
    kpress("AnyModifier+Escape","WMoveresMode.cancel(_)"),

    bdoc("End the resize mode."),
    kpress("AnyModifier+Return","WMoveresMode.finish(_)"),

    bdoc("Grow in specified direction."),
    kpress("h",     "WMoveresMode.resize(_, 1, 0, 0, 0)"),
    kpress("l",     "WMoveresMode.resize(_, 0, 1, 0, 0)"),
    kpress("k",     "WMoveresMode.resize(_, 0, 0, 1, 0)"),
    kpress("j",     "WMoveresMode.resize(_, 0, 0, 0, 1)"),
    kpress("f",     "WMoveresMode.resize(_, 1, 0, 0, 0)"),
    kpress("b",     "WMoveresMode.resize(_, 0, 1, 0, 0)"),
    kpress("o",     "WMoveresMode.resize(_, 0, 0, 1, 0)"),
    kpress("n",     "WMoveresMode.resize(_, 0, 0, 0, 1)"),
    
    bdoc("Shrink in specified direction."),
    kpress("Shift+l",     "WMoveresMode.resize(_,-1, 0, 0, 0)"),
    kpress("Shift+h",     "WMoveresMode.resize(_, 0,-1, 0, 0)"),
    kpress("Shift+j",     "WMoveresMode.resize(_, 0, 0,-1, 0)"),
    kpress("Shift+k",     "WMoveresMode.resize(_, 0, 0, 0,-1)"),
    kpress("Shift+f",     "WMoveresMode.resize(_,-1, 0, 0, 0)"),
    kpress("Shift+b",     "WMoveresMode.resize(_, 0,-1, 0, 0)"),
    kpress("Shift+p",     "WMoveresMode.resize(_, 0, 0,-1, 0)"),
    kpress("Shift+n",     "WMoveresMode.resize(_, 0, 0, 0,-1)"),
    
    bdoc("Move in specified direction."),
    kpress(META.."Left",  "WMoveresMode.move(_,-1, 0)"),
    kpress(META.."Right", "WMoveresMode.move(_, 1, 0)"),
    kpress(META.."Up",    "WMoveresMode.move(_, 0,-1)"),
    kpress(META.."Down",  "WMoveresMode.move(_, 0, 1)"),
    kpress(META.."f",     "WMoveresMode.move(_,-1, 0)"),
    kpress(META.."b",     "WMoveresMode.move(_, 1, 0)"),
    kpress(META.."o",     "WMoveresMode.move(_, 0,-1)"),
    kpress(META.."n",     "WMoveresMode.move(_, 0, 1)"),
})


--
-- Menu definitions
--


-- Main menu
defmenu("mainmenu", {
    menuentry("Run...",         "mod_query.query_exec(_)"),
    menuentry("Terminal",       "ioncore.exec_on(_, XTERM or 'xterm')"),
    menuentry("Lock screen",    "ioncore.exec_on(_, 'alock -bg shade:shade=30 -cursor glyph -auth pam')"),
    menuentry("Help",           "mod_query.query_man(_)"),
    menuentry("About Ion",      "mod_query.show_about_ion(_)"),
    submenu("Styles",           "stylemenu"),
    submenu("Session",          "sessionmenu"),
})

-- Session control menu
defmenu("sessionmenu", {
    menuentry("Save",           "ioncore.snapshot()"),
    menuentry("Restart",        "ioncore.restart()"),
    menuentry("Exit",           "ioncore.shutdown()"),
})


-- Context menu (frame actions etc.)
defctxmenu("WFrame", "Frame", {
    -- Note: this propagates the close to any subwindows; it does not
    -- destroy the frame itself, unless empty. An entry to destroy tiled
    -- frames is configured in cfg_tiling.lua.
    menuentry("Close",          "WRegion.rqclose_propagate(_, _sub)"),
    -- Low-priority entries
    menuentry("Attach tagged", "ioncore.tagged_attach(_)", { priority = 0 }),
    menuentry("Clear tags",    "ioncore.tagged_clear()", { priority = 0 }),
    menuentry("Window info",   "mod_query.show_tree(_, _sub)", { priority = 0 }),
})


-- Context menu for groups (workspaces, client windows)
defctxmenu("WGroup", "Group", {
    menuentry("Toggle tag",     "WRegion.set_tagged(_, 'toggle')"),
    menuentry("De/reattach",    "ioncore.detach(_, 'toggle')"), 
})


-- Context menu for workspaces
defctxmenu("WGroupWS", "Workspace", {
    menuentry("Close",          "WRegion.rqclose(_)"),
    menuentry("Rename",         "mod_query.query_renameworkspace(nil, _)"),
    menuentry("Attach tagged",  "ioncore.tagged_attach(_)"),
})


-- Context menu for client windows
defctxmenu("WClientWin", "Client window", {
    menuentry("Kill",           "WClientWin.kill(_)"),
})
