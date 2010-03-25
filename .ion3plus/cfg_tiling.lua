-- Ion tiling module configuration file
-- milomouse <vincent[at]fea.st>

-- Bindings for the tilings. 
defbindings("WTiling", {
    bdoc("Split current frame vertically."),
    kpress(META.."minus", "WTiling.split_at(_, _sub, 'top', false)"),
    kpress(META.."plus", "WTiling.split_at(_, _sub, 'left', false)"),
    
    bdoc("Go to frame above/below/right/left of current frame."),
    kpress(META..UP, "ioncore.goto_next(_sub, 'up', {no_ascend=_})"),
    kpress(META..DOWN, "ioncore.goto_next(_sub, 'down', {no_ascend=_})"),
    kpress(META..LEFT, "ioncore.goto_next(_sub, 'left')"),
    kpress(META..RIGHT, "ioncore.goto_next(_sub, 'right')"),

    bdoc("Destroy current frame."),
    kpress(META.."z", "WTiling.unsplit_at(_, _sub)"),
})

-- Frame bindings
defbindings("WFrame.floating", {
    submap(META.."semicolon", {
        bdoc("Tile frame, if no tiling exists on the workspace"),
        kpress("B", "mod_tiling.mkbottom(_)"),
    }),
})

-- Context menu for tiled workspaces.
defctxmenu("WTiling", "Tiling", {
    menuentry("Destroy frame", 
              "WTiling.unsplit_at(_, _sub)"),

    menuentry("Split vertically", 
              "WTiling.split_at(_, _sub, 'bottom', false)"),
    menuentry("Split horizontally", 
              "WTiling.split_at(_, _sub, 'right', false)"),
    
    menuentry("Flip", "WTiling.flip_at(_, _sub)"),
    menuentry("Transpose", "WTiling.transpose_at(_, _sub)"),
    
    menuentry("Untile", "mod_tiling.untile(_)"),
    
    submenu("Float split", {
        menuentry("At left", 
                  "WTiling.set_floating_at(_, _sub, 'toggle', 'left')"),
        menuentry("At right", 
                  "WTiling.set_floating_at(_, _sub, 'toggle', 'right')"),
        menuentry("Above",
                  "WTiling.set_floating_at(_, _sub, 'toggle', 'up')"),
        menuentry("Below",
                  "WTiling.set_floating_at(_, _sub, 'toggle', 'down')"),
    }),

    submenu("At root", {
        menuentry("Split vertically", 
                  "WTiling.split_top(_, 'bottom')"),
        menuentry("Split horizontally", 
                  "WTiling.split_top(_, 'right')"),
        menuentry("Flip", "WTiling.flip_at(_)"),
        menuentry("Transpose", "WTiling.transpose_at(_)"),
    }),
})

-- Extra context menu extra entries for floatframes. 
defctxmenu("WFrame.floating", "Floating frame", {
    append=true,
    menuentry("New tiling", "mod_tiling.mkbottom(_)"),
})
