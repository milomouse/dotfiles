-- Common settings for the "emboss" styles

de.defstyle("frame", {
    background_colour = "#030303",
    -- The special "inherit" value causes setting 'background_colour'
    -- above not to set padding_colour, but this colour being inherited.
    padding_colour = "inherit",
    de.substyle("quasiactive", {
        -- Something detached from the frame is active
        padding_colour = "#ac648d",
    }),
    de.substyle("userattr1", {
        -- For user scripts
        padding_colour = "#3e6d43",
    }),
    border_style = "ridge",
    padding_pixels = 0,
    highlight_pixels = 0,
    shadow_pixels = 0,
    spacing = 0,
})

de.defstyle("frame-tiled", {
    border_style = "inlaid",
    padding_pixels = 1,
})

--de.defstyle("frame-tiled-alt", {
--    bar = "none",
--})

de.defstyle("frame-floating", {
    bar = "shaped",
    spacing = 0,
})

de.defstyle("frame-transient", {
    --bar = "none",
    spacing = 0,
})

de.defstyle("actnotify", {
    shadow_colour = "#c04040",
    highlight_colour = "#111",
    background_colour = "#c5b457",
    foreground_colour = "#444",
})

de.defstyle("tab", {
    de.substyle("*-*-*-unselected-activity", {
        shadow_colour = "#c04040",
        highlight_colour = "#444",
        background_colour = "#574e54",
        foreground_colour = "#272727",
    }),
    
    de.substyle("*-*-*-selected-activity", {
        shadow_colour = "#c04040",
        highlight_colour = "#444",
        background_colour = "#373737",
        foreground_colour = "#ccc",
    }),
    
    de.substyle("*-*-*-tabnumber", {
        background_colour = "black",
        foreground_colour = "#789abb",
    }),
})


de.defstyle("tab-frame", {
    spacing = 1,
})

de.defstyle("tab-frame-floating", {
    spacing = 0,
})

de.defstyle("tab-frame-transient", {
    spacing = 0,
})

de.defstyle("tab-menuentry", {
    text_align = "left",
    highlight_pixels = 0,
    shadow_pixels = 0,
})

de.defstyle("tab-menuentry-big", {
    font = "-*-fixed-*-*-*-*-9-*-*-*-*-*-*-*",
    padding_pixels = 2,
})


de.defstyle("stdisp", {
    shadow_pixels = 0,
    highlight_pixels = 0,
    text_align = "left",

    de.substyle("important", {
        foreground_colour = "#8097cb",
    }),

    de.substyle("critical", {
        foreground_colour = "#75606c",
    }),
})
