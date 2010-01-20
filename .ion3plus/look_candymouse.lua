-- look_candymouse.lua drawing engine configuration file for Ion.
-- milomouse < vincent [at] fea . st >

local vivid   = "#ddb4c4"
local basic   = "#b188a9"
local faded   = "#ae969f"
local night   = "#766169"

local regular_ghost = "#494949"
local faded_ghost   = "#222"
local baleful_ghost = "#101010"

if not gr.select_engine("de") then return end

de.reset()

de.defstyle("*", {
    padding_pixels = 0,
    spacing = 0,
    foreground_colour = night,
    background_colour = "#0c0c0c",
    highlight_pixels = 3,
    highlight_colour = "black",
    shadow_pixels = 3,
    shadow_colour = "black",
    border_style = "elevated",
    text_align = "right",
    font = "-*-fixed-*-*-*-*-9-*-*-*-*-*-*-*",
    de.substyle("important", { foreground_colour = "#b092a4", }),
    de.substyle("critical", { foreground_colour = "#b67caa", }),
    de.substyle("gray", { foreground_colour = "#c6c7c5", }),
    de.substyle("red", { foreground_colour = "#ffadcc", }),
    de.substyle("magenta", { foreground_colour = "#baabb7", }),
    de.substyle("blue", { foreground_colour = "#3f4f60", }),
    de.substyle("cyan", { foreground_colour = "#809cb9", }),
    de.substyle("green", { foreground_colour = "#619489", }),
    de.substyle("yellow", { foreground_colour = "#b7b872", }),
})

de.defstyle("frame", {
    based_on = "*",
    background_colour = "black",
})

de.defstyle("frame-floating", {
    based_on = "frame",
    padding_pixels = 0,
    highlight_pixels = 2,
    highlight_colour = regular_ghost,
    shadow_pixels = 2,
    shadow_colour = regular_ghost,
    de.substyle("active", {
    	highlight_colour = night,
        shadow_colour = basic,
    }),
})

de.defstyle("frame-tiled", {
    based_on = "frame",
    spacing = 0,
    padding_pixels = 0,
    highlight_pixels = 1,
    highlight_colour = "#a488d9",
    shadow_pixels = 1,
    shadow_colour = "#9ed3d7",
    de.substyle("active", {
    	highlight_colour = "#111",
        shadow_colour = "#111",
    	--highlight_colour = "#a488d9",
        --shadow_colour = "#9ed3d7",
    }),
    de.substyle("inactive", {
	--highlight_colour = "#464051",
	highlight_colour = "#131117",
	shadow_colour = "#000",
})
})

de.defstyle("tab", {
    based_on = "*",
    spacing = 2,
    padding_pixels = 1,
    highlight_pixels = 0,
    shadow_pixels = 1,
    shadow_colour = baleful_ghost,
    text_align = "right",
    font = "-*-fixed-*-*-*-*-7-*-*-*-*-*-*-*",
    de.substyle("active-selected", {
	foreground_colour = faded_ghost,
        background_colour = "#625873",
        ---shadow_colour = "#625873",
    }),
    de.substyle("active-unselected", {
	foreground_colour = "#101010",
        background_colour = "#363040",
        ---shadow_colour = "#363040",
    }),
    de.substyle("inactive-selected", {
	foreground_colour = "#222",
        background_colour = "#030303",
	--foreground_colour = "#222",
        --background_colour = "#030303",
    }),
    de.substyle("inactive-unselected", {
	foreground_colour = "#111",
	background_colour = "#000",
    }),
})

de.defstyle("input-edln", {
    based_on = "*",
    padding_pixels = 1,
    background_colour = night,
    highlight_pixels = 1,
    highlight_colour = "black",
    shadow_pixels = 1,
    shadow_colour = "black",
    foreground_colour = "black",
    font ="-*-fixed-*-*-*-*-9-*-*-*-*-*-*-*",
    de.substyle("*-cursor", {
        foreground_colour = night,
        background_colour = faded,
    }),
    de.substyle("*-selection", {
        foreground_colour = "black",
        background_colour = faded,
    }),
})

de.defstyle("stdisp", {
    based_on = "*",
    padding_pixels = 0,
    shadow_pixels = 0,
    highlight_pixels = 0,
    background_colour = "black",
    foreground_colour = night,
    font ="-*-fixed-*-*-*-*-9-*-*-*-*-*-*-*",
})

de.defstyle("tab-menuentry", {
    based_on = "*",
    text_align = "left",
    padding_pixels = 2,
    spacing = 0,
    shadow_pixels = 0,
    highlight_pixels = 0,
    font = "-*-fixed-*-*-*-*-9-*-*-*-*-*-*-*",
    foreground_colour = night,
    background_colour = faded_ghost,
    de.substyle("active-selected", {
        background_colour = faded,
	foreground_colour = "black",
    }),
    de.substyle("inactive-selected", {
	background_colour = night,
	foreground_colour = regular_ghost,
    }),
})

de.defstyle("actnotify", {
    based_on = "*",
    padding_pixels = 2,
    highlight_pixels = 1,
    highlight_colour = basic,
    shadow_pixels = 1,
    shadow_colour = basic,
    background_colour = vivid,
    foreground_colour = baleful_ghost,
    font = "-*-fixed-*-*-*-*-9-*-*-*-*-*-*-*",
})

gr.refresh()

-- EOF
