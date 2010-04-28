-- look_candymouse.lua drawing engine configuration file for Ion.
-- milomouse <vincent[at]fea.st>
-- last updated: 2010-04-09

local vivid   = "#ddb4c4"
local basic   = "#b188a9"
local faded   = "#433b47"
local night   = "#625e69"

local regular = "#494949"
local dimmed  = "#222"
local baleful = "#101010"

if not gr.select_engine("de") then return end

de.reset()

de.defstyle("*", {
    padding_pixels = 0,
    spacing = 0,
    foreground_colour = "#0a0a0a",
    background_colour = "#0a0a0a",
    highlight_pixels = 3,
    highlight_colour = "black",
    shadow_pixels = 3,
    shadow_colour = "black",
    border_style = "elevated",
    text_align = "left",
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
    --transparent_background = "true",
    background_colour = "black",
})

de.defstyle("frame-floating", {
    based_on = "frame",
    padding_pixels = 0,
    highlight_pixels = 2,
    highlight_colour = regular,
    shadow_pixels = 2,
    shadow_colour = regular,
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
    highlight_colour = baleful,
    shadow_pixels = 1,
    shadow_colour = baleful,
    de.substyle("active", {
    	highlight_colour = "#a488d9",
        shadow_colour = "#a488d9",
        --shadow_colour = "#9ed3d7",
    }),
    de.substyle("inactive", {
	  highlight_colour = "#524e58",
	    shadow_colour = "#524e58",
	    --shadow_colour = "#090909",
})
})

de.defstyle("tab", {
    based_on = "*",
    spacing = 1,
    padding_pixels = 1,
    highlight_pixels = 1,
    shadow_pixels = 1,
    shadow_colour = baleful,
    text_align = "left",
    font = "-*-fixed-*-*-*-*-2-*-*-*-*-*-*-*",
    de.substyle("active-selected", {
	  foreground_colour = "#625873",
        background_colour = "#625873",
	  --foreground_colour = "#404040",
        --background_colour = "#0e0e0e",
    }),
    de.substyle("active-unselected", {
	  --foreground_colour = "#202020",
        --background_colour = "#54464e",
	  foreground_colour = "#2e2e2e",
        background_colour = "#2e2e2e",
    }),
    de.substyle("inactive-selected", {
	  --foreground_colour = "#333",
	  foreground_colour = "#161616",
        background_colour = "#161616",
    }),
    de.substyle("inactive-unselected", {
	  foreground_colour = "#0a0a0a",
        background_colour = "#0a0a0a",
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
    background_colour = dimmed,
    de.substyle("active-selected", {
	  foreground_colour = "black",
        background_colour = faded,
    }),
    de.substyle("inactive-selected", {
	  foreground_colour = regular,
	    background_colour = night,
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
    foreground_colour = baleful,
    font = "-*-fixed-*-*-*-*-9-*-*-*-*-*-*-*",
})

gr.refresh()

-- EOF
