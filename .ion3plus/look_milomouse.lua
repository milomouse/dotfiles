-- look_milomouse.lua drawing engine configuration file for Ion.

if not gr.select_engine("de") then return end

de.reset()

de.defstyle("*", {
    shadow_colour = "#090909",
    highlight_colour = "#090909",
    background_colour = "#000",
    foreground_colour = "#766169",
    padding_pixels = 0,
    highlight_pixels = 1,
    shadow_pixels = 0,
    border_style = "elevated",
    font = "-*-fixed-*-*-*-*-9-*-*-*-*-*-*-*",
    text_align = "center",
})

de.defstyle("tab", {
    font = "-*-fixed-*-*-*-*-9-*-*-*-*-*-*-*",
    de.substyle("active-selected", {
        shadow_colour = "#101010",
        highlight_colour = "#101010",
        background_colour = "#040404",
        foreground_colour = "#789abb",
    }),
    de.substyle("active-unselected", {
        shadow_colour = "#222",
        highlight_colour = "#101010",
        background_colour = "#060606",
        foreground_colour = "#444",
    }),
    de.substyle("inactive-selected", {
        shadow_colour = "#222",
        highlight_colour = "#101010",
        background_colour = "#060606",
        foreground_colour = "#444",
    }),
    de.substyle("inactive-unselected", {
        shadow_colour = "#222",
        highlight_colour = "#101010",
        background_colour = "#090909",
        foreground_colour = "#333",
    }),
    text_align = "center",
})

de.defstyle("input", {
    shadow_colour = "#404040",
    highlight_colour = "#111",
    background_colour = "#000000",
    foreground_colour = "#789abb",
    padding_pixels = 1,
    highlight_pixels = 1,
    shadow_pixels = 0,
    border_style = "elevated",
    de.substyle("*-cursor", {
        background_colour = "#ccc",
        foreground_colour = "#000000",
    }),
    de.substyle("*-selection", {
        background_colour = "#222",
        foreground_colour = "#ccc",
    }),
})

de.defstyle("input-menu", {
    de.substyle("active", {
        shadow_colour = "#304050",
        highlight_colour = "#708090",
        background_colour = "#506070",
        foreground_colour = "#ffffff",
    }),
})

dopath("lookcommon_emboss")

gr.refresh()

