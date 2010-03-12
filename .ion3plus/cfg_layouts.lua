--
-- Layouts for Ion
--

--
-- Helper routines and structures
--

-- Tiled frame template for the layouts below
local a_frame = {
    type="WSplitRegion",
    regparams = {
        type = "WFrame", 
        frame_style = "frame-tiled"
    }
}

-- Helper function for generating splits for layouts
local function mksplit(dir, tl, br, float)
    return {
        type = (float and "WSplitFloat" or "WSplitSplit"),
        dir = dir,
        tls = 1,
        brs = 1,
        tl = tl,
        br = br,
    }
end

local function mktiling(split_tree)
    return {
        managed = {
            {
                type = "WTiling",
                bottom = true, -- Make it the bottom of the group
                split_tree = split_tree,
            }
        }
    }
end

--
-- The layouts
--

-- Tiling with single 1:1 horizontal split
ioncore.deflayout("hsplit",
    mktiling(mksplit("horizontal", a_frame, a_frame))
)

-- Tiling with single 1:1 vertical split
ioncore.deflayout("vsplit",
    mktiling(mksplit("vertical", a_frame, a_frame))
)

-- Tiling with single 1:1 floating horizontal split
ioncore.deflayout("hfloat", 
    mktiling(mksplit("horizontal", a_frame, a_frame, true))
)

-- Tiling with single 1:1 floating vertical split
ioncore.deflayout("vfloat", 
    mktiling(mksplit("vertical", a_frame, a_frame, true))
)

-- Tiling with horizontal split and then a vertical float-split
ioncore.deflayout("slide",
    mktiling(mksplit("horizontal", 
                     mksplit("vertical", a_frame, a_frame),
		     mksplit("horizontal", a_frame),
		     mksplit("vertical", a_frame, true))
))

-- Tiling with vertical and then two horizontal splits
ioncore.deflayout("1-2",
    mktiling(mksplit("vertical", 
                     mksplit("horizontal", a_frame),
                     mksplit("horizontal", a_frame, a_frame))
))

-- Tiling with vertical split and then two horizontal splits
ioncore.deflayout("1|2",
    mktiling(mksplit("horizontal", 
                     mksplit("vertical", a_frame),
                     mksplit("vertical", a_frame, a_frame))
))

-- Tiling with a four way split
ioncore.deflayout("2+2",
    mktiling(mksplit("vertical", 
                     mksplit("horizontal", a_frame, a_frame),
                     mksplit("horizontal", a_frame, a_frame))
))

-- Tiling with single full screen frame
local tmp=mktiling(a_frame)
ioncore.deflayout("full", tmp)
ioncore.deflayout("default", tmp)
