-- Ion mod_sp configuration file
-- milomouse <vincent[at]fea.st>

defbindings("WScreen", {
    bdoc("Toggle scratchpad."),
    kpress(META.."grave", "mod_sp.set_shown_on(_, 'toggle')"),
})

