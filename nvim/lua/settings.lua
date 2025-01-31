-- file: ${XDG_CONFIG_HOME}/nvim/lua/settings.lua

-- disable builtin plugins
local disabled_built_ins = {
    "netrw",
    "netrwFileHandlers",
    "netrwPlugin",
    "netrwSettings",
}
for _, plugin in pairs(disabled_built_ins) do
    vim.g["loaded_" .. plugin] = 1
end


-- colorscheme (TODO: need to update my theme from .vim to lua, eventually)
local colorscheme = 'candymouse'
local is_ok, _ = pcall(vim.cmd, "colorscheme " .. colorscheme)
if not is_ok then
    vim.notify('colorscheme ' .. colorscheme .. ' not found!')
    return
end
vim.opt.termguicolors = false

-- vim.cmd [[colorscheme testmouse]]


-- general
vim.opt.encoding = 'UTF-8'
vim.opt.autochdir = true
vim.opt.directory = { vim.env.XDG_DATA_HOME .. "/nvim/state/swap//" }
vim.opt.swapfile = true
vim.opt.backupdir = { vim.env.XDG_DATA_HOME .. "/nvim/state/backup//" }
vim.opt.backup = false
vim.opt.undodir = { vim.env.XDG_DATA_HOME .. "/nvim/state/undo//" }
vim.opt.undofile = false
vim.opt.undolevels = 100
vim.opt.history = 1000
vim.opt.hidden = true
vim.opt.scrolloff = 2
vim.opt.splitbelow = true
vim.opt.splitright = true
vim.opt.ttyfast = true
vim.opt.spell = false
vim.opt.spelllang = { 'en_us' }
vim.opt.wrap = false
vim.opt.foldmethod = 'manual'
vim.opt.foldenable = true
vim.opt.jumpoptions = 'clean'


-- visuals
vim.opt.laststatus = 2
vim.opt.showcmd = true
vim.opt.showmode = false
vim.opt.modeline = false
vim.opt.number = true
vim.opt.relativenumber = false
vim.opt.guicursor = 'i:block'
vim.opt.cursorline = false
vim.opt.cursorcolumn = false
vim.opt.signcolumn = 'no'
vim.opt.ruler = true
vim.opt.listchars = {
    tab = '│·',
    trail = '·',
    nbsp = '¬',
    precedes = '▒',
    extends = '▒',
}
vim.opt.list = true
vim.opt.errorbells = false
vim.opt.shortmess:append("I") -- +=
-- vim.opt.shortmess:prepend("I") -- ^=
-- vim.opt.shortmess:remove("I") -- -=


-- indentation
vim.opt.tabstop = 4
vim.opt.shiftwidth = 4
vim.opt.softtabstop = 4
vim.opt.expandtab = true
vim.opt.smarttab = true
vim.opt.autoindent = true
vim.opt.breakindent = true
vim.opt.cindent = false


-- searching
vim.opt.incsearch = true
vim.opt.hls = true
vim.opt.showmatch = true
vim.opt.ignorecase = true
vim.opt.smartcase = true


-- globbing
vim.opt.wildmode = 'list:longest,full'
vim.opt.wildignore = vim.opt.wildignore + {
    '*/.git/*,*/.hg/*,*/.svn/*,*/node_modules/*,=npm-debug.log,*/pip_modules/*,__pycache__',
    '*.o,*.obj,*.exe,*.dll,*.jar,*.pyc,*.rbc,*.class,*.so',
    '*/cache/*',
}


-- menus
vim.opt.complete = vim.opt.complete - 'i'
vim.opt.completeopt = { "menu", "menuone", "noselect", "popup" }


-- mouse
vim.opt.mouse = 'a'
vim.opt.mousehide = true


-- performance
vim.opt.updatetime = 300
vim.opt.ttimeoutlen = 10


-- netrw (disabled plugin; fallback options if re-enabled)
vim.g.netrw_banner = 0
vim.g.netrw_liststyle = 3
vim.g.netrw_winsize = '30'
vim.g.netrw_browsex_viewer = "xdg-open"
