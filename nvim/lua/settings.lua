-- file: ${XDG_CONFIG_HOME}/nvim/lua/settings.lua

--------------------
-- nvim section   --
--------------------

-- colorscheme
local colorscheme = 'candymouse'
local is_ok, _ = pcall(vim.cmd, "colorscheme " .. colorscheme)
if not is_ok then
    vim.notify('colorscheme ' .. colorscheme .. ' not found!')
    return
end
vim.opt.termguicolors = false


-- general
vim.opt.encoding = 'UTF-8'
vim.opt.backup = false
vim.opt.swapfile = true
vim.opt.history = 1000
vim.opt.hidden = true
vim.opt.foldenable = false
vim.opt.wrap = false
vim.opt.scrolloff = 2
vim.opt.splitbelow = true
vim.opt.splitright = true
vim.opt.ttyfast = true
vim.opt.spell = false
vim.opt.spelllang = { 'en_us' }


-- visuals
vim.opt.laststatus = 2
vim.opt.showcmd = true
vim.opt.showmode = false
vim.opt.modeline = false
vim.opt.number = true
vim.opt.relativenumber = false
vim.opt.signcolumn = 'no'
vim.opt.guicursor = 'i:block'
vim.opt.cursorline = false
vim.opt.cursorcolumn = false
vim.opt.ruler = true
vim.opt.listchars = {
    tab = '│·',
    trail = '·',
    nbsp = '¬',
    precedes = '▒',
    extends = '▒',
}
vim.opt.errorbells = false
vim.opt.list = true


-- indentation
vim.opt.ignorecase = true
vim.opt.smartcase = true
vim.opt.smarttab = true
vim.opt.autoindent = true
vim.opt.breakindent = true
vim.opt.cindent = false


-- tabs
vim.opt.tabstop = 2
vim.opt.softtabstop = 2
vim.opt.shiftwidth = 2
vim.opt.expandtab = true


-- searching
vim.opt.incsearch = false
vim.opt.hls = false
vim.opt.showmatch = true


-- globbing
vim.opt.wildmode = 'list:longest,full'
vim.opt.wildignore = vim.opt.wildignore + {
    '*/.git/*,*/.hg/*,*/.svn/*,*/node_modules/*,=npm-debug.log,__pycache__',
    '*.o,*.obj,*.exe,*.dll,*.jar,*.pyc,*.rbc,*.class,*.so',
    '*/cache/*',
}


-- menus
vim.opt.complete = vim.opt.complete - 'i'
vim.opt.completeopt = { "menu", "menuone", "noselect" }
-- vim.opt.completeopt = 'longest'


-- netrw
vim.g.netrw_banner = 0
vim.g.netrw_liststyle = 3
vim.g.netrw_winsize = '30'
vim.g.netrw_browsex_viewer = "xdg-open"


-- mouse
vim.opt.mouse = 'a'
vim.opt.mousehide = true


-- performance
vim.opt.updatetime = 300
vim.opt.ttimeoutlen = 10
