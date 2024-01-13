-- file: ${XDG_CONFIG_HOME}/nvim/lua/keybindings.lua

-- define options for bindings
local opts = {
    noremap = true, -- non-recursive
    silent = true,  -- do not show message
}

-- define leader key
vim.g.mapleader = ','


--------------------
-- normal mode    --
--------------------

-- unset keybindings [if applicable]
vim.keymap.set('n', '<space>', '<Nop>', opts)

-- buffers: faster navigation
vim.keymap.set('n', 'gn', ':bnext<CR>', opts)
vim.keymap.set('n', 'gp', ':bprevious<CR>', opts)
vim.keymap.set('n', 'gd', ':bdelete<CR>', opts)
vim.keymap.set('n', 'gD', ':bdelete!<CR>', opts)
vim.keymap.set('n', 'gw', ':close<CR>', opts)
vim.keymap.set('n', 'gl', '<C-^>', opts)


-- windows: faster navigation
vim.keymap.set('n', '<C-h>', '<C-w>h', opts)
vim.keymap.set('n', '<C-j>', '<C-w>j', opts)
vim.keymap.set('n', '<C-k>', '<C-w>k', opts)
vim.keymap.set('n', '<C-l>', '<C-w>l', opts)


-- windows: faster resizing
vim.keymap.set('n', '<C-Up>', ':resize -2<CR>', opts)
vim.keymap.set('n', '<C-Down>', ':resize +2<CR>', opts)
vim.keymap.set('n', '<C-Left>', ':vertical resize -2<CR>', opts)
vim.keymap.set('n', '<C-Right>', ':vertical resize +2<CR>', opts)


-- windows: more intuitive bindings
vim.keymap.set('n', '<C-w>=', ':vsplit<CR>', opts)
vim.keymap.set('n', '<C-w>-', ':split<CR>', opts)


-- code: faster operations
vim.keymap.set('n', '<F4>', ':retab<CR>', opts)
-- vim.keymap.set('n', '<F5>', ':let _s=@/<Bar>:%s/\s\+$//e<Bar>:let @/=_s<Bar><CR>', opts)
