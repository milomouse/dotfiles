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
vim.keymap.set('n', 'g[', ':blast<CR>', opts)
vim.keymap.set('n', 'gp', ':bprevious<CR>', opts)
vim.keymap.set('n', 'g]', ':bfirst<CR>', opts)
vim.keymap.set('n', 'gd', ':bwipeout<CR>', opts)-- [usually] prevents re-opening closed buffers..
vim.keymap.set('n', 'gD', ':bwipeout!<CR>', opts)-- e.g. when using :blast/<c-^>, <c-i>, <c-o>,..
-- vim.keymap.set('n', 'gd', ':bdelete<CR>', opts)
-- vim.keymap.set('n', 'gD', ':bdelete!<CR>', opts)
vim.keymap.set('n', 'gw', ':close<CR>', opts)
vim.keymap.set('n', 'go', ':enew<CR>', opts)
vim.keymap.set('n', 'gl', '<C-^>', opts)


-- windows: faster resizing
vim.keymap.set('n', '<C-Up>', ':resize +2<CR>', opts)
vim.keymap.set('n', '<C-Down>', ':resize -2<CR>', opts)
vim.keymap.set('n', '<C-Left>', ':vertical resize -2<CR>', opts)
vim.keymap.set('n', '<C-Right>', ':vertical resize +2<CR>', opts)


-- windows: more intuitive bindings
vim.keymap.set('n', '<C-w>=', ':vsplit<CR>', opts)
vim.keymap.set('n', '<C-w>-', ':split<CR>', opts)


-- windows: open terminal
vim.keymap.set('n', '<C-space>', ':15%split | :terminal<CR>', opts)


--------------------
-- insert mode    --
--------------------

-- completion: faster navigation
vim.keymap.set('i', '<C-space>', '<C-x><C-o>', opts)--open completion
vim.keymap.set('i', '<Tab>', 'pumvisible() ? "\\<C-y>" : "\\<Tab>"', {expr = true})--select menu option with Tab
vim.keymap.set('i', '<CR>', 'pumvisible() ? "\\<C-y>" : "\\<CR>"', {expr = true})--select menu option with Return
vim.keymap.set('i', '<C-j>', 'pumvisible() ? "\\<C-n>" : "\\<C-j>"', {expr = true})--navigate down with ctrl+j
vim.keymap.set('i', '<C-k>', 'pumvisible() ? "\\<C-p>" : "\\<C-k>"', {expr = true})--navigate up with ctrl+k


--------------------
-- terminal mode  --
--------------------

-- windows: revert to Normal mode
vim.keymap.set('t', '<C-space>', '<C-\\><C-N><CR>', opts)

-- windows: faster terminal navigation
vim.keymap.set('t', '<C-w>w', '<C-\\><C-N><C-w>w', opts)
vim.keymap.set('t', '<C-w>j', '<C-\\><C-N><C-w>j', opts)
vim.keymap.set('t', '<C-w>k', '<C-\\><C-N><C-w>k', opts)
vim.keymap.set('t', '<C-w>h', '<C-\\><C-N><C-w>h', opts)
vim.keymap.set('t', '<C-w>l', '<C-\\><C-N><C-w>l', opts)

-- windows: faster resizing
vim.keymap.set('t', '<C-Up>', '<C-\\><C-N>:resize +2<CR>', opts)
vim.keymap.set('t', '<C-Down>', '<C-\\><C-N>:resize -2<CR>', opts)
vim.keymap.set('t', '<C-Left>', '<C-\\><C-N>:vertical resize -2<CR>', opts)
vim.keymap.set('t', '<C-Right>', '<C-\\><C-N>:vertical resize +2<CR>', opts)
