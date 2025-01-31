-- file: ${XDG_CONFIG_HOME}/nvim/lua/config/vifm.lua

-- keybindings
vim.keymap.set('n', '<leader>ve', ':Vifm<CR>', opts)-- select file to edit
vim.keymap.set('n', '<leader>vh', ':SplitVifm<CR>', opts)-- split buffer and select file(s) to open
vim.keymap.set('n', '<leader>vv', ':VsplitVifm<CR>', opts)-- split buffer veritcally and select file(s) to open
vim.keymap.set('n', '<leader>vp', ':PeditVifm<CR>', opts)-- select file to preview in a split
vim.keymap.set('n', '<leader>vd', ':DiffVifm<CR>', opts)-- select file(s) to compare with the current file
