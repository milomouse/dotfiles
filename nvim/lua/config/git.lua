-- file: ${XDG_CONFIG_HOME}/nvim/lua/config/flog.lua

-- keybindings
vim.keymap.set('n', '<leader>gg', ':Git<CR>', opts)
vim.keymap.set('n', '<leader>gd', ':Gdiffsplit<CR>', opts)
vim.keymap.set('n', '<leader>gs', ':Flogsplit<CR>', opts)
