-- file: ${XDG_CONFIG_HOME}/nvim/lua/config/flog.lua

-- fugitive (dependency)
vim.keymap.set('n', '<leader>gs', ':Git<CR>', opts)
vim.keymap.set('n', '<leader>ge', ':Gdiffsplit<CR>', opts)


-- flog
vim.keymap.set('n', '<leader>gg', ':Flogsplit<CR>', opts)
