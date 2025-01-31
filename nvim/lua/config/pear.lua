-- file: ${XDG_CONFIG_HOME}/nvim/lua/config/pear.lua

-- keybindings
vim.keymap.set('i', '<C-l>', '<Plug>(PearTreeJump)', opts)--jump to outer pair
vim.keymap.set('i', '<C-o>', '<Plug>(PearTreeJNR)', opts)--jump down a line
