-- file: ${XDG_CONFIG_HOME}/nvim/lua/config/fzf.lua

-- colors
vim.g.fzf_colors = {
  fg =      {'fg', 'LineNr'},
  bg =      {'bg', 'Normal'},
  hl =      {'fg', 'Normal'},
  info =    {'fg', 'LineNr'},
  border =  {'fg', 'Comment'},
  prompt =  {'fg', 'Conditional'},
  pointer = {'fg', 'Conditional'},
  marker =  {'fg', 'Error'},
  spinner = {'fg', 'Label'},
  header =  {'fg', 'Comment'},
}

-- keybindings
vim.keymap.set('n', '<C-i>', ':FZF $H<CR>', opts)
vim.keymap.set('n', '<C-o>', ':FZF<CR>', opts)
vim.keymap.set('n', '<C-p>', ':FZF ..<CR>', opts)
vim.keymap.set('n', '<C-f>', ':History<CR>', opts)
vim.keymap.set('n', '<C-s>', ':BLines<CR>', opts)
vim.keymap.set('n', '<C-b>', ':Buffers<CR>', opts)
vim.keymap.set('n', '<leader>ge', ':GFiles<CR>', opts)
vim.cmd([[
  let g:fzf_action = {
    \ 'ctrl-t': 'tab split',
    \ 'ctrl-h': 'split',
    \ 'ctrl-v': 'vsplit' }
]])
