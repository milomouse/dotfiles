-- file: ${XDG_CONFIG_HOME}/nvim/lua/config/fzf.lua

-- colors
vim.g.fzf_colors = {
    fg =        {'fg', 'Comment'},
    bg =        {'bg', 'Normal'},
    hl =        {'fg', 'Identifier'},
    info =      {'fg', 'Number'},
    border =    {'fg', 'Comment'},
    prompt =    {'fg', 'Identifier'},
    pointer =   {'fg', 'Question'},
    marker =    {'fg', 'Error'},
    spinner =   {'fg', 'Label'},
    header =    {'fg', 'Comment'},
}


-- keybindings: general
vim.keymap.set('n', '<leader>ab', ':Buffers<CR>', opts)-- access open buffers
vim.keymap.set('n', '<leader>ac', ':FZF<CR>', opts)-- access current directory
vim.keymap.set('n', '<leader>ap', ':FZF ..<CR>', opts)-- access parent directory
vim.keymap.set('n', '<leader>ah', ':FZF $H<CR>', opts)-- access user base directory
vim.keymap.set('n', '<leader>au', ':FZF $HOME<CR>', opts)-- access user config directory

-- keybindings: search/find/grep
vim.keymap.set('n', '<leader>fd', ':Rg<CR>', opts)-- grep within directory
vim.keymap.set('n', '<leader>fa', ':Lines<CR>', opts)-- grep within active buffers
vim.keymap.set('n', '<leader>fb', ':BLines<CR>', opts)-- grep within active buffer
vim.keymap.set('n', '<leader>fh', ':History<CR>', opts)-- show file history
vim.keymap.set('n', '<leader>fc', ':History:<CR>', opts)-- show cmd history

-- keybindings: git
vim.keymap.set('n', '<leader>gf', ':GFiles<CR>', opts)-- show git files

-- keybindings: menu
vim.cmd([[
    let g:fzf_action = {
    \ 'ctrl-h': 'split',
    \ 'ctrl-v': 'vsplit',
    \ 'ctrl-n': 'tab split' }
]])
