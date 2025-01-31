-- file: ${XDG_CONFIG_HOME}/nvim/lua/autocmds.lua

local augroup = vim.api.nvim_create_augroup
local autocmd = vim.api.nvim_create_autocmd

--------------------
-- general        --
--------------------

-- enable backup/undo for specific directories only
autocmd({ 'BufWinEnter', 'WinEnter', 'FocusGained' }, {
    pattern = { vim.env.XDG_CONFIG_HOME .. "/*", vim.fn.expand("$H") .. "/code/*" },
    command = 'setlocal undofile backup'
})


-- do not automatically comment newlines
autocmd('BufEnter', {
    pattern = '',
    command = 'set fo-=c fo-=r fo-=o'
})


--------------------
-- filetypes      --
--------------------

-- signature column for specific filetypes
augroup('fto_signcol', { clear = true })
autocmd('Filetype', {
    group = 'fto_signcol',
    pattern = { 'c', 'cpp', 'go' },
    command = 'setlocal signcolumn=yes:1'
})


-- smaller tabs/spaces
augroup('fto_small_tabs', { clear = true })
autocmd('Filetype', {
    group = 'fto_small_tabs',
    pattern = { 'text', 'xml', 'html', 'xhtml', 'css', 'scss', 'javascript', 'typescript' },
    command = 'setlocal shiftwidth=2 tabstop=2 softtabstop=2'
})


-- skeletons per language
augroup('fto_skeleton', { clear = true })
autocmd('BufNewFile', {
    group = 'fto_skeleton',
    pattern = "*.c",
    command = "0r ~/nvim/templates/base.c",
})


-- plain text documents
augroup('ft_text', { clear = true })
autocmd('Filetype', {
    group = 'ft_text',
    pattern = { 'text', "*.txt" },
    command = 'setlocal linebreak wrap spell backspace=indent,eol,start'
})


-- Go (hard tabs, respecting pre-formatted files)
augroup('ft_go', { clear = true })
autocmd('Filetype', {
    group = 'ft_go',
    pattern = 'go',
    command = 'setlocal noexpandtab tabstop=4 shiftwidth=4 softtabstop=-1 preserveindent copyindent'
})


--------------------
-- terminal       --
--------------------

-- upon entering :terminal set some local options
autocmd('TermOpen', {
    command = 'setlocal listchars= nonumber norelativenumber nocursorline nobuflisted'
})


-- automatically enter Insert mode when creating a new :terminal
autocmd({ 'TermOpen', 'BufWinEnter', 'WinEnter' }, {
    pattern = 'term://*',
    command = 'startinsert'
})
