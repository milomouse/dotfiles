-- file: ${XDG_CONFIG_HOME}/nvim/lua/config/lsp.lua

-- initialize server(s)
local lspconfig = require('lspconfig')

-- c/c++
lspconfig.clangd.setup{}

-- go
lspconfig.gopls.setup{}


-- diagnostic configuration
local signs = { Error = " ", Warn = " ", Hint = " ", Info = " " }
for type, icon in pairs(signs) do
    local hl = "DiagnosticSign" .. type
    vim.fn.sign_define(hl, { text = icon })
end
vim.diagnostic.config({
    severity_sort = true,
    signs = true,
    virtual_text = false,
    float = {
        scope = 'line',
        source = 'always',
        border = 'single',
    },
})


-- add borders to more popups
vim.lsp.handlers['textDocument/hover'] = vim.lsp.with(vim.lsp.handlers.hover, {
    border = 'single',
})
vim.lsp.handlers['textDocument/signatureHelp'] = vim.lsp.with(vim.lsp.handlers.signature_help, {
    border = 'single',
})


-- Use LspAttach autocommand to only map the following keys
-- after the language server attaches to the current buffer
vim.api.nvim_create_autocmd('LspAttach', {
    group = vim.api.nvim_create_augroup('UserLspConfig', {}),
    callback = function(ev)
        -- enable completion triggered by <c-x><c-o> (changed in keybindings.lua)
        vim.bo[ev.buf].omnifunc = 'v:lua.vim.lsp.omnifunc'

        -- buffer localized keybindings (only used if LSP is active in buffer)
        local opts = { buffer = ev.buf }
        vim.keymap.set('n', '<space>q', vim.diagnostic.setloclist, opts)
        vim.keymap.set('n', '<space>o', vim.diagnostic.open_float, opts)
        vim.keymap.set('n', '<space>n', vim.diagnostic.goto_next, opts)
        vim.keymap.set('n', '<space>p', vim.diagnostic.goto_prev, opts)
        vim.keymap.set('n', '<space>f', vim.lsp.buf.code_action, opts)
        vim.keymap.set('n', '<space>i', vim.lsp.buf.hover, opts)
        vim.keymap.set('n', '<space>r', vim.lsp.buf.references, opts)
        vim.keymap.set('n', '<space>s', vim.lsp.buf.signature_help, opts)
        vim.keymap.set('n', '<space>e', vim.lsp.buf.rename, opts)
        vim.keymap.set('n', '<space>dc', vim.lsp.buf.declaration, opts)
        vim.keymap.set('n', '<space>df', vim.lsp.buf.definition, opts)
        vim.keymap.set('n', '<space>dt', vim.lsp.buf.type_definition, opts)
        vim.keymap.set('n', '<space>di', vim.lsp.buf.implementation, opts)
        -- workspaces don't work well with "autochdir" option enabled
        -- might want to use an external plugin for projects instead
        -- NOTE: these issues could also be server dependent
        vim.keymap.set('n', '<space>wa', vim.lsp.buf.add_workspace_folder, opts)
        vim.keymap.set('n', '<space>wr', vim.lsp.buf.remove_workspace_folder, opts)
        vim.keymap.set('n', '<space>wl', function()
            print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
        end, opts)
        -- C/CPP (clangd) files are formatted according to the ".clang-format" file
        vim.keymap.set('n', '<F3>', function()
            vim.lsp.buf.format { async = true }
        end, opts)
    end,
})


-- -- (TODO: lspkind.nvim) kinds completion icons
-- local M = {}
-- M.icons = {
--     Class = "a ",
--     Color = "b ",
--     Constant = "c ",
--     Enum = "d ",
--     EnumMember = "e ",
--     Field = "f ",
--     File = "g ",
--     Folder = "h ",
--     Function = "i ",
--     Interface = "j ",
--     Keyword = "k ",
--     Method = "l ",
--     Module = "m ",
--     Property = "n ",
--     Snippet = "o ",
--     Struct = "p ",
--     Text = "q ",
--     Unit = "r ",
--     Value = "s ",
--     Variable = "t ",
-- }
--
-- function M.setup()
--     local kinds = vim.lsp.protocol.CompletionItemKind
--     for i, kind in ipairs(kinds) do
--         kinds[i] = M.icons[kind] or kind
--     end
-- end
--
-- return M
