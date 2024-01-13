-- file: ${XDG_CONFIG_HOME}/nvim/lua/config/lsp.lua

-- language server setup
local lspconfig = require('lspconfig')

lspconfig.clangd.setup{}
lspconfig.gopls.setup{}


-- severity configuration [fallback]
local signs = { Error = " ", Warn = " ", Hint = " ", Info = " " }
for type, icon in pairs(signs) do
  local hl = "DiagnosticSign" .. type
  vim.fn.sign_define(hl, { text = icon, texthl = hl, numhl = hl })
end
vim.diagnostic.config({
  virtual_text = false,
  severity_sort = true,
  float = {
    source = "always",
  },
})


-- Use LspAttach autocommand to only map the following keys
-- after the language server attaches to the current buffer
vim.api.nvim_create_autocmd('LspAttach', {
  group = vim.api.nvim_create_augroup('UserLspConfig', {}),
  callback = function(ev)
    -- Enable completion triggered by <c-x><c-o>
    -- vim.bo[ev.buf].omnifunc = 'v:lua.vim.lsp.omnifunc'

    -- buffer localized keybindings
    local opts = { buffer = ev.buf }
    vim.keymap.set('n', '<space>q', vim.diagnostic.setloclist, opts)
    vim.keymap.set('n', '<space>o', vim.diagnostic.open_float, opts)
    vim.keymap.set('n', '<space>n', vim.diagnostic.goto_next, opts)
    vim.keymap.set('n', '<space>p', vim.diagnostic.goto_prev, opts)
    vim.keymap.set('n', '<space>wa', vim.lsp.buf.add_workspace_folder, opts)
    vim.keymap.set('n', '<space>wr', vim.lsp.buf.remove_workspace_folder, opts)
    vim.keymap.set('n', '<space>wl', function()
      print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
    end, opts)
    vim.keymap.set('n', '<space>rn', vim.lsp.buf.rename, opts)
    vim.keymap.set('n', '<space>rf', vim.lsp.buf.references, opts)
    vim.keymap.set('n', '<space>i', vim.lsp.buf.hover, opts)
    vim.keymap.set('n', '<space>s', vim.lsp.buf.signature_help, opts)
    vim.keymap.set('n', '<F3>', function()
      vim.lsp.buf.format { async = true }
    end, opts)
  end,
})
