-- file: ${XDG_CONFIG_HOME}/nvim/lua/plugins.lua

-- auto-install 'packer', if not found on system
local ensure_packer = function()
    local fn = vim.fn
    local install_path = fn.stdpath('data')..'/site/pack/packer/start/packer.nvim'
    if fn.empty(fn.glob(install_path)) > 0 then
        fn.system({'git', 'clone', '--depth', '1', 'https://github.com/wbthomason/packer.nvim', install_path})
        vim.cmd [[packadd packer.nvim]]
        return true
    end
    return false
end
local packer_bootstrap = ensure_packer()


-- specify plugins to install, if not found on system
return require('packer').startup(function(use)
    -- 'packer' can self manage
    use 'wbthomason/packer.nvim'

    -- LSP framework
    use 'neovim/nvim-lspconfig'

    -- LSP completion menu symbols
    use 'onsails/lspkind.nvim'

    -- statusbar/tabline customization
    use { 'nvim-lualine/lualine.nvim', requires = { 'nvim-tree/nvim-web-devicons' } }

    -- git integration
    use { 'rbong/vim-flog', requires = 'tpope/vim-fugitive' }

    -- file picker ['vifm' inside 'neovim']
    use 'vifm/vifm.vim'

    -- auto-pairing
    use 'tmsvg/pear-tree'

    -- quick comment/uncomment
    use 'tomtom/tcomment_vim'

    -- fuzzy finder (prefer this simpler version over 'ibhagwan/fzf-lua')
    use 'junegunn/fzf.vim'

    -- automatically set up configuration after cloning packer.nvim
    if packer_bootstrap then
        require('packer').sync()
    end
end)
