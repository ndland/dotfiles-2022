-------------------- PLUGINS -------------------------------
local fn = vim.fn

local install_path = fn.stdpath('data') .. '/site/pack/packer/start/packer.nvim'
if fn.empty(fn.glob(install_path)) > 0 then
    packer_bootstrap = fn.system({'git', 'clone', '--depth', '1', 'https://github.com/wbthomason/packer.nvim', install_path})
end
-- Only required if you have packer configured as `opt`
vim.cmd [[packadd packer.nvim]]
-- Only if your version of Neovim doesn't have https://github.com/neovim/neovim/pull/12632 merged
-- vim._update_package_paths()

return require('packer').startup(function(use)
    -- Packer can manage itself
    use 'wbthomason/packer.nvim'

    use 'shougo/deoplete-lsp'
    -- Post-install/update hook with call of vimscript function with argument
    use {
        'shougo/deoplete.nvim',
        run = function()
            vim.fn['remote#host#UpdateRemotePlugins'](0)
        end
    }
    use 'neovim/nvim-lspconfig'
    use {
        'junegunn/fzf',
        run = function()
            vim.fn['fzf#install'](0)
        end
    }
    use 'junegunn/fzf.vim'
    use 'ojroques/nvim-lspfuzzy'
    use 'dracula/vim'
    use 'preservim/nerdtree'

    -- Post-install/update hook with neovim command
    use {
        'nvim-treesitter/nvim-treesitter',
        run = ':TSUpdate'
    }

    -- Track my code stats
    use 'wakatime/vim-wakatime'
    use 'tpope/vim-fugitive'

    -- Automatically set up your configuration after cloning packer.nvim
    -- Put this at the end after all plugins
    if packer_bootstrap then require('packer').sync() end
end)

