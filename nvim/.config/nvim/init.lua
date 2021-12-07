-- init.lua -- 2021-10-16
------------------- HELPERS -------------------------------
local cmd = vim.cmd -- to execute Vim commands e.g. cmd('pwd')
local g = vim.g -- a table to access global variables
local opt = vim.opt -- to set options
local map = vim.api.nvim_set_keymap

-- Set numbers
vim.o.number = true

-- Set the leader key
g.mapleader = ','

local Plug = vim.fn['plug#']
vim.call('plug#begin', '~/.config/nvim/plugged')

-- Github theme
Plug 'projekt0n/github-nvim-theme'

-- Sidebar tree
Plug 'kyazdani42/nvim-web-devicons'
Plug 'kyazdani42/nvim-tree.lua'

Plug 'nvim-lualine/lualine.nvim'

Plug 'nvim-lua/plenary.nvim'
Plug 'TimUntersberger/neogit'

Plug('nvim-treesitter/nvim-treesitter', {['do'] = 'TSUpdate'})

vim.call('plug#end')

local neogit = require('neogit')
neogit.setup {}

require('github-theme').setup()
require('nvim-web-devicons').get_icons()

-- Language settings
opt.tabstop = 2
opt.shiftwidth = 2

-- Mapings
-- Map ,n to toggle nvimTree
map('n', '<leader>n', ':NvimTreeToggle<CR>', {})
map('n', '<leader>g', ':Neogit<CR>', {})

require'nvim-treesitter.configs'.setup {
	ensure_installed = "maintained", -- one of "all", "maintained" (parsers with maintainers), or a list of languages
	sync_install = false, -- install languages synchronously (only applied to `ensure_installed`)
	ignore_install = { }, -- List of parsers to ignore installing
	highlight = {
		enable = true,              -- false will disable the whole extension
		disable = { },  -- list of language that will be disabled
		-- Setting this to true will run `:h syntax` and tree-sitter at the same time.
		-- Set this to `true` if you depend on 'syntax' being enabled (like for indentation).
		-- Using this option may slow down your editor, and you may see some duplicate highlights.
		-- Instead of true it can also be a list of languages
		additional_vim_regex_highlighting = false,
	},
	indent = {
		enable = true,              -- false will disable the whole extension
	}
}

-- following options are the default
-- each of these are documented in `:help nvim-tree.OPTION_NAME`
require'nvim-tree'.setup {
	disable_netrw       = true,
	hijack_netrw        = true,
	open_on_setup       = false,
	ignore_ft_on_setup  = {},
	auto_close          = false,
	open_on_tab         = false,
	hijack_cursor       = false,
	update_cwd          = false,
	update_to_buf_dir   = {
		enable = true,
		auto_open = true,
	},
	diagnostics = {
		enable = false,
		icons = {
			hint = "",
			info = "",
			warning = "",
			error = "",
		}
	},
	update_focused_file = {
		enable      = false,
		update_cwd  = false,
		ignore_list = {}
	},
	system_open = {
		cmd  = nil,
		args = {}
	},
	filters = {
		dotfiles = true,
		custom = {}
	},
	git = {
		enable = true,
		ignore = true,
		timeout = 500,
	},
	view = {
		width = 30,
		height = 30,
		hide_root_folder = false,
		side = 'left',
		auto_resize = false,
		mappings = {
			custom_only = false,
			list = {}
		},
		number = false,
		relativenumber = false
	},
	trash = {
		cmd = "trash",
		require_confirm = true
	}
}

-- Lualine config
require'lualine'.setup {
	options = {
		theme = 'github',
		icons_enabled = true,
		component_separators = { left = '', right = ''},
		section_separators = { left = '', right = ''},
		disabled_filetypes = {},
		always_divide_middle = true,
	},
	sections = {
		lualine_a = {'mode'},
		lualine_b = {'branch', 'diff',
			{'diagnostics', sources={'nvim_lsp', 'coc'}}},
		lualine_c = {'filename'},
		lualine_x = {'encoding', 'fileformat', 'filetype'},
		lualine_y = {'progress'},
		lualine_z = {'location'}
	},
	inactive_sections = {
		lualine_a = {},
		lualine_b = {},
		lualine_c = {'filename'},
		lualine_x = {'location'},
		lualine_y = {},
		lualine_z = {}
	},
	tabline = {},
	extensions = {}
}

