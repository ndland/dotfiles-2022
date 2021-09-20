" init.vim

if (has('termguicolors'))
  set termguicolors
endif

" Specify a directory for plugins
" - For Neovim: stdpath('data') . '/plugged'
" - Avoid using standard Vim directory names like 'plugin'
call plug#begin(stdpath('data') . '/plugged')
  Plug 'wojciechkepka/vim-github-dark'
  Plug 'kaicataldo/material.vim', { 'branch': 'main' }
  Plug 'vim-airline/vim-airline'
  Plug 'vim-airline/vim-airline-themes'
  Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}  " We recommend updating the parsers on update
  Plug 'preservim/nerdtree' |
            \ Plug 'Xuyuanp/nerdtree-git-plugin'
  Plug 'tiagofumo/vim-nerdtree-syntax-highlight'
  " JSX/Javascript
  Plug 'pangloss/vim-javascript'
  Plug 'leafgarland/typescript-vim'
  Plug 'peitalin/vim-jsx-typescript'

  Plug 'neoclide/coc.nvim', {'branch': 'release'}

  Plug 'ryanoasis/vim-devicons' " Always load this last
call plug#end()

set encoding=UTF-8

colorscheme material
set number

let mapleader = ","

nnoremap <leader>n :NERDTreeToggle<CR>
let NERDTreeShowHidden=1

let g:airline_theme='base16_material'

autocmd BufEnter *.{js,jsx,ts,tsx} :syntax sync fromstart
autocmd BufLeave *.{js,jsx,ts,tsx} :syntax sync clear

let g:coc_global_extensions = [
	'coc-tsserver'
]

autocmd FileType javascript setlocal shiftwidth=2 tabstop=2 softtabstop=0 expandtab
autocmd FileType javascriptreact setlocal shiftwidth=2 tabstop=2 softtabstop=0 expandtab

luafile ~/.config/nvim/lua/treesitter.rc.lua
