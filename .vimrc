syntax enable
set encoding=utf-8
set fileencoding=utf-8
set fileencodings=ucs-boms,utf-8,euc-jp,cp932
set fileformats=unix,dos,mac
set ambiwidth=double
set number
set expandtab
set tabstop=2
set autoindent
set shiftwidth=2
set incsearch
set hlsearch
set ignorecase
set smartcase
set clipboard+=unnamed
set laststatus=2
set ruler
set virtualedit=onemore
set scrolloff=8
set whichwrap=b,s,h,l,<,>,[,]
set linebreak
let g:mapleader = "\<Space>"
nnoremap <Leader>w :w<CR>
nnoremap <Esc> :noh<CR>

call plug#begin()
  Plug 'preservim/nerdtree'
  Plug 'tpope/vim-commentary'
  Plug 'jiangmiao/auto-pairs'
  Plug 'neoclide/coc.nvim', {'branch': 'release'}
"  Plug 'dracula/vim', { 'as': 'dracula' }
  Plug 'nordtheme/vim'
call plug#end()

colorscheme iceberg
" colorscheme dracula
" colorscheme nord


" NERDTree ファイル指定なしでVimを起動した場合だけ表示したい場合は下記行を追加。
autocmd StdinReadPre * let s:std_in=1
autocmd VimEnter * if argc() == 0 && !exists("s:std_in") | NERDTree | endif
let g:NERDTreeShowHidden=1

" The NERD Treeのウィンドウだけが残るような場合にVimを終了したい場合は下記行を追加。
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif

nnoremap <silent><C-e> :NERDTreeToggle<CR>
