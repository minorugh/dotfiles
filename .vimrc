" --------------------------------------------------------------------
" Rescue Vim Configuration
"
" 日本語:
" Emacs設定の編集・復旧、および sudoedit 用の軽量Vim設定。
" 普段の編集環境はEmacsであり、このVimはEmacsが起動できない場合や
" システム設定ファイルを編集する場合の補助ツールとして使用する。
"
" English:
" Lightweight Vim configuration for editing and recovering Emacs
" configuration files, and for use with sudoedit.
" Emacs is the primary editing environment; this Vim is intended as
" a fallback tool when Emacs cannot start or when editing system files.
"
" --------------------------------------------------------------------

" ============================================================
" 基本設定
" ============================================================

syntax enable

set encoding=utf-8
set fileencoding=utf-8
set fileencodings=utf-8,euc-jp,cp932
set fileformats=unix,dos,mac
set ambiwidth=double

set noswapfile

" ============================================================
" 表示
" ============================================================

set number
set scrolloff=8
set linebreak
set laststatus=2

" ============================================================
" 編集・インデント
" ============================================================

set expandtab
set tabstop=2
set shiftwidth=2
set autoindent
set virtualedit=onemore
set whichwrap=b,s,h,l,<,>,[,]

set autoread
set hidden

" ============================================================
" 検索
" ============================================================

set incsearch
set hlsearch
set ignorecase
set smartcase

" ============================================================
" クリップボード
" ============================================================

set clipboard+=unnamed

" ============================================================
" キーマップ
" ============================================================

let g:mapleader = "\<Space>"

" Space+w で保存
nnoremap <Leader>w :w<CR>

" Esc 2回で検索ハイライト解除
nnoremap <Esc><Esc> :noh<CR>

" ============================================================
" vim-plug
" ============================================================

call plug#begin()

Plug 'preservim/nerdtree'
Plug 'tpope/vim-commentary'
Plug 'nordtheme/vim'

call plug#end()

colorscheme nord

" ============================================================
" NERDTree
" ============================================================

let g:NERDTreeShowHidden=1

" Ctrl+e でツリー開閉
nnoremap <silent><C-e> :NERDTreeToggle<CR>

" ============================================================
" 終了
" ============================================================

" NERDTreeだけ残った場合は終了
autocmd BufEnter *
      \ if (winnr("$") == 1 &&
      \ exists("b:NERDTree") &&
      \ b:NERDTree.isTabTree()) |
      \ q |
      \ endif
