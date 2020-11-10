"NeoBundle Scripts-----------------------------
if &compatible
  set nocompatible               " Be iMproved
endif

" Required:
set runtimepath+=/home/minoru/.vim/bundle/neobundle.vim/

" Required:
call neobundle#begin(expand('/home/minoru/.vim/bundle'))


" Let NeoBundle manage NeoBundle
" Required:
NeoBundleFetch 'Shougo/neobundle.vim'

" Add or remove your Bundles here:
NeoBundle 'Shougo/neosnippet.vim'
NeoBundle 'Shougo/neosnippet-snippets'
NeoBundle 'tpope/vim-fugitive'
NeoBundle 'ctrlpvim/ctrlp.vim'
NeoBundle 'flazz/vim-colorschemes'

" You can specify revision/branch/tag.
NeoBundle 'Shougo/vimshell', { 'rev' : '3787e5' }

NeoBundle 'scrooloose/nerdtree'

" Required:
call neobundle#end()

" Required:
filetype plugin indent on

" If there are uninstalled bundles found on startup,
" this will conveniently prompt you to install them.
NeoBundleCheck
"End NeoBundle Scripts-------------------------

" If there are uninstalled bundles found on startup,
" this will conveniently prompt you to install them.
NeoBundleCheck


"NERDTree settings----------------
nnoremap <silent><C-e> :NERDTreeToggle<CR>

" Open NERDTree after opening vim with no arguments,
" If there is an argument, NERDTree will not start and the file passed as an argument will be opened.
autocmd vimenter * if !argc() | NERDTree | endif

" If NERDTree is open when all other buffers are closed, NERDTree is also closed.
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif

" Show hidden files by default
let NERDTreeShowHidden = 1

" Automatically close NERDtree when opening a file
let g:NERDTreeQuitOnOpen = 1
"End NERDTree settings----------------

" colorscheme iceberg

set encoding=utf-8
set fileencoding=utf-8
set fileencodings=ucs-boms,utf-8,euc-jp,cp932
set fileformats=unix,dos,mac
set ambiwidth=double
set wildmenu
set clipboard=unnamedplus
set number
set title
set hidden
set showmatch
set expandtab
set tabstop=4
set noswapfile
set shiftwidth=4
set smartindent
set ignorecase
set smartcase
set nowrapscan
set hlsearch
set wrap
set incsearch
set ruler
set showcmd
set hidden
set history=2000
inoremap <silent> jj <esc>
set sh=zsh
set laststatus=2
set statusline=%F%m%=[%p%%]\ (%l,%c)\ %{'['.(&fenc!=''?&fenc:&enc).']\ ['.&fileformat.']'}
set nocompatible
filetype plugin indent on
syntax enable
syntax on
highlight StatusLine term=none cterm=none ctermfg=white ctermbg=black
let _curfile=expand("%:r")
if _curfile == 'Makefile'
  set noexpandtab
endif
