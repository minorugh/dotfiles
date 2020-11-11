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
NeoBundle 'scrooloose/nerdtree'

" You can specify revision/branch/tag.
NeoBundle 'Shougo/vimshell', { 'rev' : '3787e5' }

" Required:
call neobundle#end()

" Required:
filetype plugin indent on

" If there are uninstalled bundles found on startup,
" this will conveniently prompt you to install them.
NeoBundleCheck
"End NeoBundle Scripts-------------------------

map <C-e> :NERDTreeToggle<CR>

" デフォルトで隠しファイルを表示
let NERDTreeShowHidden = 1
" ファイル指定なしでVimを起動した場合だけ表示
autocmd VimEnter * if argc() == 0 && !exists("s:std_in") | NERDTree | endif
" The NERD Treeのウィンドウだけが残るような場合にVimを終了
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif
" ブックマークメニューをデフォルトで表示
let NERDTreeShowBookmarks=1
" ファイルを開くときにNERDtreeを自動的に閉じる
let g:NERDTreeQuitOnOpen = 1

colorscheme iceberg

set encoding=utf-8
set fileencoding=utf-8
set fileencodings=ucs-boms,utf-8,euc-jp,cp932
set fileformats=unix,dos,mac
set ambiwidth=double
set wildmenu
set clipboard=unnamedplus
set clipboard=autoselect
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
