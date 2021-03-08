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

" dein.vim settings {{{
" install dir {{{
let s:dein_dir = expand('~/.cache/dein')
let s:dein_repo_dir = s:dein_dir . '/repos/github.com/Shougo/dein.vim'
" }}}

" dein installation check {{{
if &runtimepath !~# '/dein.vim'
  if !isdirectory(s:dein_repo_dir)
    execute '!git clone https://github.com/Shougo/dein.vim' s:dein_repo_dir
  endif
  execute 'set runtimepath^=' . s:dein_repo_dir
endif
" }}}

" begin settings {{{
if dein#load_state(s:dein_dir)
  call dein#begin(s:dein_dir)

  " .toml file
  let s:rc_dir = expand('~/.vim')
  if !isdirectory(s:rc_dir)
    call mkdir(s:rc_dir, 'p')
  endif
  let s:toml = s:rc_dir . '/dein.toml'

  " read toml and cache
  call dein#load_toml(s:toml, {'lazy': 0})

  " end settings

  call dein#add('scrooloose/nerdtree')
  call dein#end()
  call dein#save_state()
endif
" }}}

" plugin installation check {{{
if dein#check_install()
  call dein#install()
endif
" }}}

" plugin remove check {{{
let s:removed_plugins = dein#check_clean()
if len(s:removed_plugins) > 0
  call map(s:removed_plugins, "delete(v:val, 'rf')")
  call dein#recache_runtimepath()
endif
" }}}

" autocmd VimEnter * execute 'NERDTree'
colorscheme iceberg
syntax enable
let NERDTreeShowHidden = 1

map <C-e> :NERDTreeToggle<CR>


" ファイル指定なしでVimを起動した場合だけ表示したい場合は下記行を追加。

autocmd VimEnter * if argc() == 0 && !exists("s:std_in") | NERDTree | endif

" The NERD Treeのウィンドウだけが残るような場合にVimを終了したい場合は下記行を追加。

autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif