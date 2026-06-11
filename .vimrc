" ============================================================
" 基本設定
" ============================================================

syntax enable                        " シンタックスハイライト有効化
set encoding=utf-8                   " Vim内部の文字コード
set fileencoding=utf-8               " 保存時の文字コード
set fileencodings=utf-8,euc-jp,cp932 " 読み込み時の文字コード候補（BOM不要のため ucs-boms は除去）
set fileformats=unix,dos,mac         " 改行コードの自動判別順
set ambiwidth=double                 " 全角文字（記号など）の幅を2として扱う

" ============================================================
" 表示
" ============================================================

set number                           " 行番号を表示
set scrolloff=8                      " カーソル上下に常に8行の余白を確保
set linebreak                        " 長い行を単語単位で折り返し表示
set laststatus=2                     " ステータスラインを常時表示（ruler は laststatus=2 で不要なため削除）

" ============================================================
" 編集・インデント
" ============================================================

set expandtab                        " タブをスペースに展開
set tabstop=2                        " タブ幅を2スペースに
set shiftwidth=2                     " インデント幅を2スペースに
set autoindent                       " 改行時に前の行のインデントを継続
set virtualedit=onemore              " 行末の1文字後ろにもカーソルを移動可能
set whichwrap=b,s,h,l,<,>,[,]       " 行頭・行末でカーソルが前後の行に移動できる文字を指定

" ============================================================
" 検索
" ============================================================

set incsearch                        " 入力中にリアルタイム検索
set hlsearch                         " 検索結果をハイライト
set ignorecase                       " 大文字小文字を区別しない
set smartcase                        " 検索語に大文字が含まれる場合は区別する

" ============================================================
" クリップボード
" ============================================================

set clipboard+=unnamed               " OSのクリップボードとyank/putを共有

" ============================================================
" キーマップ
" ============================================================

let g:mapleader = "\<Space>"         " Leaderキーをスペースに設定

nnoremap <Leader>w :w<CR>            " Space+w で保存

" Esc 2回押しでハイライト消去（Esc単体のリマップはターミナルのキーコードと衝突するため非推奨）
nnoremap <Esc><Esc> :noh<CR>

" ============================================================
" vim-plug（プラグインマネージャー）
" ============================================================

" vim-plug が未インストールの場合は自動でダウンロード
if empty(glob(expand('~/.vim/autoload/plug.vim')))
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
   \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

call plug#begin()
  Plug 'preservim/nerdtree'          " ファイルツリー表示
  Plug 'tpope/vim-commentary'        " gcc でコメントアウト
  Plug 'LunarWatcher/auto-pairs'     " 括弧・クォートの自動補完（jiangmiao/auto-pairs のメンテ継続フォーク）
  Plug 'nordtheme/vim'               " カラースキーム: Nord
call plug#end()

colorscheme nord                     " Nord カラースキームを適用

" ============================================================
" NERDTree
" ============================================================

let g:NERDTreeShowHidden=1           " 隠しファイル（ドットファイル）を表示

" 引数なしで起動した場合のみ NERDTree を自動表示
autocmd StdinReadPre * let s:std_in=1
autocmd VimEnter * if argc() == 0 && !exists("s:std_in") | NERDTree | endif

" NERDTree のウィンドウだけ残った場合に自動終了
autocmd BufEnter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif

nnoremap <silent><C-e> :NERDTreeToggle<CR>  " Ctrl+e でツリー開閉
