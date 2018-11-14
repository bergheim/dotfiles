set nocompatible " must be first

if empty(glob('~/.vim/autoload/plug.vim'))
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

call plug#begin('~/.vim/plugged')

Plug 'tpope/vim-surround'
Plug 'tpope/vim-commentary'
Plug 'editorconfig/editorconfig-vim'
Plug 'airblade/vim-gitgutter'
Plug 'leafgarland/typescript-vim', { 'for': 'typescript' }
" bracket mappings
Plug 'tpope/vim-unimpaired'
Plug 'tpope/vim-fugitive'
Plug 'moll/vim-node'
Plug 'francoiscabrol/ranger.vim'
let g:ranger_map_keys = 0
Plug 'mbbill/undotree'

" themes
Plug 'itchyny/lightline.vim'

Plug 'captbaritone/molokai'
Plug 'KeitaNakamura/neodark.vim'
Plug 'morhetz/gruvbox'
Plug 'arcticicestudio/nord-vim'
Plug 'rakr/vim-one'
Plug 'ayu-theme/ayu-vim'

" Initialize plugin system
call plug#end()

""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" general
""""""""""""""""""""""""""""""""""""""""""""""""""""""""
filetype on " detect the filetype
set backspace=indent,eol,start
filetype plugin indent on
set history=150          " keep command line history
set showcmd             " display incomplete commands
set hidden              " allow multiple buffers without saving
set encoding=utf-8
set fileencoding=utf-8
" assumes fast connection
set ttyfast
" every window gets a status line
set laststatus=2

set background=dark
syntax on
"Allow  256 colors in Terminal
set t_Co=256

" colorscheme nord
let ayucolor="mirage"   " for dark version of theme
colorscheme gruvbox

set wildmenu " command line completion in statusbar for commands
set ruler " always show current position at bottom
set cmdheight=2
set number " linenumbers

" jump to the last position when reopening a file
if has("autocmd")
  au BufReadPost * if line("'\"") > 0 && line("'\"") <= line("$") | exe "normal! g`\"" | endif
endif

""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" files/backups
""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set cursorline " Highlight the cursor screen line "
set colorcolumn=80 " Draws a vertical line at column 80 "
" String to put at the start of lines that have been wrapped "
let &showbreak='↪ '
" Minimal number of screen lines to keep above and below the cursor "
set scrolloff=3

set nobackup
set nowb
set noswapfile

" Reload files modified outside of Vim"
set autoread

""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Visual Cues
""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set showmatch " show matching brackets
set mat=5 " how many tenths for a sec to blink matches
set hlsearch
set incsearch
set ignorecase
set so=1 "keep x lines top/bottom for scope
set novisualbell " don't blink
set noerrorbells " and don't scream


""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Text formatting/layout
""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"set fo=tcrqn " See Help (complex)
set ai " autoindent
set si " smart indent
set cindent " c-style
set tabstop=4
set shiftwidth=4
set softtabstop=4
set wrap " do wrap lines

" Inspired by https://github.com/tpope/vim-unimpaired "
" Sets paste on and set nopaste when leaving insert mode "
" using an autocommand "
nnoremap <silent> yo  :set paste<cr>o
nnoremap <silent> yO  :set paste<cr>O

" ensure every file does syntax highlighting (full)
autocmd BufEnter * :syntax sync fromstart

""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Useful abbrevs
""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Remove any trailing whitespace that is in the file
autocmd BufRead,BufWrite * if ! &bin | silent! %s/\s\+$//ge | endif

:imap jj <Esc>
:imap jk <Esc>:w<CR>
