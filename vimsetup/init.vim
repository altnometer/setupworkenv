set nocompatible              " be iMproved, required
filetype off                  " required

" Plugins--------------------------------------------------------------------{{{
" :PlugInstall, :PlugUpdate, :PlugClean,
" :PlugUpgrade (plug itself), :PlugStatus
call plug#begin('~/.local/share/nvim/plugged')
Plug 'airblade/vim-gitgutter'
Plug 'fatih/vim-go', { 'do': ':GoUpdateBinaries', 'for': 'go'}
Plug 'vim-airline/vim-airline'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-unimpaired'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-dispatch'
Plug 'mhartington/oceanic-next'
"""""""""""neosnippet recommended setup""""""""""""
Plug 'Shougo/deoplete.nvim'
Plug 'Shougo/neosnippet.vim'
Plug 'Shougo/neosnippet-snippets'
Plug 'zchee/deoplete-go', {'do': 'make'}
"""""""""""""""""""""""""""""""""""""""""""""""""""
Plug 'kassio/neoterm'
Plug 'janko-m/vim-test'
" provides insert mode auto-completion for quotes, parens, brackets, etc.
" Plug 'Raimondi/delimitMate'
" display the indention levels with thin vertical lines
Plug 'Yggdroot/indentLine'
Plug 'Valloric/MatchTagAlways'
" Initialize plugin system
call plug#end()
"}}}

" System Settings  ----------------------------------------------------------{{{
" Don't use TABs but spaces
autocmd BufNewFile,BufRead *.go setlocal noexpandtab tabstop=4 shiftwidth=4
set tabstop=4
set softtabstop=4
set shiftwidth=4
set shiftround
set expandtab
" Make search case insensitive
set hlsearch
set incsearch
set ignorecase
set smartcase
" vim-gitgutter needs reasonably fast updatetime. Default 4s is too slow.
set updatetime=250
" Remember cursor position between vim sessions
 autocmd BufReadPost *
             \ if line("'\"") > 0 && line ("'\"") <= line("$") |
             \   exe "normal! g'\"" |
             \ endif
             " center buffer around cursor when opening files

" }}}

let g:ruby_host_prog = "/var/lib/gems/2.3.0/gems/neovim-0.7.1/bin/neovim-ruby-host"
let g:python_host_prog = "/usr/bin/python2"
let g:python3_host_prog = "/usr/bin/python3"
" Skip the check of neovim module
let g:python3_host_skip_check = 1
" Or if you have Neovim >= 0.1.5
if (has("termguicolors"))
 set termguicolors
endif

" Theme
syntax enable
colorscheme OceanicNext
let g:airline_theme='oceanicnext'

" enable vim-airline integration with plugins.
let g:airline_enable_fugitive=1
let g:airline_enable_syntastic=1
let g:airline_enable_bufferline=1

" Rebind <Leader> key
" I like to have it here becuase it is easier to reach than the default and
" it is next to ``m`` and ``n`` which I use for navigating between tabs.
let mapleader = ","

" Quick quit command
noremap <Leader>e :quit<CR>  " Quit current window
noremap <Leader>E :qa!<CR>   " Quit all windows
" easier moving between tabs
map <Leader>n <esc>:tabprevious<CR>
map <Leader>m <esc>:tabnext<CR>

" Quicksave command
noremap <C-Z> :update<CR>
vnoremap <C-Z> <C-C>:update<CR>
" this  would save and go back to insert mode.
" inoremap <C-Z> <C-O>:update<CR>
" Added by sam@lf
" this  would save and exit insert mode.
inoremap <C-Z> <C-C>:update<CR>

" Disable stupid backup and swap files - they trigger too many events
" for file system watchers
set nobackup
set nowritebackup
set noswapfile

"""""""""""""""""""""""""neosnippet""""""""""""""""""""""""""""""""""""""""""""
" Plugin key-mappings.
" Note: It must be "imap" and "smap".  It uses <Plug> mappings.
imap <C-j>     <Plug>(neosnippet_expand_or_jump)
smap <C-j>     <Plug>(neosnippet_expand_or_jump)
xmap <C-j>     <Plug>(neosnippet_expand_target)

let g:deoplete#enable_at_startup = 1
let g:deoplete#sources#go#gocode_binary = $GOPATH.'/bin/gocode'
let g:deoplete#sources#go#sort_class = ['package', 'func', 'type', 'var', 'const']

" Showing line numbers and length
set relativenumber  " show line numbers
set number  " show line numbers
set tw=79   " width of document (used by gd)
set nowrap  " don't automatically wrap on load
set fo-=t   " don't automatically wrap text when typing
set colorcolumn=80
highlight ColorColumn ctermbg=233

hi CursorLineNR guifg=#ffffff
" Better copy & paste
" When you want to paste large blocks of code into vim, press F2 before you
" paste. At the bottom you should see ``-- INSERT (paste) --``.
set pastetoggle=<F2>
" set clipboard=unnamed
set clipboard=unnamedplus


" Mouse and backspace
set mouse=a  " on OSX press ALT and click
set bs=2     " make backspace behave like normal again
" Slow ESC fix according to https://www.johnhawthorn.com/2012/09/vi-escape-delays/
" This delay exists because many keys (arrows keys, ALT) rely on it as an escape character.
" timeoutlen is used for mapping delays
" ttimeoutlen is used for key code delays
" set timeoutlen=1000 ttimeoutlen=10
set timeoutlen=1000 ttimeoutlen=0

" backspace in normal mode
nnoremap <bs> X
""""""""""""""""""""""""delimitMate""""""""""""""""""""""""""""""""""""""""""""
" <S-tab> jump over delimeter
imap <C-Tab> <S-Tab>
let delimitMate_expand_cr = 1
let delimitMate_expand_space = 1
let delimitMate_jump_expansion = 1
""""""""""""""""""""""""neoterm""""""""""""""""""""""""""""""""""""""""""""""""
let neoterm_default_mod = 'vertical'

" Fold, gets it's own section  ----------------------------------------------{{{
" credit to Mike Hartington https://github.com/mhartington/dotfiles/
  function! MyFoldText() " {{{
      let line = getline(v:foldstart)
      let nucolwidth = &fdc + &number * &numberwidth
      let windowwidth = winwidth(0) - nucolwidth - 3
      let foldedlinecount = v:foldend - v:foldstart

      " expand tabs into spaces
      let onetab = strpart('          ', 0, &tabstop)
      let line = substitute(line, '\t', onetab, 'g')

      let line = strpart(line, 0, windowwidth - 2 -len(foldedlinecount))
      " let fillcharcount = windowwidth - len(line) - len(foldedlinecount) - len('lines')
      " let fillcharcount = windowwidth - len(line) - len(foldedlinecount) - len('lines   ')
      let fillcharcount = windowwidth - len(line)
      " return line . '…' . repeat(" ",fillcharcount) . foldedlinecount . ' Lines'
      return line . '…'. repeat(" ",fillcharcount)
  endfunction " }}}

  set foldtext=MyFoldText()

  autocmd InsertEnter * if !exists('w:last_fdm') | let w:last_fdm=&foldmethod | setlocal foldmethod=manual | endif
  autocmd InsertLeave,WinLeave * if exists('w:last_fdm') | let &l:foldmethod=w:last_fdm | unlet w:last_fdm | endif

  " autocmd FileType vim setlocal fdc=1
  set foldlevel=99

  " Space to toggle folds.
  "nnoremap <Space> za
  "vnoremap <Space> za
  autocmd FileType vim setlocal foldmethod=marker
  autocmd FileType vim setlocal foldlevel=0

  autocmd FileType javascript,html,css,scss,typescript setlocal foldlevel=99

  autocmd FileType css,scss,json setlocal foldmethod=marker
  autocmd FileType css,scss,json setlocal foldmarker={,}

  autocmd FileType coffee setl foldmethod=indent
  let g:xml_syntax_folding = 1
  autocmd FileType xml setl foldmethod=syntax

  autocmd FileType html setl foldmethod=expr
  autocmd FileType html setl foldexpr=HTMLFolds()

  " autocmd FileType javascript,typescript,json setl foldmethod=syntax
  autocmd FileType javascript,typescript,typescriptreact,json setl foldmethod=syntax

" }}}


