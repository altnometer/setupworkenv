" install vim-plug --------------------------------------------------------{{{
" https://github.com/junegunn/vim-plug
" https://github.com/junegunn/vim-plug#commands
if empty(glob('~/.local/share/nvim/site/autoload/plug.vim'))
  silent !curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif
" }}}

" Plugins -----------------------------------------------------------------{{{1
set nocompatible              " be iMproved, required
filetype off                  " required
" :PlugInstall, :PlugUpdate, :PlugClean,
" :PlugUpgrade (plug itself), :PlugStatus
call plug#begin('~/.local/share/nvim/plugged')
" Plug 'tmhedberg/SimpylFold'
Plug 'tyru/open-browser.vim'
Plug 'jeetsukumaran/vim-indentwise'
Plug 'vim-scripts/ReplaceWithRegister'
Plug 'michaeljsmith/vim-indent-object'
Plug 'AndrewRadev/splitjoin.vim'
Plug 'justinmk/vim-sneak'
Plug 'wellle/targets.vim'
Plug 'machakann/vim-highlightedyank'
Plug 'christoomey/vim-tmux-navigator'

" go {{{2
Plug 'fatih/vim-go', { 'do': ':GoUpdateBinaries', 'for': 'go'}
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
" Plug 'mdempsky/gocode', { 'rtp': 'nvim', 'do': '~/.local/share/nvim/plugged/gocode/nvim/symlink.sh' }
" 2}}}

" syntax {{{2
Plug 'scrooloose/syntastic'
" 'vim-polyglot' must be after vim-go.
Plug 'sheerun/vim-polyglot'
"}}}

Plug 'vim-airline/vim-airline'

" git {{{2
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-rhubarb'
Plug 'airblade/vim-gitgutter'
Plug 'idanarye/vim-merginal'
" 2}}}

Plug 'tpope/vim-unimpaired'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-dispatch'
Plug 'mhartington/oceanic-next'
Plug 'lifepillar/vim-solarized8'
Plug 'romainl/flattened'

" denite {{{2
Plug 'Shougo/denite.nvim'
Plug 'nixprime/cpsm', {'do': 'PY3=ON ./install.sh'}
Plug 'Shougo/neomru.vim'
" 2}}}

" deoplete {{{2
Plug 'Shougo/deoplete.nvim'
" adds suggestions from running sessions
Plug 'wellle/tmux-complete.vim'
"  2}}}

" neosnippet recommended {{{2
Plug 'Shougo/neosnippet.vim'
Plug 'Shougo/neosnippet-snippets'
Plug 'honza/vim-snippets'
Plug 'zchee/deoplete-go', {'do': 'make'}
" 2}}}

" html,css,js {{{2
" https://github.com/mattn/emmet-vim
Plug 'mattn/emmet-vim'
" 2}}}

Plug 'kassio/neoterm'
Plug 'janko-m/vim-test'
" provides insert mode auto-completion for quotes, parens, brackets, etc.
" Plug 'Raimondi/delimitMate'
" display the indention levels with thin vertical lines
" Plug 'Yggdroot/indentLine'
Plug 'Valloric/MatchTagAlways'
" Initialize plugin system
Plug '~/.local/share/nvim/myvimplugins'
call plug#end()
" 1}}}

" System Settings  ----------------------------------------------------------{{{
" Don't use TABs but spaces
filetype plugin indent on
set tabstop=4 softtabstop=4 shiftwidth=4 shiftround expandtab
set nostartofline " do not move cursor to start of line on commands like 'jump'
" do not remove inserted indentation when switching to normal mode.
" https://stackoverflow.com/questions/7413036/stopping-vim-from-removing-indentation-on-empty-lines
" http://vim.wikia.com/wiki/Get_the_correct_indent_for_new_lines_despite_blank_lines
inoremap <CR> <CR>x<BS>
nnoremap o ox<BS>
nnoremap O Ox<BS>
augroup augroup_remove_trailing_whitespaces
  autocmd!
  autocmd BufWritePre *.html,*.tmpl,*.vim,*.sh :%s/\s\+$//e
augroup END
" Make search case insensitive
set hlsearch
set incsearch
set ignorecase
set smartcase
" vim-gitgutter needs reasonably fast updatetime. Default 4s is too slow.
set updatetime=100
" program to use for the |K| command, e.g. :Man option will open man page for
" the word under cursor.
set keywordprg=:Man
" Open 'help' in a new buffer of the same window.
command! -nargs=1 -complete=help H :enew | :set buftype=help | :h <args>
" let xdg-open decide which browser to open a url link with.
let g:netrw_browsex_viewer="qutebrowser"

" auto {{{2
augroup auto_system
    autocmd!
    " Remember cursor position between switching buffers.
    autocmd BufLeave * let b:winview = winsaveview()
    autocmd BufEnter * if(exists('b:winview')) | call winrestview(b:winview) | endif
    " Remember cursor position between vim sessions
    autocmd BufReadPost *
             \ if line("'\"") > 0 && line ("'\"") <= line("$") |
             \   exe "normal! g'\"" |
             \ endif
    " center buffer around cursor when opening files
    autocmd BufRead * normal zz
    " Show whitespace
    " MUST be inserted BEFORE the colorscheme command
    " as per http://vim.wikia.com/wiki/Highlight_unwanted_spaces
    autocmd ColorScheme * highlight ExtraWhitespace ctermbg=red guibg=red
    highlight ExtraWhitespace ctermbg=red guibg=red
    match ExtraWhitespace /\s\+$/
    autocmd BufWinEnter * match ExtraWhitespace /\s\+$/
    autocmd InsertEnter * match ExtraWhitespace /\s\+\%#\@<!$/
    autocmd InsertLeave * match ExtraWhitespace /\s\+$/
    autocmd BufWinLeave * call clearmatches()
    " leave a file mark when you leave the buffer.
    " Get to where you left it with '|` followed by 'C|H|J|G|...'
    autocmd BufLeave *.css  normal! mC
    autocmd BufLeave *.html normal! mH
    autocmd BufLeave *.tmpl normal! mH
    autocmd BufLeave *.js   normal! mJ
    autocmd BufLeave *.go   normal! mG
    autocmd BufLeave *.vim  normal! mV
    autocmd Filetype vim,neosnippet,html,gohtmltmpl setlocal tabstop=2 shiftwidth=2
    autocmd Filetype vim,neosnippet setlocal keywordprg=:help
    " stop jumping to the next line when the current line goes over the limit.
autocmd Filetype html,gohtmltmpl,sh setlocal textwidth=0
    " Wrap lines in quickfix
    autocmd FileType qf setlocal wrap
    " manpage with table of contents sidebar with neovim
    " https://asciinema.org/a/165076
    " add to shellrc: export MANPAGER="nvim +set\ filetype=man -"
    if has('nvim')
        autocmd FileType man
            \ call man#show_toc() |
            \ setlocal laststatus=0 nonumber norelativenumber |
            \ nnoremap <buffer> l <Enter> |
            \ wincmd H |
            \ vert resize 35 |
            \ wincmd p
    endif
augroup END
" }}}2
"
let g:ruby_host_prog = "/var/lib/gems/2.3.0/gems/neovim-0.7.1/bin/neovim-ruby-host"
let g:python_host_prog = "/usr/bin/python2"
let g:python3_host_prog = "/usr/bin/python3"
" Skip the check of neovim module
let g:python3_host_skip_check = 1
" Or if you have Neovim >= 0.1.5
if (has("termguicolors"))
 set termguicolors
endif
syntax enable
" Showing line numbers and length
set relativenumber  " show line numbers
set number  " show line numbers
set tw=79   " width of document (used by gd)
" set nowrap  " don't automatically wrap on load
set wrap
" set fo-=t   " don't automatically wrap text when typing
set colorcolumn=80
highlight ColorColumn ctermbg=233
hi CursorLineNR guifg=#ffffff
" Better copy & paste
" When you want to paste large blocks of code into vim, press F2 before you
" paste. At the bottom you should see ``-- INSERT (paste) --``.
set pastetoggle=<F2>
" set clipboard=unnamed
set clipboard=unnamedplus
" Disable stupid backup and swap files - they trigger too many events
" for file system watchers
set nobackup
set nowritebackup
set noswapfile
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
" Treat '-' as part of the word rather than a word separator.
" set iskeyword+=-
set iskeyword+=
set encoding=utf-8
set fileencoding=utf-8
" Define order of searches for word completion. kspell will add
" dictionary search only if ":set[local] spell" is enabled.
set complete=.,w,b,u,t,i,kspell
" colors ------------------------------------------------------------------{{{2
colorscheme OceanicNext
" colorscheme desert
augroup CursorLineOnlyInActiveWindow
  autocmd!
  " set cursorline, cursorcolumn only in active buffer
  autocmd VimEnter,WinEnter,BufWinEnter * setlocal cursorline cursorcolumn
  autocmd WinLeave * setlocal nocursorline nocursorcolumn
augroup END
" Default Colors for CursorLine
" highlight  CursorLine ctermbg=Yellow ctermfg=None
" highlight  CursorLine ctermbg=8 ctermfg=15

" colorscheme 'OceanicNext' colors for reference.
"               '#1b2b34' " background
" let s:base08=['#ec5f67', '203'] red
" let s:base09=['#f99157', '209'] red-yellow
" let s:base0A=['#fac863', '221'] yellow
" let s:base0B=['#99c794', '114'] cian green
" let s:base0C=['#62b3b2', '73'] cian
" let s:base0D=['#6699cc', '68'] ciun blue
" let s:base0E=['#c594c5', '176'] pink
" let s:base0F=['#ab7967', '137'] biege

" let cursorlinebg = '#1b2b34' " background
" let cursorlinebg = '#4b4b2b' " " yellow
" let cursorlinebg = '#5b5b2b' " " yellow
" let cursorlinebg = '#2b5b34' " " green
" let cursorlinebg = '#2b5b2b' " " green
let cursorcolbg = '#1b4b24' " green
let cursorlinebg = '#1b4b24' " " green
" let cursorlinebg = '#0b1b0b' " dark green
" let cursorlinebg = '#1b3b4b' " cian
" let cursorlineinsertbg = '#2b4b2b' " " green
" let cursorlineinsertbg = '#4b4b2b' " " yellow
" let cursorlineinsertbg = '#5b5b2b' " " yellow
" let cursorlineinsertbg = '#0b1b24' " " darker background
" let cursorcolinsertbg = '#2b4b5b' " " lighter cian
let cursorcolinsertbg = '#0b0b1b' " " darker background
let cursorlineinsertbg = '#0b0b1b' " " darker background
" execute "highlight CursorColumn ctermfg=White ctermbg=Yellow guifg=fg guibg=" . cursorcolbg
" execute "highlight CursorLine ctermfg=White ctermbg=Yellow guifg=fg guibg=" . cursorlinebg
" execute "highlight CursorColumn guifg=fg guibg=" . cursorlinebg
" execute "highlight CursorLine guifg=fg guibg=" . cursorlinebg
execute "highlight CursorColumn guibg=" . cursorcolbg
execute "highlight CursorLine guibg=" . cursorlinebg
augroup color_cursor
    autocmd!
    " Change Color when entering Insert Mode
    autocmd InsertEnter * execute "highlight CursorColumn guibg=" . cursorcolinsertbg
    autocmd InsertEnter * execute "highlight CursorLine guibg=" . cursorlineinsertbg
    " Revert Color to default when leaving Insert Mode
    " autocmd InsertLeave * execute "highlight CursorColumn guifg=fg guibg=" . cursorlinebg
    " autocmd InsertLeave * execute "highlight CursorLine guifg=fg guibg=" . cursorlinebg
    autocmd InsertLeave * execute "highlight CursorColumn guibg=" . cursorcolbg
    autocmd InsertLeave * execute "highlight CursorLine guibg=" . cursorlinebg
augroup END
" }}}2
set completeopt-=preview " do not open preview window for completion.
" diff {{{2
set diffopt+=iwhite
set diffexpr=""
" }}}

" System mappings  ----------------------------------------------------------{{{
" Rebind <leader> key
" I like to have it here becuase it is easier to reach than the default and
" it is next to ``m`` and ``n`` which I use for navigating between tabs.
let mapleader = ","
" shadowing '/' for search slows it down.
" let maplocalleader = "/"
vnoremap <A-/> <esc>/\%V
" open all folds in the fold and put the cursor to the center.
nnoremap za zAzz
" window manipulation {{{2
" help window-moving
nnoremap <leader>ww <C-w><C-w>
" switch to horizontal layout.
nnoremap <leader>wv <C-w>H
" switch to vertical layout.
nnoremap <leader>wh <C-w>K
" diff between windows.
nnoremap <leader>wd :windo diffthis<CR><C-w><C-w>
nnoremap <leader>wu :diffupdate!<CR>
nnoremap <leader>wo :diffoff!<CR>
" }}}2
" put the word in qoutes.
" inoremap <C-u> <c-c>bi"<c-c>ea"<c-c>a
" inoremap <C-u> <c-c>:<c-u>let save_pos=getcurpos() <bar> call searchpos('\v[^\[({< ]+', 'b', line('.'))<cr>i"<c-c>:call setpos('.', save_pos)<cr>la"<c-c>a
" jump to the next search result and put the cursor to
" the center if is too close to the edges.
function! s:IsVisible(line) " {{{2
  " IsVisible returns true if 'line' is within screenlimits.
  " line("w0"), line("w$") do not work correctly with folds.
  let l:diff = a:line - line(".")
  if l:diff < 0 | let l:diff = - l:diff | endif
  return (winheight(0) - winline()) - l:diff > 2
endfunction
" }}}2

nnoremap <expr> n <sid>IsVisible(search(@/, 'nw')) ?  'nzO' :  'nzOzz'
nnoremap <expr> N <sid>IsVisible(search(@/, 'bnw')) ?  'NzO' :  'NzOzz'
nnoremap <expr> * <sid>IsVisible(search(expand("<cword>"), 'nw')) ?  '*zO' :  '*zOzz'
nnoremap <expr> # <sid>IsVisible(search(expand("<cword>"), 'bnw')) ?  '#zO' :  '#zOzz'

" maximize window horizontally.
noremap _ <c-w>_
" minimize window horizontally.
noremap - <c-w>1_
" equalize window horizontally.
" see ':map =', it is mapped to something. single '=' is slow because of that.
noremap == <c-w>=
" Toggle Uppercase of the word.
" Open vimrc/vim.init file.
" nnoremap <leader>v :vsplit $MYVIMRC<cr>
" nnoremap <localleader>v :vsplit $MYVIMRC<cr>
" nnoremap <leader>v :edit $MYVIMRC<cr>
" 'fugitive' does not follow symlinks yet.
" https://github.com/tpope/vim-fugitive/issues/147
nnoremap <leader>v :edit $HOME/redmoo/setupworkenv/vimsetup/init.vim<cr>
" Source vimrc/vim.init file.
nnoremap <leader>V :source $MYVIMRC<cr>
" Quit current window
noremap <leader>e :quit<CR>
noremap <leader>x :quitall<CR>
noremap <leader>E <c-w>o
" Close_QFix_Or_Other_Win defined in QFixClose.vim myvimplugins
nmap q <plug>(Close_QFix_Or_Other_Win)
noremap <leader>k :wincmd b <bar> :bdelete<CR>
" Bonly, BOnly, Bufonly, BufOnly are of BufOnly.vim plugin
nnoremap <leader>K :Bonly<CR>
" easier moving between tabs
" map <leader>n <esc>:tabprevious<CR>
" map <leader>m <esc>:tabnext<CR>
nnoremap <PageUp>   :bprevious<CR>
nnoremap <PageDown> :bnext<CR>
nnoremap <Home>   :bfirst<CR>
nnoremap <End> :blast<CR>
" Quicksave command
noremap <C-s> :update<CR>
nnoremap <C-z> <nop>
" this  would save and go back to insert mode.
" inoremap <C-Z> <C-O>:update<CR>
" this  would save and exit insert mode.
inoremap <C-s> <Esc>:update<CR>
inoremap <C-z> <c-c>zza
" Toggle spell checking on and off with `,z`
" normal mode
nmap <silent> <leader>z :setlocal spell! spelllang=en_us<CR>
" insert mode
imap <F5> <C-o>:setlocal spell! spelllang=en_us<CR>
"inoremap <F5> <C-\><C-O>:setlocal spelllang=en_us spell! spell?<CR>
" Removes highlight of your last search
noremap <C-n> :nohl<CR>
vnoremap <C-n> :nohl<CR>
" inoremap <C-n> :nohl<CR>
" help ins-special-special
" This breaks undo at each line break. It also expands abbreviations before this.
inoremap <CR> <C-]><C-G>u<CR>
inoremap <C-w> <C-G>u<C-w>
" Delete line
" inoremap <C-u> <C-G>u<C-u>
" Ctrl+<movement> to move around the windows.
" this is taken care with 'christoomey/vim-tmux-navigator'
" if these mappings are kept here, the plugin mappings stop working.
" map <C-j> <C-w>j map <C-k> <C-w>k map <C-l> <C-w>l map <C-h> <C-w>h
inoremap <C-j> <esc><C-w>j
inoremap <C-k> <esc><C-w>k
inoremap <C-l> <esc><C-w>l
inoremap <C-h> <esc><C-w>h
tnoremap <C-j> <C-\><C-n><C-w>j
tnoremap <C-k> <C-\><C-n><C-w>k
tnoremap <C-l> <C-\><C-n><C-w>l
tnoremap <C-h> <C-\><C-n><C-w>h
" FILE FINDING
" Search down into subdirrectories
" Provides tab-comletion for all file-relates tasks
set path+=**
" Display all matching files when we tab complete
set wildmenu
" set wildmode=list:full
set wildignore+=*.pyc,*.bak
set wildignore+=*/.git/**/*
set wildignore+=tags
set wildignore+=*.tar.
" Hit tab to :find by partial match
" Use * to make it fuzzy
" FILE BROWSING, start with :Vex[plore], or :Sex[plore] for horizontal split.
" hit <i> to change view.
let g:netrw_banner=0   " disable the banner
let g:netrw_list_hide=netrw_gitignore#Hide()
let g:netrw_list_hide=',\(^\|\s\s\)\zs\.\S\+\'
let g:netrw_list_hide=',.*\.pyc$'
" close quickfix window.
" nnoremap <leader>q :cclose<CR>
" Add 'relative number'k and 'relative number'j to jump list
nnoremap <expr> k (v:count > 1 ? "m'" . v:count : '') .'gk'
nnoremap <expr> j (v:count > 1 ? "m'" . v:count : '') .'gj'
" jump to '{' or '}', :help section
map [[ ?{<CR>w99[{
map ][ /}<CR>b99]}
map ]] j0[[%/{<CR>
map [] k$][%?}<CR>
" search for visually selected text.
vnoremap // y/\V<C-r>=escape(@",'/\')<CR><CR>
" }}}

" setfiletype -------------------------------------------------------------{{{
augroup filetypedetect
    " autocmd BufNew,BufNewFile,BufRead *.tmpl,*.gohtml,*.template :setfiletype html
    " autocmd BufNew,BufNewFile,BufRead *.html set ft=html.javascript
    " autocmd BufNew,BufNewFile,BufRead *.html.tmpl set ft=gohtmltmpl.html.javascript

augroup END
" }}}

" Fold, gets it's own section  ----------------------------------------------{{{
" credit to Mike Hartington https://github.com/mhartington/dotfiles/blob/master/vimrc
" https://github.com/mhartington/dotfiles/blob/linux/vimrc
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
  set foldlevel=99
  let g:xml_syntax_folding = 1
  " Space to toggle folds.
  "nnoremap <Space> za
  "vnoremap <Space> za
  " open folds when searched string in focus.
  set foldopen=search
" auto ----------------------------------------------------------------------{{{
    augroup auto_fold
        autocmd!
        " Don't screw up folds when inserting text that might affect them, until
        " leaving insert mode. Foldmethod is local to the window.
        " http://vim.wikia.com/wiki/Keep_folds_closed_while_inserting_text
        autocmd InsertEnter * if !exists('w:last_fdm') | let w:last_fdm=&foldmethod | setlocal foldmethod=manual | endif
        autocmd InsertLeave,WinLeave * if exists('w:last_fdm') | let &l:foldmethod=w:last_fdm | unlet w:last_fdm | endif
        autocmd FileType vim,tmux,sh,conf setlocal fdc=1
        autocmd FileType vim,tmux,sh,conf setlocal foldmethod=marker
        autocmd FileType vim,tmux,sh,conf setlocal foldlevel=0
        autocmd FileType javascript,html,css,scss,typescript setlocal foldlevel=99
        autocmd FileType css,scss,json setlocal foldmethod=marker
        autocmd FileType css,scss,json setlocal foldmarker={,}
        autocmd FileType coffee setl foldmethod=indent
        autocmd FileType xml,cpp,c setl foldmethod=syntax
        autocmd FileType html setl foldmethod=expr
        autocmd FileType html setl foldexpr=HTMLFolds()
        " autocmd FileType javascript,typescript,json setl foldmethod=syntax
        autocmd FileType javascript,typescript,typescriptreact,json setl foldmethod=syntax
    augroup END
" }}}
" }}}

" airline -------------------------------------------------------------------{{{
" enable vim-airline integration with plugins.
    let g:airline_enable_fugitive=1
    let g:airline_enable_syntastic=1
    let g:airline_enable_bufferline=1
    let g:webdevicons_enable_airline_statusline = 1
    " if !exists('g:airline_symbols')
    "   let g:airline_symbols = {}
    " endif

    let g:airline#extensions#tabline#enabled = 1
    let g:airline#extensions#mike#enabled = 0
    set hidden
    let g:airline#extensions#tabline#fnamemod = ':t'
    let g:airline#extensions#tabline#buffer_idx_mode = 1
  let g:airline_powerline_fonts = 0
  " let g:airline_symbols.branch = ''
  let g:airline_theme='oceanicnext'
  " cnoreabbrev <silent> <expr> x getcmdtype() == ":" && getcmdline() == 'x' ? 'Bdelete' : 'x'
  " cnoreabbrev x Sayonara
  nmap <leader>, :b#<CR>
  tmap <leader>, <C-\><C-n>:b#<cr>
  nmap <leader>. :bprevious<CR>
  tmap <leader>. <C-\><C-n>:bprevious<CR>
  tmap <leader>1  <C-\><C-n><Plug>AirlineSelectTab1
  tmap <leader>2  <C-\><C-n><Plug>AirlineSelectTab2
  tmap <leader>3  <C-\><C-n><Plug>AirlineSelectTab3
  tmap <leader>4  <C-\><C-n><Plug>AirlineSelectTab4
  tmap <leader>5  <C-\><C-n><Plug>AirlineSelectTab5
  tmap <leader>6  <C-\><C-n><Plug>AirlineSelectTab6
  tmap <leader>7  <C-\><C-n><Plug>AirlineSelectTab7
  tmap <leader>8  <C-\><C-n><Plug>AirlineSelectTab8
  tmap <leader>9  <C-\><C-n><Plug>AirlineSelectTab9
  nmap <leader>1 <Plug>AirlineSelectTab1
  nmap <leader>2 <Plug>AirlineSelectTab2
  nmap <leader>3 <Plug>AirlineSelectTab3
  nmap <leader>4 <Plug>AirlineSelectTab4
  nmap <leader>5 <Plug>AirlineSelectTab5
  nmap <leader>6 <Plug>AirlineSelectTab6
  nmap <leader>7 <Plug>AirlineSelectTab7
  nmap <leader>8 <Plug>AirlineSelectTab8
  nmap <leader>9 <Plug>AirlineSelectTab9
  " let g:airline#extensions#branch#format = 0
  " let g:airline_detect_spelllang=0
  " let g:airline_detect_spell=0
  " let g:airline#extensions#hunks#enabled = 0
  " let g:airline#extensions#wordcount#enabled = 0
  " let g:airline#extensions#whitespace#enabled = 0
  " let g:airline_section_c = '%f%m'
  " let g:airline_section_x = ''
  " " let g:airline_section_y = '%{WebDevIconsGetFileFormatSymbol()}'
  " let g:airline_section_y = ''
  " let g:webdevicons_enable_airline_statusline_fileformat_symbols = 0
  " let g:airline_section_z = '%l:%c'
  " " let g:airline_section_z = '%{LineNoIndicator()} :%2c'
  " let g:airline#parts#ffenc#skip_expected_string=''
  " " let g:line_no_indicator_chars = [' ', '▁', '▂', '▃', '▄', '▅', '▆', '▇', '█']
  " " let g:line_no_indicator_chars = ['⎺', '⎻', '⎼', '⎽', '_']
  " let g:airline_mode_map = {
        \ 'n' : '',
        \ 'i' : '',
        \ 'R' : '',
        \ 'v' : '',
        \ 'V' : '',
        \ 'c' : '',
        \ 's' : '',
        \ 'S' : '',
        \ ''  : '',
        \ 't' : '',
        \}
  let g:airline#extensions#tabline#buffer_idx_format = {
        \ '0': '0 ',
        \ '1': '1 ',
        \ '2': '2 ',
        \ '3': '3 ',
        \ '4': '4 ',
        \ '5': '5 ',
        \ '6': '6 ',
        \ '7': '7 ',
        \ '8': '8 ',
        \ '9': '9 ',
        \}
" }}}

" commentary --------------------------------------------------------------{{{
xmap <C-c>  <Plug>Commentary
nmap <C-c>  <Plug>Commentary
omap <C-c>  <Plug>Commentary
nmap <C-c><C-c> <Plug>CommentaryLine
nmap <C-c>u <Plug>Commentary<Plug>Commentary
" }}}

" custom plugins ----------------------------------------------------------{{{

" quote {{{2
imap <C-u> <Plug>(QuoteToTheLeft)
" }}}2

" capitalize {{{2
" this is my custom plugin, user discretion advised.
imap <C-e> <plug>(ToggleCharInsertMode)
nmap <C-e> <plug>(ToggleChar)
" imap <c-u> <plug>(ToggleLastNChars0)
imap <A-e> <plug>(ToggleLastNChars0)
nmap <A-e> <plug>(TogglePrevNChars)
" }}}2
"
" }}}

" ale ---------- ----------------------------------------------------------{{{
let g:ale_elixir_elixir_ls_release = '/home/puppy/.local/share/elixir-ls/rel'
set completeopt=menu,menuone,preview,noselect,noinsert
" let g:ale_completion_enabled = 1
let g:ale_elixir_elixir_ls_config = {
    \   'elixirLS': {
    \     'dialyzerEnabled': v:false,
    \   },
    \ }
augroup ale_elixir
  autocmd!
  autocmd FileType elixir let b:ale_warn_about_trailing_blank_lines = 0
  autocmd FileType elixir let b:ale_warn_about_trailing_whitespace = 0
  autocmd FileType elixir nnoremap <buffer> <c-]> :ALEGoToDefinition<cr>
  autocmd FileType elixir nnoremap <buffer> <c-]> :ALEGoToDefinition<cr>
  autocmd FileType elixir nnoremap <buffer> ]l :ALENext<cr>
  autocmd FileType elixir nnoremap <buffer> [l :ALEPrevious<cr>
  " nmap <silent> <leader>aj :ALENext<cr>
  " nmap <silent> <leader>ak :ALEPrevious<cr>
augroup END
" When set to `1`, only the linters from |g:ale_linters| and |b:ale_linters|
let g:ale_linters_explicit = 1
let g:ale_linters = {}
let g:ale_linters.elixir = ['credo']
" let g:ale_linters.elixir = ['elixir-ls', 'credo']

let g:ale_fixers = {'*': ['remove_trailing_lines', 'trim_whitespace']}
let g:ale_fixers.javascript = ['eslint']
let g:ale_fixers.scss = ['stylelint']
let g:ale_fixers.css = ['stylelint']
let g:ale_fixers.elixir = ['mix_format']

let g:ale_sign_column_always = 1
let g:ale_echo_cursor = 0
let g:ale_fix_on_save = 1
let g:ale_lint_on_enter = 1
let g:ale_lint_on_insert_leave = 1
let g:ale_lint_delay = 200  " msec
let g:ale_lint_on_text_changed = 0
let g:ale_open_list = 0

" nnoremap df :ALEFix<CR>
" }}}

" elixir ------------------------------------------------------------------{{{
" credit to vim-go function! go#alternate#Switch(bang, cmd) abort
" Test alternates between the implementation of code and the test code.
function! s:alternate_test(bang, cmd) abort
  let file = expand('%')
  if empty(file)
    " call go#util#EchoError("no buffer name")
    echohl ErrorMsg
    echomsg "No buffer name"
    echohl None
    return
  elseif file =~# '^\f\+_test\.exs$'
    let l:root = split(file, '_test.exs$')[0]
    let l:alt_file = substitute(l:root, 'test', 'lib', "") . ".ex"
  elseif file =~# '^\f\+\.ex$'
    let l:root = split(file, ".ex$")[0]
    let l:alt_file = substitute(l:root, 'lib', 'test', "") . "_test.exs"
  else
    echohl ErrorMsg
    echomsg "not an elixir file"
    echohl None
    return
  endif
  if !filereadable(alt_file) && !bufexists(alt_file) && !a:bang
    echohl ErrorMsg
    echomsg "couldn't find " . alt_file
    echohl None
    return
  elseif empty(a:cmd)
    " execute ":" . go#config#AlternateMode() . " " . alt_file
    execute ":" . "edit" . " " . alt_file
  else
    execute ":" . "edit" . " " . alt_file
    " execute ":" . a:cmd . " " . alt_file
  endif
endfunction
command! -bang ElAlternate call <SID>alternate_test(<bang>0, '')
" autocmd FileType elixir nmap <leader>r :<C-u>call <SID>build_go_files()<CR>
augroup elixir_cmds
  autocmd!
  " autocmd FileType elixir setlocal makeprg=mix\ credo\ suggest\ --format=flycheck
  " autocmd FileType elixir setlocal makeprg=mix\ compile
  " autocmd FileType elixir setlocal makeprg='mix'
  " autocmd FileType elixir setlocal makeef=/tmp/elixir_vim_compile_error.txt
  " autocmd FileType elixir setlocal shellpipe=2>
  " autocmd FileType elixir setlocal errorformat=%A%f:%l:\ %m,%C%m
  " autocmd FileType elixir setlocal errorformat=%f:%l:\ %t:\ %m
  " autocmd FileType elixir setlocal errorformat=%A%t%*[^:]:\ %m,%C%f:%l:\ %m,%C%f:%l,%Z
  " noremap <M-1> :w<CR>:set ch=5<CR>:make -d C:\\dev\\classes %:p<CR>
  " autocmd FileType elixir nmap <buffer> <leader>b :w<CR>:set ch=5<CR>:make %:p<CR>
  autocmd FileType elixir nmap <buffer> <leader>do :ExDoc<CR>
  autocmd FileType elixir nmap <buffer> <leader>dd :ExDef<CR>
  autocmd FileType elixir nmap <buffer> <leader>b :MixCompile<CR> :copen<CR>
  autocmd FileType elixir nmap <buffer> <leader>b :MixCompile<CR> :copen<CR>
  autocmd FileType elixir nmap <buffer> <leader>a :ElAlternate<CR>
    " autocmd FileType go nmap <leader>a :GoAlternate<CR>
  " autocmd BufWritePost *.ex,*.exs normal! :MixCompile<CR> :copen<CR>
  " autocmd FileType elixir nmap <buffer> <leader>sc :wall!<CR> :T clear<CR>
  autocmd FileType elixir nmap <buffer> <leader>sc :wall!<CR> :Texec iex clear<CR>
  autocmd FileType elixir tmap <a-c> <c-\><c-n>:T clear<CR>a
  " autocmd FileType elixir nmap <leader>r :wall!<CR> :terminal elixir %<CR>
  " autocmd FileType elixir nmap <leader>r :wall!<CR> :terminal iex %<CR>a
  autocmd FileType elixir nmap <buffer> <leader>r :wall!<CR>:let g:neoterm_autoinsert=1 <bar> T iex  %<CR>
  " autocmd FileType elixir nmap <leader>r :wall!<CR>:let g:neoterm_autoinsert=1<CR>:T iex  %<CR>
  autocmd FileType elixir nmap <buffer> <A-r> :wall!<CR> :terminal elixir %<CR>
  autocmd FileType elixir imap <buffer> <A-r> <C-c>:wall!<CR> :terminal elixir %<CR>
  " autocmd FileType elixir nmap <leader>r :<C-u>Texec elixir %"<CR>
  " autocmd FileType elixir nmap <leader>f :wall!<CR>:silent !mix format %<CR>
  autocmd FileType elixir nmap <buffer> <silent> <leader>sO :let g:neoterm_autoinsert=1 <BAR> Texec iex clear<CR>
" nnoremap <leader>sO :let g:neoterm_autoinsert=1 <bar> Topen<cr>
" nnoremap <leader>sV :let g:neoterm_autoinsert=1 <bar> vertical Topen<cr>
  " autocmd BufWritePost *.ex,*.exs :silent !mix format % :redraw!
  autocmd BufNewFile,BufRead *.ex,*.exs setlocal autowrite
augroup END
" }}}

" emmet-vim ---------------------------------------------------------------{{{
" Filters                |emmet-filters-list|
" Customize              |emmet-customize|
" http://docs.emmet.io/customization/snippets/
" http://docs.emmet.io/cheat-sheet/
" http://docs.emmet.io/filters/
" https://github.com/mattn/emmet-vim
" let g:user_emmet_mode='n'    "only enable normal mode functions.
let g:user_emmet_mode='a'    "enable all function in all mode.
let g:user_emmet_install_global = 0  " 1, Emmet will create global mappings.
let g:emmet_html5 = 1
let g:user_emmet_complete_tag = 1 " setlocal omnifunc=emmet#completeTag
let g:user_emmet_leader_key = '<C-t>'
augroup autogroup_emmet
  autocmd!
  autocmd FileType html,css,gohtmltmpl EmmetInstall
augroup END
let g:user_emmet_settings = {
\  'indentation': '  ',
\  'html': {
\    'default_attributes': {
\       'link': [{'rel': 'stylesheet'}, {'href': ''}],
\       'input': [{'type': ''}, {'name': ''}, {'value': ''}],
\       'textarea': [{'name': ''}, {'id': ''}, {'cols': '20'}, {'rows': '5'}],
\    },
\    'filters': 'html',
\    'indentation': '  ',
\    'indent_blockelement': 1,
\    'block_all_childless': 0,
\  },
\  'gohtmltmpl': {
\     'extends': 'html',
\  },
\}
" }}}

" neosnippet ----------------------------------------------------------------{{{
" Plugin key-mappings.
" Note: It must be "imap" and "smap".  It uses <Plug> mappings.
" imap <A-j>     <Plug>(neosnippet_expand)
imap <C-a>     <Plug>(neosnippet_expand_or_jump)
smap <C-a>     <Plug>(neosnippet_expand_or_jump)
" imap <A-t>     <Plug>(neosnippet_expand_target)
" imap <C-y>     <Plug>(neosnippet_expand_or_jump)
imap <A-a>     <Plug>(neosnippet_jump)
smap <A-a>     <Plug>(neosnippet_jump)
" xmap <A-j>     <Plug>(neosnippet_expand_target)
let g:neosnippet#snippets_directory='~/.local/share/nvim/snippets'
let g:neosnippet#enable_snipmate_compatibility = 1
" augroup neosnippet_augroup
"   autocmd!
"   autocmd InsertLeave * NeoSnippetClearMarkers
" augroup END
" }}}

" neoterm -------------------------------------------------------------------{{{
" let neoterm_default_mod = 'tab'
let neoterm_default_mod = 'split'
nnoremap <expr> <Plug>(CloseAllNeoterms) ':let g:neoterm_autoinsert=0 <bar> TcloseAll!<cr>'
nmap <leader>se <Plug>(CloseAllNeoterms)
" nmap <A-e> <Plug>(CloseAllNeoterms)
tnoremap <leader>se <C-\><C-n>:let g:neoterm_autoinsert=0 <bar> TcloseAll!<cr>
tnoremap <A-e> <C-\><C-n>:let g:neoterm_autoinsert=0 <bar> TcloseAll!<cr>
nnoremap <leader>so :Topen<cr>
nnoremap <leader>sv :vertical  Topen<cr>
nnoremap <leader>sO :let g:neoterm_autoinsert=1 <bar> Topen<cr>
nnoremap <A-o> :let g:neoterm_autoinsert=1 <bar> Topen<cr>
nnoremap <leader>sc :Tclear<cr>
" let neoterm_autoinsert = 1
" }}}

" denite --------------------------------------------------------------------{{{
let s:menus = {}
call denite#custom#option('_', {
    \ 'prompt': '❯',
    \ 'winheight': 10,
    \ 'updatetime': 1,
    \ 'auto_resize': 0,
    \ 'highlight_matched_char': 'Underlined',
    \ 'highlight_mode_normal': 'CursorLine',
    \ 'reversed': 1,
    \})
call denite#custom#option('TSDocumentSymbol', {
    \ 'prompt': ' @' ,
    \})
call denite#custom#option('TSWorkspaceSymbol', {
    \ 'prompt': ' #' ,
    \})
call denite#custom#source('file_rec', 'vars', {
    \'command': ['rg', '--files', '--glob', '!.git'],
    \'sorters':['sorter_sublime'],
    \'matchers': ['matches_cpsm']
    \})
"     \ 'command': ['ag', '--follow','--nogroup','--hidden', '--column', '-g', '', '--ignore', '.git', '--ignore', '*.png', '--ignore', 'node_modules'
call denite#custom#source('grep', 'vars', {
    \'command': ['rg'],
    \'default_opts': ['-i', '--vimgrep'],
    \'recursive_opts': [],
    \'pattern_opt': [],
    \'separator': ['--'],
    \'final_opts': [],
    \})
nnoremap <silent> <c-p> :Denite file_rec<CR>
nnoremap <silent> <leader>w :Denite help<CR>
" nnoremap <silent> <leader>c :Denite colorscheme<CR>
" nnoremap <silent> <leader>b :Denite buffer<CR>
" edge case: does not show search result, but indicates one match.
" example: search this buffer with 'leader>r'. There must be only one such
" line, remove the example line to replicate.
" nnoremap <silent> <leader>lb :Denite line:bufers:noempty<CR>
" " ':all:' means search from the top in the current buffer.
" nnoremap <silent> <leader>la :Denite line:all:noempty<CR>
" nnoremap <silent> <leader>a :Denite grep:::!<CR>
" nnoremap <silent> <leader>g :Denite grep:::`expand('<cword>')`<CR>
call denite#custom#map('insert','<C-n>','<denite:move_to_next_line>','noremap')
call denite#custom#map('insert','<C-p>','<denite:move_to_previous_line>','noremap')
call denite#custom#filter('matcher_ignore_globs', 'ignore_globs',
\ [ '.git/', '.ropeproject/', '__pycache__/',
\   'venv/', 'images/', '*.min.*', 'img/', 'fonts/'])
call denite#custom#var('menu', 'menus', s:menus)
let g:ctrlp_user_command = 'ag %s -l --nocolor -g ""'
" --column: Show column number
" --line-number: Show line number
" --no-heading: Do not show file headings in results
" --fixed-strings: Search term as a literal string
" --ignore-case: Case insensitive search
" --no-ignore: Do not respect .gitignore, etc...
" --hidden: Search hidden files and folders
" --follow: Follow symlinks
" --glob: Additional conditions for search (in this case ignore everything in the .git/ folder)
" --color: Search color options
" :Find someword, will show files containing the word
command! -bang -nargs=* Find call fzf#vim#grep('rg --column --line-number --no-heading --fixed-strings --ignore-case --no-ignore --hidden --follow --glob "!.git/*" --color "always" '.shellescape(<q-args>), 1, <bang>0)
"}}}

" deoplete ------------------------------------------------------------------{{{
" <TAB>: completion.
" inoremap <expr><TAB>  pumvisible() ? "\<C-n>" : "\<TAB>"
let g:deoplete#enable_at_startup = 1
" let g:deoplete#enable_debug = 1
" call deoplete#enable_logging('DEBUG', 'deoplete.log')
" call deoplete#custom#source('go', 'is_debug_enabled', 1)
  " Pass a dictionary to set multiple options
  call deoplete#custom#option({
  \ 'auto_complete_delay': 20,
  \ 'smart_case': v:false,
  \ })
<
"}}}

" fzf ---------------------------------------------------------------------{{{1
" Augmenting Ag command using fzf#vim#with_preview function
"   * fzf#vim#with_preview([[options], preview window, [toggle keys...]])
"     * For syntax-highlighting, Ruby and any of the following tools are required:
"       - Highlight: http://www.andre-simon.de/doku/highlight/en/highlight.php
"       - CodeRay: http://coderay.rubychan.de/
"       - Rouge: https://github.com/jneen/rouge
"
"   :Ag  - Start fzf with hidden preview window that can be enabled with "?" key
"   :Ag! - Start fzf in fullscreen and display the preview window above
command! -bang -nargs=* Ag
  \ call fzf#vim#ag(<q-args>,
  \                 <bang>0 ? fzf#vim#with_preview('up:60%')
  \                         : fzf#vim#with_preview('right:50%:hidden', '?'),
  \                 <bang>0)

" Similarly, we can apply it to fzf#vim#grep. To use ripgrep instead of ag:
command! -bang -nargs=* Rg
  \ call fzf#vim#grep(
  \   'rg --column --line-number --no-heading --color=always --smart-case '.shellescape(<q-args>), 1,
  \   <bang>0 ? fzf#vim#with_preview('up:60%')
  \           : fzf#vim#with_preview('right:50%:hidden', '?'),
  \   <bang>0)
command! -bang -nargs=? -complete=dir FzfFiles
  \ call fzf#vim#files(<q-args>, fzf#vim#with_preview(), <bang>0)
" command! -bang -nargs=0 FzfBuffers
"   \ call fzf#vim#buffers(<q-args>, fzf#vim#with_preview('up:60%'), <bang>0)
let g:fzf_command_prefix = 'Fzf'
nnoremap <leader>ff :FzfFiles<cr>
nnoremap <leader>fg :FzfGFiles<cr>
nnoremap <leader>fb :FzfBuffers!<cr>
nnoremap <leader>f: :FzfHistory:<cr>
nnoremap <leader>f/ :FzfHistory/<cr>
nnoremap <leader>fl :FzfBLines<CR>
" nnoremap <leader>fL :FzfLines<CR>
nnoremap <leader>fL :execute ':Ag ' . expand('<cword>')<cr>
nnoremap <leader>fc :FzfBCommits!<CR>
nnoremap <leader>fC :FzfCommits<CR>
" <bang> use as per the above cmd modification
nnoremap <leader>fs :FzfRg!<CR>
nnoremap <leader>fh :FzfHelp<CR>
nnoremap <leader>fm :FzfCommands<cr>
nnoremap <leader>ft :FzfBTags<cr>
" }}}

" gitgutter ---------------------------------------------------------------{{{1
" keep the signcolumn always on.
  set signcolumn=yes
" }}}

" mta MatchTagAlways ------------------------------------------------------{{{
  let g:mta_filetypes = {
      \ 'html' : 1,
      \ 'xhtml' : 1,
      \ 'xml' : 1,
      \ 'jinja' : 1,
      \ 'gohtmltmpl' : 1,
      \}
" let g:mta_use_matchparen_group = 1 " use the plugins colors (lightgrey)
" highlight MatchTag ctermfg=black ctermbg=lightgreen guifg=black guibg=lightgreen
let g:mta_use_matchparen_group = 0 " use colorscheme defined colors
" }}}

" merginal ----------------------------------------------------------------{{{1
map <silent> <leader>m <esc>:Merginal<CR>
tnoremap <leader>m <c-\><c-n>:Merginal<CR>
" }}}

" syntastic ---------------------------------------------------------------{{{
" On by default, turn it off for html
let g:syntastic_mode_map = { 'mode': 'active',
    \ 'active_filetypes': ['python', 'javascript', 'css', 'html'],
    \ 'passive_filetypes': ['go'] }
    " " \ 'active_filetypes': ['go', 'python', 'javascript', 'css'],
    " \ 'passive_filetypes': ['html'] }
" let g:syntastic_go_checkers = ['golint', 'govet']
" let g:syntastic_mode_map = { 'mode': 'active', 'passive_filetypes': ['go'] }
" Use flake8
" !!! Install 'sudo pip flake8' to enable python checking
"let g:syntastic_python_checkers = ['pep8']
let g:syntastic_python_checkers = ['flake8']
"let g:syntastic_python_checkers = ['pyflakes']
"let g:syntastic_python_checkers = ['pylint']
"let g:syntastic_python_flake8_args = '--ignore="E501,E302,E261,E701,E241,E126,E127,E128,W801"'
" E256 - block comment should start with "# " - Incorrectly interpreted on
" raspbian
" [E402] module level import not at top of file [E402]
let g:syntastic_python_flake8_args = '--ignore="E501,W601,E265,E402"'
let g:syntastic_always_populate_loc_list=1
let g:syntastic_auto_loc_list=1
" }}}

" splitjoin ---------------------------------------------------------------{{{
augroup splitjoin_set_callbacks
  autocmd!
  autocmd FileType gohtmltmpl let b:splitjoin_split_callbacks = [
            \ 'sj#html#SplitTags',
            \ 'sj#html#SplitAttributes'
            \ ]
  autocmd FileType gohtmltmpl let b:splitjoin_join_callbacks = [
            \ 'sj#html#JoinAttributes',
            \ 'sj#html#JoinTags'
            \ ]
augroup END
" }}}

" vim-fugitive --------------------------------------------------------------{{{
" set diffopt+=vertical
"nnoremap <space>ga :Git add %:p<CR><CR>
nnoremap <space>gs :Gstatus<CR>
nnoremap <space>gc :Gcommit -v -q<CR>
nnoremap <space>gt :Gcommit -v -q %:p<CR>
nnoremap <space>gd :Gdiff<CR>
"nnoremap <space>ge :Gedit<CR>
nnoremap <space>gr :Gread<CR>
nnoremap <space>gw :Gwrite<CR><CR>
"nnoremap <space>gl :silent! Glog<CR>:bot copen<CR>
"nnoremap <space>gp :Ggrep<Space>
"nnoremap <space>gm :Gmove<Space>
"nnoremap <space>gb :Git branch<Space>
"nnoremap <space>go :Git checkout<Space>
"nnoremap <space>gps :Dispatch! git push<CR>
"nnoremap <space>gpl :Dispatch! git pull<CR>

augroup auto_fugitive
    autocmd!
    "autocmd! BufWritePost * :Gstatus " does not work
    " Auto-clean fugitive bufferes.
    autocmd BufReadPost fugitive://* set bufhidden=delete
    " next maping doesn't work
    "autocmd User fugitive if fugitive#buffer().type() =~# '^\%(tree\|blob\)$' | nnoremap <buffer> .. :edit %:h<CR> | endif
augroup END

" }}}

" vim-go --------------------------------------------------------------------{{{

" function! s:build_go_files() --------------------------------------------{{{2
" run :GoBuild or :GoTestCompile based on the go file
function! s:build_go_files()
  let l:file = expand('%')
  if l:file =~# '^\f\+_test\.go$'
    call go#test#Test(0, 1)
  elseif l:file =~# '^\f\+\.go$'
    call go#cmd#Build(0)
  endif
endfunction
" }}}2

" go mappings -------------------------------------------------------------{{{2
augroup auto_vim-go
    autocmd!
    " https://github.com/fatih/vim-go/issues/502
    autocmd BufWritePost *.go normal! zv
    autocmd FileType go setlocal foldmethod=syntax
    " :help go_echo_go_info for info on noshowmode
    autocmd FileType go setlocal noshowmode
    autocmd FileType go setlocal foldlevel=99 foldnestmax=1
    " autocmd FileType go nmap <leader>b  <Plug>(go-build)
    autocmd FileType go nmap <leader>b :<C-u>call <SID>build_go_files()<CR>
    autocmd FileType go imap <A-b> <Esc>:<C-u>call <SID>build_go_files()<CR>a
    autocmd FileType go nmap <buffer> <leader>r <Plug>(go-run)
    autocmd FileType go nmap <leader>a :GoAlternate<CR>
    " autocmd FileType go nmap <leader>T :GoTestFunc -v -race<CR>
    autocmd FileType go nmap <leader>T :GoTestFunc <CR>
    " autocmd FileType go nmap <leader>T :GoTest -v -race<CR>
    autocmd FileType go nmap <leader>tt :GoTest<CR>
    autocmd FileType go nmap <leader>c <Plug>(go-coverage-toggle)
    " GoFiles, GoDeps
    " use default 'gd' '<c-]>' mappings for :GoDef
    " autocmd FileType go nmap <leader>df :GoDef<CR>
    autocmd FileType go nmap <leader>de :GoDecls<CR>
    autocmd FileType go nmap <leader>dr :GoDeclsDir<CR>
    autocmd FileType go nmap <leader>do :GoDoc<CR>
    autocmd FileType go nmap <leader>di :GoInfo<CR>
    " Guru commands:
    autocmd FileType go nmap <leader>gr :GoReferrers<CR>
    autocmd FileType go nmap <leader>gh :GoChannelPeers<CR>
    autocmd FileType go nmap <leader>gc :GoCallers<CR>
    autocmd FileType go nmap <leader>ge :GoWhicherrs<CR>
    autocmd FileType go nmap <leader>gd :GoDescribe<CR>
    autocmd FileType go nmap <leader>gi :GoImplements<CR>
    autocmd FileType go nmap <leader>gs :GoCallstack<CR>
    autocmd FileType go vmap <leader>gf :GoFreevars<CR>
    " au FileType go nmap RT (go-run-tab)
    autocmd FileType go nmap <leader>if :GoFmt<CR>
    " https://stackoverflow.com/questions/50678503/is-it-possible-to-ignore-specific-warnings-with-visual-studio-codes-linter
    autocmd FileType go nmap <leader>il :GoMetaLinter --exclude=exported\s(var\|const\|function\|method\|type)\s[\w.]+\sshould\shave\scomment\sor\sbe\sunexported<CR>
    autocmd FileType go nmap <leader>ir :GoRename<CR>
    autocmd FileType go nmap <leader>ii :GoImport<space>
    autocmd FileType go nmap <leader>ia :GoImportAs
    " autocmd FileType go map f :GoFillStruct
    " autocmd FileType go map at :GoAddTags
    autocmd FileType go nmap <leader>id :GoSameIds<CR>
    " Build/Test on save.
    " autocmd BufWritePost *.go :GoBuild
    autocmd BufWritePost *.go :GoMetaLinter --exclude=exported\s(var|const|function|method|type)\s[\w.]+\sshould\shave\scomment\sor\sbe\sunexported
    " autocmd BufWritePost *_test.go :GoTest
    autocmd BufNewFile,BufRead *.go setlocal autowrite
    " Toggle alternate files, code and test files.
    autocmd Filetype go command! -bang A call go#alternate#Switch(<bang>0, 'edit')
    autocmd Filetype go command! -bang AS call go#alternate#Switch(<bang>0, 'split')
    autocmd Filetype go command! -bang AV call go#alternate#Switch(<bang>0, 'vsplit')
augroup END
augroup my_go_plugin_mappings
    autocmd!
    autocmd FileType go imap <a-i> <plug>(MyGoImport)
    " autocmd FileType go nmap <a-i> <plug>(MyGoImport)
    autocmd FileType go nmap <leader>im <plug>(StartDocLine)
augroup END
" }}}

" settings ----------------------------------------------------------------{{{2
" default is 'guru'
let g:go_def_mode = 'godef'
" let g:go_list_type = 'quickfix'
" let g:go_list_type_commands = {'GoMetaLinter': 'quickfix', 'GoTest': 'quickfix'}
" let g:go_list_type_commands = {'GoMetaLinterAutoSave': 'quickfix', 'GoTest': 'quickfix'}
" let g:go_list_type_commands = {'GoTest': 'locationlist'}
" let g:go_list_type_commands = {'GoBuild': 'locationlist'}
let g:go_list_type_commands = {'GoFmt': 'quickfix'}
" let g:go_list_type_commands = {'GoMetaLinter': 'quickfix'}
" let g:go_list_type_commands = {'GoMetaLinter': 'locationlist'}
let g:go_autodetect_gopath = 0
" let g:go_info_mode = 'gocode' " default value
let g:go_info_mode = 'guru'
" formats go code and manages imports.
let g:go_fmt_command = 'goimports'
" formats go on save.
let g:go_fmt_autosave = 1
" let g:go_snippet_engine = "automatic"
let g:go_snippet_engine = "neosnippet"
" Specifies the window height for the quickfix and location list windows.
let g:go_list_height = 15
" Use this option to jump to an existing buffer for the split, vsplit and tab
" mappings of |:GoDef|. By default it's disabled. >
" let g:go_def_reuse_buffer = 0
" stop folding on 'write' as per https://github.com/fatih/vim-go/issues/502
let g:go_fmt_experimental = 1
" you might not want all the highlighting.
let g:go_highlight_types = 1
let g:go_highlight_fields = 1
let g:go_highlight_functions = 1
let g:go_highlight_function_calls = 0
let g:go_highlight_methods = 1
let g:go_highlight_operators = 1
let g:go_highlight_extra_types = 0
let g:go_highlight_build_constraints = 1
" let g:go_metalinter_enabled = ['vet', 'golint', 'errcheck']
let g:go_metalinter_enabled = ['vet', 'golint']
let g:go_metalinter_autosave = 0
let g:go_metalinter_autosave_enabled = ['vet', 'golint']
" let g:go_metalinter_autosave_enabled = ['vet']
let g:go_metalinter_deadline = '10s'
let g:ale_go_gometalinter_options =
      \ '--tests ' .
      \ '--fast ' .
      \ '--disable=gotype ' .
      \ '--disable=gotypex ' .
      \ '--exclude="should have comment" ' .
      \ '--exclude="error return value not checked \(defer"'
let g:go_auto_type_info = 0 " shows signature of fn under cursor
" let g:go_auto_sameids = 1 " too slow
let g:go_gocode_unimported_packages = 1
let g:go_term_enabled = 0
" let g:go_term_mode = 'vsplit'
let g:go_term_mode = 'split'
let g:go_term_height = 10
let g:go_term_width = 50
" Specifies whether `gocode` should use source files instead of binary packages
" It is currently much slower for source files.
" let g:go_gocode_propose_source = 1
" let g:go_guru_scope = ['github.com/...', expand("%:p:h")] " too slow
" let g:go_guru_scope = [expand("%:p:h")]  " does not work
" Echoes information about various Go commands, such as `:GoBuild`, `:GoTest`,
" `:GoCoverage`, etc... Useful to disable if you use the statusline integration,
" i.e: |go#statusline#Show()|. By default it's enabled
" let g:go_echo_command_info = 1
let g:go_fold_enable = ['block', 'import', 'varconst', 'package_comment', 'comment']
" }}}

" }}}

" vim-polyglot ------------------------------------------------------------{{{
let g:polyglot_disabled = ['go']
" }}}

" vim-sneak -----------------------------------------------------------------{{{
" press 's'|'f'|'t' again to move to the next match.
let g:sneak#s_next = 1
" 1 : Case sensitivity is determined by 'ignorecase' and 'smartcase'.
let g:sneak#use_ic_scs = 1
map f <Plug>Sneak_f
map F <Plug>Sneak_F
map t <Plug>Sneak_t
map T <Plug>Sneak_T
" }}}

" vim-test ----------------------------------------------------------------{{{
" make test commands execute using neoterm.
" https://github.com/janko-m/vim-test#strategies
let test#strategy = {
  \ 'nearest': 'neoterm',
  \ 'file':    'neoterm',
  \ 'suite':   'neoterm',
\}
" https://github.com/janko-m/vim-test#configuring
let test#go#gotest#options = {
  \ 'nearest': '-v',
  \ 'file':    '-failfast',
\}
  " \ 'suite':   '--tag ~slow',
" let test#go#runner = 'ginkgo'
" Runners available are 'gotest', 'ginkgo'
" https://github.com/janko-m/vim-test#setup
nmap <silent> <leader>tn :TestNearest<CR>
nmap <silent> <leader>tf :TestFile<CR>
nmap <silent> <leader>ts :TestSuite<CR>
nmap <silent> <leader>tl :TestLast<CR>
" go back to the tests file.
nmap <silent> <leader>tv :TestVisit<CR>
nmap <silent> <leader>te <Plug>(CloseAllNeoterms)
augroup go_test_mappings
    autocmd!
    autocmd FileType go nmap <leader>tn :TestNearest -count=1<CR>
    autocmd FileType go nmap <leader>tr :TestNearest -race -count=1<CR>
augroup END
" }}}

" vim-indentwise ----------------------------------------------------------{{{1
" 'matchit' plugin messes ]%, [% mappings for vim-indentwise. Disable it.
" let loaded_matchit = 1
map ]<bar> <Plug>(IndentWiseBlockScopeBoundaryEnd)
sunmap ]<bar>
map [<bar> <Plug>(IndentWiseBlockScopeBoundaryBegin)
sunmap [<bar>
" 1}}}

" Go ------------------------------------------------------------------------{{{
augroup auto_go
    autocmd!
    autocmd BufNewFile,BufRead *.go setlocal expandtab tabstop=4 shiftwidth=4
augroup END
" should't this be in deoplete section?
let g:deoplete#sources#go#gocode_binary = $GOPATH.'/bin/gocode'
let g:deoplete#sources#go#sort_class = ['package', 'func', 'type', 'var', 'const']
let g:deoplete#sources#go#pointer = 1
" incredibly slow
" let g:deoplete#sources#go#source_importer = 1
"}}}

" terminal ----------------------------------------------------------------{{{
highlight TermCursor ctermfg=red guifg=red
highlight TermCursorNC ctermfg=blue guifg=blue
set splitbelow
set splitright
tnoremap <Leader><ESC> <C-\><C-n>
tnoremap <leader>k <c-\><c-n>:bp! <BAR> bd! #<CR>
tnoremap <PageUp> <C-\><C-n>:bprevious<cr>
tnoremap <PageDown> <C-\><C-n>:bnext<cr>
tnoremap <Home> <C-\><C-n>:bfirst<cr>
tnoremap <End> <C-\><C-n>:blast<cr>

" Disable netrw gx mapping.
let g:netrw_nogx = get(g:, 'netrw_nogx', 1)
nmap gx <Plug>(openbrowser-open)
vmap gx <Plug>(openbrowser-open)

augroup auto_term " {{{
  autocmd!
  autocmd TermOpen * nnoremap <buffer> <leader>k :bp! <BAR> bd! #<CR>
  " does not work
  " autocmd TermOpen <buffer> * :startinsert
  " au TermOpen * let g:last_terminal_job_id = b:terminal_job_id
augroup END " }}}
function! s:get_visual_selection() " {{{
    " credit: https://stackoverflow.com/a/6271254
    " Why is this not a built-in Vim script function?!
    let [line_start, column_start] = getpos("'<")[1:2]
    let [line_end, column_end] = getpos("'>")[1:2]
    let lines = getline(line_start, line_end)
    if len(lines) == 0
        return ''
    endif
    let lines[-1] = lines[-1][: column_end - (&selection == 'inclusive' ? 1 : 2)]
    let lines[0] = lines[0][column_start - 1:]
    for i in range(len(lines))
      let lines[i] = substitute(lines[i], '\\$', "", "")
    endfor
    return join(lines, "; ")
    " return join(lines, "\n")
    " return lines
endfunction
" }}}
function! TermSend(lines, mods) " {{{
  " credit: https://vi.stackexchange.com/a/3390
  " call jobsend(g:last_terminal_job_id, add(a:lines, ''))
  " execute "normal! :a:mods T " .  a:lines . "\<cr>"
  " execute "normal! " . a:mods . " T " .  a:lines . "\<cr>"
  execute a:mods . " T " .  a:lines
endfunction " }}}
command! TermSendLine call TermSend(substitute(getline('.'), '\\$', "", ""), <q-mods>)
command! TermSendVisLine call TermSend(<sid>get_visual_selection())
nnoremap <silent> <leader>sr :belowright TermSendLine<cr>
nnoremap <silent> <leader>sR :vertical TermSendLine<cr>
vnoremap <silent> <leader>sr <esc>:TermSendVisLine<cr>
" To simulate |i_CTRL-R| in terminal-mode: >
" <C-R> clashes with zsh fzf shortcut to search history.
" tnoremap <expr> <C-R> '<C-\><C-N>"'.nr2char(getchar()).'pi'
tnoremap <expr> <A-R> '<C-\><C-N>"'.nr2char(getchar()).'pi'

" Quickly create a new terminal in a new tab
" tnoremap <Leader>c <C-\><C-n>:tab new<CR>:term<CR>
" noremap <Leader>c :tab new<CR>:term<CR>
" inoremap <Leader>c <Esc>:tab new<CR>:term<CR>

" " Quickly create a new terminal in a vertical split
" tnoremap <Leader>% <C-\><C-n>:vsp<CR><C-w><C-w>:term<CR>
" noremap <Leader>% :vsp<CR><C-w><C-w>:term<CR>
" inoremap <Leader>% <Esc>:vsp<CR><C-w><C-w>:term<CR>

" " Quickly create a new terminal in a horizontal split
"
" tnoremap <Leader>" <C-\><C-n>:sp<CR><C-w><C-w>:term<CR>
" noremap <Leader>" :sp<CR><C-w><C-w>:term<CR>
" inoremap <Leader>" <Esc>:sp<CR><C-w><C-w>:term<CR>
"

" abbrev --------------------------------------------------------------------{{{
" common typos.
iabbrev adn and
iabbrev waht what
iabbrev tehn then
" }}}

" html ----------------------------------------------------------------------{{{
" Do not wrap text in html files.
augroup auto_html
    autocmd!
    autocmd BufNewFile,BufRead *.html,*.tmpl setlocal nowrap
augroup END
" }}}
