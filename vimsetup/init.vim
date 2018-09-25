set nocompatible              " be iMproved, required
filetype off                  " required

" Plugins -----------------------------------------------------------------{{{1
" :PlugInstall, :PlugUpdate, :PlugClean,
" :PlugUpgrade (plug itself), :PlugStatus
call plug#begin('~/.local/share/nvim/plugged')
" Plug 'tmhedberg/SimpylFold'
Plug 'jeetsukumaran/vim-indentwise'
Plug 'vim-scripts/ReplaceWithRegister'
Plug 'michaeljsmith/vim-indent-object'
Plug 'AndrewRadev/splitjoin.vim'
Plug 'justinmk/vim-sneak'

" go {{{2
Plug 'fatih/vim-go', { 'do': ':GoUpdateBinaries', 'for': 'go'}
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
" Plug 'mdempsky/gocode', { 'rtp': 'nvim', 'do': '~/.local/share/nvim/plugged/gocode/nvim/symlink.sh' }
" 2}}}

Plug 'vim-airline/vim-airline'

" git {{{2
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-rhubarb'
Plug 'airblade/vim-gitgutter'
" 2}}}

Plug 'tpope/vim-unimpaired'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-dispatch'
Plug 'mhartington/oceanic-next'

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

Plug 'kassio/neoterm'
Plug 'janko-m/vim-test'
" provides insert mode auto-completion for quotes, parens, brackets, etc.
" Plug 'Raimondi/delimitMate'
" display the indention levels with thin vertical lines
Plug 'Yggdroot/indentLine'
Plug 'Valloric/MatchTagAlways'
" Initialize plugin system
call plug#end()
" 1}}}

" System Settings  ----------------------------------------------------------{{{
" Don't use TABs but spaces
filetype plugin indent on
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
" Open 'help' in a new buffer of the same window.
command! -nargs=1 -complete=help H :enew | :set buftype=help | :h <args>

" auto ----------------------------------------------------------------------{{{
augroup auto_system
    autocmd!
    " Remember cursor position between vim sessions
    autocmd BufReadPost *
             \ if line("'\"") > 0 && line ("'\"") <= line("$") |
             \   exe "normal! g'\"" |
             \ endif
             " center buffer around cursor when opening files
    " Show whitespace
    " MUST be inserted BEFORE the colorscheme command
    autocmd ColorScheme * highlight ExtraWhitespace ctermbg=red guibg=red
    autocmd InsertLeave * match ExtraWhitespace /\s\+$/
    " leave a file mark when you leave the buffer.
    " Get to where you left it with '|` followed by 'C|H|J|G|...'
    autocmd BufLeave *.css  normal! mC
    autocmd BufLeave *.html normal! mH
    autocmd BufLeave *.tmpl normal! mH
    autocmd BufLeave *.js   normal! mJ
    autocmd BufLeave *.go   normal! mG
    autocmd Filetype neosnippet setlocal tabstop=2
    autocmd Filetype neosnippet setlocal shiftwidth=2
    autocmd Filetype vim setlocal tabstop=2
    autocmd Filetype vim setlocal shiftwidth=2
    " Wrap lines in quickfix
    autocmd FileType qf setlocal wrap
augroup END
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
colorscheme OceanicNext
set completeopt-=preview " do not open preview window for completion.
" }}}

" System mappings  ----------------------------------------------------------{{{
" Rebind <leader> key
" I like to have it here becuase it is easier to reach than the default and
" it is next to ``m`` and ``n`` which I use for navigating between tabs.
let mapleader = ","
" shadowing '/' for search slows it down.
" let maplocalleader = "/"

" Move the line down.
noremap <leader>- yyddp
" Move the line up.
noremap <leader>_ yydd2kp
" Uppercase the word.
inoremap <c-u> <esc>gUiwea
nnoremap <c-u> gUiwe
" Open vimrc/vim.init file.
" nnoremap <leader>v :vsplit $MYVIMRC<cr>
" nnoremap <localleader>v :vsplit $MYVIMRC<cr>
nnoremap <leader>v :edit $MYVIMRC<cr>
" Source vimrc/vim.init file.
nnoremap <leader>V :source $MYVIMRC<cr>
" Quit current window
noremap <leader>e :quit<CR>
noremap <leader>E :qa!<CR>
" delete buffer
" nnoremap <leader>x :bdelete<CR>
nnoremap <leader>x :bdelete<CR>
" easier moving between tabs
" map <leader>n <esc>:tabprevious<CR>
" map <leader>m <esc>:tabnext<CR>
nnoremap <PageUp>   :bprevious<CR>
nnoremap <PageDown> :bnext<CR>
nnoremap <Home>   :bfirst<CR>
nnoremap <End> :blast<CR>
" Quicksave command
noremap <C-Z> :update<CR>
vnoremap <C-Z> <C-C>:update<CR>
" this  would save and go back to insert mode.
" inoremap <C-Z> <C-O>:update<CR>
" Added by sam@lf
" this  would save and exit insert mode.
inoremap <C-Z> <C-C>:update<CR>
" open buffer list and wait for # input
nnoremap <leader>f :ls<CR>:b<Space>
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
" bind Ctrl+<movement> keys to move around the windows, instead of using Ctrl+w + <movement>
" Every unnecessary keystroke that can be saved is good for your health :)
map <C-j> <C-w>j
map <C-k> <C-w>k
map <C-l> <C-w>l
map <C-h> <C-w>h
" FILE FINDING
" Search down into subdirrectories
" Provides tab-comletion for all file-relates tasks
set path+=**
" Display all matching files when we tab complete
set wildmenu
set wildmode=list:full
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
nnoremap <leader>q :cclose<CR>
" Add 'relative number'k and 'relative number'j to jump list
nnoremap <expr> k (v:count > 1 ? "m'" . v:count : '') .'gk'
nnoremap <expr> j (v:count > 1 ? "m'" . v:count : '') .'gj'
" }}}

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
  set foldlevel=99
  let g:xml_syntax_folding = 1
  " Space to toggle folds.
  "nnoremap <Space> za
  "vnoremap <Space> za
" auto ----------------------------------------------------------------------{{{
    augroup auto_fold
        autocmd!
        autocmd InsertEnter * if !exists('w:last_fdm') | let w:last_fdm=&foldmethod | setlocal foldmethod=manual | endif
        autocmd InsertLeave,WinLeave * if exists('w:last_fdm') | let &l:foldmethod=w:last_fdm | unlet w:last_fdm | endif
        autocmd FileType vim setlocal fdc=1
        autocmd FileType vim setlocal foldmethod=marker
        autocmd FileType vim setlocal foldlevel=0
        autocmd FileType javascript,html,css,scss,typescript setlocal foldlevel=99
        autocmd FileType css,scss,json setlocal foldmethod=marker
        autocmd FileType css,scss,json setlocal foldmarker={,}
        autocmd FileType coffee setl foldmethod=indent
        autocmd FileType xml setl foldmethod=syntax
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
  nmap <leader>, :bnext<CR>
  tmap <leader>, <C-\><C-n>:bnext<cr>
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

" neosnippet ----------------------------------------------------------------{{{
" Plugin key-mappings.
" Note: It must be "imap" and "smap".  It uses <Plug> mappings.
imap <C-j>     <Plug>(neosnippet_expand)
imap <C-b>     <Plug>(neosnippet_jump)
" smap <C-j>     <Plug>(neosnippet_expand_or_jump)
smap <C-b>     <Plug>(neosnippet_jump)
xmap <C-j>     <Plug>(neosnippet_expand_target)
let g:neosnippet#snippets_directory='~/.local/share/nvim/snippets'
" }}}

" neoterm -------------------------------------------------------------------{{{
let neoterm_default_mod = 'tab'
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
nnoremap <silent> <leader>b :Denite buffer<CR>
nnoremap <silent> <leader>lb :Denite line:buffers:noempty<CR>
" ':all:' means search from the top in the current buffer.
nnoremap <silent> <leader>la :Denite line:all:noempty<CR>
nnoremap <silent> <leader>a :Denite grep:::!<CR>
nnoremap <silent> <leader>g :Denite grep:::`expand('<cword>')`<CR>
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
inoremap <expr><TAB>  pumvisible() ? "\<C-n>" : "\<TAB>"
let g:deoplete#enable_at_startup = 1
" let g:deoplete#enable_debug = 1
" call deoplete#enable_logging('DEBUG', 'deoplete.log')
" call deoplete#custom#source('go', 'is_debug_enabled', 1)
"}}}

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
" run :GoBuild or :GoTestCompile based on the go file
function! s:build_go_files()
  let l:file = expand('%')
  if l:file =~# '^\f\+_test\.go$'
    call go#test#Test(0, 1)
  elseif l:file =~# '^\f\+\.go$'
    call go#cmd#Build(0)
  endif
endfunction
" auto ----------------------------------------------------------------------{{{
augroup auto_vim-go
    autocmd!
    " autocmd FileType go nmap <leader>b  <Plug>(go-build)
    autocmd FileType go nmap <leader>b :<C-u>call <SID>build_go_files()<CR>
    autocmd FileType go nmap <leader>r  <Plug>(go-run)
    autocmd FileType go nmap <leader>t  <Plug>(go-test)
    autocmd FileType go nmap <leader>c <Plug>(go-coverage-toggle)
    autocmd FileType go nmap <leader>df :GoDef<CR>
    autocmd FileType go nmap <leader>de :GoDecls<CR>
    autocmd FileType go nmap <leader>dr :GoDeclsDir<CR>
    autocmd FileType go nmap <leader>do :GoDoc<CR>
    autocmd FileType go nmap <leader>di <Plug>(go-info)
    autocmd FileType go nmap <leader>ds :GoDescribe<CR>
    " au FileType go nmap RT (go-run-tab)
    " autocmd FileType go map l :GoMetaLinter
    " autocmd FileType go nmap R :GoRename
    " autocmd FileType go map o :GoDecls
    " autocmd FileType go map O :GoDeclsDir
    " autocmd FileType go map d :GoDoc
    " autocmd FileType go map I :GoInfo
    autocmd FileType go nmap <leader>ii :GoImport
    autocmd FileType go nmap <leader>ia :GoImportAs
    " autocmd FileType go map f :GoFillStruct
    " autocmd FileType go map t :GoTestFunc -v -race
    " autocmd FileType go map T :GoTest -v -race
    " autocmd FileType go map c :GoTestCompile
    " autocmd FileType go map at :GoAddTags
    " Other Guru commands:GoReferrers, GoImplements, GoWhichErr, GoChannelPeers
    " GoFreeVars
    autocmd FileType go nmap <leader>id :GoSameIds<CR>
    " Build/Test on save.
    autocmd BufWritePost *.go :GoBuild
    " autocmd BufWritePost *_test.go :GoTest
    " autocmd BufWritePost *.go :GoTest
    autocmd BufNewFile,BufRead *.go setlocal autowrite
    " Toggle alternate files, code and test files.
    autocmd Filetype go command! -bang A call go#alternate#Switch(<bang>0, 'edit')
    " autocmd Filetype go command! -bang AV call go#alternate#Switch(<bang>0, 'vsplit')
    " autocmd Filetype go command! -bang AS call go#alternate#Switch(<bang>0, 'split')
    " autocmd Filetype go command! -bang AT call go#alternate#Switch(<bang>0, 'tabe')
    " Install plugins.
augroup END
" }}}
" settings ------------------------------------------------------------------{{{
" let g:go_list_type = "quickfix"
let g:go_list_type_commands = {"GoMetaLinter": "quickfix", "GoTest": "quickfix"}
" let g:go_list_type_commands = {"GoMetaLinter": "locationlist"}
let g:go_autodetect_gopath = 0
let g:go_info_mode = "gocode"
" formats go code and manages imports.
let g:go_fmt_command = "goimports"
" stop folding on 'write' as per https://github.com/fatih/vim-go/issues/502
let g:go_fmt_experimental = 1
" you might not want all the highlighting.
let g:go_highlight_types = 1
let g:go_highlight_fields = 1
let g:go_highlight_functions = 1
let g:go_highlight_function_calls = 1
let g:go_highlight_methods = 1
let g:go_highlight_operators = 1
let g:go_highlight_extra_types = 1
let g:go_highlight_build_constraints = 1
let g:go_metalinter_enabled = ['vet', 'golint', 'errcheck']
let g:go_metalinter_autosave = 1
" echo linting started and linting finished messages
" let g:go_echo_command_info = 0
let g:go_metalinter_autosave_enabled = ['vet', 'golint']
" let g:go_metalinter_autosave_enabled = ['vet']
let g:go_metalinter_deadline = '5s'
let g:ale_go_gometalinter_options =
      \ '--tests ' .
      \ '--fast ' .
      \ '--disable=gotype ' .
      \ '--disable=gotypex ' .
      \ '--exclude="should have comment" ' .
      \ '--exclude="error return value not checked \(defer"'
" does not seem to work.
let g:go_auto_type_info = 1 " shows signature of fn under cursor
" let g:go_auto_sameids = 1 " too slow
let g:go_gocode_unimported_packages = 1
let g:go_term_enabled = 1
let g:go_term_mode = 'split'
let g:go_term_height = 10
" Specifies whether `gocode` should use source files instead of binary packages
" It is currently much slower for source files.
" let g:go_gocode_propose_source = 1
" let g:go_guru_scope = ['github.com/...', expand("%:p:h")] " too slow
" let g:go_guru_scope = [expand("%:p:h")]  " does not work
" }}}
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

" vim-indentwise ----------------------------------------------------------{{{1
nnoremap <silent> <Plug>(IndentWiseBlockScopeBoundaryBegin)  :<C-U>call <SID>move_to_indent_block_scope_boundary(0, "n")<CR>
vnoremap <silent> <Plug>(IndentWiseBlockScopeBoundaryBegin)       :call <SID>move_to_indent_block_scope_boundary(0, "v")<CR>
onoremap <silent> <Plug>(IndentWiseBlockScopeBoundaryBegin) V:<C-U>call <SID>move_to_indent_block_scope_boundary(0, "o")<CR>
nnoremap [% <Plug>(IndentWiseBlockScopeBoundaryBegin)
nnoremap <silent> <Plug>(IndentWiseBlockScopeBoundaryEnd)      :<C-U>call <SID>move_to_indent_block_scope_boundary(1, "n")<CR>
vnoremap <silent> <Plug>(IndentWiseBlockScopeBoundaryEnd)           :call <SID>move_to_indent_block_scope_boundary(1, "v")<CR>
onoremap <silent> <Plug>(IndentWiseBlockScopeBoundaryEnd)     V:<C-U>call <SID>move_to_indent_block_scope_boundary(1, "o")<CR>
nnoremap ]% <Plug>(IndentWiseBlockScopeBoundaryEnd)
" 1}}}

" Go ------------------------------------------------------------------------{{{
augroup auto_go
    autocmd!
    autocmd BufNewFile,BufRead *.go setlocal noexpandtab tabstop=4 shiftwidth=4
augroup END
" should't this be in deoplete section?
let g:deoplete#sources#go#gocode_binary = $GOPATH.'/bin/gocode'
let g:deoplete#sources#go#sort_class = ['package', 'func', 'type', 'var', 'const']
let g:deoplete#sources#go#pointer = 1
" incredibly slow
" let g:deoplete#sources#go#source_importer = 1
"}}}

" terminal ------------------------------------------------------------------{{{
highlight TermCursor ctermfg=red guifg=red
set splitbelow
set splitright
tnoremap <Leader><ESC> <C-\><C-n>
tnoremap <leader>x <c-\><c-n>:bp! <BAR> bd! #<CR>
augroup auto_term
  autocmd!
  autocmd TermOpen * nnoremap <buffer> <leader>x :bp! <BAR> bd! #<CR>
  " does not work
  " autocmd TermOpen <buffer> * :startinsert
augroup END
" }}}

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
