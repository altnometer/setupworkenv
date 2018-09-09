set nocompatible              " be iMproved, required
filetype off                  " required

" Plugins--------------------------------------------------------------------{{{
" :PlugInstall, :PlugUpdate, :PlugClean,
" :PlugUpgrade (plug itself), :PlugStatus
call plug#begin('~/.local/share/nvim/plugged')
" Plug 'tmhedberg/SimpylFold'
Plug 'jeetsukumaran/vim-indentwise'
Plug 'vim-scripts/ReplaceWithRegister'
Plug 'michaeljsmith/vim-indent-object'
Plug 'honza/vim-snippets'
Plug 'AndrewRadev/splitjoin.vim'
Plug 'justinmk/vim-sneak'
Plug 'airblade/vim-gitgutter'
Plug 'fatih/vim-go', { 'do': ':GoUpdateBinaries', 'for': 'go'}
" Plug 'mdempsky/gocode', { 'rtp': 'nvim', 'do': '~/.local/share/nvim/plugged/gocode/nvim/symlink.sh' }
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
" Remember cursor position between vim sessions
 autocmd BufReadPost *
             \ if line("'\"") > 0 && line ("'\"") <= line("$") |
             \   exe "normal! g'\"" |
             \ endif
             " center buffer around cursor when opening files
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
" }}}

" System mappings  ----------------------------------------------------------{{{
" Rebind <leader> key
" I like to have it here becuase it is easier to reach than the default and
" it is next to ``m`` and ``n`` which I use for navigating between tabs.
let mapleader = ","

" Quick quit command
noremap <leader>e :quit<CR>  " Quit current window
noremap <leader>E :qa!<CR>   " Quit all windows
" easier moving between tabs
map <leader>n <esc>:tabprevious<CR>
map <leader>m <esc>:tabnext<CR>

" Quicksave command
noremap <C-Z> :update<CR>
vnoremap <C-Z> <C-C>:update<CR>
" this  would save and go back to insert mode.
" inoremap <C-Z> <C-O>:update<CR>
" Added by sam@lf
" this  would save and exit insert mode.
inoremap <C-Z> <C-C>:update<CR>
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

" airline -------------------------------------------------------------------{{{
" enable vim-airline integration with plugins.
let g:airline_enable_fugitive=1
let g:airline_enable_syntastic=1
let g:airline_enable_bufferline=1
" }}}

" neosnippet ----------------------------------------------------------------{{{
" Plugin key-mappings.
" Note: It must be "imap" and "smap".  It uses <Plug> mappings.
imap <C-j>     <Plug>(neosnippet_expand_or_jump)
smap <C-j>     <Plug>(neosnippet_expand_or_jump)
xmap <C-j>     <Plug>(neosnippet_expand_target)
" }}}

" neoterm -------------------------------------------------------------------{{{
let neoterm_default_mod = 'vertical'
" }}}

" deoplete ------------------------------------------------------------------{{{
" <TAB>: completion.
inoremap <expr><TAB>  pumvisible() ? "\<C-n>" : "\<TAB>"
let g:deoplete#enable_at_startup = 1
" let g:deoplete#enable_debug = 1
" call deoplete#enable_logging('DEBUG', 'deoplete.log')
" call deoplete#custom#source('go', 'is_debug_enabled', 1)
"}}}

" vim-fugitive --------------------------------------------------------------{{{
" fugitive git options
" set diffopt+=vertical
" fugitive git bindings
"autocmd! BufWritePost * :Gstatus " does not work
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

" next maping doesn't work
"autocmd User fugitive if fugitive#buffer().type() =~# '^\%(tree\|blob\)$' | nnoremap <buffer> .. :edit %:h<CR> | endif

" Auto-clean fugitive bufferes.
autocmd BufReadPost fugitive://* set bufhidden=delete

" }}}

" vim-go --------------------------------------------------------------------{{{
" vim-go settings
" autocmd FileType go nmap <leader>b  <Plug>(go-build)
" run :GoBuild or :GoTestCompile based on the go file
function! s:build_go_files()
  let l:file = expand('%')
  if l:file =~# '^\f\+_test\.go$'
    call go#test#Test(0, 1)
  elseif l:file =~# '^\f\+\.go$'
    call go#cmd#Build(0)
  endif
endfunction
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
autocmd FileType go nmap <leader>i :GoSameIds<CR>

" Build/Test on save.
augroup auto_go
    autocmd!
    autocmd BufWritePost *.go :GoBuild
    " autocmd BufWritePost *_test.go :GoTest
    " autocmd BufWritePost *.go :GoTest
augroup end
" [q and [q should do the same as the next shortcuts
" map <C-n> :cnext<CR>
" map <C-m> :cprevious<CR>
nnoremap <leader>a :cclose<CR>
" let g:go_list_type = "quickfix"
" let g:go_list_type_commands = {"GoMetaLinter": "quickfix"} " default
" let g:go_list_type_commands = {"GoMetaLinter": "locationlist"}
" vim setting: saves buffer when :make is called.
" so, calling GoBuild write the file out.
set autowrite

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
" let g:go_auto_type_info = 1 " shows signature of fn under cursor
" let g:go_auto_sameids = 1 " too slow
let g:go_gocode_unimported_packages = 1
let g:go_term_enabled = 1
" Specifies whether `gocode` should use source files instead of binary packages
" It is currently much slower for source files.
let g:go_gocode_propose_source = 1

" Toggle alternate files, code and test files.
autocmd Filetype go command! -bang A call go#alternate#Switch(<bang>0, 'edit')
" autocmd Filetype go command! -bang AV call go#alternate#Switch(<bang>0, 'vsplit')
" autocmd Filetype go command! -bang AS call go#alternate#Switch(<bang>0, 'split')
" autocmd Filetype go command! -bang AT call go#alternate#Switch(<bang>0, 'tabe')
" Install plugins.
" }}}

" Go ------------------------------------------------------------------------{{{
let g:deoplete#sources#go#gocode_binary = $GOPATH.'/bin/gocode'
let g:deoplete#sources#go#sort_class = ['package', 'func', 'type', 'var', 'const']
let g:deoplete#sources#go#pointer = 1
" incredibly slow
" let g:deoplete#sources#go#source_importer = 1
"}}}
