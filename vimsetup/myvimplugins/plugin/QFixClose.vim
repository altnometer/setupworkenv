" inspired by:
" File: qfixtoggle.vim
" Author: Jason Heddings (vim at heddway dot com)
" Version: 1.0
" Last Modified: 05 October, 2005

" close quickfix buffers, if they exist, if not, close all other windows.
nnoremap <expr> <silent> <Plug>(Close_QFix_Or_Other_Win) <SID>QFix_Is_Open() ? ':cclose <bar> :lclose<cr>' : '<c-w>o'

function! s:QFix_Is_Open() " {{{
  if exists("g:QFix_Bufnr")
    return g:QFix_Bufnr
  else
    return 0
  endif
endfunction " }}}

function! s:QFix_Close() " {{{
  if exists("g:QFix_Bufnr") && expand("<abuf>") == g:QFix_Bufnr
    unlet! g:QFix_Bufnr
  endif
endfunction " }}}

function! s:QFix_Setup() " {{{
  let g:QFix_Bufnr = bufnr("$")
endfunction " }}}

augroup QFix_Autocmds " {{{
  autocmd!
  autocmd BufWinEnter quickfix call s:QFix_Setup()
  autocmd BufWinLeave * call s:QFix_Close()
augroup END " }}}
