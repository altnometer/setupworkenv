" inspired by:
" File: qfixtoggle.vim
" Author: Jason Heddings (vim at heddway dot com)
" Version: 1.0
" Last Modified: 05 October, 2005

" close quickfix buffers, if they exist, if not, close all other windows.
" nnoremap <expr> <silent> <Plug>(MySmartClose) <SID>QFix_Is_Open() ? ':cclose <bar> :lclose<cr>' : '<c-w>o'
" nnoremap <expr> <silent> <Plug>(MySmartClose) <SID>QFix_Is_Open() ? ':cclose <bar> :lclose<cr>' : <SID>Testing()
" nnoremap <silent> <Plug>(MySmartClose) :<C-u>call <SID>MySmartClose()<CR>
nnoremap <Plug>(MySmartClose) :<C-u>call <SID>MySmartClose()<CR>

function! s:DeleteBuffers(bufs) abort
  for b in a:bufs
      exe "normal! :bdelete! " . b . "\r"
  endfor
endfunction

" 0. remove help buffers.
" 1. If quickfix or locationlist is open, close them and exit.
" 2. If any terminals are open, close them and exit.
" 3. If there is only one window open, delete current buffer and exit.
" 4. If there is more than one window left, close them except the first one.
function! s:MySmartClose() abort
  " TODO: implement winbufnr()
  " collect all buffers
  let l:b_all = filter(range(1, bufnr("$")), 'bufexists(v:val)')
  " 0. delete 'help' buffer and exit.
  let l:help_all = filter(copy(l:b_all), 'getbufvar(v:val, ''&ft'') ==# ''help''')
  if len(l:help_all) > 0
    for b in l:help_all
        exe "normal! :bdelete! " . b . "\r"
    endfor
    return
  endif
  " l:qf_all holds quickfix, locationlist buffers
  let l:qf_all = filter(copy(l:b_all), 'getbufvar(v:val, ''&ft'') ==# ''qf''')
  " echom "l:qf_all"
  " echom l:qf_all[0]
  " return
  if len(l:qf_all) > 0
    for b in l:qf_all
        exe "normal! :bdelete! " . b . "\r"
    endfor
    return
  endif
  " 1. If quickfix or locationlist is open, close them and exit.
  " if s:QFix_Is_Open()
  "   exe "normal! :cclose \<bar> :lclose\r"
  "   return
  " endif
  " 2. Close terminals.
  " 2.1 Close fzf terminals.
  let l:fzfterm_all = filter(copy(l:b_all), 'getbufvar(v:val, ''&ft'') ==# ''fzf''')
  if len(l:fzfterm_all) > 0
    for b in l:fzfterm_all
      exe "normal! :bdelete! " . b . "\r"
    endfor
    return
  endif
  " 2.2 Close neoterm terminals.
  let l:neoterm_all = filter(copy(l:b_all), 'getbufvar(v:val, ''&ft'') ==# ''neoterm''')
  if len(l:neoterm_all) > 0
    let g:neoterm_autoinsert=0
    for b in l:neoterm_all
      " hide neoterm buffer
      exe "normal! :Tclose\r"
      " delete neoterm buffer
      " exe "normal! :Tclose!\r"
      " kill process in neoterm buffer
      " exe "normal! :Tkill"
      " kill IEx (elixir interactive shell, REPL)
      " exe "normal! :Tkill | :Tkill\r"
      " delete the buffer
      " exe "normal! :bdelete! " . b . "\r"
    endfor
    return
  endif
  " 2.5. delete buffers with filetype 'diff'
  let l:diff_all = filter(copy(l:b_all), 'getbufvar(v:val, ''&ft'') ==# ''diff''')
  if len(l:diff_all) > 0
    for b in l:diff_all
        exe "normal! :bdelete! " . b . "\r"
    endfor
    return
  endif
  " 3. If there is only one window open, delete current buffer and exit.
  " !!!MUST be called before closing the other windows.
  if winnr('$') == 1
    " if there is only one buffer, close the window and quit vim.
    if len(filter(range(1, bufnr('$')), 'buflisted(v:val)')) == 1
      exe "normal! :quit\r"
      return
    endif
    exe "normal! :bdelete " . bufnr('%') . "\r"
    return
  endif
  " 4. If there is more than one window left, close them except the first one.
  for w in range(2, winnr("$"))
    exe "normal! :" . w . "quit\r"
  endfor
endfunction

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

function! s:CloseAllNeoTerms() abort " {{{
  " exe "normal! :let g:neoterm_autoinsert=0 \<bar> TcloseAll!\r"
  let g:neoterm_autoinsert=0
  exe "normal! :TcloseAll!\r"
endfunction " }}}

augroup QFix_Autocmds " {{{
  autocmd!
  autocmd BufWinEnter quickfix call s:QFix_Setup()
  autocmd BufWinLeave * call s:QFix_Close()
augroup END " }}}
