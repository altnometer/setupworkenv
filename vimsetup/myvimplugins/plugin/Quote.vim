
" Qoute/unquote whatever is to the left until delemeters.

" limit the quoted chars to the first '[', '(', '{', '<'.
" on the left.
if !exists("g:deletere")
  let g:quote_delemeter = '\v[^\[({< ]+'
endif

if !exists("g:quote_char")
  let g:quote_char = '"'
endif

inoremap <silent> <Plug>(QuoteToTheLeft) <C-c>:<C-u>call <SID>QuoteUnquoteToTheLeft()<CR>a

function! s:QuoteUnquoteToTheLeft() abort " {{{
  " may misbehave if cursor is next to '})]'
  " https://stackoverflow.com/questions/23323747/vim-vimscript-get-exact-character-under-the-cursor
  let l:prev_char = getline('.')[col('.') - 1]
  if l:prev_char ==# g:quote_char
    call s:UnQuote()
  else
    call s:Quote()
  endif
endfunction " }}}

function! s:Quote() abort " {{{
  let l:save_pos = getcurpos()
  call searchpos(g:quote_delemeter, 'b', line('.'))
  " exe "normal! i" . g:quote_char . "\<c-c>"
  exe "normal! i" . g:quote_char
  let l:save_pos[2] += 1
  call setpos('.', l:save_pos)
  exe "normal! a" . g:quote_char
endfunction " }}}

function! s:UnQuote() abort " {{{
  let l:save_pos = getcurpos()
  exe "normal! x"
  call searchpos(g:quote_delemeter, 'b', line('.'))
  exe "normal! x"
  let l:save_pos[2] -= 2
  call setpos('.', l:save_pos)
endfunction " }}}

" if !hasmapto('<Plug>(QuoteToTheLeft)') || maparg('<C-u>','i') ==# ''
if !hasmapto('<Plug>(QuoteToTheLeft)')
  imap <C-u> <Plug>(QuoteToTheLeft)
endif
