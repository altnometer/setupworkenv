
" Qoute/unquote whatever is to the left until delemeters.

" limit the quoted chars to the first '[', '(', '{', '<'.
" on the left.
if !exists("g:delimiter")
  let g:quote_delimiter = '\v[^\[({< ]+'
endif

if !exists("g:quote_char")
  let g:quote_char = '"'
endif

if !exists("g:unquote_chars")
  " let g:unquote_chars = '"'''
  let g:unquote_chars = {"'": "'", '"': '"', '>': '<'}
endif

" inoremap <silent> <Plug>(QuoteToTheLeft) <C-c>:<C-u>call <SID>QuoteUnquoteToTheLeft()<CR>a
inoremap  <Plug>(QuoteToTheLeft) <C-c>:<C-u>call <SID>QuoteUnquoteToTheLeft()<CR>a

function! s:QuoteUnquoteToTheLeft() abort " {{{
  " may misbehave if cursor is next to '})]'
  " https://stackoverflow.com/questions/23323747/vim-vimscript-get-exact-character-under-the-cursor
  let l:prev_char = getline('.')[col('.') - 1]
  let l:next_char = getline('.')[col('.')]
  for char in keys(g:unquote_chars)
    " sometimes, expanding snippet leaves you inside unneeded quotes
    " ORDER IS IMPORTANT, UnQuoteWhenInsideQuotes must be the first
    if l:next_char ==# char
      call s:UnQuoteWhenInsideQuotes(l:next_char)
      return
    endif
    if l:prev_char ==# char
      call s:UnQuoteWhenOutsideQuotes(l:prev_char)
      return
    endif
  endfor
  call s:Quote()
endfunction " }}}

function! s:Quote() abort " {{{
  let l:save_pos = getcurpos()
  call searchpos(g:quote_delimiter, 'b', line('.'))
  " exe "normal! i" . g:quote_char . "\<c-c>"
  exe "normal! i" . g:quote_char
  let l:save_pos[2] += 1
  call setpos('.', l:save_pos)
  exe "normal! a" . g:quote_char
endfunction " }}}

function! s:UnQuoteWhenOutsideQuotes(key_of_char_pair) abort " {{{
  let l:save_pos = getcurpos()
  exe "normal! x"
  call searchpos(g:unquote_chars[a:key_of_char_pair], 'b', line('.'))
  exe "normal! x"
  let l:save_pos[2] -= 2
  call setpos('.', l:save_pos)
endfunction " }}}

function! s:UnQuoteWhenInsideQuotes(key_of_char_pair) abort " {{{
  let l:save_pos = getcurpos()
  exe "normal! lx"
  call searchpos(g:unquote_chars[a:key_of_char_pair], 'b', line('.'))
  exe "normal! x"
  let l:save_pos[2] -= 1
  call setpos('.', l:save_pos)
endfunction " }}}

" if !hasmapto('<Plug>(QuoteToTheLeft)') || maparg('<C-u>','i') ==# ''
if !hasmapto('<Plug>(QuoteToTheLeft)')
  imap <C-u> <Plug>(QuoteToTheLeft)
endif
