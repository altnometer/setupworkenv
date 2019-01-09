
" Toggle case of the characters in the word.

let g:PREV = [0, 0, 0]
let g:NEXT = [0, 0, 0]

inoremap <silent> <Plug>(ToggleCharInsertMode) <c-c>:call <SID>ToggleCharInsertMode()<cr>a
imap <a-u> <plug>(ToggleCharInsertMode)
nnoremap <silent> <Plug>(ToggleChar) :call <SID>ToggleChar()<cr>
nmap <a-u> <plug>(ToggleChar)

nnoremap <silent> <Plug>(TogglePrevNChars) :<c-u>call <SID>TogglePrevNChars(v:count)<cr>
nmap <c-u> <plug>(TogglePrevNChars)
inoremap <silent> <Plug>(ToggleLastNChars0) <c-c>:<c-u>call <SID>TogglePrevNChars(0)<cr>a
imap <c-u> <plug>(ToggleLastNChars0)


function! s:TogglePrevNChars(count) abort " {{{
  let l:save_cursor = getcurpos()
  let l:first_char_pos = GetFirstCharPos(l:save_cursor)
  let l:last_char_pos = s:GetLastCharPos(l:save_cursor)
  call setpos('.', l:last_char_pos)
  " 0. v:count is given, ignore other cases.
  if a:count > 1
    " echomsg "case a:count > 1"
    let l:count = a:count - 1
    let g:PREV = deepcopy(l:save_cursor)
    let g:PREV[2] -= l:count
    call s:ToggleLastNCharsRange(g:PREV, l:save_cursor)
    let g:NEXT = l:last_char_pos
    call setpos('.', l:last_char_pos)
    return
  endif
  " 0.1 v:count is given, ignore other cases.
  if a:count == 1
    " echomsg "case a:count == 1"
    let g:NEXT = l:last_char_pos
    let g:PREV = l:save_cursor
    " toggle the char under cursor.
    exe "normal! ~"
    call setpos('.', l:last_char_pos)
    return
  endif
  " 1. Marker is on the same line, in the word and before the cursor.
  if a:count == 0 && l:save_cursor[1] == g:NEXT[1] && l:first_char_pos[2] < g:NEXT[2] && l:save_cursor[2] > g:NEXT[2]
    let g:NEXT[2] += 1
    call s:ToggleLastNCharsRange(g:NEXT, l:last_char_pos)
    let g:PREV = g:NEXT
    let g:NEXT = l:last_char_pos
    return
  endif
  " 2. Marker is in the same col and line as the cursor.
  if l:save_cursor[1:2] == g:NEXT[1:2]
    call s:ToggleLastNCharsRange(g:PREV, l:last_char_pos)
    return
  endif
  if a:count == 0
    " toggle the whole word.
    exe "normal! g~iwe"
    call setpos('.', l:last_char_pos)
    let g:PREV = l:first_char_pos
    let g:NEXT = l:last_char_pos
  endif
endfunction " }}}

function! s:ToggleLastNCharsRange(start, end) abort " {{{
  let l:toggle_pos = deepcopy(a:start)
  for i in range(a:start[2], a:end[2])
    let l:toggle_pos[2] = i
    call setpos('.', l:toggle_pos)
    exe "normal! ~"
  endfor
  call setpos('.', a:end)
endfunction " }}}

function! s:ToggleCharInsertMode() abort " {{{
  let l:save_cursor = getcurpos()
  " check if the cursor jumped over the word begining when the mapping was
  " envoked with the cursor being on the first letter.
  " may misbehave if cursor is next to '})]'
  " https://stackoverflow.com/questions/23323747/vim-vimscript-get-exact-character-under-the-cursor
  let l:char_under_cursor = getline('.')[col('.') - 1]
  if l:char_under_cursor =~ '\s'
    let l:shift_to_word_start = deepcopy(l:save_cursor)
    let l:shift_to_word_start[2] += 1
    call setpos('.', l:shift_to_word_start)
  endif
  call s:ToggleChar()
  call setpos('.', l:save_cursor)
endfunction " }}}

function! s:ToggleChar() " {{{
  let l:save_cursor = getcurpos()
  let l:first_char_pos = GetFirstCharPos(l:save_cursor)
  let l:last_char_pos = s:GetLastCharPos(l:save_cursor)
  " 1. g:NEXT is not set.
  if g:NEXT[1] == 0
    " echomsg  "g:NEXT[1] == 0 , g:NEXT is not set."
    call s:ToggleFirstChar(l:first_char_pos, l:last_char_pos)
    return
  endif
  " 2. g:NEXT is on another line.
  if l:save_cursor[1] != g:NEXT[1]
    " echomsg  "l:save_cursor[1] != g:NEXT[1] , g:NEXT is on another line."
    call s:ToggleFirstChar(l:first_char_pos, l:last_char_pos)
    return
  endif
  " 3. g:NEXT is in the same col and line as the cursor.
  if l:save_cursor[1:2] == g:NEXT[1:2]
    " echomsg  "l:save_cursor[1:2] == g:NEXT[1:2] , g:NEXT is in the same col and line as the cursor."
    call s:TogglePrev(g:PREV, l:last_char_pos)
    return
  endif
  " 4. g:NEXT is not in the word or at the 1st char.
  if l:first_char_pos[2] > g:NEXT[2]
    " echomsg  "l:first_char_pos[2] > g:NEXT[2] , g:NEXT is not in the word or at the 1st char."
    call s:ToggleFirstChar(l:first_char_pos, l:last_char_pos)
    return
  endif
  " 5. g:NEXT is on the same line, in the word and before the cursor.
  if l:save_cursor[1] == g:NEXT[1] && l:first_char_pos[2] < g:NEXT[2] && l:save_cursor[2] > g:NEXT[2]
    " echomsg  "g:NEXT is on the same line, in the word and before the cursor."
    call s:ToggleNext(g:NEXT, l:last_char_pos)
    return
  endif
  " 6. Cursor is on the first char.
  if l:first_char_pos[2] == l:save_cursor[2]
    " echomsg  "Cursor is on the first char."
    call s:ToggleFirstChar(l:first_char_pos, l:last_char_pos)
    return
  endif
endfunction " }}}

function! s:ToggleNext(next, last_char_pos) " {{{
  " go to marker.
  let a:next[2] = a:next[2] + 1
  call setpos('.', a:next)
  " toggle.
  exe "normal! ~"
  " go back.
  call setpos('.', a:last_char_pos)
  " reset marker.
  let g:PREV = a:next
  let g:NEXT = a:last_char_pos
endfunction " }}}

function! s:ToggleFirstChar(first_char, last_char_pos) " {{{
  " go to first char.
  call setpos('.', a:first_char)
  " toggle.
  exe "normal! ~"
  " go back.
  call setpos('.', a:last_char_pos)
  " reset marker.
  let g:PREV = a:first_char
  let g:NEXT = a:last_char_pos
endfunction " }}}

function! s:TogglePrev(prev, last_char_pos) " {{{
  " go to last change position.
  call setpos('.', a:prev)
  " toggle.
  exe "normal! ~"
  " go back.
  call setpos('.', a:last_char_pos)
  " reset marker.
  let g:NEXT = a:last_char_pos
endfunction " }}}

function! GetFirstCharPos(save_cursor) " {{{
  " may misbehave if cursor is next to '})]'
  " https://stackoverflow.com/questions/23323747/vim-vimscript-get-exact-character-under-the-cursor
  let l:prev_char = getline('.')[col('.') - 2]
  if l:prev_char =~ '\w' " cursor is not on the first word char.
    exe "normal! b"
    let l:first_char_pos = getcurpos()
    call setpos('.', a:save_cursor)
  else " cursor is on the first char.
    let l:first_char_pos = getcurpos()
  endif
  return l:first_char_pos
endfunction " }}}

function! s:GetLastCharPos(save_cursor) " {{{
  " may misbehave if cursor is next to '})]'
  " https://stackoverflow.com/questions/23323747/vim-vimscript-get-exact-character-under-the-cursor
  let l:next_char = getline('.')[col('.')]
  if l:next_char =~ '\w' " cursor is not on the last word char.
    exe "normal! e"
    let l:last_char_pos = getcurpos()
    call setpos('.', a:save_cursor)
  else " cursor is on the last char.
    let l:last_char_pos = getcurpos()
  endif
  return l:last_char_pos
endfunction " }}}
