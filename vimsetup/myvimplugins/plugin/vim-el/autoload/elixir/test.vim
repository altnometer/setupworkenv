
function! elixir#test#Run_file_tests(bang, cmd) abort " {{{
  let file = expand('%')
  if empty(file)
    echohl ErrorMsg | echomsg "No buffer name" | echohl None
    return
  elseif file =~# '^\f\+_test\.exs$'
    let l:test_file = file
  elseif file =~# '^\f\+\.ex$'
    let l:root = split(file, ".ex$")[0]
    let l:test_file = substitute(l:root, 'lib/\([^/]\+/\)*', 'test/', "") . "_test.exs"
  elseif file =~# '^\f\+\.exs$'
    echohl ErrorMsg | echomsg "[testing] don't accept .exs files, let g:el_compile_and_test=0 to stop this message" | echohl None
    return
  else
    echohl ErrorMsg | echomsg "not an elixir file" | echohl None
    return
  endif
  if !filereadable(test_file) && !bufexists(test_file) && !a:bang
    echohl ErrorMsg
    echomsg "[test] couldn't find " . test_file
    echohl None
    return
  elseif empty(a:cmd)
    execute ":Topen | :T mix test --timeout 400 " . test_file
    vertical resize 86
    " execute "normal! :T mix test " .  test_file . "\<cr>"
  else
    execute ":Topen | :T mix test --timeout 400 " . test_file
    vertical resize 86
  endif
endfunction " }}}

