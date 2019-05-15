" credit to vim-go function! go#alternate#Switch(bang, cmd) abort
" Test alternates between the implementation of code and the test code.
function! elixir#alternate#Switch(bang, cmd) abort " {{{
  let file = expand('%')
  if empty(file)
    " call go#util#EchoError("no buffer name")
    echohl ErrorMsg
    echomsg "No buffer name"
    echohl None
    return
  elseif file =~# '^\f\+_test\.exs$'
    let l:root = split(file, '_test.exs$')[0]
    let l:lib_path = substitute(expand('%:h'), "test", "lib", "")
    let l:source_file = split(expand('%:t'), '_test.exs')[0] . ".ex"
    let l:alt_file = globpath(l:lib_path, "**/" . l:source_file)
    " let l:alt_file = substitute(l:root, 'test', 'lib', "") . ".ex"
  elseif file =~# '^\f\+\.ex$'
    let l:root = split(file, ".ex$")[0]
    let l:alt_file = substitute(l:root, 'lib/\([^/]\+/\)*', 'test/', "") . "_test.exs"
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
    execute ":" . "edit" . " " . alt_file | write
  else
    execute ":" . a:cmd . " " . alt_file
    " execute ":" . a:cmd . " " . alt_file
  endif
endfunction " }}}
