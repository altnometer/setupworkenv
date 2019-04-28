" copied
" https://github.com/fatih/vim-go/blob/master/autoload/go/util.vim
"
" Echo a message to the screen and highlight it with the group in a:hi.
"
" The message can be a list or string; every line with be :echomsg'd separately.
function! s:echo(msg, hi)
  let l:msg = []
  if type(a:msg) != type([])
    let l:msg = split(a:msg, "\n")
  else
    let l:msg = a:msg
  endif

  " Tabs display as ^I or <09>, so manually expand them.
  let l:msg = map(l:msg, 'substitute(v:val, "\t", "        ", "")')

  exe 'echohl ' . a:hi
  for line in l:msg
    echom "vim-el: " . line
  endfor
  echohl None
endfunction

function! elixir#echo#Success(msg)
  call s:echo(a:msg, 'Function')
endfunction
function! elixir#echo#Error(msg)
  call s:echo(a:msg, 'ErrorMsg')
endfunction
function! elixir#echo#Warning(msg)
  call s:echo(a:msg, 'WarningMsg')
endfunction
function! elixir#echo#Progress(msg)
  redraw
  call s:echo(a:msg, 'Identifier')
endfunction
function! elixir#echo#Info(msg)
  call s:echo(a:msg, 'Debug')
endfunction
