" copied from
" https://github.com/fatih/vim-go/blob/master/autoload/go/config.vim

function! elixir#config#ListType() abort
  return get(g:, 'el_list_type', '')
endfunction

function! elixir#config#ListTypeCommands() abort
  return get(g:, 'el_list_type_commands', {})
endfunction

function! elixir#config#ListHeight() abort
  return get(g:, "el_list_height", 0)
endfunction

function! elixir#config#Debug() abort
  return get(g:, 'el_debug', [])
endfunction

" Report if the user enabled a debug flag in g:el_debug.
function! elixir#config#HasDebug(flag)
  return index(elixir#config#Debug(), a:flag) >= 0
endfunction

function! elixir#config#ListAutoclose() abort
  return get(g:, 'el_list_autoclose', 1)
endfunction
