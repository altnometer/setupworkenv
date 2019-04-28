" copied from
" https://github.com/fatih/vim-go/blob/master/autoload/go/config.vim

function! elixir#config#ListTypeCommands() abort
  return get(g:, 'elixir_list_type_commands', {})
endfunction
