
" command! -bang GoAlternate call go#alternate#Switch(<bang>0, '')
command! -bang ElixirAlternate call elixir#alternate#Switch(<bang>0, '')
" format file with 'mix format'
command! -buffer -bar ElixirFormat call elixir#fmt#Format()

command! -bang ElixirCompile call elixir#compile#Build(<bang>0, '')
