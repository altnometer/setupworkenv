" credit to https://github.com/fatih/vim-go



" Compile the source to catche errors.
function! elixir#cmd#Build(bang, ...) abort
    " Create our command arguments. go build discards any results when it
    " compiles multiple packages. So we pass the `errors` package just as an
    " placeholder with the current folder (indicated with '.').
    let l:args =
          \ ['build', '-tags', go#config#BuildTags()] +
          \ map(copy(a:000), "expand(v:val)") +
          \ [".", "errors"]

  let default_makeprg = &makeprg
  let &makeprg = "go " . join(go#util#Shelllist(args), ' ')

  let l:listtype = go#list#Type("GoBuild")
  " execute make inside the source folder so we can parse the errors
  " correctly
  let cd = exists('*haslocaldir') && haslocaldir() ? 'lcd ' : 'cd '
  let dir = getcwd()
  try
    execute cd . fnameescape(expand("%:p:h"))
    if l:listtype == "locationlist"
      silent! exe 'lmake!'
    else
      silent! exe 'make!'
    endif
    redraw!
  finally
    execute cd . fnameescape(dir)
    let &makeprg = default_makeprg
  endtry

  let errors = go#list#Get(l:listtype)
  call go#list#Window(l:listtype, len(errors))
  if !empty(errors) && !a:bang
    call go#list#JumpToFirst(l:listtype)
  else
    call go#util#EchoSuccess("[build] SUCCESS")
  endif
endfunction
