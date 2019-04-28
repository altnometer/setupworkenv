let s:compile_opt = [
  \ '--no-archives-check',
  \ '--no-deps-check',
  \ '--no-protocol-consolidation',
  \ '--force',
  \ '--return-errors',
  \]
" credit to https://github.com/gasparch/vim-elixir-exunit
function! elixir#compile#findMixDirectory() "{{{
    let fName = expand("%:p:h")

    while 1
        let mixFileName = fName . "/mix.exs"
        if file_readable(mixFileName)
            return fName
        endif

        let fNameNew = fnamemodify(fName, ":h")
        " after we reached top of heirarchy
        if fNameNew == fName
            return ''
        endif
        let fName = fNameNew
    endwhile
endfunction "}}}

" Compile the file to catch errors.
function! elixir#compile#Build(bang, ...) abort " {{{
  " Create our command arguments. go build discards any results when it
  " compiles multiple packages. So we pass the `errors` package just as an
  " placeholder with the current folder (indicated with '.').
  " let l:args =
        " \ ['mix', 'compile', '--force'] +
        " \ map(copy(a:000), "expand(v:val)")
  " let l:args = ['echo', 'hello from vim-el']
  let l:args = [ 'iex', shellescape(expand('%')) ]

  call s:cmd_job({
        \ 'cmd': args,
        \ 'bang': a:bang,
        \ 'for': 'ElixirCompile',
        \ 'statustype': 'compile'
        \})
endfunction " }}}

function! s:cmd_job(args) abort  " {{{
  " autowrite is not enabled for jobs
  call elixir#compile#autowrite()

  call elixir#job#Spawn(a:args.cmd, a:args)
endfunction " }}}

function! elixir#compile#autowrite() abort " {{{
  if &autowrite == 1 || &autowriteall == 1
    silent! wall
  else
    for l:nr in range(0, bufnr('$'))
      if buflisted(l:nr) && getbufvar(l:nr, '&modified')
        " Sleep one second to make sure people see the message. Otherwise it is
        " often immediacy overwritten by the async messages (which also don't
        " invoke the "hit ENTER" prompt).
        call elixir#echo#Warning('[No write since last change]')
        sleep 1
        return
      endif
    endfor
  endif
endfunction " }}}

" function! vimelixirexunit#runMixCompileCommand(arg) " {{{
"     let mixDir = vimelixirexunit#findMixDirectory()

"     let makeprg = 'mix compile'
"     if a:arg == '!'
"         let makeprg .= ' --force'
"     endif

"     let compilerDef = {
"         \ 'makeprg': makeprg,
"         \ 'target': 'qfkeep',
"         \ 'cwd': mixDir,
"         \ 'errorformat': s:ERROR_FORMATS['mix_compile']
"         \ }

"     let errors = s:runCompiler(compilerDef)
" endfunction " }}}

" function! s:runCompiler(options) " {{{
"     " save options and locale env variables
"     let old_cwd = getcwd()

"     let options = deepcopy(a:options)

"     if !has_key(options, 'target')
"         let options['target'] = 'llist'
"     endif

"     if has_key(options, 'cwd')
"         execute 'lcd ' . fnameescape(options['cwd'])
"     endif

"     let error_content = s:system(options['makeprg'])

"     if has_key(options, 'cwd')
"         execute 'lcd ' . fnameescape(old_cwd)
"     endif

"     " only for tests
"     let s:dump_error_content=error_content

"     let errors = s:parseErrorLines(options, error_content)

"     call s:revertQFLocationWindow(options)

"     return errors
" endfunction " }}}
