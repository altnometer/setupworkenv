let s:compile_opt = [
  \ '--no-archives-check',
  \ '--no-deps-check',
  \ '--no-protocol-consolidation',
  \ '--force',
  \ '--return-errors',
  \]
" credit to https://github.com/gasparch/vim-elixir-exunit
function! elixir#compile#findMixDirectory(filename) "{{{
    let fName = a:filename

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
  let l:mix_project_dir = elixir#compile#findMixDirectory(expand("%:p:h"))
  if l:mix_project_dir
    let l:args =
          \ ['mix', 'compile', '--force'] +
          \ map(copy(a:000), "expand(v:val)")
    call s:cmd_job({
          \ 'cmd': args,
          \ 'bang': a:bang,
          \ 'jobdir': l:mix_project_dir,
          \ 'for': 'ElixirCompile',
          \ 'statustype': 'mix_compile',
          \ 'errorformat': s:ERROR_FORMATS['mix_compile']
          \})
          " \ 'errorformat': s:ERROR_FORMATS['mix_compile_errors_only']
  else
    " not a mix ploject: run in elixir IEx to catch errors.
    let l:args = [ 'iex', expand('%:p') ]
    call s:cmd_job({
          \ 'cmd': args,
          \ 'bang': a:bang,
          \ 'for': 'ElixirCompile',
          \ 'statustype': 'iex_compile',
          \ 'errorformat': s:ERROR_FORMATS['iex_compile']
          \})
          " \ 'errorformat': s:ERROR_FORMATS['mix_compile_errors_only']
  endif

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
  let s:ERROR_FORMATS = {
              \ "mix_compile":
                \'%-G%[\ ]%[\ ]%[\ ]%#(%.%#,'.
                \'%E**\ (%[A-Z]%[%^)]%#)\ %f:%l:\ %m,'.
                \'%Z%^%[\ ]%#%$,'.
                \'%W%>warning:\ %m,'.
                \'%-C\ \ %f:%l,'.
                \'%-G==\ Compilation error%.%#,'.
                \'%-G%[\ ]%#',
              \ "iex_compile":
                \'%E**\ (%[A-Z]%[%^)]%#)\ %f:%l:\ %m,'.
                \'%E**\ (%[%^)]%#)\ %m,'.
                \'%-C%[\ ]%#(elixir)\ %.%#,'.
                \'%-C%[\ ]%#%f:%l:%.%#,'.
                \'%Z%[\ ]%#,'.
                \'%-G%[\ ]%#',
              \ "mix_compile_errors_only":
                \ "%-D**CWD**%f,".
                \'%-G==\ Compilation error%.%#,'.
                \'%-Gwarning:%.%#,'.
                \'%-G%[\ ]%#,'.
                \'%-G\ %.%#,'.
                \'%E**\ (%[%^)]%#)\ %f:%l:\ %m',
              \ "exunit_run":
                \   '%D**CWD**%f,' .
                \  '%-G%\\s%#,'.
                \  '%-GSKIP,'.
                \  '%-GGenerated\ %.%#\ app,'.
                \  '%-GIncluding\ tags:%.%#,'.
                \  '%-GExcluding\ tags:%.%#,'.
                \  '%-GFinished\ in\ %.%#,'.
                \  '%-G\ \ \ \ \ stacktrace:,'.
                \     '**\ (%[A-Z]%\\w%\\+%trror)\ %f:%l:\ %m,'.
                \  '%+G\ \ \ \ \ %\\w%\\+:\ ,'.
                \   '%E\ \ %\\d%\\+)\ %m,' .
                \   '%Z\ \ \ \ \ %f:%l,'.
                \  '%+G\ \ \ \ \ %\\w%\\+,'.
                \     '%t\ \ \ \ \ \ %f:%l:\ %m,'.
                \ ""
              \ }
  " }}}
