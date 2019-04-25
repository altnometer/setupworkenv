" asynchronous formatting with 'mix format' for nvim on linux
" copied, from https://github.com/mhinz/vim-mix-format
" simplified.
" the original supported setting:
" let g:mix_format_options
" let g:mix_format_silent_errors

if exists('b:loaded_mix_format')
      \ || fnamemodify(expand('%'), ':e') == 'eex'
      \ || &compatible
  finish
endif

function! s:on_stdout_nvim(_job, data, _event) dict abort
  if empty(a:data[-1])
    " Second-last item is the last complete line in a:data.
    let self.stdout += self.stdoutbuf + a:data[:-2]
    let self.stdoutbuf = []
  else
    if empty(self.stdoutbuf)
      " Last item in a:data is an incomplete line. Put into buffer.
      let self.stdoutbuf = [remove(a:data, -1)]
      let self.stdout += a:data
    else
      " Last item in a:data is an incomplete line. Append to buffer.
      let self.stdoutbuf = self.stdoutbuf[:-2]
            \ + [self.stdoutbuf[-1] . get(a:data, 0, '')]
            \ + a:data[1:]
    endif
  endif
endfunction

function! s:on_exit(_job, exitval, ...) dict abort
  let source_win_id = win_getid()
  call win_gotoid(self.win_id)

  if filereadable(self.undofile)
    execute 'silent rundo' self.undofile
    call delete(self.undofile)
  endif

  if a:exitval && get(g:, 'mix_format_silent_errors')
    for line in self.stdout
      echomsg line
    endfor
    redraw | echohl ErrorMsg | echo 'Formatting failed. Check :messages.' | echohl NONE
    return
  end
  " quickfix window is closed by 'cwindow' if it is open and there is no
  " errors. Therefore, we do not touch cwindow if there is no errors.
  if a:exitval
    let old_efm = &errorformat
    let &errorformat  = '%-Gmix format failed%.%#'
    let &errorformat .= ',** (%.%#) %f:%l: %m'
    cgetexpr self.stdout
    let &errorformat = old_efm
    " TODO: handle quickfix list more gracefully, like, do not touch if it
    " isn't a 'format' list. see autoload/go/fmt.vim for example implementation
    cwindow
    if &buftype == 'quickfix'
      let w:quickfix_title = s:build_cmd(fnamemodify(self.origfile, ':.'))
    endif
  endif

  if a:exitval
    redraw | echohl ErrorMsg | echo 'Formatting failed.' | echohl NONE
    return
  endif

  " Need to keep file properties to restore later.
  let column_old = col('.')
  let total_lines_old = line('$')

  let [fdl, sol, ur] = [&foldlevel, &startofline, &undoreload]
  let [&startofline, &undoreload] = [0, 10000]
  try
    silent edit!
  finally
    let [&foldlevel, &startofline, &undoreload] = [fdl, sol, ur]
  endtry
  call win_gotoid(source_win_id)
  " attempt to relocate the cursor intelligently: move current line by number
  " of lines changed, as in
  " https://github.com/fatih/vim-go/blob/master/autoload/go/fmt.vim
  call cursor(line('.') + (line('$') - total_lines_old), column_old)
  return
endfunction

function! s:build_cmd(filename) abort
  let options = get(g:, 'mix_format_options', '--check-equivalent')

  let dot_formatter = findfile('.formatter.exs', expand('%:p:h').';')
  if !empty(dot_formatter)
    let options .= ' --dot-formatter '. shellescape(fnamemodify(dot_formatter, ':p'))
  endif
  let filename = shellescape(a:filename)

  return printf('mix format %s %s', options, filename)
endfunction

function! elixir#fmt#Format() abort
  if &modified
    redraw | echohl WarningMsg | echo 'Unsaved buffer. Quitting.' | echohl NONE
    return
  endif

  let origfile = expand('%:p')
  let cmd = ['sh', '-c', s:build_cmd(origfile)]
  if &verbose
    echomsg 'MixFormat: '. (type(cmd) == type([]) ? string(cmd) : cmd)
  endif

  let undofile = tempname()
  execute 'wundo!' undofile

  let options = {
        \ 'cmd':       type(cmd) == type([]) ? join(cmd) : cmd,
        \ 'origfile':  origfile,
        \ 'undofile':  undofile,
        \ 'win_id':    win_getid(),
        \ 'stdout':    [],
        \ 'stdoutbuf': [],
        \ }

  silent! call jobstop(s:id)
  let s:id = jobstart(cmd, extend(options, {
        \ 'on_stdout': function('s:on_stdout_nvim'),
        \ 'on_stderr': function('s:on_stdout_nvim'),
        \ 'on_exit':   function('s:on_exit'),
        \ }))
endfunction

let b:loaded_mix_format = 1
