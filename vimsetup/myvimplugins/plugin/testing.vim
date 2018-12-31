
function! s:MyGoImport() abort " {{{
  let w = expand("<cWORD>")
  if len(w) <= 0
    echohl WarningMsg
    echom 'no WORD under cursor'
    echohl None
    return
  endif
  " echom w
  let m = matchstr(w, '\v.*<\zs\w+\ze\.')
  " echom m
  " return
  if m ==# "http"
    let m = "net/http"
  elseif m ==# "httptest"
    let m = "net/http/httptest"
  elseif m ==# "rand"
    let m = "math/rand"
  elseif m ==# "template"
    let m = "html/template"
  elseif m ==# "json"
    let m = "encoding/json"
  elseif m ==# "ioutil"
    let m = "io/ioutil"
  endif
  execute 'normal! :GoImport ' . m . "\r"
endfunction " }}}

function! s:StartDocLine() abort " {{{
  " jump above the type definition.
  exec "normal! {"
  let line = getline(line(".") + 1)
  " let m = matchstr(w, '\v.*<\zs\w+\ze\.')
  let m =  matchstr(line, '\v^\w+( \([^()]+\))? \zs\w+\ze')
  let failed = append(line("."), "// " . m)
  exec "normal! jg$a "
endfunction " }}}


inoremap <silent> <Plug>(MyGoImport) <ESC>h :<C-u>call <SID>MyGoImport()<CR>a
nnoremap <silent> <Plug>(MyGoImport) :<C-u>call <SID>MyGoImport()<CR>:w<CR>
nnoremap <silent> <Plug>(StartDocLine) :<C-u>call <SID>StartDocLine()<CR>a
