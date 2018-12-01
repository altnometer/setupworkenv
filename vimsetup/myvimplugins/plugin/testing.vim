
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

augroup test_plug
    autocmd!
    autocmd FileType go inoremap <c-y> <ESC>h :<C-u>call <SID>MyGoImport()<CR>a
    autocmd FileType go nnoremap <c-y> :<C-u>call <SID>MyGoImport()<CR>:w<CR>
    autocmd FileType go nnoremap <leader>im :<C-u>call <SID>StartDocLine()<CR>a
augroup END
