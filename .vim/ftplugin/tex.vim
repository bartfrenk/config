setlocal errorformat=%f:%l:\ %m,%f:%l-%\\d%\\+:\ %m
if filereadable('Makefile')
  setlocal makeprg=make
else
  exec "setlocal makeprg=make\\ -f\\ ~/.rubber/latex.mk\\ " . substitute(bufname("%"),"tex$","pdf", "")
endif

set shiftwidth=2
set tabstop=2
