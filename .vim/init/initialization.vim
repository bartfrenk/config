filetype off
call pathogen#incubate()
call pathogen#helptags()
syntax enable
filetype plugin indent on
" definitions that need to go before other settings

command! -nargs=1 Silent
\ | execute ':silent '.<q-args>
\ | execute ':redraw!'
let mapleader=','
