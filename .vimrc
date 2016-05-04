syntax enable
filetype plugin indent on 

command! -nargs=1 Silent
\ | execute ':silent '.<q-args>
\ | execute ':redraw!'
let mapleader=','
