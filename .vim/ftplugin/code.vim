setlocal tabstop=4  " number of spaces to use for a tab
setlocal shiftwidth=4   " number of spaces to use for autoindent
setlocal smarttab
setlocal expandtab
setlocal autoindent
set nowrap

setlocal list
setlocal cf " enable error files and error jumping.
setlocal autowrite  " writes on make and shell commands
setlocal nowrap " line wrapping off
setlocal formatoptions=tcqr
setlocal showmatch  " show matching brackets
setlocal mat=2  " bracket blinking
setlocal lcs=tab:\ \ ,trail:~,extends:>,precedes:<  " show trailing space as ~

setlocal foldmethod=indent  " fold based on indent
setlocal foldnestmax=10 " deepest fold is 10 levels
setlocal nofoldenable   " don't fold by default
setlocal foldlevel=10

set colorcolumn=120
