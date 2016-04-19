" Gundo
map <leader>g :GundoToggle<CR>

" NERDTree
map <leader>n :NERDTreeToggle<CR>
map <F2> :NERDTreeToggle<CR>

" RopeVim
map <leader>j :RopeGotoDefinition<CR> map <leader>r :RopeRename<CR>

" TagBar
map <leader>tl :TlistToggle<CR>
map <leader>tu :TlistUpdate<CR>
map <F3> :TagbarToggle<CR>

" TaskList
map <leader>td <Plug>TaskList
map <F4> <Plug>TaskList

" Make and quickfix
map <silent> <F11> :Silent :make<CR>:cw<CR>

" PEP8
let g:pep8_map='<leader>p'

" Syntastic
map <leader>s :SyntasticCheck<CR><CR>

" Gundo
let g:gundo_help=0
let g:gundo_preview_bottom=1


" enabling both of the options below seems to give a bug: unable to navigate
" buffer window with hjkl or arrow keys

" use Ctrl-Tab and Ctrl-Shift-Tab to cycle through buffers
" let g:miniBufExplMapCTabSwitchBufs=1
" hide buffer numbers
" let g:miniBufExplShowBufNumbers=0

" ignore PEP8 violation E501: lines longer than 80 characters'
let g:pep8_ignore='E501'

" Pyflakes
let g:pyflakes_use_quickfix = 0

" Supertab
let g:SuperTabDefaultCompletionType = "context"

" Syntastic
" let g:syntastic_always_populate_loc_list=1
let g:syntastic_check_on_open=0
let g:syntastic_check_on_wq=0
let g:syntastic_error_symbol="e"
let g:syntastic_warning_symbol="w"
let g:syntastic_filetype_map={ "latex" : "tex" }
let g:syntastic_mode_map={ "mode" : "passive" , "passive" : ["python", "scala"]}
let g:syntastic_scala_scalac_exec="/opt/scala/scala-2.11.2/bin/scalac"
let g:syntastic_scala_checkers=["scalac"]
let g:syntastic_debug=0
let g:syntastic_aggregate_errors=0
let g:syntastic_always_populate_loc_list=1


" taglist
let Tlist_Compact_Format=1
let Tlist_Exit_OnlyWindow=1
let Tlist_File_Fold_Auto_Close=0
