""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" locate: ${XDG_CONFIG_HOME}/.vim/autoload/airline/themes/candymouse.vim "
" author: Vincent (github.com/milomouse)                                 "
" detail: ViM Airline plugin theme file                                  "
"________________________________________________________________________"

scriptencoding utf-8

let s:normal  = [246, '#8d8d8d'] "gray
let s:insert  = [ 97, '#ad73f9'] "magenta
let s:visual  = [184, '#d2d41b'] "yellow
let s:replace = [198, '#e71594'] "pink
let s:modify  = [ 33, '#0087ff'] "blue
let s:outerfg = [234, '#121212']
let s:innerbg = [234, '#1c1c1c']
let s:middle  = ['#bcbcbc', '#444444', 250, 238]

let s:N1 = [s:outerfg[1], s:normal[1], s:outerfg[0], s:normal[0]]
let s:N3 = [s:normal[1], s:innerbg[1], s:normal[0], s:innerbg[0]]
let s:I1 = [s:outerfg[1], s:insert[1], s:outerfg[0], s:insert[0]]
let s:I3 = [s:insert[1], s:innerbg[1], s:normal[0], s:innerbg[0]]
let s:V1 = [s:outerfg[1], s:visual[1], s:outerfg[0], s:visual[0]]
let s:V3 = [s:visual[1], s:innerbg[1], s:normal[0], s:innerbg[0]]
let s:R1 = [s:outerfg[1], s:replace[1], s:outerfg[0], s:replace[0]]
let s:R3 = [s:replace[1], s:innerbg[1], s:normal[0], s:innerbg[0]]

let s:IA = [s:middle[1], s:innerbg[1], s:middle[3], s:innerbg[0]]

let g:airline#themes#candymouse#palette = {}
let g:airline#themes#candymouse#palette.accents = {
    \ 'red': ['#ff009a', '', 200, '', '']}

let g:airline#themes#candymouse#palette.inactive = {
    \ 'airline_a': s:IA,
    \ 'airline_b': s:IA,
    \ 'airline_c': s:IA}

let g:airline#themes#candymouse#palette.normal = airline#themes#generate_color_map(s:N1, s:middle, s:N3)
let g:airline#themes#candymouse#palette.normal_modified = {
    \ 'airline_a': ['', s:modify[1], '', s:modify[0], '']}

let g:airline#themes#candymouse#palette.insert = airline#themes#generate_color_map(s:I1, s:middle, s:I3)
let g:airline#themes#candymouse#palette.insert_modified = {}

let g:airline#themes#candymouse#palette.replace = airline#themes#generate_color_map(s:R1, s:middle, s:R3)
let g:airline#themes#candymouse#palette.replace_modified = {}

let g:airline#themes#candymouse#palette.visual = airline#themes#generate_color_map(s:V1, s:middle, s:V3)
let g:airline#themes#candymouse#palette.visual_modified = {}

" CtrlP
if !get(g:, 'loaded_ctrlp', 0)
  finish
endif

let g:airline#themes#candymouse#palette.ctrlp = airline#extensions#ctrlp#generate_color_map(s:N1, s:middle, s:N3)
