" Vim syntax file
" Language:	lox
" Maintainer:	Adam McCartney <adam@mur.at>
" Last Change:	2021 July 30

" Quit when a (custom) syntax file was already loaded
if exists("b:current_syntax")
  finish
endif

" define comments
syn match loxComment "//.*$"

" Useful lox keywords
syn keyword loxBoolox          true false
syn keyword loxClass           class super this
syn keyword loxConditional     if else
syn keyword loxExpression      var nil fun
syn keyword loxRepeat          while for 
syn keyword loxStatement       print return

syn keyword loxBlockCmd      loxConditional loxExpression loxRepeat loxStatement loxBlockCmd 
syn keyword loxExpression    loxBool loxClass 

" Integer with - + or nothing in front
syn match loxNumber '\d\+'
syn match loxNumber '[-+]\d\+'

" Floating point number with decimal no E or e 
syn match loxNumber '[-+]\d\+\.\d*'

" Floating point like number with E and no decimal point (+,-)
syn match loxNumber '[-+]\=\d[[:digit:]]*[eE][\-+]\=\d\+'
syn match loxNumber '\d[[:digit:]]*[eE][\-+]\=\d\+'

" Floating point like number with E and decimal point (+,-)
syn match loxNumber '[-+]\=\d[[:digit:]]*\.\d*[eE][\-+]\=\d\+'
syn match loxNumber '\d[[:digit:]]*\.\d*[eE][\-+]\=\d\+'

" Strings
syn region loxString start='"' end='"' contained
syn region loxDesc start='"' end='"'

" Add block 
syn region loxControlBlock start="{" end="}" fold transparent contains=loxNumber,loxBlockCmd


" Tell vim how to highlight
let b:current_syntax = "lox"

hi def link loxBlockCmd   Statement
hi def link loxComment    Comment
hi def link loxExpression Expression
hi def link loxNumber     Constant 
hi def link loxString     Constant

au BufRead,BufNewFile *.lox setfiletype lox
