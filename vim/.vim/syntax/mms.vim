" Vim syntax file
" Language: MMiX-Assembler
" Maintainer: Thomas Murschall
" Latest Version: 24 Juli 2016

if exists("b:current_syntax")
    finish
endif

" Matches
syn match Default '.'
syn match Register '$\d\{,3}'
syn match Load '\(\(LD\|ST\)\(BU\|B\|WU\|W\|TU\|T\|OU\|O\|HT\|SF\|UNC\)\)\|STCO'  
syn match Comment '%.*'
syn match Hex '#\x*'
syn match Doppelpunkt '\(:\a*\)\+'
syn match Preld 'PR\(ELD\|EST\|EGO\)'

" Keywords
syn keyword DataType BYTE WYDE TETRA OCTA
syn keyword Arithmetic ADD ADDU SUB SUBU MUL MULU DIV DIVU 
syn keyword InstructionSet1 NEG NEGU FLOT FLOTU FIX FIXU SFLOT SFLOTU SWYM SYNC SYNCID SYNCD LDVTS
syn keyword InstructionSet2 LDA GETA 2ADDU 4ADDU 8ADDU 16ADDU SET SETH SETMH SETML SETL
syn keyword Instructionset3 LOC IS PUT GET TRIP TRAP RESUME
syn keyword Instructionset4 SAVE UNSAVE CSWAP
syn keyword Compare CMP CMPU FCMP CEQL FUN FCMPE FEQL FEQLE FUNE
syn keyword InstructionSet5 PREFIX GREG
syn keyword Bitwise AND OR XOR ANDN ORN HAND NOR NXOR SL SLU SR SRU MOR MXOR BDIF WDIF TDIF ODIF ANDH ANDNH ANDMH ANDNMH ANDML ANDNML ANDL ANDNL ORH ORNH ORMH ORNMH ORML ORNML ORL ORNL SADD MUX 
syn keyword Jumps JMP GO PUSHJ PUSHGO POP BZ BNZ BN BNN BP BNP BOD BEV
syn keyword FloitingPoint FADD FSUB FMUL FDIV DREM FSQRT FINT SFLOT SFLOTU
syn keyword Increment INCH INCMH INCML INCL
syn keyword BedZuw CSZ CSNZ CSN CSNN CSP CSNP CSOD CSEV

" Regions
syn region String start=/\v"/ skip=/\v\\./ end=/\v"/

" highighting colours
highlight Default ctermfg=darkgreen
highlight DataType ctermfg=darkblue
highlight Arithmetic ctermfg=darkblue
highlight InstructionSet1 ctermfg=darkblue
highlight InstructionSet5 ctermfg=lightmagenta
highlight Comment ctermfg=Darkgray
highlight Register ctermfg=Lightblue
highlight Load ctermfg=darkblue
highlight Compare ctermfg=darkblue
highlight Bitwise ctermfg=darkblue
highlight Instructionset2 ctermfg=darkblue
highlight InstructionSet3 ctermfg=Darkblue
highlight Jumps ctermfg=Darkblue
highlight Hex ctermfg=Yellow
highlight Doppelpunkt ctermfg=green guifg=black
highlight FloitingPoint ctermfg=darkblue
highlight Increment ctermfg=darkblue
highlight BedZuw ctermfg=darkblue
highlight InstructionSet4 ctermfg=darkblue
highlight Preld ctermfg=darkblue
highlight String ctermfg=darkcyan
