#lang racket/base
(require racket/string)
(require racket/list)
(require parser-tools/lex)	
(require "lexer.rkt")
;Idan's Nand2Tetris Comiler under the MIT License


(define test "class if function method fieldstatic () 21321312213213123 ++ -- ()()()()(( 32767 \"idan maman\" 01__idan0123_idan0")
(define ftest (open-input-string test))
(for ([i (fileAnlyze ftest)])
        (writeln i))

;(define ExampleProgram "if(x<153){let city=\"Paris\";}" )
;(define Progrm-Input (open-input-string ExampleProgram))
;(display ExampleProgram)