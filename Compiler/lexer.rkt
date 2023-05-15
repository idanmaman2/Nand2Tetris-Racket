;ALL the rights reserved to the one and only IDAN MAMAN - by the MIT License .
;NAND2TETRIS LEXER 
;imports 
#lang racket/base
(require racket/string)
(require racket/list)
(require parser-tools/lex)	
(require parser-tools/lex-sre)

;exports 
; the racket lexer 
(provide Nand2TetrisLexer)
; function for using the lexer on file - returns list of tokens 
(provide fileAnlyze)
; all the tokens I declared 
(provide token-KEYWORD)
(provide token-SYMBOL)
(provide token-INTEGER)


;tokens 
(define-tokens  keywords-tokens ( KEYWORD ) )
(define-tokens symbol-tokens    (SYMBOL))
(define-tokens INT-tokens       (INTEGER))
(define-tokens STR-tokens       (STRING))
(define-tokens ID-tokens       (ID))







;lexer 
(define Nand2TetrisLexer 
    (lexer 

        ;keyword tokens rule 
        [
            (union  "class" "constructor" "function" "method" "field"
                    "static" "var" "int" "char" "boolean" "void" "true" 
                    "false" "null" "this" "let" "do" "if" "else" "while"
                    "return")  
            ;=>
            (begin (token-KEYWORD lexeme))
        ]


        ;symbol tokens rule 
        [
            (union  "(" ")" "{" "}" "[" "]" 
                    "." "," ";" "+" "-" "*" 
                    "/" "&" "|" "<" ">" "=" 
                    "~" ) 
            ;=>
            (begin (token-SYMBOL lexeme))
        ]


        ;number tokens rule 
        ;([0-9]|[1-9][0-9]{1,3}|[12][0-9]{4}|3[01][0-9]{3}|32[0-6][0-9]{2}|327[0-5][0-9]|3276[0-7])
        [   (or
                (:
                    numeric
                )
                (:
                    (char-range #\1 #\9)
                    (repetition  
                        1 
                        3
                        numeric 
                    )
                )
                (:
                    "12"
                    (repetition  
                        4
                        4
                        numeric 
                    )
                )
                (:
                    "3"
                     (char-range #\0 #\1)
                    (repetition  
                        3
                        3
                        numeric
                    )
                )
                (:
                    "32"
                    (char-range #\0 #\6)
                    (repetition  
                        2
                        2
                        numeric 
                    )
                )
                (:
                    "327"
                     (char-range #\0 #\5)
                     numeric
                )
                (:
                    "3276"
                    (char-range #\0 #\7)
                )

            )
            ;=> 
            (begin  (token-INTEGER lexeme ))
        ]

        ;string tokens rule 
        [
            (: #\" any-string  #\") 
            (begin (token-STRING lexeme ))
        ]

        ;id tokens rule 
        [
            (:  
                (or 
                    lower-case
                    upper-case
                    #\_ 
                ) 
                (* 
                    (or 
                        #\_ 
                        lower-case
                        upper-case
                        numeric
                    )
                )
            )
            ;=>
            (begin    (token-ID lexeme ) )
        ]


   [#\space (void)]
   [#\newline  (void)]
   [(eof)  eof]

   ))
(define (fileAnlyze ftest)

   (define res '())

    (define (anlyzeRec) 
        (let (
        [line (Nand2TetrisLexer ftest) ])   
        
        (cond 
            [(eof-object? line ) (void)]
            [(void? line ) (anlyzeRec)]
            [else   (set! res (append res (list line) )) (anlyzeRec)]
        )
        ) 
    )
    (anlyzeRec)
    res

)



