#lang racket/base
(require racket/string)
(require racket/list)
(require parser-tools/lex)
(require xml)	
 (require racket/dict)
(require "lexer.rkt")
;Idan's Nand2Tetris Comiler under the MIT License

(define test "if(x<153){let city=\"Paris\";}\n//cool world!!!\n/*idan maman leaves on the top of ***the\nworldm***bro\n*idan maman*\\")
(define ftest (open-input-string test))
(define tokens-list (fileAnlyze ftest) )

;cause they are suck in choosing names I translated it with hashmap to their name it the last minute - Nand2Tetris names are suck...
(define suckNames 
        (hash
                'KEYWORD 'keyword
                'SYMBOL 'symbol
                'INTEGER 'integerConstant
                'ID     'identifier
                'STRING 'StringConstant
        )
)


(for ([i tokens-list])
       (writeln i))


(define tokens '(tokens ))
(for ([i tokens-list])
        (set! tokens (append tokens (list(list (hash-ref suckNames (token-name i)) (token-value i) )))))
(writeln tokens)

(define outputFile (open-output-file "Test.xml" #:exists 'replace #:replace-permissions? #t))


(write-xexpr tokens outputFile)





;(define xexprs (for/list ([a (in-list tokens-list)])
                
 ;                `(,(token-name a)  ,(token-value a))))
;(writeln xexprs)
;(write-xexpr xexprs)
;(define ExampleProgram "if(x<153){let city=\"Paris\";}" )
;(define Progrm-Input (open-input-string ExampleProgram))
;(display ExampleProgram)