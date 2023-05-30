#lang racket/base
 (require racket/string)
 (require racket/list)
 (require xml)
 (require parser-tools/lex)
 (require "../tokens.rkt")
 (require "../semantic_parser.rkt")
 ; the file ment to test the parser in some condions to check for bugs and UB





 (define (dumpToScreen filename listx )
     (display "DUMPING LIST ::: ")
     (display filename)
     (display "\n")
     (map (lambda (x) (writeln x )) listx)
     (display "FINISH DUMPPING\n\n")
 )

    (let* (
        [tokens (list 
                (token-KEYWORD "class")
                (token-ID "test")
                (token-SYMBOL "{")
                (token-KEYWORD "static")
                (token-KEYWORD "int")
                (token-ID "idan")
                (token-SYMBOL ",")
                (token-ID "idan")
                (token-SYMBOL ";")
                (token-KEYWORD "field")
                (token-KEYWORD "int")
                (token-ID "idan")
                (token-SYMBOL ";")
                (token-SYMBOL "}")
        ) ]
        [better-tokens (append (type2TokensConvertor tokens) (list (token-EOF 'eof)))]
        [current-token -1 ]
        [get-tokens (lambda () (set! current-token (add1 current-token) ) (list-ref better-tokens current-token))]
        [main (XMLjackParserPrivate get-tokens)]
    
    
    )
    (dumpToScreen "test" tokens )
    (writeln main)
    (write-xexpr main (current-output-port))


     )