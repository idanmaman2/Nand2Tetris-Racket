#lang racket/base
(require racket/string)
(require racket/list)
(require parser-tools/lex)
(require parser-tools/lex-sre)
(require parser-tools/cfg-parser)
(require (for-syntax racket/base
                     syntax/boundmap
                     parser-tools/private-lex/token-syntax))

(provide cfg-parser)
(define-tokens non-terminals (PLUS MINUS STAR BAR COLON EOF))
(define-tokens cool (XOOL))
  (define lex
    (lexer
     ["+" (token-PLUS '+)]
     ["-" (token-MINUS '-)]
     ["*" (token-STAR '*)]
     ["|" (token-BAR '||)]
     [":" (token-COLON '|:|)]
     [whitespace (lex input-port)]
     [(eof) (token-EOF 'eof)]))
  
  (define parse
    (cfg-parser
     (tokens non-terminals cool)
     (start <program>)
     (end EOF)
     (error (lambda (a b stx) 
              (error 'parse "failed at ~s" stx)))
     (grammar [<program> 
                         [(PLUS <test>) (list 'plus)]
                         [(<minus-program> BAR <minus-program>) (list $1 $2 $3)]
                         [(<program> COLON <program>) (append $1 $3)]]
              [<minus-program> [(MINUS) "minus"]
                               [(<program> STAR) (cons $1 $2)]]
              [<simple> [(<alts> <alts> <alts> MINUS) "yes"]]
              [<test> [(MINUS ) ( list "minus") ]
                      [ () '() ]
              
              ]
              [<alts> [(PLUS) 'plus]
                      [(MINUS) 'minus]]
              [<random> [() '0]
                        [(<random> PLUS) (add1 $1)]
                        [(<random> PLUS) (add1 $1)]])))
  



(let* (
        [tokens (list (token-PLUS '+)  (token-COLON '|:|) (token-PLUS '+)  (token-COLON '|:|)(token-PLUS '+) ) ]
        [better-tokens (append tokens (list (token-EOF 'eof)))]
        [current-token -1 ]
        [get-tokens (lambda () (set! current-token (add1 current-token) ) (list-ref better-tokens current-token))]
    )
    ;(dumpToScreen i tokens )
    ;(display better-tokens )
    ;(writeln (get-tokens))
    ;(writeln (get-tokens))
    ;(display (get-tokens))
    ;(display (get-tokens))
    (display (parse get-tokens))
    ;(display (XMLjackParserPrivate get-tokens))


     )


  (let ([p (open-input-string "+-:-|-:+:+")])
    (display  (parse (lambda () (lex p)))))
        

 