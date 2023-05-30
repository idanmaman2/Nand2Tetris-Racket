#lang racket/base
(require parser-tools/lex)	

; all the right reserved to IDHM under the MIT license . 


; all the tokens I declared 
(provide    token-KEYWORD)
(provide    token-SYMBOL)
(provide    token-INTEGER)
(provide    token-STRING)
(provide    token-ID)
(provide    token-EOF)
; groups 
(provide    ID-tokens)
(provide    STR-tokens)
(provide    INT-tokens)
(provide    symbol-tokens   )
(provide    keywords-tokens  )
(provide    stopTokens)
;tokens 


;we have 5 types of tokens  
;and here they are 
(define-tokens      keywords-tokens      (KEYWORD )  )
(define-tokens      symbol-tokens        (SYMBOL)    )
(define-tokens      INT-tokens           (INTEGER)   )
(define-tokens      STR-tokens           (STRING)    )
(define-tokens      ID-tokens            (ID)        )
(define-tokens      stopTokens           (EOF)       )






  