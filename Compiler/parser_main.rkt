#lang racket/base
(require racket/string)
(require racket/list)
(require xml)
(require parser-tools/yacc)	
(require "tokens.rkt")
;Idan's Nand2Tetris Compiler under the MIT License
;that program gets dump of tokens and do syntax parsing on them and dump that into xml file ends with .xml

;global vars 
(define argFolder (vector*-ref (current-command-line-arguments) 0)) ;path 


;xml functions

(define (anlyzeXmlTokens path name )

    (define suckNameToToken (hash 
        'keyword            token-KEYWORD
        'symbol             token-SYMBOL
        'integerConstant    token-INTEGER
        'identifier         token-STRING
        'stringConstant     token-ID
    
    ))   
    (let* 
        (
            [file (open-input-file (string-append path "/" name))]
            [sytaxTokenList  (syntax-e (syntax:read-xml  file))]
            [cleanedList (take-right sytaxTokenList (- (length sytaxTokenList) 2))]
        )  
   (filter token? (map (lambda (x) (let 
            ([root (syntax->datum x)]
             ;[a (first root ) ]
             ;[b (list-ref root 2)]
            )
            (cond 
                [(list? root) 
                (let  (
                    [a (first root )]
                    [b (third root)]
                
                )
                ((hash-ref suckNameToToken  a) b)
                )
                ]
                [else (void)] ; for xml formatted files ... 
            )
            ;(writeln a)
        )) cleanedList) ) 
    ) 
    )





;read dump => list of tokens


(define (dumpToScreen filename listx )
    (display "DUMPING LIST ::: ")
    (display filename)
    (display "\n\n\n")
    (map (lambda (x) (writeln x )) listx)
    (display "FINISH DUMPPING\n\n")
)


(for ([i (map path->string (directory-list argFolder))] #:when (string-suffix? i "IT.xml"))
    
    
    
    (dumpToScreen i (anlyzeXmlTokens argFolder i) )
    
    
    
    )



;anlyze 

;dump 