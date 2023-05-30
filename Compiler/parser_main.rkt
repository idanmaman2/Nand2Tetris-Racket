#lang racket/base
 (require racket/string)
 (require racket/list)
 (require xml)
 (require parser-tools/lex)
 (require "tokens.rkt")
 (require "semantic_parser.rkt")
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
         'identifier         token-ID
         'stringConstant     token-STRING

     ))   
     (let* 
         (
             [file (open-input-file (string-append path "/" name))]
             [sytaxTokenList  (syntax-e (syntax:read-xml  file))]
             [cleanedList (take-right sytaxTokenList (- (length sytaxTokenList) 2))]
         )  
    (filter token? (map (lambda (x) (let 
             ([root (syntax->datum x)]
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
    (define count 0 )
     (display "DUMPING LIST ::: ")
     (display filename)
     (display "\n\n\n")
     (map (lambda (x) (display (number->string count))(set! count (add1 count))(writeln x )) listx)
     (display "FINISH DUMPPING\n\n")
 )


 (for ([name (map path->string (directory-list argFolder))] #:when (string-suffix? name "IT.xml"))



     
    (let* (
        [tokens (anlyzeXmlTokens argFolder name) ]
        [better-tokens (append (type2TokensConvertor tokens) (list (token-EOF 'eof)))]
        [current-token -1 ]
        [get-tokens (lambda () (set! current-token (add1 current-token) ) (writeln current-token) (list-ref better-tokens current-token))]
        
        [fileName (string-append (string-trim name "IT.xml" #:left? #f #:right? #t #:repeat? #f)
                                  "IX.xml")]
        [output-file (open-output-file (string-append argFolder "/" fileName)
                                       #:exists 'replace
                                       #:replace-permissions? #t)]
       [parser-output (XMLjackParser get-tokens)]
    
    )
    (dumpToScreen name better-tokens)
    (display parser-output)
    (write-xexpr parser-output output-file)



     )
 )


 ;anlyze 

 ;dump  