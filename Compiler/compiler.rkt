#lang racket/base
 (require racket/string)
 (require racket/list)
 (require parser-tools/lex)
 (require "semantic_parser.rkt")
 (require "lexer.rkt")
 ;Idan's Nand2Tetris Compiler under the MIT License
 ;that program gets dump of tokens and do syntax parsing 


(define JACK-FILE-END ".jack")
(define VM-FILE-END   ".vm")

 ;global vars 
 (define argFolder (vector*-ref (current-command-line-arguments) 0)) ;path 

 (for ([name (map path->string (directory-list argFolder))] #:when (string-suffix? name JACK-FILE-END))  
    (letrec (
        [fileName (string-append (string-trim name JACK-FILE-END #:left? #f #:right? #t #:repeat? #f)
                                 VM-FILE-END)]
        [output-file (open-output-file (string-append argFolder "/" fileName)
                                       #:exists 'replace
                                       #:replace-permissions? #t)]
       
       [get_non_val_token (lambda () (let
                                    ([tokenN (Nand2TetrisLexer input-file)])
                                    (cond 
                                        [(void? tokenN) (get_non_val_token)]
                                        [else tokenN]
                                    )
       ))]
       [input-file (open-input-file (string-append argFolder "/" name))]
       [parser-output (VMjackParser get_non_val_token)]
    
    )
    (display parser-output output-file) 
    (writeln fileName )
     )
 )
 