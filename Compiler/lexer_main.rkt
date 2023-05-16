#lang racket/base
(require racket/string)
(require racket/list)
(require parser-tools/lex)
(require xml)
(require racket/dict)
(require "lexer.rkt")
;Idan's Nand2Tetris Compiler under the MIT License
;This program translate the code to tokens and dump it in xml file that ends in T.xml
;cause they are suck in choosing names I translated it with hashmap to their name it the last minute - Nand2Tetris names are suck...
(define suckNames
  (hash 'KEYWORD 'keyword
        'SYMBOL 'symbol
        'INTEGER 'integerConstant
        'ID 'identifier
        'STRING 'stringConstant))

;anlyze single file
(define argFolder (vector*-ref (current-command-line-arguments) 0))

(define (fileAnalyze path name)
  (let* ([fileName (string-append (string-trim name ".jack" #:left? #f #:right? #t #:repeat? #f)
                                  "IT.xml")]
         [outputfile (open-output-file (string-append argFolder "/" fileName)
                                       #:exists 'replace
                                       #:replace-permissions? #t)]
         [file (open-input-file (string-append path "/" name))]
         [tokens '(tokens)]
         [tokens-list (fileAnlyze file)])
    (for ([i tokens-list])
      (set! tokens (append tokens (list (list (hash-ref suckNames (token-name i)) (token-value i))))))
    for ([i tokens-list])
      (writeln i))
    (writeln tokens)
    (write-xexpr tokens outputfile)))

;loop over the files in the directory
(for ([i (map path->string (directory-list argFolder))] #:when (string-suffix? i ".jack"))

  (fileAnalyze argFolder i))