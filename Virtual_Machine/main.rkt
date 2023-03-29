#lang racket/base
(require racket/string)
(require racket/list)
(require "commands.rkt")

(define (fileAnalyze inputName outputName)
  (define commands
    (hash 
    "push" hack-push
    "pop"  hack-pop
    "add"  hack-add
    "sub"  hack-sub
    "or"   hack-or
    "neg"  hack-neg
    "not"  hack-not 
    "and"  hack-and
    "eq"   hack-eq
    "gt"   hack-gt
    "lt"   hack-lt))

  (define inputFile (open-input-file inputName))
  (define outputFile (open-output-file outputName #:exists 'replace #:replace-permissions? #t))
  (define (analyze fileLine)
    (let* ([splitted (string-split fileLine " ")] [command (car splitted)])
      (apply (hash-ref commands command) (take-right splitted (- (length splitted) 1)))))

  (define (looped fileo)
    (let ([line (read-line fileo)])
      (cond
        [(eof-object? line)  (void) ] 
        [(regexp-match? #rx"^((//| ).*)?$" (string-trim line)) (looped fileo)] 
        [else 
        (display (analyze (string-trim line)) outputFile)  
        (display "\n" outputFile )  
          (looped fileo)
        
        ]))) 

  (looped inputFile))

(define inputFileName (vector*-ref (current-command-line-arguments) 0))
(define namespace (car (string-split inputFileName ".")) )
(define outputFileName (string-append namespace ".asm"))
(hack-set-namepsace namespace)
(fileAnalyze inputFileName outputFileName)