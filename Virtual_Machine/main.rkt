#lang racket/base
(require racket/string)
(require racket/list)
(require "commands.rkt")
(define commands (hash "push"   hack-push  "pop"        hack-pop  
                    "add"    hack-add    "sub"       hack-sub 
                    "or"     hack-or     "neg"       hack-neg 
                    "not"     hack-not    "and"      hack-and
                    "eq"      hack-eq     "gt"       hack-gt
                    "lt"      hack-lt     "label"    hack-label 
                    "goto"    hack-goto   "if-goto"  hack-if-goto 
                    "function" hack-function "call"  hack-call  
                    "return"   hack-return ))
(define (ProcessVMToASM  inputFile outputFile)
  (define (analyze fileLine)
    (let* ([splitted (string-split fileLine " ")] [command (car splitted)])
      (apply (hash-ref commands command) (take-right splitted (- (length splitted) 1)))))
  (define (looped fileo)
    (let ([line (read-line fileo)])
      (cond
        [(eof-object? line)  (void) ] 
        [(regexp-match? #rx"^((//| ).*)?$" (string-trim line)) (looped fileo)] 
        [else 
        (display (analyze (string-trim  (regexp-replace #rx"//.*" line "" ))) outputFile)  
        (display "\n" outputFile )  
          (looped fileo)
        ]))) 

  (looped inputFile))
(let* (
    [namespaceFolder (vector*-ref (current-command-line-arguments) 0)]
    [namespace (last (string-split namespaceFolder "/") )]
    ; map of the command name to the import from commands.rkt 
    [outputFileName (string-append namespaceFolder "/" namespace ".asm")]
    [outputFile (open-output-file outputFileName #:exists 'replace #:replace-permissions? #t)]
  ) 
  ;writing the bootstrap in the head of the file 
  (display hack-bootstrap outputFile)
  ;lets anlayze each file and add it to the file ... 
  (display (format "\n//nice job for ~a\n" "bootstrap" ) outputFile)
  (for ([name (map path->string (directory-list namespaceFolder))] #:when (string-suffix? name ".vm"))
  (let ([inputFile (open-input-file (string-append namespaceFolder "/" name ))])
    (writeln name)
    (hack-set-namepsace (string-trim name ".vm" #:right? #t #:left? #f))
    (ProcessVMToASM inputFile outputFile)
    (display (format "\n//nice job for ~a\n" name ) outputFile)
  )
  ))
  