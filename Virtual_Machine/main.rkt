#lang racket/base
(require racket/string)
(require racket/list)
(require "commands.rkt")

(define (fileAnalyze name)
  (define commands
    (hash "push"
          hack-push
          "pop"
          hack-pop
          "add"
          hack-add
          "sub"
          hack-sub
          "or"
          hack-or
          "neg"
          hack-neg
          "not"
          hack-not
          "and"
          hack-and
          "eq"
          hack-eq
          "gt"
          hack-gt
          "lt"
          hack-lt))
  (define file (open-input-file name))
  (define (analyze fileLine)
    (let* ([splitted (string-split fileLine " ")] [command (car splitted)])
      (display (take-right splitted (- (length splitted) 1)))
      (apply (hash-ref commands command) (take-right splitted (- (length splitted) 1)))))

  (define (looped fileo)
    (let ([line (read-line fileo)])
      (cond
        [(eof-object? line) ""]
        [(regexp-match? #rx"^((//| ).*)?$" (string-trim line)) (looped fileo)]
        [else (string-append (analyze (string-trim line)) "\n" (looped fileo))])))

  (looped file))

(define inputFileName (vector*-ref (current-command-line-arguments) 0))

(define outputFileName (string-append (car (string-split inputFileName ".")) ".asm"))

(define outputfile (open-output-file outputFileName #:exists 'replace #:replace-permissions? #t))
(display (fileAnalyze inputFileName) outputfile)