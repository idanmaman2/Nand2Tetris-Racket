#lang racket/base
(require racket/string)
(require racket/list)
(define totalbuy 0)
(define totalcell 0)

(define argFolder (vector*-ref (current-command-line-arguments) 0))

(define fileName (string-append (last (string-split argFolder "/")) ".asm"))

(define (fileAnalyze path name)
  (define file (open-input-file (string-append path "/" name)))
  (define (analyze fileLine)
    (define (HandleBuy productName amount price)
      (define mul (* (string->number amount) (string->number price)))
      (set! totalbuy (+ totalbuy mul))
      (string-append "### BUY " productName " ###\n" (number->string mul) "\n"))

    (define (HandleCell productName amount price)
      (define mul (* (string->number amount) (string->number price)))
      (set! totalcell (+ totalcell mul))
      (string-append "$$$ CELL " productName " $$$\n" (number->string mul) "\n"))

    (let* ([splitted (string-split fileLine " ")] [command (car splitted)])
      (cond
        [(string=? "buy" command) (HandleBuy (second splitted) (third splitted) (fourth splitted))]
        [(string=? "cell" command) (HandleCell (second splitted) (third splitted) (fourth splitted))]
        [else ""])))
  (define (looped fileo)
    (let ([line (read-line fileo)])
      (cond
        [(eof-object? line) ""]
        [else (string-append (analyze line) (looped fileo))])))

  (string-append name "\n" (looped file)))

(define outputfile
  (open-output-file (string-append argFolder "/" fileName)
                    #:exists 'replace
                    #:replace-permissions? #t))
(for ([i (map path->string (directory-list argFolder))] #:when (string-suffix? i ".vm"))
  (display (fileAnalyze argFolder i) outputfile))
(display "TOTAL BUY:" outputfile)
(display totalbuy outputfile)
(display "\nTOTAL CELL:" outputfile)
(display totalcell outputfile)