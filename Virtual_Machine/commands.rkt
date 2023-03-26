#lang racket/base
(require racket/string)
(require racket/list)

(provide hack-add)
(provide hack-sub )
(provide hack-neg )
(provide hack-or  )
(provide hack-not )
(provide hack-and )
(provide hack-eq  )
(provide hack-gt  )
(provide hack-lt  )
(provide hack-push)
(provide hack-pop)

(define comp-count 0)


(define hack-binary-operator-str 
"@SP 
M=M-1
A=M 
D=M    
@SP  
A=M-1
M=D~aM")

(define hack-unari-operator-str 
"@SP
A=M-1
M=~aM
")

(define hack-push-str 
"~a
@SP
A=M
M=D
@SP
M=M+1")

(define hack-pop-str 
"@SP
A=M-1
D=M
~a
M=D
@SP
M=M-1")

(define hack-comp-str 
"@SP
A=M-1
D=M
A=A-1
D=D-M
@IF_TRUE_~a
D;~a
D=0
@IF_FALSE_~a
0;JMP
(IF_TRUE_~a) 
D=-1
(IF_FALSE_~a)
@SP
A=M-1
A=A-1
M=D
@SP
M=M-1")



(define (hack-push segment arg) 
(define segments-push (hash 
        "argument" (lambda (val) (format "@~a\nD=A\n@ARG\nA=M\nA=A+D\nD=M"  val ))
        "local"    (lambda (val) (format "@~a\nD=A\n@LCL\nA=M\nA=A+D\nD=M"  val ))
        "this"     (lambda (val) (format "@~a\nD=A\n@THIS\nA=M\nA=A+D\nD=M"  val ))
        "that"     (lambda (val) (format "@~a\nD=A\n@THAT\nA=M\nA=A+D\nD=M"  val )) 
        "temp"     (lambda (val) (format "@~a\nD=M"  (+ (string->number val) 5) ) )
        "constant" (lambda (val) (format "@~a\nD=A" val))))
(format hack-push-str ((hash-ref segments-push segment) arg)))

(define (hack-pop segment arg) 
(define (A-add-command val )
(string-join (map (lambda (ex) "A=A+1") (range (string->number val)) ) "\n"))
(define segments-pop (hash 
        "argument" (lambda (val) (format "@ARG\nA=M\n~a" (A-add-command val)))
        "local"    (lambda (val) (format "@LCL\nA=M\n~a"  (A-add-command val)))
        "this"     (lambda (val) (format "@THIS\nA=M\n~a"  (A-add-command val)))
        "that"     (lambda (val) (format "@THAT\nA=M\n~a" (A-add-command val)))
        "temp"     (lambda (val) (format "@~a"  (+ (string->number val ) 5 )))))
(format hack-pop-str ((hash-ref segments-pop segment) arg)))

(define (hack-add) (format hack-binary-operator-str "+"))
(define (hack-sub) (format hack-binary-operator-str "-"))
(define (hack-or)  (format hack-binary-operator-str "|"))
(define (hack-and) (format hack-binary-operator-str "&"))
(define (hack-neg) (format hack-unari-operator-str "-"))
(define (hack-not) (format hack-unari-operator-str "!"))
(define (hack-eq)  (set! comp-count (+ comp-count 1))(format hack-comp-str comp-count "JEQ"  comp-count comp-count comp-count))
(define (hack-gt)  (set! comp-count (+ comp-count 1))(format hack-comp-str comp-count "JGE"  comp-count comp-count comp-count))
(define (hack-lt)  (set! comp-count (+ comp-count 1))(format hack-comp-str comp-count "JLT"  comp-count comp-count comp-count))



