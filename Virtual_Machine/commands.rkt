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
(provide hack-set-namepsace)

(define comp-count 0)
(define hack-namespace "")

(define (hack-set-namepsace name)
    (set! hack-namespace name)
)

(define hack-binary-operator-str 
"@SP 
M=M-1
A=M 
D=M    
@SP  
A=M-1
M=M~aD")

(define hack-unary-operator-str 
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
@IF_TRUE~a
D;~a
D=0
@IF_FALSE~a
0;JMP
(IF_TRUE~a) 
D=-1
(IF_FALSE~a)
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
        "constant" (lambda (val) (format "@~a\nD=A" val))
        "static"    (lambda (val) (format "@~a.~a\nD=M" hack-namespace val ))
        "pointer"   (lambda (val) (format "@~a\nD=M"  (cond [(string=? "0" val) "THIS"] 
                                                            [(string=? "1" val) "THAT"] 
                                                            [else (error 'method-a "failed because ~a" "pointer pop value is not valid")])))
        ) )
(format hack-push-str ((hash-ref segments-push segment) arg)))

(define (hack-pop segment arg) 
(define (A-add-command val )
(string-join (map (lambda (ex) "A=A+1") (range (string->number val)) ) "\n"))
(define segments-pop (hash 
        "argument" (lambda (val) (format "@ARG\nA=M\n~a" (A-add-command val)))
        "local"    (lambda (val) (format "@LCL\nA=M\n~a"  (A-add-command val)))
        "this"     (lambda (val) (format "@THIS\nA=M\n~a"  (A-add-command val)))
        "that"     (lambda (val) (format "@THAT\nA=M\n~a" (A-add-command val)))
        "temp"     (lambda (val) (format "@~a"  (+ (string->number val ) 5 )))
        "static"   (lambda (val) (format "@~a.~a" hack-namespace val) ) 
        "pointer"   (lambda (val) (format "@~a"  (cond [(string=? "0" val) "THIS"] 
                                                       [(string=? "1" val) "THAT"] 
                                                       [else (error 'method-a "failed because ~a" "pointer pop value is not valid")])))
        
        ))
(format hack-pop-str ((hash-ref segments-pop segment) arg)))

(define (hack-add) (format hack-binary-operator-str "+"))
(define (hack-sub) (format hack-binary-operator-str "-"))
(define (hack-or)  (format hack-binary-operator-str "|"))
(define (hack-and) (format hack-binary-operator-str "&"))
(define (hack-neg) (format hack-unary-operator-str "-"))
(define (hack-not) (format hack-unary-operator-str "!"))
(define (hack-eq)  (set! comp-count (+ comp-count 1))(format hack-comp-str comp-count "JEQ"  comp-count comp-count comp-count))
(define (hack-gt)  (set! comp-count (+ comp-count 1))(format hack-comp-str comp-count "JLT"  comp-count comp-count comp-count))
(define (hack-lt)  (set! comp-count (+ comp-count 1))(format hack-comp-str comp-count "JGT"  comp-count comp-count comp-count))



