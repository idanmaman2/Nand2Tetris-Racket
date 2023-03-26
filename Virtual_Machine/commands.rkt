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

(define hack-add-str 
"@SP 
M=M-1
A=M 
D=M    
@SP  
A=M-1
M=D+M")

(define hack-sub-str 
"@SP 
M=M-1 
A=M 
D=M     
@SP  
A=M-1
M=D-M")

(define hack-neg-str 
"@SP
A=M-1
M=-M
")

(define hack-and-str 
"@SP 
M=M-1 
A=M 
D=M    
@SP  
A=M-1
M=D&M")

(define hack-or-str 
"@SP 
M=M-1 
A=M 
D=M    
@SP  
A=M-1
M=D|M")

(define hack-not-str 
"@SP
A=M-1
M=!M")

(define hack-eq-str 
"@SP
A=M-1
D=M
A=A-1
D=D-M
@IF_TRUE0
D;JEQ
D=0
@IF_FALSE0
0;JMP
(IF_TRUE0) //label 
D=-1
(IF_FALSE0)
@SP
A=M-1
A=A-1
M=D
@SP
M=M-1")

(define hack-gt-str 
"@SP
A=M-1
D=M
A=A-1
D=D-M
@IF_TRUE0
D;JGT
D=0
@IF_FALSE0
0;JMP
(IF_TRUE0) //label 
D=-1
(IF_FALSE0)
@SP
A=M-1
A=A-1
M=D
@SP
M=M-1")

(define hack-lt-str 
"@SP
A=M-1
D=M
A=A-1
D=D-M
@IF_TRUE0
D;JLT
D=0
@IF_FALSE0
0;JMP
(IF_TRUE0) //label 
D=-1
(IF_FALSE0)
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

(define (hack-add) hack-add-str)
(define (hack-sub) hack-sub-str)
(define (hack-neg) hack-neg-str)
(define (hack-or)  hack-or-str)
(define (hack-not) hack-not-str)
(define (hack-and) hack-and-str)
(define (hack-eq)  hack-eq-str)
(define (hack-gt)  hack-gt-str)
(define (hack-lt)  hack-lt-str)



