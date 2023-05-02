;importing
#lang racket/base
(require racket/string)
(require racket/list)

;exporting 
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
(provide hack-label)
(provide hack-goto)
(provide hack-if-goto)
(provide hack-function)
(provide hack-call)
(provide hack-return)
(provide hack-set-namepsace)

;global vars 
(define comp-count 0)
(define hack-namespace "")

;functions and help global vars
(define (hack-set-namepsace name)
    (set! hack-namespace name)
)

;"Preform binary operator action - hack-binary-operator-str is a generic function"
(define hack-binary-operator-str (string-join (list "@SP" "M=M-1" "A=M" "D=M" "@SP" "A=M-1""M=M~aD") "\n"))
(define (hack-add) (format hack-binary-operator-str "+"))
(define (hack-sub) (format hack-binary-operator-str "-"))
(define (hack-or)  (format hack-binary-operator-str "|"))
(define (hack-and) (format hack-binary-operator-str "&"))

;"Preform unary operator action - hack-unary-operator-str is a generic function"
(define hack-unary-operator-str (string-join (list "@SP" "A=M-1""M=~aM") "\n"))
(define (hack-neg) (format hack-unary-operator-str "-"))
(define (hack-not) (format hack-unary-operator-str "!"))

;"hack-push-str get in D the value to push and pushes it , hack-push enter code that loads value to D by segment"
(define hack-push-str (string-join (list "~a" "@SP" "A=M" "M=D" "@SP" "M=M+1" ) "\n"))
(define (hack-push segment arg) 
(define segments-push (hash 
        "argument"  (lambda (val) (format (string-join  (list "@~a" "D=A" "@ARG" "A=M" "A=A+D" "D=M" ) "\n")  val ))
        "local"     (lambda (val) (format (string-join  (list "@~a" "D=A" "@LCL" "A=M" "A=A+D" "D=M" ) "\n")  val ))
        "this"      (lambda (val) (format (string-join  (list "@~a" "D=A" "@THIS" "A=M" "A=A+D" "D=M") "\n")  val ))
        "that"      (lambda (val) (format (string-join  (list "@~a" "D=A" "@THAT" "A=M" "A=A+D" "D=M") "\n")  val )) 
        "temp"      (lambda (val) (format (string-join  (list "@~a" "D=M"                            ) "\n")  (+ (string->number val) 5) ) )
        "constant"  (lambda (val) (format (string-join  (list "@~a" "D=A"                            ) "\n") val))
        "static"    (lambda (val) (format (string-join  (list "@~a.~a" "D=M"                         ) "\n") hack-namespace val ))
        "pointer"   (lambda (val) (format (string-join  (list "@~a" "D=M"                            ) "\n")  (cond [(string=? "0" val) "THIS"] 
                                                                                                                  [(string=? "1" val) "THAT"] 
                                                                                                                  [else (error 'method-a "failed because ~a" "pointer pop value is not valid")])))
        ) )
(format hack-push-str ((hash-ref segments-push segment) arg)))

;"hack-pop-str get in A the address to pop the head of the stack into , hack-pop enter code that loads address to A by segment"
(define hack-pop-str (string-join (list "@SP" "A=M-1" "D=M" "~a" "M=D" "@SP" "M=M-1") "\n"))
(define (hack-pop segment arg) 
(define (A-add-command val )
(string-join (map (lambda (ex) "A=A+1") (range (string->number val)) ) "\n"))
(define segments-pop (hash 
        "argument" (lambda (val)  (string-trim (format (string-join (list "@ARG" "A=M" "~a") "\n") (A-add-command val))))
        "local"    (lambda (val)  (string-trim (format (string-join (list "@LCL" "A=M" "~a") "\n")  (A-add-command val))))
        "this"     (lambda (val)  (string-trim (format (string-join (list "@THIS" "A=M" "~a") "\n" )  (A-add-command val))))
        "that"     (lambda (val)  (string-trim (format (string-join (list "@THAT" "A=M" "~a") "\n") (A-add-command val))))
        "temp"     (lambda (val)  (format                                 "@~a"  (+ (string->number val ) 5 )))
        "static"   (lambda (val)  (format                                 "@~a.~a" hack-namespace val) ) 
        "pointer"   (lambda (val) (format                                  "@~a"  (cond [(string=? "0" val) "THIS"] 
                                                                                        [(string=? "1" val) "THAT"] 
                                                                                        [else (error 'method-a "failed because ~a" "pointer pop value is not valid")])))
        
        ))
        (format hack-pop-str ((hash-ref segments-pop segment) arg)))

;"Compare between 2 values - hack-comp is a generic function"
(define hack-comp-str (string-join (list "@SP" "A=M-1" "D=M" "A=A-1" "D=D-M" "@IF_TRUE~a" "D;~a" "D=0" "@IF_FALSE~a" "0;JMP" "(IF_TRUE~a)" "D=-1" "(IF_FALSE~a)" "@SP" "A=M-1" "A=A-1" "M=D" "@SP" "M=M-1") "\n"))
(define (hack-eq)  (set! comp-count (+ comp-count 1))(format hack-comp-str comp-count "JEQ"  comp-count comp-count comp-count))
(define (hack-gt)  (set! comp-count (+ comp-count 1))(format hack-comp-str comp-count "JLT"  comp-count comp-count comp-count))
(define (hack-lt)  (set! comp-count (+ comp-count 1))(format hack-comp-str comp-count "JGT"  comp-count comp-count comp-count))

;"preform goto"
(define hack-goto-str (string-join (list "@LABEL_~a" "0;JMP") "\n"))
(define (hack-goto labelName) (format hack-goto-str labelName))

;"creates label"
(define hack-label-str "(LABEL_~a)")
(define (hack-label labelName) (format hack-label-str labelName))

;"preforms goto if the last value in stack is not 0-false"
(define hack-if-goto-str (string-join (list "@SP" "M=M-1" "A=M" "D=M" "D;JNE" ) "\n"))
(define (hack-if-goto labelName) (set! comp-count (+ comp-count 1)) (format hack-if-goto-str comp-count labelName comp-count))

;"preform a call to function"
(define hack-call-str (string-join (list "~a" "~a" "~a" "~a" "~a" "@~a" "D=A" "@SP" "D=M-D" "@ARG" "M=D" "@SP" "D=M" "@LCL" "M=D" "~a" "~a") "\n" ))
(define (hack-call funcName nArgs) (format hack-call-str
                                        (hack-push "constant" (format "RETURN_~a" funcName))
                                        (format hack-push-str "@LCL\nD=M")
                                        (format hack-push-str "@ARG\nD=M")
                                        (format hack-push-str "@THIS\nD=M")
                                        (format hack-push-str "@THAT\nD=M")
                                        (number->string (- (string->number nArgs) 5 ))
                                        (hack-goto (format "FUNCTION_~a" funcName ))
                                        (hack-label (format "RETURN_~a" funcName ))))

;"create function"
(define hack-function-str (string-join (list "(FUNCTION_~a)" "~a") "\n" ) )
(define (hack-function funcName nArgs) (format hack-function-str 
        funcName 
        (string-join (map (lambda (ex) (hack-push "constant" "0")) (range (string->number nArgs)) ) "\n")
        
)) 

;"return from function"


(define hack-return-str

(string-join (list "@LCL" "D=M" "@5" "A=D-A" "D=M" "@13" "M=D" "~a" "@ARG" "D=M" "@SP" "M=D+1" "~a" "~a" "~a" "~a" "@13" "A=M" "0;JMP" ) "\n") 

)

(define (hack-return) 

(define (mini_recovery_segment segment) (string-join (list "@LCL" "M=M-1" "A=M" "D=M" (format "@~a" segment) "M=D") "\n"))
(format hack-return-str 
        (hack-pop "argument" "0")
        (mini_recovery_segment "THAT") 
        (mini_recovery_segment "THIS") 
        (mini_recovery_segment "ARG") 
        (mini_recovery_segment "LCL")
))
