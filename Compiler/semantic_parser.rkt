#lang racket/base
(require racket/string)
(require racket/list)
(require parser-tools/yacc)
(require parser-tools/lex)
(require (for-syntax racket/base syntax/boundmap parser-tools/private-lex/token-syntax))
(require "tokens.rkt")
(require "symbol_table.rkt")
(require "code_writer.rkt")

;exports
(provide type2TokensConvertor)
(provide XMLjackParser)

;Idan's Nand2Tetris Compiler under the MIT License

; because the cfg-parser can take variety of symbols on the same token name - I created conversion from the classic token to one that the CFG understand ( I not so nice But i am not a slave and i am not going to change all the code)

(define-tokens keywords-tokensC (CLASS CONSTRUCTOR FUNCTION METHOD FIELD STATIC INT VAR CHAR BOOLEAN VOID TRUE FALSE NULL THIS LET DO IF ELSE WHILE RETURN))
(define-tokens symbol-tokensC (JSYM-RROUND JSYM-LROUND JSYM-LCURLY JSYM-RCURLY JSYM-LREC JSYM-RREC JSYM-DOT JSYM-COMMA JSYM-DOTCOM JSYM-PLUS JSYM-MINUS JSYM-STAR JSYM-SLASH JSYM-ANDC JSYM-ORC JSYM-BIGER JSYM-SMALLER JSYM-EQULAS JSYM-NOTC))
(define (token-convert classicToken)
  (define convertTableKEYWORD
    (hash "class" token-CLASS
          "constructor" token-CONSTRUCTOR
          "function" token-FUNCTION
          "method" token-METHOD
          "field" token-FIELD
          "static" token-STATIC
          "var" token-VAR
          "int" token-INT
          "char" token-CHAR
          "boolean" token-BOOLEAN
          "void" token-VOID
          "true" token-TRUE
          "false" token-FALSE
          "null" token-NULL
          "this" token-THIS
          "let" token-LET
          "do" token-DO
          "if" token-IF
          "else" token-ELSE
          "while" token-WHILE
          "return" token-RETURN))

  (define convertTableSymbol
    (hash "(" token-JSYM-LROUND
          ")" token-JSYM-RROUND
          "{" token-JSYM-LCURLY
          "}" token-JSYM-RCURLY
          "[" token-JSYM-LREC
          "]" token-JSYM-RREC
          "." token-JSYM-DOT
          "," token-JSYM-COMMA
          ";" token-JSYM-DOTCOM
          "+" token-JSYM-PLUS
          "-" token-JSYM-MINUS
          "*" token-JSYM-STAR
          "/" token-JSYM-SLASH
          "&" token-JSYM-ANDC
          "|" token-JSYM-ORC
          "<" token-JSYM-SMALLER
          ">" token-JSYM-BIGER
          "=" token-JSYM-EQULAS
          "~" token-JSYM-NOTC))

  (let ([name (token-name classicToken)] [val (token-value classicToken)])
    (cond
      [(equal? name 'KEYWORD) ((hash-ref convertTableKEYWORD val) val)]
      [(equal? name 'SYMBOL) ((hash-ref convertTableSymbol val) val)]
      [else classicToken])))

(define (type2TokensConvertor tokenList) (map token-convert tokenList))

(define XMLjackParser
  (parser
   (tokens keywords-tokens stopTokens keywords-tokensC symbol-tokensC INT-tokens STR-tokens ID-tokens)
   (start <class>)
   (end EOF)
   (error (lambda (a b stx)
            (error 'parse "@@@ JACK COMPILER @@@  failed at:\n\ttoken_nickname : `~a`\n\ttoken_symbol:  `~a`" b stx)))
   (grammar
    ;###################### grammer rules #####################
    ; ----------------------------------------------------------------
    ;class -> 'class' className '{' classVarDec* subroutineDec* '}'
    [<class>
     [(<CLASS-XML> <classNameDec> <JSYM-LCURLY-XML> <ITERclassVarDec> <ITERsubRoutineDec> <JSYM-RCURLY-XML>)
      (begin (clear-all-class-scope) (string-join $5 "\n") )]]
    ;classNameDec -> ID
    [<classNameDec> [(<ID-VM>) (begin (create-class-scope $1) $1)]]
    ;classVarDec -> ('static' | 'field' ) type VarName (',' varName)* ';'
    [<classVarDec>
     [(<STATIC-XML> <type> <varName> <ITERVarName> <JSYM-DOTCOM-XML>)
      (begin (insert-static $3 $2) (for ([i $4])(insert-static i $2))(void))]
     [(<FIELD-XML> <type> <varName> <ITERVarName> <JSYM-DOTCOM-XML>)
      (begin (insert-field $3 $2) (for ([i $4]) (insert-field i $2)) (void))]]

    ;type -> 'int' | 'char' | 'boolean' | className
    [<type> [(<INT-VM>) $1] [(<CHAR-VM>) $1] [(<BOOLEAN-VM>) $1] [(<className>) $1]]
    ;subRoutineDec -> ('construcntor' | 'function' | 'method' ) ('void' | type ) subroutineName '(' paramterList ') subroutineBody
    [<subRoutineDec>

     [(<CONSTRUCTOR-VM> <type> <subroutineName> <JSYM-LROUND-XML> <parameterList> <JSYM-RROUND-XML> <subroutineBody>)
      (begin
        ;(dumpSymbolTables)
        (let 
          ([loclnum (get-count-local)])
          (clear-scope) (string-append (compile-create-ctor (get-class-scope) $3 loclnum (get-count-field)) "\n" $7)
        )
        )]

     [(<FUNCTION-VM> <type> <subroutineName> <JSYM-LROUND-XML> <parameterList> <JSYM-RROUND-XML> <subroutineBody>)
      (begin
        ;(dumpSymbolTables)
        (let 
          ([loclnum (get-count-local)])
           (clear-scope) (string-append (compile-create-function (get-class-scope) $3 loclnum) "\n" $7)
        )
       )]

     [(<METHOD-VM> <type> <subroutineName> <JSYM-LROUND-XML> <parameterList> <JSYM-RROUND-XML> <subroutineBody>)
      (begin
        ;(dumpSymbolTables)
        (let 
          ([loclnum (get-count-local)])
          (clear-scope) (string-append (compile-create-method (get-class-scope) $3  loclnum) "\n" $7)
        )
        )]

     [(<CONSTRUCTOR-VM> <VOID-XML> <subroutineName> <JSYM-LROUND-XML> <parameterList> <JSYM-RROUND-XML> <subroutineBody>)
      (begin
        ;(dumpSymbolTables)
        (let 
          ([loclnum (get-count-local)])
          (clear-scope) (string-append (compile-create-ctor (get-class-scope) $3 loclnum (get-count-field)) "\n" $7)
        )
        )]

     [(<FUNCTION-VM> <VOID-XML> <subroutineName> <JSYM-LROUND-XML> <parameterList> <JSYM-RROUND-XML> <subroutineBody>)
      (begin
        ;(dumpSymbolTables)
        (let 
          ([loclnum (get-count-local)])
          (clear-scope) (string-append (compile-create-function (get-class-scope) $3 loclnum) "\n" $7)
        )
        )]

     [(<METHOD-VM> <VOID-XML> <subroutineName> <JSYM-LROUND-XML> <parameterList> <JSYM-RROUND-XML> <subroutineBody>)
      (begin
        ;(dumpSymbolTables)
        (let 
          ([loclnum (get-count-local)])
          (clear-scope) (string-append (compile-create-method (get-class-scope) $3  loclnum) "\n" $7)
        )
        )]]

    ;parameterList -> ( (type varName) (','  type VarName)* )?
    [<parameterList>
     [()
      (begin 0)]
     [(<type> <varName> <ITERParamterList>)
      (begin (insert-parameter $2 $1) (for ([i $3]) (insert-parameter (list-ref i 1) (list-ref i 0))) (+ 1 (length $3)))]]
    ;subroutineBody -> '{' varDec* statements '}'
    [<subroutineBody> [(<JSYM-LCURLY-XML> <ITERVarDec> <statements> <JSYM-RCURLY-XML>) $3]]
    ;varDec -> 'var' type VarName (',' varName)* ';'
    [<varDec>
     [(<VAR-XML> <type> <varName> <ITERVarName> <JSYM-DOTCOM-XML>)
      (begin (insert-local-var $3 $2) (for ([i $4]) (insert-local-var i $2)) (void))]]
    ;className -> id
    [<className> [(<ID-VM>) $1]]
    ;subroutineName -> id
    [<subroutineName> [(<ID-VM>) $1]]
    ;varName -> id
    [<varName> [(<ID-VM>) $1]]
    ; <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
    ;statements -> statement*
    [<statements>
     [(<ITERStatement>) (begin (string-join $1 "\n"))]]
    ; statement -> letStatement | ifStatement | whileStatement | doStatement | returnStatement |
    [<statement> [(<letStatement>) $1] [(<ifStatement>) $1] [(<whileStatement>) $1] [(<doStatement>) $1] [(<returnStatement>) $1]]
    ;letStatement -> 'let' varName ('[' expression ']')? '=' expresion ';'
    [<letStatement>
     [(<LET-XML> <varName> <JSYM-LREC-XML> <expression> <JSYM-RREC-XML> <JSYM-EQULAS-XML> <expression> <JSYM-DOTCOM-XML>)
      (begin (compile-let-statement (searchVariable $2) $7 'vararray $4))]
     [(<LET-XML> <varName> <JSYM-EQULAS-XML> <expression> <JSYM-DOTCOM-XML>)
      (begin (compile-let-statement (searchVariable $2) $4 'var))]]
    ;ifStatement -> 'if' '(' expression ')' '{' statements '}' ('else' '{' statements '}' ) ?
    [<ifStatement>
     [(<IF-XML> <JSYM-LROUND-XML> <expression> <JSYM-RROUND-XML> <JSYM-LCURLY-XML> <statements> <JSYM-RCURLY-XML>)
      (begin (compile-if-statement $3 $6))]
     [(<IF-XML> <JSYM-LROUND-XML> <expression> <JSYM-RROUND-XML> <JSYM-LCURLY-XML> <statements> <JSYM-RCURLY-XML> <ELSE-XML> <JSYM-LCURLY-XML> <statements> <JSYM-RCURLY-XML>)
      (begin (compile-if-statement $3 $6 $10))]]
    ;whileStatement -> 'while' '(' expression ')' '{' statements '}'
    [<whileStatement>
     [(<WHILE-XML> <JSYM-LROUND-XML> <expression> <JSYM-RROUND-XML> <JSYM-LCURLY-XML> <statements> <JSYM-RCURLY-XML>)
      (begin (compile-while-statement $3 $6))]]
    ;doStatement -> 'do' subroutineCall ';'
    [<doStatement>
     [(<DO-XML> <subroutineCall> <JSYM-DOTCOM-XML>)
      (begin (compile-do-statement $2))]]
    ;ReturnStatement -> 'return' expression? ';'
    [<returnStatement>
     [(<RETURN-XML> <expression> <JSYM-DOTCOM-XML>)
      (begin (string-append $2 "\n" (compile-return-statement #t)))]

     [(<RETURN-XML> <JSYM-DOTCOM-XML>)
      (begin (compile-return-statement))]]
    ; ----------------------------------------------------------------
    ;expression -> term (op term)*
    [<expression>
     [(<term> <ITERExpression>)
      (begin (string-join (append (list $1) $2) "\n"))]]
    ;term ->  intgerConstant | stringConstant | keywordConstant | varName | VarName '[' expression ']' |
    ;         subroutineCall | '(' expression ')' | unaryOp term
    [<term>
     [(<INTEGER-VM>) $1]
     [(<STRING-XML>) $1]
     [(<KeyWordConstant>) $1]
     [(<varName>)
      (begin (compile-term (searchVariable $1) 'var))]
     [(<varName> <JSYM-LREC-XML> <expression> <JSYM-RREC-XML>)
      (begin (compile-term (searchVariable $1) 'arrayvar $3))]
     [(<subroutineCall>)  $1]
     [(<JSYM-LROUND-XML> <expression> <JSYM-RROUND-XML>)  $2]
     [(<unaryOp> <term>) (begin (string-append $2 "\n" $1))]]
    ;subroutineCall -> subroutineName '(' expressionList ')' | (className | varName)'.'subroutineName '(' expressionList ')'
    [<subroutineCall>
     [(<subroutineName> <JSYM-LROUND-XML> <expressionList> <JSYM-RROUND-XML>) 
     (begin (compile-subroutine-call (get-class-scope) $1 (list-ref $3 1) (string-join (list-ref $3 0) "\n")))]

     [(<className> <JSYM-DOT-XML> <subroutineName> <JSYM-LROUND-XML> <expressionList> <JSYM-RROUND-XML>)
      (begin
        (cond 
          [(inSymbolTable? $1) (compile-subroutine-var-class-call (searchVariable $1) $3 (list-ref $5 1) (string-join (list-ref $5 0) "\n"))]
          [else (compile-sunroutine-class-call $1 $3 (list-ref $5 1) (string-join (list-ref $5 0) "\n"))]
        )   
      )
    ]

     ;[(<varName> <JSYM-DOT-XML> <subroutineName> <JSYM-LROUND-XML> <expressionList> <JSYM-RROUND-XML>)
      ;(begin (compile-subroutine-var-class-call (searchVariable $1) $3 (list-ref $5 1) (string-join (list-ref $5 0) "\n")))]
    ]
    ;expressionList -> (expression (',' expression)* )?
    [<expressionList>
     [() (list '() 0)]
     [(<expression> <ITERExpressionList>)
      (begin (list (append (list $1) $2) (add1 (length $2))))]]
    ;op -> '+' | '-' | '*' | '/' | '&' | '|' | '<' | '>' | '='
    [<op>
     [(<JSYM-PLUS-VM>) $1]
     [(<JSYM-MINUS-VM>) "sub"]
     [(<JSYM-STAR-VM>) $1]
     [(<JSYM-SLASH-VM>) $1]
     [(<JSYM-ANDC-VM>) $1]
     [(<JSYM-ORC-VM>) $1]
     [(<JSYM-SMALLER-VM>) $1]
     [(<JSYM-BIGER-VM>) $1]
     [(<JSYM-EQULAS-VM>) $1]]
    ;unaryOp -> '-' | '~'
    [<unaryOp> [(<JSYM-MINUS-VM>) "neg"] [(<JSYM-NOTC-VM>) $1]]
    ;KeyWordConstant -> 'true' | 'false' | 'null' | 'this'
    [<KeyWordConstant> [(<TRUE-VM>) $1] [(<FALSE-VM>) $1] [(<NULL-VM>) $1] [(<THIS-VM>) $1]]
    ; ----------------------------------------------------------------
    ;###################### grammer rules #####################
    ; help grammar - Cause it sucks and I don't have tons of important stuff I created it by my self ...
    ;classVarDec*
    [<ITERclassVarDec> [() (void)] [(<classVarDec> <ITERclassVarDec>) (void)]] ;throw result - dont care
    ;subroutineDec*
    [<ITERsubRoutineDec> [() '()] [(<subRoutineDec> <ITERsubRoutineDec>) (append (list $1) $2)]]
    ;(',' varName)*
    [<ITERVarName> [() '()] [(<JSYM-COMMA-XML> <varName> <ITERVarName>) (append (list $2) $3)]]
    ;(','  type VarName)*
    [<ITERParamterList>
     [() '()]
     [(<JSYM-COMMA-XML> <type> <varName> <ITERParamterList>) (append (list (list $2 $3)) $4)]]
    ;VarDec*
    [<ITERVarDec> [() (void)] [(<varDec> <ITERVarDec>) (void)]]
    ;(op term)*
    [<ITERExpression>
     [() '()]
     [(<op> <term> <ITERExpression>)
      (begin
        (append (list $2 $1) $3))]]
    ;(',' expression)*
    [<ITERExpressionList>
     [() '()]
     [(<JSYM-COMMA-XML> <expression> <ITERExpressionList>)
      (begin
        (append (list $2) $3))]]
    ;statement*
    [<ITERStatement>
     [() '()]
     [(<statement> <ITERStatement>)
      (begin
        (append (list $1) $2))]]
    ; :::  XML PARSING PART :::
    ; each rule that contains a leaf will be translated to XML cause I am too lazy to create a new  function ...
    [<JSYM-LROUND-XML> [(JSYM-LROUND) (list 'symbol $1)]]
    [<JSYM-RROUND-XML> [(JSYM-RROUND) (list 'symbol $1)]]
    [<JSYM-LCURLY-XML> [(JSYM-LCURLY) (list 'symbol $1)]]
    [<JSYM-RCURLY-XML> [(JSYM-RCURLY) (list 'symbol $1)]]
    [<JSYM-LREC-XML> [(JSYM-LREC) (list 'symbol $1)]]
    [<JSYM-RREC-XML> [(JSYM-RREC) (list 'symbol $1)]]
    [<JSYM-DOT-XML> [(JSYM-DOT) (list 'symbol $1)]]
    [<JSYM-COMMA-XML> [(JSYM-COMMA) (list 'symbol $1)]]
    [<JSYM-DOTCOM-XML> [(JSYM-DOTCOM) (list 'symbol $1)]]
    [<JSYM-PLUS-XML> [(JSYM-PLUS) (list 'symbol $1)]]
    [<JSYM-MINUS-XML> [(JSYM-MINUS) (list 'symbol $1)]]
    [<JSYM-STAR-XML> [(JSYM-STAR) (list 'symbol $1)]]
    [<JSYM-SLASH-XML> [(JSYM-SLASH) (list 'symbol $1)]]
    [<JSYM-ANDC-XML> [(JSYM-ANDC) (list 'symbol $1)]]
    [<JSYM-ORC-XML> [(JSYM-ORC) (list 'symbol $1)]]
    [<JSYM-SMALLER-XML> [(JSYM-SMALLER) (list 'symbol $1)]]
    [<JSYM-BIGER-XML> [(JSYM-BIGER) (list 'symbol $1)]]
    [<JSYM-EQULAS-XML> [(JSYM-EQULAS) (list 'symbol $1)]]
    [<JSYM-NOTC-XML> [(JSYM-NOTC) (list 'symbol $1)]]
    [<CLASS-XML> [(CLASS) (list 'keyword $1)]]
    [<CONSTRUCTOR-XML> [(CONSTRUCTOR) (list 'keyword $1)]]
    [<FUNCTION-XML> [(FUNCTION) (list 'keyword $1)]]
    [<METHOD-XML> [(METHOD) (list 'keyword $1)]]
    [<FIELD-XML> [(FIELD) (list 'keyword $1)]]
    [<STATIC-XML> [(STATIC) (list 'keyword $1)]]
    [<VAR-XML> [(VAR) (list 'keyword $1)]]
    [<INT-XML> [(INT) (list 'keyword $1)]]
    [<CHAR-XML> [(CHAR) (list 'keyword $1)]]
    [<BOOLEAN-XML> [(BOOLEAN) (list 'keyword $1)]]
    [<VOID-XML> [(VOID) (list 'keyword $1)]]
    [<TRUE-XML> [(TRUE) (list 'keyword $1)]]
    [<FALSE-XML> [(FALSE) (list 'keyword $1)]]
    [<NULL-XML> [(NULL) (list 'keyword $1)]]
    [<THIS-XML> [(THIS) (list 'keyword $1)]]
    [<LET-XML> [(LET) (list 'keyword $1)]]
    [<DO-XML> [(DO) (list 'keyword $1)]]
    [<IF-XML> [(IF) (list 'keyword $1)]]
    [<ELSE-XML> [(ELSE) (list 'keyword $1)]]
    [<WHILE-XML> [(WHILE) (list 'keyword $1)]]
    [<RETURN-XML> [(RETURN) (list 'keyword $1)]]
    [<INTEGER-XML> [(INTEGER) (list 'integerConstant $1)]]
    [<STRING-XML> [(STRING) (list 'stringConstant $1)]]
    [<ID-XML> [(ID) (list 'identifier $1)]]
    ; --------- <><><><> ---------
    [<ID-VM> [(ID) $1]]
    [<INT-VM> [(INT) "int"]]
    [<CHAR-VM> [(CHAR) "char"]]
    [<BOOLEAN-VM> [(BOOLEAN) "boolean"]]
    [<INTEGER-VM>[(INTEGER) (begin (compile-integer $1))]]
    [<STRING-VM>[(STRING) (begin (compile-string $1))]]
    [<TRUE-VM> [(TRUE)(begin (compile-true))]]
    [<FALSE-VM>[(FALSE) (begin (compile-false))]]
    [<NULL-VM> [(NULL) (begin (compile-null))]]
    [<THIS-VM> [(THIS) (begin (compile-this))]]
    ; unary operations :
    [<JSYM-NOTC-VM> [(JSYM-NOTC) "not"]]
    ;[<JSYM-MINUS-VM> [(JSYM-MINUS)      (void)]]; defined in upper levels
    ;binary operations :
    [<JSYM-PLUS-VM> [(JSYM-PLUS) "add"]]
    [<JSYM-MINUS-VM> [(JSYM-MINUS) (void)]] ; defined in upper levels
    [<JSYM-STAR-VM> [(JSYM-STAR) "call Math.multiply 2"]]
    [<JSYM-SLASH-VM> [(JSYM-SLASH) "call Math.divide 2"]]
    [<JSYM-ANDC-VM> [(JSYM-ANDC) "and"]]
    [<JSYM-ORC-VM> [(JSYM-ORC) "or"]]
    [<JSYM-SMALLER-VM> [(JSYM-SMALLER) "lt"]]
    [<JSYM-BIGER-VM> [(JSYM-BIGER) "gt"]]
    [<JSYM-EQULAS-VM> [(JSYM-EQULAS) "eq"]]
    ;leaf with callback
    [<METHOD-VM> [(METHOD)(begin (add-this-parameter) (void))]]
    [<FUNCTION-VM> [(FUNCTION) (void)]]
    [<CONSTRUCTOR-VM> [(CONSTRUCTOR) (void)]]

    ; --------- <><><><> ---------
    )))