#lang racket/base
(require racket/string)
(require racket/list)
(require parser-tools/cfg-parser)
(require parser-tools/lex)
(require (for-syntax racket/base syntax/boundmap parser-tools/private-lex/token-syntax))
(require "tokens.rkt")


;exports 
(provide type2TokensConvertor)
(provide XMLjackParser)


;Idan's Nand2Tetris Compiler under the MIT License

; because the cfg-parser can take variety of symbols on the same token name - I created conversion from the classic token to one that the CFG understand ( I not so nice But i am not a slave and i am not going to change all the code)

(define-tokens keywords-tokensC
            (CLASS 
            CONSTRUCTOR
              FUNCTION
              METHOD
              FIELD
              STATIC
              INT
              VAR
              CHAR
              BOOLEAN
              VOID
              TRUE
              FALSE
              NULL
              THIS
              LET
              DO
              IF
              ELSE
              WHILE
              RETURN))
(define-tokens symbol-tokensC (JSYM-RROUND 
                            JSYM-LROUND
                            JSYM-LCURLY
                            JSYM-RCURLY
                            JSYM-LREC
                            JSYM-RREC
                            JSYM-DOT
                            JSYM-COMMA
                            JSYM-DOTCOM
                            JSYM-PLUS
                            JSYM-MINUS
                            JSYM-STAR
                            JSYM-SLASH
                            JSYM-ANDC
                            JSYM-ORC
                            JSYM-BIGER
                            JSYM-SMALLER
                            JSYM-EQULAS
                            JSYM-NOTC))
(define (token-convert classicToken)
  (define convertTableKEYWORD
    (hash 
      "class" token-CLASS
      "constructor" token-CONSTRUCTOR
      "function"    token-FUNCTION
      "method"      token-METHOD
      "field"       token-FIELD
      "static"      token-STATIC
      "var"         token-VAR
      "int"         token-INT
      "char"        token-CHAR
      "boolean"     token-BOOLEAN
      "void"        token-VOID
      "true"        token-TRUE
      "false"       token-FALSE
      "null"        token-NULL
      "this"        token-THIS
      "let"         token-LET
      "do"          token-DO
      "if"          token-IF
      "else"        token-ELSE
      "while"       token-WHILE
      "return"      token-RETURN))
  (define convertTableSymbol
    (hash 
        "(" token-JSYM-LROUND
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

(define (type2TokensConvertor tokenList) 


(map token-convert tokenList)
)

(define XMLjackParser
  (cfg-parser
   (tokens keywords-tokens stopTokens keywords-tokensC symbol-tokensC INT-tokens STR-tokens ID-tokens)
   (start <class>)
   (end EOF)
   (error (lambda (a b stx) 
              (error 'parse "@@@ JACK COMPILER @@@  failed at:\n\ttoken_nickname : `~a`\n\ttoken_symbol:  `~a`"  b stx)))
   (grammar
    ;###################### grammer rules #####################
    ; ----------------------------------------------------------------
    ;class -> 'class' className '{' classVarDec* subroutineDec* '}'
    [<class>
     [(<CLASS-XML> <className> <JSYM-LCURLY-XML> <ITERclassVarDec> <ITERsubRoutineDec> <JSYM-RCURLY-XML>)
      ( append (list 'class $1 $2 $3)  $4  $5 (list $6))]]
    ;classVarDec -> ('static' | 'field' ) type VarName (',' varName)* ';'
    [<classVarDec>
     [(<STATIC-XML> <type> <varName> <ITERVarName> <JSYM-DOTCOM-XML>)
      (append (list 'classVarDec $1 $2 $3) $4 (list $5))]
     [(<FIELD-XML> <type> <varName> <ITERVarName> <JSYM-DOTCOM-XML>)
      (append (list 'classVarDec $1 $2 $3) $4 (list $5))]]
    ;type -> 'int' | 'char' | 'boolean' | className
    [<type> [(<INT-XML>) $1] [(<CHAR-XML>) $1] [(<BOOLEAN-XML>) $1] [(<className>) $1]]
    ;subRoutineDec -> ('construcntor' | 'function' | 'method' ) ('void' | type ) subroutineName '(' paramterList ') subroutineBody
    [<subRoutineDec>
     [(<CONSTRUCTOR-XML> <type> <subroutineName> <JSYM-LROUND-XML> <parameterList> <JSYM-RROUND-XML> <subroutineBody>) (list 'subroutineDec $1 $2 $3 $4 $5 $6 $7)]
     [(<FUNCTION-XML> <type> <subroutineName> <JSYM-LROUND-XML> <parameterList> <JSYM-RROUND-XML> <subroutineBody>) (list 'subroutineDec $1 $2 $3 $4 $5 $6 $7)]
     [(<METHOD-XML> <type> <subroutineName> <JSYM-LROUND-XML> <parameterList> <JSYM-RROUND-XML> <subroutineBody>) (list 'subroutineDec $1 $2 $3 $4 $5 $6 $7)]
     [(<CONSTRUCTOR-XML> <VOID-XML> <subroutineName> <JSYM-LROUND-XML> <parameterList> <JSYM-RROUND-XML> <subroutineBody>) (list 'subroutineDec $1 $2 $3 $4 $5 $6 $7)]
     [(<FUNCTION-XML> <VOID-XML> <subroutineName> <JSYM-LROUND-XML> <parameterList> <JSYM-RROUND-XML> <subroutineBody>) (list 'subroutineDec $1 $2 $3 $4 $5 $6 $7)]
     [(<METHOD-XML> <VOID-XML> <subroutineName> <JSYM-LROUND-XML> <parameterList> <JSYM-RROUND-XML> <subroutineBody>) (list 'subroutineDec $1 $2 $3 $4 $5 $6 $7)]]
    ;parameterList -> ( (type varName) (','  type VarName)* )?
    [<parameterList>
     [() (list 'parameterList)]
     [(<type> <varName> <ITERParamterList>) (append (list 'parameterList $1 $2) $3)]]
    ;subroutineBody -> '{' varDec* statements '}'
    [<subroutineBody>
     [(<JSYM-LCURLY-XML> <ITERVarDec> <statements> <JSYM-RCURLY-XML>)
     (append  (list 'subroutineBody $1)  $2 (list $3 $4) )]]
    ;varDec -> 'var' type VarName (',' varName)* ';'
    [<varDec> [(<VAR-XML> <type> <varName> <ITERVarName> <JSYM-DOTCOM-XML>) (append (list 'varDec $1 $2 $3)  $4 (list $5))]]
    ;className -> id
    [<className> [(<ID-XML>) $1]]
    ;subroutineName -> id
    [<subroutineName> [(<ID-XML>) $1]]
    ;varName -> id
    [<varName> [(<ID-XML>) $1]]
    ; ----------------------------------------------------------------
    ;statements -> statement*
    [<statements>
     [(<ITERStatement>) (append (list 'statements ) $1 ) ]
    ]
    ; statement -> letStatement | ifStatement | whileStatement | doStatement | returnStatement |
    [<statement>
     [(<letStatement>)    $1]
     [(<ifStatement>)     $1]
     [(<whileStatement>)  $1]
     [(<doStatement>)     $1]
     [(<returnStatement>) $1]]
    ;letStatement -> 'let' varName ('[' expression ']')? '=' expresion ';'
    [<letStatement>
     [(<LET-XML> <varName>
                 <JSYM-LREC-XML>
                 <expression>
                 <JSYM-RREC-XML>
                 <JSYM-EQULAS-XML> 
                 <expression>
                 <JSYM-DOTCOM-XML>)
      (list 'letStatement $1 $2 $3 $4 $5 $6 $7 $8)]
     [(<LET-XML> <varName> <JSYM-EQULAS-XML> <expression> <JSYM-DOTCOM-XML>)
      (list 'letStatement $1 $2 $3 $4 $5)]]
    ;ifStatement -> 'if' '(' expression ')' '{' statements '}' ('else' '{' statements '}' ) ?
    [<ifStatement>
     [(<IF-XML> <JSYM-LROUND-XML>
                <expression>
                <JSYM-RROUND-XML>
                <JSYM-LCURLY-XML>
                <statements>
                <JSYM-RCURLY-XML>)
      (list 'ifStatement $1 $2 $3 $4 $5 $6 $7)]
     [(<IF-XML> <JSYM-LROUND-XML>
                <expression>
                <JSYM-RROUND-XML>
                <JSYM-LCURLY-XML>
                <statements>
                <JSYM-RCURLY-XML>
                <ELSE-XML>
                <JSYM-LCURLY-XML>
                <statements>
                <JSYM-RCURLY-XML>)
      (list 'ifStatement $1 $2 $3 $4 $5 $6 $7 $8 $9 $10 $11)]]
    ;whileStatement -> 'while' '(' expression ')' '{' statements '}'
    [<whileStatement>
     [(<WHILE-XML> <JSYM-LROUND-XML>
                   <expression>
                   <JSYM-RROUND-XML>
                   <JSYM-LCURLY-XML>
                   <statements>
                   <JSYM-RCURLY-XML>)
      (list 'whileStatement $1 $2 $3 $5 $5 $6 $7)]]
    ;doStatement -> 'do' subroutineCall ';'
    [<doStatement> [(<DO-XML> <subroutineCall> <JSYM-DOTCOM-XML>) (append (list 'doStatement $1) $2 (list $3) )]]
    ;ReturnStatement -> 'return' expression? ';'
    [<returnStatement>
     [(<RETURN-XML> <expression> <JSYM-DOTCOM-XML>) (list 'returnStatement $1 $2 $3)]
     [(<RETURN-XML> <JSYM-DOTCOM-XML>) (list 'returnStatement $1 $2)]]
    ; ----------------------------------------------------------------
    ;expression -> term (op term)*
    [<expression> [(<term> <ITERExpression>) (append (list 'expression $1) $2)]]
    ;term ->  intgerConstant | stringConstant | keywordConstant | varName | VarName '[' expression ']' |
    ;         subroutineCall | '(' expression ')' | unaryOp term
    [<term>
     [(<INTEGER-XML>) (list 'term $1)]
     [(<STRING-XML>) (list 'term $1)]
     [(<KeyWordConstant>) (list 'term $1)]
     [(<varName>) (list 'term $1)]
     [(<varName> <JSYM-LREC-XML> <expression> <JSYM-RREC-XML>) (list 'term $1 $2 $3 $4)]
     [(<subroutineCall>) (append (list 'term) $1)]
     [(<JSYM-LROUND-XML> <expression> <JSYM-RROUND-XML>) (list 'term $1 $2 $3)]
     [(<unaryOp> <term>) (list 'term $1 $2)]]
    ;subroutineCall -> subroutineName '(' expressionList ')' | (className | varName)'.'subroutineName '(' expressionList ')'
    [<subroutineCall>
     [(<subroutineName> <JSYM-LROUND-XML>
                        <expressionList>
                        <JSYM-RROUND-XML>)
      (list  $1 $2 $3 )]
     [(<className> <JSYM-DOT-XML>
                   <subroutineName>
                   <JSYM-LROUND-XML>
                   <expressionList>
                   <JSYM-RROUND-XML>)
      (list  $1 $2 $3 $4 $5 $6)]
     [(<varName> <JSYM-DOT-XML> <subroutineName> <JSYM-LROUND-XML> <expressionList> <JSYM-RROUND-XML>)
      (list  $1 $2 $3 $4 $5 $6)]]
    ;expressionList -> (expression (',' expression)* )?
    [<expressionList>
     [() (list 'expressionList)]
     [(<expression> <ITERExpressionList>) (append (list 'expressionList $1) $2)]]
    ;op -> '+' | '-' | '*' | '/' | '&' | '|' | '<' | '>' | '='
    [<op>
     [(<JSYM-PLUS-XML>) $1]
     [(<JSYM-MINUS-XML>) $1]
     [(<JSYM-STAR-XML>) $1]
     [(<JSYM-SLASH-XML>) $1]
     [(<JSYM-ANDC-XML>) $1]
     [(<JSYM-ORC-XML>) $1]
     [(<JSYM-SMALLER-XML>) $1]
     [(<JSYM-BIGER-XML>) $1]
     [(<JSYM-EQULAS-XML>) $1]]
    ;unaryOp -> '-' | '~'
    [<unaryOp> [(<JSYM-MINUS-XML>) $1] [(<JSYM-NOTC-XML>) $1]]
    ;KeyWordConstant -> 'true' | 'false' | 'null' | 'this'
    [<KeyWordConstant> [(<TRUE-XML>) $1] [(<FALSE-XML>) $1] [(<NULL-XML>) $1] [(<THIS-XML>) $1]]
    ; ----------------------------------------------------------------
    ;###################### grammer rules #####################
    ; help grammar - Cause it sucks and I don't have tons of important stuff I created it by my self ...
    
    ;classVarDec*
    [<ITERclassVarDec> [() '()] [(<classVarDec> <ITERclassVarDec>) (append (list $1) $2)]]
    ;subroutineDec*
    [<ITERsubRoutineDec> [() '()] [(<subRoutineDec> <ITERsubRoutineDec>) (append (list $1) $2)]]
    ;(',' varName)*
    [<ITERVarName> [() '()] [(<JSYM-COMMA-XML> <varName> <ITERVarName>) (append (list $1 $2 ) $3)]]
    ;(','  type VarName)*
    [<ITERParamterList>
     [() '()]
     [(<JSYM-COMMA-XML>  <type> <varName> <ITERParamterList>) (append (list $1 $2 $3) $4)]]
    ;VarDec*
    [<ITERVarDec> [() '()] [(<varDec> <ITERVarDec>) (append (list $1) $2)]]
    ;(op term)*
    [<ITERExpression> [() '()] [(<op> <term> <ITERExpression>) (append (list $1 $2) $3)]]
    ;(',' expression)*
    [<ITERExpressionList>
     [() '()]
     [(<JSYM-COMMA-XML> <expression> <ITERExpressionList>) (append (list $1 $2) $3)]]
   ;statement*
   [<ITERStatement> 
    [() '( )]
    [(<statement> <ITERStatement> ) (append (list $1) $2)]]
   
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
    [<INTEGER-XML> [(INTEGER) (list 'integerConstant  $1)]]
    [<STRING-XML> [(STRING) (list 'stringConstant $1)]]
    [<ID-XML> [(ID) (list 'identifier $1)]])
))