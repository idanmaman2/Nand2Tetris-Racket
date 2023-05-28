#lang racket/base
(require racket/string)
(require racket/list)
(require parser-tools/cfg-parser)
(require parser-tools/lex)	

(require "tokens.rkt")

;Idan's Nand2Tetris Compiler under the MIT License




; because the cfg-parser can take variety of symbols on the same token name - I created conversion from the classic token to one that the CFG understand ( I not so nice But i am not a slave and i am not going to change all the code)

(define-tokens stopTokens (EOF))
(define-tokens  keywords-tokensC (
        CLASS
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
        RETURN
    )  )  
(define-tokens  symbol-tokensC(

        JSYM-RROUND
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
        JSYM-NOTC  ) )
(define (token-convert classicToken)
    (define convertTableKEYWORD (hash 
            "class" token-CLASS
            "constructor" token-CONSTRUCTOR
            "function" token-FUNCTION
            "method" token-METHOD
            "field" token-FIELD
            "static" token-STATIC
            "var" token-VAR
            "int" token-INT
            "char" token-CHAR
            "boolean"   token-BOOLEAN
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
            "return" token-RETURN
    ))
    (define convertTableSymbol (hash 
            "(" token-JSYM-RROUND
            ")" token-JSYM-LROUND
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
            "~" token-JSYM-NOTC
    ))
    
    (let 
    (
        [name (token-name classicToken)]
        [val (token-value classicToken)]
    )
    
    (cond 
    [(equal? name 'KEYWORD) ( (hash-ref convertTableKEYWORD val) val)]
    [(equal? name 'SYMBOL) ( (hash-ref convertTableSymbol name ) val )]
    [else classicToken])))




(define jackParser (cfg-parser 

    (tokens keywords-tokens stopTokens keywords-tokensC symbol-tokensC INT-tokens STR-tokens ID-tokens)
    (start <class> )
    (end EOF)
    (error (lambda (a b c ) (error 'JACK_PARSING "failed to parse ~s" c  )))
    (grammar 
    ;###################### grammer rules ##################### 
    ; ----------------------------------------------------------------
    ;class -> 'class' className '{' classVarDec* subroutineDec* '}' 
    [<class>  [ <className>  <classVarDec> <subRoutineDec>  ] (list ) ]
    ;classVarDec -> ('static' | 'field' ) type VarName (',' varName)* ';' 
    [<classVarDec> [()]  [()] ]
    ;type -> 'int' | 'char' | 'boolean' | className 
    [<type>  [()]  [()]  [()]  [()]]
    ;subRoutineDec -> ('construcntor' | 'function' | 'method' ) ('void' | type ) subroutineName
    [<subRoutineDec>  [()]  [()]  [()]  [()]  [()]  [()]  [()]]
    ;parameterList -> ( (type varName) (','  type VarName)* )? 
    [<parameterList>  [()]]
    ;subroutineBody -> '{' varDec* statements '}' 
    [<subroutineBody>  [()]]
    ;varDec -> 'var' type VarName (',' varName)* ';'
    [<varDec>  [()]]
    ;className -> id 
    [<className>  [()]]
    ;subroutineName -> id 
    [<subroutineName>  [()]]
    ;varName -> id
    [<varName>  [()]] 
    ; ----------------------------------------------------------------
    ;statements -> statement* 
    [<statements>  [()]]
    ; statement -> letStatement | ifStatement | whileStatement | doStatement | returnStatement |
    [<statement>  [()]  [()]  [()]  [()]  [()] ]
    ;letStatement -> 'let' varName ('[' expression ']')? '=' expresion ';' 
    [<letStatement>  [()]]
    ;ifStatement -> 'if' '(' expression ')' '{' statements '}' ('else' '{' statements '}' ) ? 
    [<ifStatement>  [()]]
    ;whileStatement -> 'while' '(' expression ')' '{' statements '}'
    [<whileStatement>  [()]]
    ;doStatement -> 'do' subroutineCall ';'
    [<doStatement>  [()]]
    ;ReturnStatement -> 'return' expression? ';'
    [<ReturnStatement>  [()]]
    ; ----------------------------------------------------------------
    ;expression -> term (op term)*
    [<expression>  [()]] 
    ;term ->  intgerConstant | stringConstant | keywordConstant | varName | VarName '[' expression ']' | 
    ;         subroutineCall | '(' expression ')' | unaryOp term 
    [<term>  [()]  [()]  [()]  [()]  [()]  [()]  [()]  [()] ]
    ;subroutineCall -> subroutineName '(' expressionList ')' | (className | varName)'.'subroutineName '(' expressionList ')' 
    [<subroutineCall>  [()]  [()]  [()] ]
    ;expressionList -> (expression (',' expression)* )? 
    [<expressionList>  [()] ]
    ;op -> '+' | '-' | '*' | '/' | '&' | '|' | '<' | '>' | '='  
    [<op>  [()]  [()]  [()]  [()]  [()]  [()]  [()]  [()]  [()]]
    ;unaryOp -> '-' | '~'
    [<unaryOp>  [()]  [()] ]
    ;KeyWordConstant -> 'true' | 'false' | 'null' | 'this' 
    [<KeyWordConstant>  [()]  [()]  [()]  [()]]
    ; ----------------------------------------------------------------
    ;###################### grammer rules ##################### 
    
    ; help grammar - Cause it sucks and I don't have tons of important stuff I created it by my self ... 
    [<ITERclassVarDec> 
    [() ()]
    [(<classVarDec> <ITERclassVarDec>) (list $1 $2)]
    ] 
    [<ITERsubRoutineDec> 
    [() ()]
    [(<subRoutineDec> <ITERsubRoutineDec>) (list $1 $2)]
    ]
    )
) )




