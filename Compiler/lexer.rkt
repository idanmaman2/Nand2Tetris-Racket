;ALL the rights reserved to the one and only IDAN MAMAN - by the MIT License .
;NAND2TETRIS LEXER
;imports
#lang racket/base
(require racket/string)
(require racket/list)
(require parser-tools/lex)
(require parser-tools/lex-sre)
(require "tokens.rkt")

;exports
; the racket lexer
(provide Nand2TetrisLexer)
; function for using the lexer on file - returns list of tokens
(provide fileAnlyze)

;lexer
(define Nand2TetrisLexer
  ;keyword tokens rule
  (lexer [(union "class" "constructor" "function" "method" 
                "field" "static" "var" "int" "char" "boolean"
                 "void" "true" "false" "null" "this" "let" 
                 "do" "if" "else" "while" "return")
          ;=>
          (begin
            (token-KEYWORD lexeme))]
         ;symbol tokens rule
         [(union "(" ")" "{" "}" 
                "[" "]" "." "," 
                ";" "+" "-" "*" 
                "/" "&" "|" "<" 
                ">" "=" "~")
          ;=>
          (begin
            (token-SYMBOL lexeme))]
         ;number tokens rule between 0 to 32767 - if the number is above it will be splitted to a few tokens ... 
         ;([0-9]|[1-9][0-9]{1,3}|[12][0-9]{4}|3[01][0-9]{3}|32[0-6][0-9]{2}|327[0-5][0-9]|3276[0-7])
         [(or (:(char-range #\0 #\9))
              (: (char-range #\1 #\9) (repetition 1 3 (char-range #\0 #\9)))
              (: "12" (repetition 4 4 (char-range #\0 #\9)))
              (: "3" (char-range #\0 #\1) (repetition 3 3 (char-range #\0 #\9)))
              (: "32" (char-range #\0 #\6) (repetition 2 2 (char-range #\0 #\9)))
              (: "327" (char-range #\0 #\5) (char-range #\0 #\9))
              (: "3276" (char-range #\0 #\7)))
          ;=>
          (begin
            (token-INTEGER lexeme))]
         ;string tokens rule
         [(: #\" (repetition 0 +inf.0 (~ #\")) #\")
          (begin
            (token-STRING (substring lexeme 1 (sub1 (string-length lexeme)))))]
         ;id tokens rule
         [(: (or lower-case upper-case #\_) (* (or #\_ lower-case upper-case numeric)))
          ;=>
          (begin
            (token-ID lexeme))]
         ; comment type 1 //
         [(: "//" (repetition 0 +inf.0 (~ #\newline)) #\newline) (void)]
         ;comment type 2 \**\
         [(: "/*" (repetition 0 +inf.0 (complement (intersection "*" "/" ))) "*/") (void)]
         ;igoring all those chars 
         [#\space (void)]
         [#\newline (void)]
         [whitespace (void)]
         [blank (void)]
         ;stop lexing 
         [(eof) eof]))
(define (fileAnlyze ftest)

  (define res '())

  (define (anlyzeRec)
    (let ([line (Nand2TetrisLexer ftest)])
      (writeln line)
      (cond
        [(eof-object? line) (void)]
        [(void? line) (anlyzeRec)]
        [else
         (set! res (append res (list line)))
         (anlyzeRec)])))
  (anlyzeRec)
  res)