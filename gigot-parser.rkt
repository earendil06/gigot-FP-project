#lang racket
;;; gigot-parser.rkt -- Traducteur Gigot vers Scheme un peu spartiate...

(require parser-tools/lex 
         parser-tools/yacc 
         (prefix-in : parser-tools/lex-sre))     ; pour les abreviations :or, etc

(provide get-lexeme parse)

(define debug? #f)       ; #t en cas d'erreur du parser...

;;; ********** le lexeur (analyseur lexical) **********

(define-lex-abbrevs
  (eol (:or #\newline #\return))
  (blanc (:or #\tab #\space))
  (letter (:or (:/ "a" "z") (:/ "A" "Z")))
  (digit (:/ "0" "9"))
  (int (:+ digit))
  (float (:: int "." int))
  (char (:- (:/ " " "~") "'"))
  (chaine (:: "'" (:* char) "'"))
  (var (:: letter (:* (:or letter digit)))))

(define-tokens value-tokens (NUM VAR STRING))

(define-empty-tokens op-tokens 
  (LPAR RPAR + - * / ^ =  < <= == EOL FINI GOTO INPUT PRINT PRINTLN END NEG IF THEN ELSE))

(define get-lexeme
  (lexer
   [(eof) 'FINI]
   ; on invoque recursivement le lexer pour sauter les espaces. La variable input-port est liee dans lexer
   [(repetition 1 +inf.0 blanc) (get-lexeme input-port)]
   [eol 'EOL]
   [(union "=" "+" "-" "*" "/" "<" "<=" "==" "print" "println" "goto" "if" "then" "else" "input" "end") (string->symbol (string-upcase lexeme))]
   ["(" 'LPAR]
   [")" 'RPAR]
   [chaine (token-STRING (substring lexeme 1 (- (string-length lexeme) 1)))]
   [var (token-VAR (string->symbol lexeme))]                      ; une variable
   [(union int float) (token-NUM (string->number lexeme))]))      ; un entier ou un flottant est un nombre

;;; ********** le parseur (analyseur syntaxique) ************

(define (goto? instr)
  (and (pair? instr) (equal? (car instr) 'goto)))

(define parse
  (parser
   
   (start prog)
   
   (end FINI)
   
   (tokens value-tokens op-tokens)
   
   (error (lambda (a b c) 
            (define err (list a b c))
            (printf "Parsing error : ~a\n" err)
            (when (token? b) (printf "Error in ~a\n" (token-value b)))))   ; hum
   
   (precs (nonassoc < <= == =)
          (left - +)
          (left * /))
   
   (grammar       ; on se laisse guider par la grammaire de GIGOT et on produit de l'AIL (pseudo-scheme)
    
    (prog  [(ninstrs) `(,@$1)])
    
    (ninstrs [() '()]
             [(ninstrs ninstr) `(,@$1 ,$2)])
    
    (ninstr [(NUM instr EOL) (begin (when debug? (printf "Line ~a ok\n" $1)) `(,$1 ,$2))])
    
    (instr [(PRINT expr) (if (string? $2) `(printf ,$2) `(printf "~a" ,$2))]
           [(PRINTLN expr) (if (string? $2) `(printf ,(string-append $2 "\n")) `(printf "~a\n" ,$2))]
           [(IF cond-expr THEN GOTO NUM ELSE GOTO NUM)`(if ,$2 (goto ,$5) (goto ,$8))]
           [(GOTO NUM) `(goto ,$2)]
           [(VAR = expr) `(set! ,$1 ,$3)]
           [(END) '(end)])
    
    (expr [(NUM) $1]
          [(VAR) $1]
          [(STRING) $1]
          [(expr + expr) `(+ ,$1 ,$3)]
          [(expr - expr) `(- ,$1 ,$3)]
          [(expr * expr) `(* ,$1 ,$3)]
          [(expr / expr) `(/ ,$1 ,$3)]
          [(- expr) `(- ,$2)]
          [(LPAR expr RPAR) $2])
    
    (cond-expr [(expr < expr) `(< ,$1 ,$3)]
               [(expr <= expr) `(<= ,$1 ,$3)]
               [(expr == expr) `(= ,$1 ,$3)]))))

 
  