#lang racket/gui

(require "gigot-parser.rkt" )         ; pour les fonctions get-lexeme et parse

 
(define prgm-gigot
  "10 goto 15
12 end
15 println 'Ce programme calcule 10!'
20 n = 10
28 i = 1
30 res = 1
35 if ((n))<i then goto 80 else goto 40
40 res=res*i
60 i = -i+2*i-(34-35)
70 goto 35
80 print n
81 print '! = '
83 println res
92 goto 12
")

"Voici un programme prgm-gigot en syntaxe GIGOT :"

(printf "\n~a\n" prgm-gigot)

(define (test-lexer)
  (define p-in (open-input-string prgm-gigot))    ; un port d'entree sur la string (cours 9)
  (for ([i (in-naturals)])                        ; i est le numero du lexeme courant
    (define x (get-lexeme p-in))                  ; lecture d'une unite lexicale dans le port p-in
    #:break (equal? x 'FINI)                      ; fin du programme
    (printf "[~a] Lecture de ~s\n" i x)))

"Test du lexeur sur ce programme :"
(printf "\n")
(test-lexer)
(printf "\n")

(define lexeur (let ((p-in (open-input-string prgm-gigot)))
                 (lambda () (get-lexeme p-in))))

(define compil0  (parse lexeur))      ; la traduction en listes Scheme dans un programme compil0 en AIL

"--------------------------- PHASE 0 ---------------------------"
"La traduction en AIL du programme prgm-gigot se nomme compil0 :"
(printf "\n")

compil0

(printf "\n")

"****************** DEBUT DU PROJET ETUDIANT *******************"
"-------------------------- PHASE 0.5 --------------------------"

(define-syntax while
  (syntax-rules ()
    ((while test e1 e2 ...) (let ()
                              (define (iter)
                                (if (not test)
                                    (void)
                                    (begin e1 e2 ... (iter))))
                              (iter)))))


(define (creer-expr numero fonction)
  (list numero fonction))

(define (take-infos expr info)
  (case info
    ((get-instr) (cadr expr))
    ((get-numero) (car expr))
    ((is-goto?) (equal? (caadr expr) 'goto))
    ((is-if?) (equal? (caadr expr) 'if))
    ((is-set?) (equal? (caadr expr) 'set!))
    ((is-end?) (equal? (caadr expr) 'end))
    ((is-print?) (equal? (caadr expr) 'printf))
    ((getset-var) (if (equal? (caadr expr) 'set!)
                      (cadadr expr)
                      (error "not a \"set!\" instruction")))
    ((getset-varval) (if (equal? (caadr expr) 'set!)
                         (third (cadr expr))
                         (error "not a \"set!\" instruction")))
    ((getif-cond) (if (equal? (caadr expr) 'if)
                      (cddadr expr)
                      (error "not an \"if\" instruction")))
    ((getif-pred) (if (equal? (caadr expr) 'if)
                      (second (cadr expr))
                      (error "not an \"if\" instruction")))
    ((getprint) (if (equal? (caadr expr) 'printf)
                    (second (cadr expr))
                    (error "not a \"printf\" instruction")))
    ((getgoto-renvoie) (if (equal? (caadr expr) 'goto)
                           (cadadr expr)
                           (error "not a \"goto\" instruction")))
    (else (error "fonction indisponnible"))))


(define (variables code)
  (define (iter code set)
    (cond ((null? code) set)
          ((take-infos (car code) 'is-set?) (iter (cdr code)
                                                  (set-add set (take-infos (car code) 'getset-var))))
          (else (iter (cdr code) set))))
  (set->list (iter code (set))))

(define (end->goto code)
  (define pt-end (+ (take-infos (car (reverse code)) 'get-numero) 10))
  (define (remplacer-end expr)
    (if (take-infos expr 'is-end?) 
        (creer-expr (take-infos expr 'get-numero) `(goto ,pt-end))
        expr))
  (define (iter)
    (cond ((null? code) (creer-expr 1 '(printf "**END OF PROGRAM**\n")))
          ((null? (cdr code)) (list (car code) (creer-expr pt-end
                                                           '(printf "**END OF PROGRAM**\n"))))
          (else (cons (car code) (end->goto (cdr code))))))
  (map remplacer-end (iter)))


(define (gotos code)
  (define pt-entree (take-infos (car code) 'get-numero))
  (define (iter code)
    (if (null? code)
        null
        (let ((expr (car code)))
          (cond ((take-infos expr 'is-goto?) (cons (take-infos expr 'getgoto-renvoie)
                                                   (iter (cdr code))))
                ((take-infos expr 'is-if?)
                 (let ((liste (map (lambda (x) (cadr x)) (take-infos expr 'getif-cond))))
                   (cons (car liste)
                         (cons (cadr liste)
                               (iter (cdr code))))))
                (else (iter (cdr code)))))))
  (cons pt-entree (iter code)))

(define (decouper code)
  (define (decouper-apres nLigne)
    (let ((CODE code))
      (let/ec return
        (while #t
               (if (= nLigne (take-infos (car CODE) 'get-numero))
                   (return CODE)
                   (set! CODE (cdr CODE)))))))
  (map decouper-apres LABELS))


(define (elaguer code)
  (define (elaguer-element elmt)
    (cond ((null? elmt) null)
          ((or (take-infos (car elmt) 'is-goto?)
               (take-infos (car elmt) 'is-if?))
           (list (car elmt)))
          (else (cons (car elmt) (elaguer-element (cdr elmt))))))
  (map elaguer-element code))


(define (forcer-goto code)
  (define numero-fonc-if
    (let ((liste (filter (lambda (x) (and (take-infos (car x) 'is-if?) (null? (cdr x)))) code)))
      (if (null? liste)
          '()
          (caaar liste))))
  (define (remplacer ligne)
    (define (remplacer-expr expr)
      (if (take-infos expr 'is-if?)
          `(,(take-infos expr 'get-numero) (goto ,numero-fonc-if))
          expr))
    (if (null? (cdr ligne))
        ligne
        (map remplacer-expr ligne)))
  (map remplacer code))


(define (elim-nums code)
  (define (num->fonc ligne)
    (let ((numero-fonc (take-infos (car ligne) 'get-numero))
          (liste-instr (map (lambda (x) (take-infos x 'get-instr))
                            ligne)))
      `(define (,(string->symbol (format "~a~a" 'fonc numero-fonc))) ,@liste-instr)))
  (map num->fonc code))


(define (elim-gotos code)
  (define (elim-gotos-if instr)
    (let ((liste (list (cadr instr) (caddr instr) (cadddr instr))))
      `(if ,(car liste) 
           (,(string->symbol (format "~a~a" 'fonc (cadr (cadr liste)))))
           (,(string->symbol (format "~a~a" 'fonc (cadr (caddr liste))))))))
  (define (elim-gotos-ligne ligne)
    (map (lambda (x) (cond ((and (list? x) (equal? (car x) 'goto))
                            `(,(string->symbol (format "~a~a" 'fonc  (cadr x)))))
                           ((and (list? x) (equal? (car x) 'if)) (elim-gotos-if x))
                           (else x)))
         ligne))
  (map elim-gotos-ligne code))

(define (build-program code)
  `(define (programme)
     (printf "**START OF PROGRAM**\n")
     (let ,(map (lambda (var) (list var 0)) VARS)
       ,@code
       ,(list (string->symbol (format "~a~a" 'fonc (car LABELS)))))))


(define VARS (variables compil0))
(define compil1 (end->goto compil0))
(define LABELS (gotos compil1))
(define compil2 (decouper compil1))
(define compil3 (elaguer compil2))
(define compil4 (forcer-goto compil3))
(define compil5 (elim-nums compil4))
(define compil6 (elim-gotos compil5))
(define compil7 (build-program compil6))



