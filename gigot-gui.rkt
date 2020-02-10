#lang racket/gui

(require "gigot-parser.rkt"
         "gigot.rkt")

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

; Maccros boucle while

(define-syntax while
  (syntax-rules ()
    ((while test e1 e2 ...) (let ()
                              (define (iter)
                                (if (not test)
                                    (void)
                                    (begin e1 e2 ... (iter))))
                              (iter)))))

; Macros TRY ... CATCH... utilisée lors de la compilation gigot->ail.
; On essaye de compiler (TRY)
; si erreur de syntaxe gigot (CATCH) une fenetre d'erreur (frame-error) apparait.

(define-syntax try
  (syntax-rules (catch)
    ((try body (catch catcher))
     (call-with-current-continuation
      (lambda (exit)
        (call-with-exception-handler
         (lambda (condition)
           catcher
           (exit condition))
         (lambda () body)))))))

(define (frame-error)
  (define bm (make-object bitmap% "images/icons/error.png"))
  (define canvas-error%
    (class canvas%
      (inherit get-dc)
      (define/override (on-paint)
        (let ([DC (get-dc)])
          (send DC draw-bitmap bm 1 1)))
      (super-new)))
  
  (define top-level
    (new dialog%
         (label "Gigot language : error !")
         (stretchable-width #f)
         (stretchable-height #f)))
  
  (define msg-line
    (new horizontal-panel%
         (parent top-level)))
  
  (define img-error
    (new canvas-error%
         (parent msg-line)
         (min-width 40)
         (min-height 40)
         (horiz-margin 25)
         (vert-margin 15)))
  (send img-error refresh)
  
  (define msg
    (new message%
         (label "Syntax error !")
         (parent msg-line)
         (horiz-margin 25)))
  
  (define button
    (new button%
         (label "OK !")
         (parent top-level)
         (callback (lambda (obj evt) (send top-level show #f)))))
  
  (send top-level show #t))

; Nous définissons ici, les compilateurs de langages.
; gigot->ail nous permettra de convertir du gigot en ail et ail->scheme du ail en scheme.

(define (gigot->ail code)
  (define (verifier/modifier code)
    (define liste (memf (lambda (i) (not (or (equal? i "\n") (equal? i " ")))) (regexp-match* "." code)))
    (define liste-rev (reverse liste))
    (define (iter)
      (cond ((not (equal? (car liste-rev) "\n")) (set! liste-rev (cons "\n" liste-rev)) (iter))
            ((equal? (cadr liste-rev) "\n") (set! liste-rev (cdr liste-rev)) (iter))
            (else (set! liste (reverse liste-rev)))))
    (iter)
    (string-append* liste))
  (define prgm-gigot (verifier/modifier code)) 
  (define (test-lexer)
    (define p-in (open-input-string prgm-gigot))    
    (for ([i (in-naturals)])                       
      (define x (get-lexeme p-in))                  
      #:break (equal? x 'FINI)                      
      (printf "[~a] Lecture de ~s\n" i x)))
  "Test du lexeur sur ce programme :"
  (printf "\n")
  (test-lexer)
  (printf "\n")
  (define lexeur (let ((p-in (open-input-string prgm-gigot)))
                   (lambda () (get-lexeme p-in))))
  (define compil0  (try (parse lexeur)
                        (catch (begin (bell) (frame-error)))))
  compil0)



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

(define (ail->scheme code)
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
  
  
  
  (define compil0 code)
  (define VARS (variables compil0))
  (define compil1 (end->goto compil0))
  (define LABELS (gotos compil1))
  (print LABELS)
  (define compil2 (decouper compil1))
  (define compil3 (elaguer compil2))
  (define compil4 (forcer-goto compil3))
  (define compil5 (elim-nums compil4))
  (define compil6 (elim-gotos compil5))
  (define compil7 (build-program compil6))
  compil7)



; Fonctions "to-string" qui permettent de convertir les listes en strings INDENTEES "à la scheme"
; Utilisées pour insérer plus tard les codes dans les éditeurs de la GUI...


(define (ail->string code)
  (if (null? code)
      ""
      (string-append (format "~s\n" (car code)) (ail->string (cdr code)))))

(define (scheme->string code)
  (define text
    (with-output-to-string
     (lambda ()
       (pretty-print code))))
  (substring text 1 (string-length text)))

(define (exec-in-string programme)
  (eval programme)
  (define texte
    (with-output-to-string
     (lambda ()
       (printf "~s" (eval '(programme))))))
  (substring texte 0 (- (string-length texte) 7)))


; programmes regroupant la renumerotation automatique du programme gigot.
(define (hash-new-numbers code depart)
  (if (null? code)
      (hash)
      (hash-set (hash-new-numbers (cdr code) (add1 depart))
                (take-infos (car code) 'get-numero)
                (+ 10 (* 10 depart)))))

(define (renumeroter code)
  (define hash (hash-new-numbers code 0))
  (define (renumeroter-ligne ligne)
    (let ((numero (hash-ref hash (take-infos ligne 'get-numero))))
      (cond ((take-infos ligne 'is-goto?) `(,numero (goto ,(hash-ref hash (take-infos ligne 'getgoto-renvoie)))))
            ((take-infos ligne 'is-if?) (let ((liste (take-infos ligne 'getif-cond)))
                                          `(,numero (if ,(cadr (take-infos ligne 'get-instr))
                                                        (goto ,(hash-ref hash (cadar (take-infos ligne 'getif-cond))))
                                                        (goto ,(hash-ref hash (cadadr (take-infos ligne 'getif-cond))))))))
            (else `(,numero ,(take-infos ligne 'get-instr))))))
  (map renumeroter-ligne code))

(define (prefix->infix expr)
  (if (null? expr)
      ""
      (cond ((not (list? expr)) (if (number? expr)
                                    (number->string expr)
                                    (symbol->string expr)))
            ((and (number? (cadr expr)) (number? (caddr expr))) (format "(~a~a~a)" (cadr expr) (car expr) (caddr expr)))
            ((not (null? (cddr expr))) (format "~a~a~a" (prefix->infix (cadr expr)) (car expr) (prefix->infix (caddr expr))))
            (else (case (car expr)
                    ((+) (format "~a" (cadr expr)))
                    ((-) (format "-~a" (cadr expr)))
                    ((*) (format "~a" (cadr expr)))
                    ((/) (format "1/~a" (cadr expr)))
                    (else (error "operator not defined")))))))

(define (new-gigot code-ail)
  (define code (renumeroter code-ail))
  (define (make-ligne ligne)
    (let ((numero (take-infos ligne 'get-numero)))
      (cond ((take-infos ligne 'is-goto?) (format "~a goto ~a" numero (take-infos ligne 'getgoto-renvoie)))
            ((take-infos ligne 'is-set?) (format "~a ~a = ~a" numero (take-infos ligne 'getset-var) (prefix->infix (take-infos ligne 'getset-varval))))
            ((take-infos ligne 'is-if?) (let ((pred (take-infos ligne 'getif-pred))
                                              (conditions (take-infos ligne 'getif-cond)))
                                          (format "~a if ((~a))~a~a then goto ~a else goto ~a"
                                                  numero
                                                  (cadr pred)
                                                  (car pred)
                                                  (caddr pred)
                                                  (cadr (car conditions))
                                                  (cadr (cadr conditions)))))
            ((take-infos ligne 'is-print?) (let* ((affiche (take-infos ligne 'getprint))
                                                  (instr (take-infos ligne 'get-instr))
                                                  (a-ln? (pair? (regexp-match* #rx"\n" affiche)))
                                                  (a-format? (pair? (cddr (take-infos ligne 'get-instr)))))
                                             (cond ((and a-ln? a-format?) (format "~a println ~a" numero (caddr instr)))
                                                   (a-ln? (format "~a println '~a'" numero (substring affiche 0 (- (string-length affiche) 1))))
                                                   (a-format? (format "~a print ~a" numero (caddr instr)))
                                                   (else (format "~a print '~a'" numero affiche)))))
            ((equal? (caadr ligne) 'end) (format "~a end" numero))
            (else ""))))
  
  (define (iter acc code)
    (if (null? code)
        acc
        (let* ((ligne (car code))
               (string (make-ligne ligne)))
          (iter (string-append acc string "\n") (cdr code)))))
  (iter "" code))


;_____________________COLORATION SYNTAXIQUE________________________
; Espace regroupant la gestion de la coloration syntaxique.
; les listes de correspondance couleur/syntaxe en fct du langage

(define Lsyntax-color-gigot '(("goto" "olive")
                              ("end" "red")
                              ("if" "orange")
                              ("else" "orange")
                              ("then" "orange")
                              ("println" "blue")
                              ("print" "blue")))
(define Lsyntax-color-ail '(("goto" "olive")
                            ("end" "red")
                            ("(" "red")
                            (")" "red")
                            ("if" "orange")
                            ("set!" "blue")
                            ("printf" "blue")))
(define Lsyntax-color-racket '(("define" "blue")
                               ("let" "blue")
                               ("(" "red")
                               (")" "red")
                               ("if" "orange")
                               ("set!" "blue")
                               ("printf" "olive")))

(define lsyntax-color-gigot Lsyntax-color-gigot) ; mutée dans le panneau de preferences
(define lsyntax-color-ail Lsyntax-color-ail) ; mutée dans le panneau de preferences
(define lsyntax-color-racket Lsyntax-color-racket) ; mutée dans le panneau de preferences


; fonction inserer-canvas avec pour arguments :
;     - txt : le texte à analyser
;     - ecrivain : l'écrivain du canvas dont la couleur d'écriture sera modifiée et
;                  dans lequel sera inséré le texte
;     - Lcolor : Liste des correspondances couleur/syntaxe
;     - e : ecrivain sous forme de string

(define (inserer-canvas txt ecrivain Lcolor e)
  (define style (case e
                  (("ecrivain-gigot") Style-delta-gigot)
                  (("ecrivain-ail") Style-delta-ail)
                  (("ecrivain-scheme") Style-delta-racket)
                  (else Style-delta-exec))) 
  (define (in? e L)
    (member e (map car L)))
  (define (changer-style color)
    (send style set-delta-foreground color)
    (send ecrivain change-style style))
  (define texte (regexp-match* "." txt))
  (define pile "")
  (while (not (null? texte))
         (if (and (in? pile Lcolor) (or (equal? (car texte) " ") (equal? (car texte) "\n")))
             (let* ((txt (send ecrivain get-text))
                    (longueurT (string-length txt))
                    (longueurP (string-length pile))
                    (debut (- longueurT longueurP))
                    (fin (+ debut longueurP)))
               (send ecrivain delete debut fin)
               (changer-style (cadr (assoc pile Lcolor)))
               (send ecrivain insert pile)
               (set! pile ""))
             (let ((e (car texte)))
               (if (in? e Lcolor)
                   (begin (changer-style (cadr (assoc e Lcolor)))
                          (send ecrivain insert e))
                   (begin (changer-style "black")
                          (send ecrivain insert e)
                          (set! pile (string-append pile e))))
               (when (or (equal? e " ") (equal? e "\n")) (set! pile ""))
               (set! texte (cdr texte))))))

; pour la coloration syntaxique en "live" : ne concerne que l'éditeur gigot.
; pour cette fonctionnalité, je me suis posé beaucoup de questions quand à la manière de m'y prendre...
; l'idée de départ était de redéfinir la méthode on-char avec un système de pile :
;    je tape et j'enregistre les lettres dans une pile. Lorsque la pile correspond à un mot de la liste des correspondances,
;    j'efface le mot, je change la couleur, je réinsère le mot, et je remets la couleur noire par défaut.
; mais problème :
;    lors de la redefinission de la methode on-char, les lettres ne s'inséraient plus
;    j'ai parcourru la mailing list et j'ai trouvé la methode : (super on-char k) cela a résolu le problème et m'a appris quelque chose de très intéressant sur les classes.
;autre soucis : l'algorithme de coloration est en O(n)... d'où un lagage au bout d'un moment... à chaque appuis sur "espace", le texte et entièrement recolorié
;               ceci pour permettre la modification à l'intérieur du code...
;sur la mailing list, ils m'ont parlé de ceci, mais je n'ai pas su l'utiliser http://docs.racket-lang.org/syntax-color/index.html
;mon idée d'amélioration, certe trop tard :-) serait de détecter quel mot est modifié/insérer en temps réel et n'appliquer la coloration syntaxique que sur ce mot en particulier.
;Conclusion : c'est une version béta que l'on peut choisir de désactiver au cas ou le code serait long... :-)


(define editor-canvas-gigot-syntax%
  (class editor-canvas%
    (define/override (on-char k)
      (super on-char k)
      (define position (send ecrivain-gigot get-end-position))
      (define key (send k get-key-code))
      (define text (send ecrivain-gigot get-text))
      (when (equal? key #\space)
        (send ecrivain-gigot erase)
        (inserer-canvas text ecrivain-gigot Lsyntax-color-gigot "ecrivain-gigot"))
      (when (= 0 (- (send ecrivain-gigot get-extend-end-position)
                    (send ecrivain-gigot get-extend-start-position)))
        (send ecrivain-gigot set-position position))
      (send ecrivain-gigot change-style Style-delta-gigot))
    (define/override (on-event evt)
      (super on-event evt)
      (define position (send ecrivain-gigot get-end-position))
      (when (equal? (send evt get-event-type) 'left-down)
        (set! cursor (send ecrivain-gigot get-start-position))
        (send ecrivain-gigot set-position position)))
    (super-new)
    ))

;____________________INTERFACE PRINCIPALE________________________
; Interface utilisant la gestion des onglets.
; Lorsqu'on change d'onglet, on sauvegarde les données de la fenetre courante dans une liste
; puis on importe les données de la nouvelle page

; DIMENSIONS DE LA FENETRE :

(define-values (w h)
  (get-display-size))
(define screen-width (- w 20))
(define screen-height (- h 150))
(define screen-width/3 (floor (/ screen-width 3)))

; structure de sauvegarde
(define-struct page (nTab txt-gigot txt-ail txt-scheme txt-exec txt-file) #:mutable #:transparent)
(define page-actuelle "Untitled1.gigot")

; liste des pages courantes : grandit au fur et à mesure que l'utilisateur créé des onglets
(define Lpages (list (make-page "Untitled1.gigot" "" "" "" "" "Untitled1.gigot")))

; nom des onglets pour le tab-panel%
; nom des onglets à supprimer dans le menu déroulant
(define Lchoices (list "Untitled1.gigot"))

; possibilité de supprimer des onglets par l'intermédiaire du menu déroulant
; les choix sont des menu-item%
; liste des couples (nom-label/nom-variable)
; cette liste des utile si le titre de l'onglet change, alors le titre du menu-item aussi...
(define Lcorres (list (list "Untitled1.gigot" "tab1")))

(define (getNomVar lbl L) ; en fonction du titre de l'onglet, renvoie le nom de la variable
  (if (equal? lbl (caar L))
      (cadar L)
      (getNomVar lbl (cdr L))))

; Styles par défaut des écrivains des canvas 
; ces styles peuvent être changés via la fenêtre des préférences
(define Style-delta-gigot (make-object style-delta%))
(define Style-delta-ail (make-object style-delta%))
(define Style-delta-racket (make-object style-delta%))
(define Style-delta-exec (make-object style-delta%))

; Couleurs par défaut des arrières-plan des canvas
; ces couleurs peuvent être changées via la fenêtre des préférences

(define background-color-gigot (make-object color% 215 240 240))
(define background-color-ail (make-object color% 170 220 245))
(define background-color-racket (make-object color% 250 230 150))
(define background-color-exec (make-object color% 170 250 150))


; un peu de fantaisie, un curseur en harmonie avec le langage !
(define bm (make-object bitmap% 16 16 #t #f))
(send bm load-file "images/cursor.png" 'png #f #t)
(define cursor (make-object cursor% bm bm 0 0))


;_______________________FENETRE PRINCIPALE_________________________
; STRUCTURE ET DISPOSITION :

(define FRAME 
  (new frame% 
       (label "GIGOT->Scheme compiler (PF2 project, Autumn 2014)")
       (stretchable-width #f)
       (min-width screen-width)
       (stretchable-height #f)
       (min-height screen-height)))

(define BARRE-MENU
  (new menu-bar%
       (parent FRAME)))

(define MENU-ICONES
  (new horizontal-panel% (parent FRAME)))

(define TAB-FRAME
  (new tab-panel%
       (parent FRAME)
       (choices Lchoices)
       (callback (lambda (b e) (fonction-change_onglet)))))

(define TITRE-FICHIER-PANEL
  (new horizontal-panel% (parent TAB-FRAME)))

(define file-title
  (new message%
       (parent TITRE-FICHIER-PANEL)
       (horiz-margin 2)
       (label "Untitled1.gigot")))

(define PANEL-INTERFACE
  (new horizontal-panel% (parent TAB-FRAME)))

(define PANEL-GIGOT
  (new vertical-panel% 
       (parent PANEL-INTERFACE)
       (stretchable-width #f)
       (stretchable-height #f)
       (border 2)
       (min-width screen-width/3)
       (min-height screen-height)))

(define PANEL-AIL
  (new vertical-panel% 
       (parent PANEL-INTERFACE)
       (stretchable-width #f)
       (stretchable-height #f)
       (border 2)
       (min-width screen-width/3)
       (min-height screen-height)))

(define PANEL-SCH/EXEC 
  (new vertical-panel% 
       (parent PANEL-INTERFACE)
       (stretchable-width #f)
       (stretchable-height #f)
       (border 2)
       (min-width screen-width/3)
       (min-height screen-height)))

(define PANEL-SCHEME 
  (new vertical-panel% (parent PANEL-SCH/EXEC)))

(define PANEL-EXEC
  (new vertical-panel% (parent PANEL-SCH/EXEC)))



; CREATION DES GRANDS TITRES DE MENUS :

(define menu-gigotcompiler
  (new menu%
       (label "&GIGOT Compiler")
       (parent BARRE-MENU)))

(define menu-edit
  (new menu%
       (label "&Edit")
       (parent BARRE-MENU)))

(define menu-file
  (new menu%
       (label "&File")
       (parent BARRE-MENU)))

(define menu-run
  (new menu%
       (label "&Run")
       (parent BARRE-MENU)))

; Création des choix dans chanque menu + création des boutons d'accès rapide :
; les fonctions de callback associées à chaque menu-item% ou button% sont extériorisées de manière à ne pas répéter le code

(define img-but1 (make-object bitmap% "images/icons/execute.png" ))
(define img-but2 (make-object bitmap% "images/icons/open.png"))
(define img-but3 (make-object bitmap% "images/icons/add_tab.png"))
(define img-but4 (make-object bitmap% "images/icons/prefs.png"))
(define img-but5 (make-object bitmap% "images/icons/renumber.png"))

(define menu-gigotcompiler-opt1
  (new menu-item%
       (label "New tab...")
       (parent menu-gigotcompiler)
       (shortcut #\n)
       (callback (lambda (obj evt) (fonction-new_tab)))))

(define menu-gigotcompiler-opt2
  (new menu%
       (label "&Delete tab")
       (parent menu-gigotcompiler)))


(define tab1
  (new menu-item%
       (label "Untitled1.gigot")
       (parent menu-gigotcompiler-opt2)
       (callback (lambda (obj evt) (fonction-delete_tab obj)))))

(define menu-gigotcompiler-opt3
  (new menu-item%
       (label "Preferences")
       (parent menu-gigotcompiler)
       (shortcut #\p)
       (shortcut-prefix '(alt ctl))
       (callback (lambda (obj evt) (fonction-prefs)))))

(define menu-gigotcompiler-sep1
  (new separator-menu-item%
       (parent menu-gigotcompiler)))

(define menu-gigotcompiler-opt4
  (new menu-item%
       (label "Quit")
       (parent menu-gigotcompiler)
       (shortcut #\q)
       (callback (lambda (obj evt) (fonction-quit)))))

(define menu-edit-opt1
  (new menu-item%
       (label "Undo")
       (shortcut #\z)
       (parent menu-edit)
       (callback (lambda (obj evt) (fonction-undo)))))

(define menu-edit-opt2
  (new menu-item%
       (label "Redo")
       (shortcut #\r)
       (parent menu-edit)
       (callback (lambda (obj evt) (fonction-redo)))))

(define menu-edit-sep1
  (new separator-menu-item%
       (parent menu-edit)))

(define menu-edit-opt3
  (new menu-item%
       (label "Copy")
       (shortcut #\c)
       (parent menu-edit)
       (callback (lambda (obj evt) (fonction-copy)))))

(define menu-edit-opt4
  (new menu-item%
       (label "Cut")
       (shortcut #\x)
       (parent menu-edit)
       (callback (lambda (obj evt) (fonction-cut)))))

(define menu-edit-opt5
  (new menu-item%
       (label "Paste")
       (shortcut #\v)
       (parent menu-edit)
       (callback (lambda (obj evt) (fonction-paste)))))

(define menu-edit-opt6
  (new menu-item%
       (label "Clear")
       (parent menu-edit)
       (callback (lambda (obj evt) (fonction-clear)))))

(define menu-edit-opt7
  (new menu-item%
       (label "Select All")
       (shortcut #\a)
       (parent menu-edit)
       (callback (lambda (obj evt) (fonction-select_all)))))

(define menu-file-opt1
  (new menu-item%
       (label "Load...")
       (parent menu-file)
       (shortcut #\o)
       (callback (lambda (obj evt) (fonction-load)))))

(define menu-file-opt2
  (new menu-item%
       (label "Save As...")
       (parent menu-file)
       (shortcut #\s)
       (shortcut-prefix '(ctl alt))
       (callback (lambda (obj evt) (fonction-save_as)))))

(define menu-file-opt3
  (new menu-item%
       (label "Save")
       (parent menu-file)
       (shortcut #\s)
       (callback (lambda (obj evt) (fonction-save)))))

(define menu-run-opt1
  (new menu-item%
       (label "GIGOT->AIL")
       (parent menu-run)
       (callback (lambda (obj evt) (fonction-gigot->ail)))))

(define menu-run-opt2
  (new menu-item%
       (label "AIL->Racket")
       (parent menu-run)
       (callback (lambda (obj evt) (fonction-ail->racket)))))

(define menu-run-opt3
  (new menu-item%
       (label "Execute !")
       (shortcut 'f5)
       (shortcut-prefix '())
       (parent menu-run)
       (callback (lambda (obj evt) (fonction-execute)))))


(define menu-run-sep1
  (new separator-menu-item%
       (parent menu-run)))

(define menu-run-opt4
  (new menu-item%
       (label "Renumber GIGOT Code")
       (shortcut #\n)
       (shortcut-prefix '(alt))
       (parent menu-run)
       (callback (lambda (obj evt) (fonction-renumber)))))

(define but1
  (new button%
       (parent MENU-ICONES)
       (label img-but1)
       (callback (lambda (obj evt)
                   (fonction-gigot->ail)
                   (fonction-ail->racket)
                   (fonction-execute)))))

(define but2
  (new button%
       (parent MENU-ICONES)
       (label img-but2)
       (callback (lambda (obj evt)
                   (fonction-load)))))

(define but3
  (new button%
       (parent MENU-ICONES)
       (label img-but3)
       (callback (lambda (obj evt)
                   (fonction-new_tab)))))

(define but4
  (new button%
       (parent MENU-ICONES)
       (label img-but4)
       (callback (lambda (obj evt)
                   (fonction-prefs)))))

(define but5
  (new button%
       (parent MENU-ICONES)
       (label img-but5)
       (callback (lambda (obj evt)
                   (fonction-renumber)))))


(define case-gigot (new check-box%	 
                        (label "GIGOT Syntax coloring")
                        (value #t)
                        (parent MENU-ICONES)
                        (callback (lambda (obj evt)
                                    (define (fonctionbox-activation)
                                      (send PANEL-GIGOT delete-child editor-canvas-gigot2)
                                      (send PANEL-GIGOT add-child editor-canvas-gigot))
                                    (define (fonctionbox-desactivation)
                                      (define text (send ecrivain-gigot get-text))
                                      (send ecrivain-gigot erase)
                                      (define style (make-object style-delta%))
                                      (send style set-delta-foreground "black")
                                      (send ecrivain-gigot change-style style)
                                      (send ecrivain-gigot insert text)
                                      (send PANEL-GIGOT delete-child editor-canvas-gigot)
                                      (send PANEL-GIGOT add-child editor-canvas-gigot2))
                                    (if (send obj get-value)
                                        (fonctionbox-activation)
                                        (fonctionbox-desactivation))))))


; Création des éditeurs de texte et des écrivains.

(define style-editors '(no-focus control-border))

(define ecrivain-gigot (new text%))
(define ecrivain-ail (new text%))
(define ecrivain-scheme (new text%))
(define ecrivain-exec (new text%))

(define label-gigot (new message%
                         (parent PANEL-GIGOT)
                         (label "GIGOT")))

(define label-ail (new message%
                       (parent PANEL-AIL)
                       (label "AIL"))) 
(define label-scheme (new message%
                          (parent PANEL-SCHEME)
                          (label "SCHEME"))) 
(define label-exec (new message%
                        (parent PANEL-EXEC)
                        (label "EXECUTION"))) 



(define editor-canvas-gigot
  (new editor-canvas-gigot-syntax%
       (parent PANEL-GIGOT)
       (style (cdr style-editors))
       (editor ecrivain-gigot)))
(send editor-canvas-gigot set-canvas-background background-color-gigot)

(define editor-canvas-gigot2
  (new editor-canvas%
       (parent PANEL-GIGOT)
       (style (cdr style-editors))
       (editor ecrivain-gigot)))
(send editor-canvas-gigot2 set-canvas-background background-color-gigot)
(send PANEL-GIGOT delete-child editor-canvas-gigot2)

(define editor-canvas-ail
  (new editor-canvas%
       (parent PANEL-AIL)
       (style style-editors)
       (editor ecrivain-ail)))
(send editor-canvas-ail set-canvas-background background-color-ail)

(define editor-canvas-scheme
  (new editor-canvas%
       (parent PANEL-SCHEME)
       (style style-editors)
       (min-height (- screen-height 400))
       (editor ecrivain-scheme)))
(send editor-canvas-scheme set-canvas-background background-color-racket)

(define editor-canvas-exec
  (new editor-canvas%
       (parent PANEL-EXEC)
       (style style-editors)
       (editor ecrivain-exec)))
(send editor-canvas-exec set-canvas-background background-color-exec)

(send FRAME set-cursor cursor)
(send ecrivain-gigot set-max-undo-history 'forever)
(send FRAME show #t)


;_________________________LES FONCTIONS ASSOCIEES______________________________
; nous définissons les fonctions précédemments extériorisées des callback.
;      + des fonctions annexes 

(define (sauvegarder) ; sauvegarde les paramètres de l'onglet courant lors du changement d'onglet
  (define liste (filter-not (lambda (i) (equal? (page-nTab i) page-actuelle)) Lpages))
  (define nTab page-actuelle)
  (define txt-gigot (send ecrivain-gigot get-text))
  (define txt-ail (send ecrivain-ail get-text))
  (define txt-scheme (send ecrivain-scheme get-text))
  (define txt-exec (send ecrivain-exec get-text))
  (define txt-file (send file-title get-label))
  (set! Lpages (cons (make-page nTab txt-gigot txt-ail txt-scheme txt-exec txt-file) liste)))

(define (charger) ; charge les nouvelles données lors du changement d'onglets.
  (define p (car (filter (lambda (i) (equal? (page-nTab i) (send TAB-FRAME get-item-label (send TAB-FRAME get-selection)))) Lpages)))
  (define ntxt-gigot (page-txt-gigot p))
  (define ntxt-ail (page-txt-ail p))
  (define ntxt-scheme (page-txt-scheme p))
  (define ntxt-exec (page-txt-exec p))
  (define ntxt-file (page-txt-file p))
  (inserer-canvas ntxt-gigot ecrivain-gigot Lsyntax-color-gigot "ecrivain-gigot")
  (inserer-canvas ntxt-ail ecrivain-ail Lsyntax-color-ail "ecrivain-ail")
  (inserer-canvas ntxt-scheme ecrivain-scheme Lsyntax-color-racket "ecrivain-scheme")
  (send ecrivain-exec insert ntxt-exec)
  (send file-title set-label ntxt-file)
  (send file-title auto-resize ntxt-file)
  (set! page-actuelle (send TAB-FRAME get-item-label (send TAB-FRAME get-selection))))

(define (fonction-change_onglet)
  (sauvegarder)
  (fonction-clear)
  (charger))

; lors de la création d'un onglet, génération du titre de l'onglet.
; titre de la forme "Untitled#.gigot" : # varie en fonction du nombre d'onglets dont le titre est de cette forme
(define (gen-titre-tab) 
  (define liste (filter (lambda (i) (equal? (substring i 0 8) "Untitled")) Lchoices))
  (format "Untitled~a.gigot" 
          (if (null? liste)
              1
              (+ 1 (apply max (map (lambda (titre)
                                     (string->number (car (regexp-match #rx"[0-9]+" titre)))) liste))))))

(define (fonction-new_tab)
  (define titre (gen-titre-tab))
  (sauvegarder)
  (define variable (format "tab~a" (random 1000)))
  (set! Lcorres (cons (list titre variable) Lcorres))
  (eval (read (open-input-string 
               (format "(define ~a (new menu-item% (label \"~a\") (parent menu-gigotcompiler-opt2) (callback (lambda (obj evt) (fonction-delete_tab obj)))))" variable titre))))
  (set! Lchoices (append Lchoices (list titre)))
  (send TAB-FRAME append titre)
  (set! Lpages (cons (make-page titre "" "" "" "" titre) Lpages))
  (send TAB-FRAME set-selection (- (length Lchoices) 1))
  (fonction-clear)
  (charger))


(define (fonction-delete_tab obj)
  (define titre (send obj get-label))
  (define var (getNomVar titre Lcorres))
  (define (delete)
    (eval (read (open-input-string 
                 (format "(send ~a delete)" var))))
    (set! Lchoices (filter-not (lambda (i) (equal? i titre)) Lchoices))
    (set! Lpages (filter-not (lambda (i) (equal? (page-nTab i) titre)) Lpages))
    (send TAB-FRAME set Lchoices)
    (fonction-clear)
    (charger))
  (when (not (null? (cdr Lchoices))) (delete)))


(define (fonction-prefs)
  (send FRAMEPREF show #t)
  (set! lsyntax-color-gigot Lsyntax-color-gigot)
  (set! lsyntax-color-ail Lsyntax-color-ail)
  (set! lsyntax-color-racket Lsyntax-color-racket))

(define (fonction-quit)
  (send FRAME show #f))

(define (fonction-undo)
  (send ecrivain-gigot undo))

(define (fonction-redo)
  (send ecrivain-gigot redo))

(define (fonction-copy)
  (send ecrivain-gigot copy)
  (send ecrivain-ail copy))

(define (fonction-cut)
  (send ecrivain-gigot cut))

(define (fonction-paste)
  (send ecrivain-gigot paste))

(define (fonction-clear)
  (send ecrivain-gigot erase)
  (send ecrivain-ail erase)
  (send ecrivain-scheme erase)
  (send ecrivain-exec erase))

(define (fonction-select_all)
  (let ((nb-char (string-length (send ecrivain-gigot get-text))))
    (send ecrivain-gigot set-position 0 nb-char)))

(define (fonction-load)
  (define label (send TAB-FRAME get-item-label (send TAB-FRAME get-selection)))
  (define fichier (get-file #f #f #f #f #f null
                            '(("Any" "*.*") ("Txt" "*.txt") ("Gigot" "*.gigot"))))
  (define texte (string-append* (filter-not (lambda (x) (equal? x "\r"))
                                            (regexp-match* "." (file->string fichier)))))
  (define path (some-system-path->string fichier))
  (define titre (some-system-path->string (file-name-from-path fichier)))
  (set! Lchoices (map (lambda (i) (if (equal? i label) titre i)) Lchoices))
  (set! Lcorres (map (lambda (i) (if (equal? (car i) label) (list titre (cadr i)) i)) Lcorres))
  (set! Lpages (map (lambda (i) (if (equal? (page-nTab i) label) (make-page titre (page-txt-gigot i) (page-txt-ail i) (page-txt-scheme i) (page-txt-exec i) path) i)) Lpages))
  (send file-title set-label path)
  (send file-title auto-resize path)
  (send TAB-FRAME set Lchoices)
  (eval (read (open-input-string 
               (format "(send ~a set-label \"~a\")" (getNomVar titre Lcorres) titre))))
  (fonction-clear)
  (inserer-canvas texte ecrivain-gigot Lsyntax-color-gigot "ecrivain-gigot")
  (set! prgm-gigot texte)
  (set! page-actuelle (send TAB-FRAME get-item-label (send TAB-FRAME get-selection))))


(define (fonction-save_as)
  (define name (send file-title get-label))
  (define path (put-file #f #f #f 
                         name "gigot" null
                         '(("Any" "*.*") ("Gigot" "*.gigot"))))
  (when path 
    (call-with-output-file path
      (lambda (p-out)
        (fprintf p-out "~a" (send ecrivain-gigot get-text)))
      #:exists 'replace)))


(define (fonction-save)
  (define name (send file-title get-label))
  (define (fonction)
    (define fichier (put-file #f #f #f 
                              name "gigot" null
                              '(("Any" "*.*") ("Gigot" "*.gigot"))))
    (define label (send TAB-FRAME get-item-label (send TAB-FRAME get-selection)))
    (define path (some-system-path->string fichier))
    (define titre (some-system-path->string (file-name-from-path fichier)))
    (set! Lchoices (map (lambda (i) (if (equal? i label) titre i)) Lchoices))
    (set! Lcorres (map (lambda (i) (if (equal? (car i) label) (list titre (cadr i)) i)) Lcorres))
    (set! Lpages (map (lambda (i) (if (equal? (page-nTab i) label) (make-page titre (page-txt-gigot i) (page-txt-ail i) (page-txt-scheme i) (page-txt-exec i) path) i)) Lpages))
    (send file-title set-label path)
    (send file-title auto-resize path)
    (send TAB-FRAME set Lchoices)
    (eval (read (open-input-string 
                 (format "(send ~a set-label \"~a\")" (getNomVar titre Lcorres) titre))))
    (call-with-output-file path
      (lambda (p-out)
        (fprintf p-out "~a" (send ecrivain-gigot get-text)))
      #:exists 'replace)
    (set! page-actuelle (send TAB-FRAME get-item-label (send TAB-FRAME get-selection))))
  (if (equal? (substring name 0 8) "Untitled")
      (fonction)
      (call-with-output-file name 
        (lambda (p-out)
          (fprintf p-out "~a" (send ecrivain-gigot get-text)))
        #:exists 'replace)))


(define (fonction-gigot->ail)
  (set! prgm-gigot (send ecrivain-gigot get-text))
  (define code-ail 
    (if (equal? prgm-gigot str-question)
        str-answer-ail
        (gigot->ail prgm-gigot)))
  (define code-ail-str 
    (if (equal? code-ail str-answer-ail)
        code-ail
        (ail->string code-ail)))
  (send ecrivain-ail erase)
  (send ecrivain-scheme erase)
  (send ecrivain-exec erase)
  (inserer-canvas code-ail-str ecrivain-ail Lsyntax-color-ail "ecrivain-ail"))


(define (fonction-ail->racket)
  (define code-ail 
    (if (equal? prgm-gigot str-question)
        str-answer-ail
        (gigot->ail prgm-gigot)))
  (define code-scheme 
    (if (equal? code-ail str-answer-ail)
        str-answer-scheme
        (ail->scheme code-ail)))
  (define code-scheme-str 
    (if (equal? code-ail str-answer-ail)
        code-scheme
        (scheme->string code-scheme)))
  (when (not (equal? (send ecrivain-ail get-text) ""))
    (send ecrivain-scheme erase)
    (send ecrivain-exec erase)
    (inserer-canvas code-scheme-str ecrivain-scheme Lsyntax-color-racket "ecrivain-scheme")))

(define (fonction-execute)
  (define code-ail 
    (if (equal? prgm-gigot str-question)
        str-answer-ail
        (gigot->ail prgm-gigot)))
  (define code-scheme 
    (if (equal? code-ail str-answer-ail)
        str-answer-scheme
        (ail->scheme code-ail)))
  (define texte 
    (if (list? code-scheme)
        (exec-in-string code-scheme)
        str-answer-exec))
  (when (not (equal? (send ecrivain-scheme get-text) ""))
    (send ecrivain-exec erase)
    (send ecrivain-exec insert texte))
  (when (equal? texte str-answer-exec) (send FRAME-EGG show #t)))

(define (fonction-renumber)
  (define new-code-ail (renumeroter (gigot->ail (send ecrivain-gigot get-text))))
  (define new-code (new-gigot new-code-ail))
  (send ecrivain-gigot erase)
  (if (send case-gigot get-value)
      (inserer-canvas new-code ecrivain-gigot Lsyntax-color-gigot "ecrivain-gigot")
      (send ecrivain-gigot insert new-code)))



;_______________________FENETRE DE PREFERENCES________________________
; dans chaque onglet des preferences se trouve un aperçu des modifications
; chaque aperçu contient un morceau de texte syntaxiquement et sémantiquement identique au langage associé

(define text-default-gigot "This is a Gigot code example :\n
10 goto 15
....
15 println 'a string'
20 n = 10
....
35 if ((n))<i then goto 80 else goto 40
40 res=res*i")

(define text-default-ail "This is an Ail code example :\n
(10 (goto 15))
....
(15 (printf \"a string\\n\"))
(20 (set! n 10))
....
(35 (if (n < i) (goto 80) (goto 40)))
(40 (set! res (* res i)))")

(define text-default-racket "This is a Racket code example :\n
(define (programme)
   (printf \"**START OF PROGRAM**\\n\")
   (let ((res 0) .....)
      (define (fonc_) expr)
      .............
      ))")

(define text-default-exec "This is an execution code example :\n
**START OF PROGRAM**

A result

**END OF PROGRAM**")


; comme pour la fenetre principal, le système d'onglet fonctionne de la même façon

(define-struct preference (tab text backcolor style) #:mutable #:transparent)
(define current-page-pref "Gigot canvas")
(define L-prefs (list (make-preference "Gigot canvas" text-default-gigot background-color-gigot Style-delta-gigot)
                      (make-preference "Ail canvas" text-default-ail background-color-ail Style-delta-ail)
                      (make-preference "Racket canvas" text-default-racket background-color-racket Style-delta-racket)
                      (make-preference "Execution canvas" text-default-exec background-color-exec Style-delta-exec)))

; style par défaut des écrivains des préferences.
; style modifié en fonction des changements
(define Style-delta-pref (make-object style-delta%))

; pour chaque onglet correspond une sauvegarde des preférences
; cette fonction renvoie la sauvegarde associée à un onglet passé en paramètres
(define (getPrefs tab)
  (car (filter (lambda (i) (equal? (preference-tab i) tab)) L-prefs)))


; STRUCTURE + CREATION DE LA FENETRE
(define FRAMEPREF
  (new frame% 
       (label "Preferences")
       (stretchable-height #f)
       (stretchable-width #f)
       (min-width (floor (/ screen-width 2)))
       (min-height (floor (/ screen-height 2)))))

(define TAB-PANEL
  (new tab-panel%
       (choices '("Gigot canvas" "Ail canvas" "Racket canvas" "Execution canvas"))
       (parent FRAMEPREF)
       (callback (lambda (obj evt) (fonctionpref-changetab)))))

(define TABZONE
  (new horizontal-panel%
       (parent TAB-PANEL)))

(define TABZONE-SET
  (new group-box-panel%
       (label "Preferences")
       (parent TABZONE)
       (horiz-margin 10)
       (vert-margin 10)
       (border 5)))

(define ecrivain-canvas (new text%))
(define CANVAS
  (new editor-canvas%
       (parent TABZONE)
       (min-height 250)
       (min-width (floor (/ screen-width 5)))
       (style '(no-focus))
       (editor ecrivain-canvas)))
(inserer-canvas text-default-gigot ecrivain-canvas Lsyntax-color-gigot "ecrivain-gigot")
(send CANVAS set-canvas-background (preference-backcolor (car (filter (lambda (i) (equal? (preference-tab i) "Gigot canvas")) L-prefs))))


(define BUTTONLINE
  (new horizontal-panel%
       (parent FRAMEPREF)))


(define button-apply
  (new button%
       (label "Apply and Quit")
       (parent BUTTONLINE)
       (callback (lambda (obj evt) (fonctionpref-apply)))))

(define button-cancel
  (new button%
       (label "Cancel")
       (parent BUTTONLINE)
       (callback (lambda (obj evt) (fonction-cancel)))))

(define zone-bc
  (new horizontal-panel%
       (parent TABZONE-SET)))
(define label-bc
  (new message%
       (label "Background color :")
       (parent zone-bc)))

(define but-bc
  (new button%
       (parent zone-bc)
       (label "Change")
       (callback (lambda (obj evt) (fonctionpref-change_bc)))))

(define zone-font-style
  (new horizontal-panel%
       (parent TABZONE-SET)))
(define label-font-style
  (new message%
       (label "Font style :")
       (parent zone-font-style)))

(define but-font-style
  (new button%
       (parent zone-font-style)
       (label "Change")
       (callback (lambda (obj evt) (fonctionpref-change_font-style)))))

(define zone-syntax-color
  (new horizontal-panel%
       (parent TABZONE-SET)))

(define syntax-text
  (new text-field%
       (label "Syntax color : ")
       (parent zone-syntax-color)
       (min-width 10)))


(define but-syntax-color
  (new button%
       (parent zone-syntax-color)
       (label "Choose color and create")
       (callback (lambda (obj evt) (fonctionpref-change_syntax-color)))))

(define zone-blank
  (new horizontal-panel%
       (parent TABZONE-SET)
       (min-height 80)))

; les fonctions associées aux callback de chaque button% de l'interface de préférences

(define (sauv tab)
  (define text (send ecrivain-canvas get-text))
  (define bg-color (send CANVAS get-canvas-background))
  (define liste (filter-not (lambda (i) (equal? (preference-tab i) tab)) L-prefs))
  (define pref (make-preference tab text bg-color Style-delta-pref))
  (set! L-prefs (cons pref liste)))

(define (fonctionpref-apply)
  (sauv current-page-pref)
  (send FRAMEPREF show #f)
  (define prefG (getPrefs "Gigot canvas"))
  (define prefA (getPrefs "Ail canvas"))
  (define prefR (getPrefs "Racket canvas"))
  (define prefE (getPrefs "Execution canvas"))
  (send editor-canvas-gigot set-canvas-background (preference-backcolor prefG))
  (send editor-canvas-gigot2 set-canvas-background (preference-backcolor prefG))
  (send editor-canvas-ail set-canvas-background (preference-backcolor prefA))
  (send editor-canvas-scheme set-canvas-background (preference-backcolor prefR))
  (send editor-canvas-exec set-canvas-background (preference-backcolor prefE))
  (send ecrivain-gigot change-style (preference-style prefG))
  (send ecrivain-ail change-style (preference-style prefA))
  (send ecrivain-scheme change-style (preference-style prefR))
  (send ecrivain-exec change-style (preference-style prefE))
  (when (and (send case-gigot get-value) (or (not (equal? Lsyntax-color-gigot lsyntax-color-gigot)) (not (equal? Style-delta-gigot (preference-style prefG)))))
    (set! Lsyntax-color-gigot lsyntax-color-gigot)
    (let ((textG (send ecrivain-gigot get-text)))
      (send ecrivain-gigot erase)
      (inserer-canvas textG ecrivain-gigot Lsyntax-color-gigot "ecrivain-gigot")))
  (when (or (not (equal? Lsyntax-color-ail lsyntax-color-ail)) (not (equal? Style-delta-ail (preference-style prefA))))
    (set! Lsyntax-color-ail lsyntax-color-ail)
    (let ((textA (send ecrivain-ail get-text)))
      (send ecrivain-ail erase)
      (inserer-canvas textA ecrivain-ail Lsyntax-color-ail "ecrivain-ail")))
  (when (or (not (equal? Lsyntax-color-racket lsyntax-color-racket)) (not (equal? Style-delta-racket (preference-style prefR))))
    (set! Lsyntax-color-racket lsyntax-color-racket)
    (let ((textR (send ecrivain-scheme get-text)))
      (send ecrivain-scheme erase)
      (inserer-canvas textR ecrivain-scheme Lsyntax-color-racket "ecrivain-scheme")))
  (when (not (equal? Style-delta-exec (preference-style prefE)))
    (let ((textE (send ecrivain-exec get-text)))
      (send ecrivain-exec erase)
      (send ecrivain-exec insert textE)))
  (set! Style-delta-gigot (preference-style prefG))
  (set! Style-delta-ail (preference-style prefA))
  (set! Style-delta-racket (preference-style prefR))
  (set! Style-delta-exec (preference-style prefE)))

(define (fonction-cancel)
  (send FRAMEPREF show #f)
  (set! lsyntax-color-gigot Lsyntax-color-gigot)
  (set! lsyntax-color-ail Lsyntax-color-ail)
  (set! lsyntax-color-racket Lsyntax-color-racket)
  (send ecrivain-canvas erase)
  (if (not (equal? current-page-pref "Execution canvas"))
      (inserer-canvas (preference-text (getPrefs current-page-pref)) ecrivain-canvas (case current-page-pref
                                                                                       (("Gigot canvas") Lsyntax-color-gigot)
                                                                                       (("Ail canvas") Lsyntax-color-ail)
                                                                                       (("Racket canvas") Lsyntax-color-racket)
                                                                                       (else (error "Error !")))
                      (case current-page-pref
                        (("Gigot canvas") "ecrivain-gigot")
                        (("Ail canvas") "ecrivain-ail")
                        (("Racket canvas") "ecrivain-scheme")
                        (else "ecrivain-exec")))
      (send ecrivain-canvas insert text-default-exec)))

(define (fonctionpref-changetab)
  (define click (send TAB-PANEL get-item-label (send TAB-PANEL get-selection)))
  (sauv current-page-pref)
  (set! current-page-pref click)
  (if (equal? current-page-pref "Execution canvas")
      (begin (send but-syntax-color enable #f)
             (send syntax-text enable #f))
      (begin (send but-syntax-color enable #t)
             (send syntax-text enable #t)))
  (define preference (getPrefs click))
  (define style-delta (preference-style preference))
  (set! Style-delta-pref style-delta)
  (send ecrivain-canvas erase)
  (send ecrivain-canvas change-style style-delta)
  (if (not (equal? click "Execution canvas"))
      (inserer-canvas (preference-text preference) ecrivain-canvas (case click
                                                                     (("Gigot canvas") Lsyntax-color-gigot)
                                                                     (("Ail canvas") Lsyntax-color-ail)
                                                                     (("Racket canvas") Lsyntax-color-racket)
                                                                     (else (error "Error !")))
                      (case current-page-pref
                        (("Gigot canvas") "ecrivain-gigot")
                        (("Ail canvas") "ecrivain-ail")
                        (("Racket canvas") "ecrivain-scheme")
                        (else "ecrivain-exec")))
      (send ecrivain-canvas insert text-default-exec))
  (send CANVAS set-canvas-background (preference-backcolor preference)))

(define (fonctionpref-change_bc)
  (define color (get-color-from-user))
  (send CANVAS set-canvas-background color))

(define (fonctionpref-change_font-style)
  (define font (get-font-from-user))
  (define style (make-object style-delta%))
  (define pref (getPrefs current-page-pref))
  (send style set-face (send font get-face))
  (send style set-weight-on (send font get-weight))
  (send style set-style-on (send font get-style))
  (send style set-delta 'change-size (send font get-point-size))
  (set! Style-delta-pref style)
  (send ecrivain-canvas erase)
  (send ecrivain-canvas change-style style)
  (if (not (equal? current-page-pref "Execution canvas"))
      (inserer-canvas (preference-text pref) ecrivain-canvas (case current-page-pref
                                                               (("Gigot canvas") Lsyntax-color-gigot)
                                                               (("Ail canvas") Lsyntax-color-ail)
                                                               (("Racket canvas") Lsyntax-color-racket)
                                                               (else (error "Error !")))
                      (case current-page-pref
                        (("Gigot canvas") "ecrivain-gigot")
                        (("Ail canvas") "ecrivain-ail")
                        (("Racket canvas") "ecrivain-scheme")
                        (else "ecrivain-exec")))
      (send ecrivain-canvas insert text-default-exec)))

(define (fonctionpref-change_syntax-color)
  (define color (get-color-from-user))
  (define (fonction)
    (define new-syntax (send syntax-text get-value))
    (case current-page-pref
      (("Gigot canvas") (set! lsyntax-color-gigot (cons (list new-syntax color)
                                                        (filter-not (lambda (i) (equal? (car i) new-syntax)) lsyntax-color-gigot))))
      (("Ail canvas") (set! lsyntax-color-ail (cons (list new-syntax color)
                                                    (filter-not (lambda (i) (equal? (car i) new-syntax)) lsyntax-color-ail))))
      (("Racket canvas") (set! lsyntax-color-racket (cons (list new-syntax color)
                                                          (filter-not (lambda (i) (equal? (car i) new-syntax)) lsyntax-color-racket))))
      (else (void)))
    (send ecrivain-canvas erase)
    (if (not (equal? current-page-pref "Execution canvas"))
        (inserer-canvas (preference-text (getPrefs current-page-pref)) ecrivain-canvas (case current-page-pref
                                                                                         (("Gigot canvas") lsyntax-color-gigot)
                                                                                         (("Ail canvas") lsyntax-color-ail)
                                                                                         (("Racket canvas") lsyntax-color-racket)
                                                                                         (else (error "Error !")))
                        (case current-page-pref
                          (("Gigot canvas") "ecrivain-gigot")
                          (("Ail canvas") "ecrivain-ail")
                          (("Racket canvas") "ecrivain-scheme")
                          (else "ecrivain-exec")))
        (send ecrivain-canvas insert text-default-exec)))
  (when color (fonction)))






;_____________________________EASTER EGG______________________________
; codes par défaut des différents éditeurs en réponse au code gigot str-question

(define str-question "10 ask 'What is Gigot ?'
20 goto kitchen") ; code gigot pour executer le easter egg


(define str-answer-ail "(10 (whaoo 'Gigot Is a Goto Oriented Torture !'))
(20 (going to cook))")
(define str-answer-scheme
  "(define (cooker)
   (Be-carefull 
       (and (this-is-a-racket-function)
            (who-generate-a-real-gigot!))))
(cooker)")

(define str-answer-exec
  "**LAUNCH OF THE COOK**
......COOKING.......
......COOKING.......
......COOKING.......
THE GIGOT IS COOKED !!!
**END OF COOK**")


; fenetre générée par le easter egg
(define FRAME-EGG
  (new frame% 
       (label "Eat me !!!")
       (stretchable-width #f)
       (stretchable-height #f)
       (min-width 320)
       (min-height 248)))

(define img-egg (make-object bitmap% "images/gigot.png"))
(define button-egg
  (new button%
       (parent FRAME-EGG)
       (label img-egg)
       (callback (lambda (b e) (fonction-clear)(send FRAME-EGG show #f)))))