#lang racket

; procédure de tests unitaires
(define (test x r)
  (unless (equal? x r)
      (error "Bad test result")))

; Implémentation de l'opérateur angélique (ou ambigu) "amb".
(define fail #f)

(define (reset)
  (set! fail (lambda () (error "No more alternatives"))))

;(reset)

(define (amb1 . l)
  (if (empty? l)
      (fail)
      (let/cc k
        (let ((old-fail fail))
          (set! fail (lambda ()
                       (set! fail old-fail)
                       (k (apply amb1 (cdr l))))))
        (car l))))

; Test de "amb".
(define (test-amb)
  (let ((x (amb1 1 2 3))
        (y (amb1 10 20 30)))
    (when (odd? (+ x y))
      (fail))
    (+ x y)))

; syntax->list : renvoie une liste de syntaxes (exp avec plusieurs expr dans une liste, le tout est une syntaxe => renvoie la liste des expr syntaxiques)

;définition de amb comme nouvelle syntaxe => évaluation des arguments au moment de l'utilisation
(define-syntax amb
  (syntax-rules ()
    ((amb)
     (fail))
    ((amb x queue ...)
       (let/cc k
         (let ((old-fail fail))
           (set! fail (lambda ()
                        (set! fail old-fail)
                        (k (amb queue ...)))))
         x))))

(define (produce n)
  (printf "Producing ~s~n" n)
  n)

;(amb (produce 1) (produce 2) (produce 3))

(define (mult)
  (let ((x (amb 1 2 3 4 5 6 7 8 9 10))
        (y (amb 1 2 3 4 5 6 7 8 9 10)))
     (if (= (* x y) 30) 
         (list x y) 
         (amb))))

(define (bag-of proc)
  (let ((results '()))
    (amb
        (begin
          (set! results (cons (proc) results))
          (fail))
        (reverse results))))


;; problème des huit reines

;conflict : si les deux reines passées en paramètres sont en conflit, renvoit #t.
(define (conflict c1 l1 c2 l2)
  (cond
    ((equal? c1 c2) #t)
    ((equal? l1 l2) #t)
    ((equal? (abs (/ (- l2 l1) (- c2 c1))) 1) #t)
    (else #f)))

;(reset)

; parcourir la liste placed: pour chaque paire, vérifier si conflit avec (col, row) : si conflit, error. sinon, ne rien faire.
(define (check col row placed)
  (unless (equal? placed '())
    (let ((x (car placed)))
      (if (conflict col row (car x) (cdr x))
          (amb)
          (check col row (cdr placed))))))

(define placed '((1 . 1) (3 . 2) (5 . 3) (7 . 4)))
;test horizontal
;(check 6 1 placed)
;test vertical
;(check 1 8 placed)
;test diagonal
;(check 4 7 placed)
;test ok
;(check 2 5 placed)
  
;queens
;(define queens
;  (lambda ()
;    (let loop ((j 1) (placed '()))
;      (if (= j 8)
;          placed
;          (amb
;           (look for line...)
;           (loop (+ 1 j) placed))))))
;            


; attention : ne pas boucler "dans l'ordre" sur j: prévoir une issue si on ne trouve aucune ligne correcte pour une colonne donnée, il faut tout recommencer ?
(define queens
  (lambda ()
    (let loop ((j 1)
               (placed '()))
      (if (equal? 8 (length placed))
          (begin 
            (display "finished")
            placed)
          (begin
            (let ((i (amb 1 2 3 4 5 6 7 8)))
              (check j i placed)
              (begin
                (set! placed (cons `(,j . ,i) placed))
                (loop (+ 1 j) placed))))))))

;(queens)