#lang racket

; Implémentation de l'opérateur angélique (ou ambigu) "amb".
(define fail #f)

(define (reset)
  (set! fail (lambda () (error "No more alternatives"))))

(reset)

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

(define-syntax amb
  (syntax-rules ()
    ((amb)
     (fail))
    ((amb x . queue)
     (begin
       (let/cc k
         (let ((old-fail fail))
           (set! fail (lambda ()
                        (set! fail old-fail)
                        (k (amb queue))))))
       (car x)))))



;(define-syntax (amb stx)
;  (syntax-case stx()
;    ((_ args ...)
;     (let ((amb (datum->syntax stx 'amb)
     

(define (mult)
  (let ((x (amb1 1 2 3 4 5 6 7 8 9 10))
        (y (amb1 1 2 3 4 5 6 7 8 9 10)))
     (if (= (* x y) 30) 
         (list x y) 
         (amb1))))
;(mult)