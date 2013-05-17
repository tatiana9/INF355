#lang racket

; procédure de tests unitaires
(define (test x r)
  (if (equal? x r)
      (void)
      (display "Bad test result")))

; plus petit nb impair strict supérieur à n
(define (next-odd n)
  (if (equal? (modulo n 2) 0)
      (+ n 1)
      (+ n 2)))

; n est-il premier
(define prime?
  (lambda (n)
    (cond
      ((< n 2) #f)
      ((= n 2) #t)
      ((equal? (modulo n 2) 0) #f)
      (else (let loop ((i 3) (n n))
              (if (> i (sqrt n))
                  #t
                  (begin
                    (if (equal? (modulo n i) 0)
                        #f
                        (loop (next-odd i) n)))))))))

; résultats des appels à f
(define (map-interval f min max)
  (let ((p (range min (+ max 1) 1)))
    (map f p)))

; liste des n premiers entiers naturels
(define (iota n)
  (cond
    ((<= n 0) '())
    (else (range 0 n))))

(test (next-odd 1) 3)
(test (next-odd 2) 3)
(test (prime? -5) #f)
(test (prime? 0) #f)
(test (prime? 1) #f)
(test (prime? 2) #t)
(test (prime? 3) #t)
(test (prime? 19) #t)
(test (prime? 21) #f)
(test (map-interval (lambda (x) (+ 2 x)) 10 13) '(12 13 14 15))
(test (iota 5) '(0 1 2 3 4))
(test (iota 0) '())
(test (iota -1) '())

; inversion des caractères dans un symbole
(define (revsymb s)
  (let ((l (string->list (symbol->string s))))
    (string->symbol (list->string (reverse l)))))
    
(test (revsymb 'foobar) 'raboof)

; inversion des caractères dans une liste de symboles
(define (trans l)
  (map revsymb l))

(test (trans '(foo bar)) '(oof rab))

; afficher les chaînes d'une liste
(define (dispstrings l)
  (for-each (lambda (x) 
              (begin
                (display x)
                (newline)))
            l))

; filtre ---> avec map ??
(define (filter test l)
  (if (empty? l)
      '()
      (let ((filtered (filter test (cdr l))))
        (if (test (car l))
            (cons (car l) filtered)
            filtered))))

(test (filter (lambda (x) (> x 3)) '(1 10 2 20)) '(10 20))

