;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                 Hulpprocedures                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (accumulate operator null-waarde lijst)
  (if (null? lijst)
      null-waarde
      (operator (car lijst) (accumulate operator null-waarde (cdr lijst)))))

(define (filter pred? lijst)
  (cond
    ((null? lijst) '())
    ((pred? (car lijst))
     (cons (car lijst) (filter pred? (cdr lijst))))
    (else
     (filter pred? (cdr lijst)))))

(define (flatten lijst)
  (accumulate append '() lijst))

;; Volgende code zijn abstracties om vectoren-van-posities af te handelen
;;(kan niet in PAD ADT gezet worden vermits de data genomen word onmiddelijke bij de aanmaak (in let* expressie))
(define neem-inflectie-counter car)
(define neem-inflectie-tekens cadr)
(define neem-vector caddr)

;; Volgende code is globaal gemaakt, vermits die gebruikt word door 2 ADTs (vermijdt dus code duplicatie)
;; Geeft de rand van een bepaald abstract object a.d.h.v 4 posities (bv toren-rand monster-rand)
(define (positie->rand! centraal-positie afstand vector)
  (let ((x-pos-cent (centraal-positie 'x))
        (y-pos-cent (centraal-positie 'y)))
    (vector-set! vector 0 (maak-positie-adt (- x-pos-cent afstand) (+ y-pos-cent afstand)))
    (vector-set! vector 1 (maak-positie-adt (+ x-pos-cent afstand) (+ y-pos-cent afstand)))
    (vector-set! vector 2 (maak-positie-adt (- x-pos-cent afstand) (- y-pos-cent afstand)))
    (vector-set! vector 3 (maak-positie-adt (+ x-pos-cent afstand) (- y-pos-cent afstand)))))

;; Kijkt als positie in een bepaalde rand zit 
(define (in-rand? positie rand)
  (let ((x-pos (positie 'x))
        (y-pos (positie 'y)))
    (and (>= x-pos ((vector-ref rand 0) 'x))
         (<= y-pos ((vector-ref rand 0) 'y))
         (<= x-pos ((vector-ref rand 3) 'x))
         (>= y-pos ((vector-ref rand 3) 'y)))))

