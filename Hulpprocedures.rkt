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
(define neem-keer-counter car)
(define neem-keer-tekens cadr)
(define neem-vector caddr)

;; Volgende code zijn abstracties om mooi data uit optionele lijst in monster-adt/power-up-adt te accesseren
(define (bereikte-positie lijst) (list-ref lijst 0))
(define (bereikte-keer-punten lijst) (list-ref lijst 1))
(define (bereikte-keer-tekens lijst) (list-ref lijst 2))
(define (bereikte-beweging-richting-x lijst) (list-ref lijst 3))
(define (bereikte-beweging-zin lijst) (list-ref lijst 4))
(define neem-power-up-drop-positie car)

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

;; Volgende code zijn abstracties om met dictionaries te werken (hier gezet want enkel hier gebruikt)
(define (associatie dict)
  (car dict))

(define (rest-dict dict)
  (cdr dict))

(define (sleutel associatie)
  (car associatie))

(define (waarde associatie)
  (cdr associatie))

;; Volgende code steekt een associatie in de dictionary
(define (insert! sleut value tagged-dict)
  (let ((toe-te-voegen (list (cons sleut value))))
    (set-cdr! toe-te-voegen (rest-dict tagged-dict))
    (set-cdr! tagged-dict toe-te-voegen)))

;; Volgende code delete een bepaalde sleutel uit de dictionary
(define (delete! sleut dict) 
  (define (delete-hulp huidige vorige)
    (cond
      ((null? (rest-dict huidige))
       (if (eq? (sleutel (associatie huidige)) sleut)
           (set-cdr! vorige '())
           #f))
      ((eq? (sleutel (associatie huidige)) sleut)
       (set-cdr! vorige (rest-dict huidige)))
      (else
       (delete-hulp (rest-dict huidige) huidige))))
  (delete-hulp (cdr dict) dict))

;; Volgende code gaat na als een object een positie is
(define (monster? obj)
  (eq? (obj 'soort) 'monster))

;; volgende code zijn abstracties om met 'monster-vector' en 'pad-vector' te werken
(define (monster-vector-verkrijg level ronde)
  (vector-ref (vector-ref monster-vector (- level 1)) (- ronde 1)))
