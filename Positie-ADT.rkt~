;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                               Positie ADT                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (maak-positie-adt x y)
    
  (define (x! nieuw-x)
    (set! x nieuw-x))

  (define (y! nieuw-y)
    (set! y nieuw-y))

;  (define (afstand positie2) ;; nog niet gebruikt
;    (define (square x)
;      (* x x))
;
;    (sqrt
;     (+ (square (- x (positie2 'x))) (square (- y (positie2 'y))))))

  (define (gelijk? positie2) 
    (and (= x (positie2 'x)) (= y (positie2 'y))))
               
;  (define (beweeg! nieuw-x nieuw-y) ;; nog niet gebruikt
;    (x! nieuw-x)
;    (y! nieuw-y))

  (define (dispatch msg)
    (cond
      ((eq? msg 'x) x)
      ((eq? msg 'y) y)
      ((eq? msg 'x!) x!)
      ((eq? msg 'y!) y!)
;      ((eq? msg 'afstand) afstand)
      ((eq? msg 'gelijk?) gelijk?)
;      ((eq? msg 'beweeg!) beweeg!)
      (else "maak-positie-adt: ongeldig bericht")))
  dispatch)
