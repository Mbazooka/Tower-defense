;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                               Positie ADT                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (maak-positie-adt x y)
    
  (define (x! nieuw-x)
    (set! x nieuw-x))

  (define (y! nieuw-y)
    (set! y nieuw-y))

  (define (gelijk? positie2) 
    (and (= x (positie2 'x)) (= y (positie2 'y))))

  ;; Volgende code is om een naar beneden afgeronde versie van de huidige positie te maken (vaak nodig)
  (define (ceil)
    (maak-positie-adt (ceiling x) (ceiling y)))

  (define (dispatch msg)
    (cond
      ((eq? msg 'x) x)
      ((eq? msg 'y) y)
      ((eq? msg 'x!) x!)
      ((eq? msg 'y!) y!)
      ((eq? msg 'gelijk?) gelijk?)
      ((eq? msg 'ceil) ceil)
      (else "maak-positie-adt: ongeldig bericht")))
  dispatch)
