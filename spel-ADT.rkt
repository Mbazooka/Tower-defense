;maak hier een spel ADT
(define (maak-spel-adt)
  (let((pad (maak-pad-adt vector-1)) ;; maak de fundamenten van het spel
       (teken-adt (maak-teken-adt 1000 600))) 

    ((teken-adt 'teken-spel!) pad) ;; Maakt 1 compenent van het spel

    (define (dispatch msg)
      (cond 
        ((eq? msg 'later) "niks")
        (else
         "Spel-adt: undefined message")))
    dispatch))