;maak hier een spel ADT
(define (maak-spel-adt)
  (let((pad (maak-pad-adt vector-1))
       (teken-adt (maak-teken-adt 1000 600))) ;; Vergroot voor menu

    ((teken-adt 'teken-spel!) pad)

    (define (dispatch msg)
      (cond 
        ((eq? msg 'later) "niks")))
    dispatch))