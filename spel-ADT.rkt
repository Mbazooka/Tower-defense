;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                  Spel ADT                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;maak hier een spel ADT
(define (maak-spel-adt)
  (let((pad (maak-pad-adt vector-1)) ;; maak de fundamenten van het spel
       (teken-adt (maak-teken-adt 1000 600))) 

    ;; Maakt basis compenenten van het spel
    ((teken-adt 'teken-spel!) pad) 

    ;; Start de dynamische werking van het spel
    (define (start!) #f)

    ;; De procedure die het klikken van muis op scherm voorstelt
    
          
          

    (define (dispatch msg)
      (cond 
        ((eq? msg 'start!) (start!))
        (else
         "Spel-adt: undefined message")))
    dispatch))