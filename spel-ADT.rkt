;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                  Spel ADT                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;maak hier een spel ADT
(define (maak-spel-adt)
  (let ((pad (maak-pad-adt vector-1)) ;; maak de fundamenten van het spel
       (teken-adt (maak-teken-adt 1000 600))
       (torens '())) 

    ;; Maakt basis compenenten van het spel
    ((teken-adt 'teken-spel!) pad) 

    ;; Start de dynamische werking van het spel
    (define (start!)
      ((teken-adt 'set-muis-toets!) muis-klik-procedure))

    ;; De procedure die het klikken van muis op scherm voorstelt    
    (define (muis-klik-procedure toets toestand x y) 
      (cond
        ((and (eq? toets 'left) (eq? toestand 'pressed))
         (cond
           ((and (>= x 840) (<= x 900) (>= y 40) (>= y 100))
            (set! torens (cons 'basis torens)))))
        ((and (eq? toets 'left) (eq? toestand 'released))
         (let ((toren (maak-toren-adt (maak-positie-adt (/ x *px-breedte*) (/ y *px-hoogte*)) (car torens))))
           (set! torens (cons toren (cdr toren)))
           ((teken-adt 'teken-toren!) toren)))))
                 
    (define (dispatch msg)
      (cond 
        ((eq? msg 'start!) (start!))
        (else
         "maak-spel-adt: undefined message")))
    dispatch))