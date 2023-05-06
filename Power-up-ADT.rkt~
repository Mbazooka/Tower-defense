;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                               Power-up ADT                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (maak-power-up-adt type positie)
  (let ((afkoeling #f))

    (define (activeer!) #f) ;; is de procedure die het gedrag implementeert
    
    (define (stop-afkoeling)
      (if afkoeling ;; Is de afkoeling true? dan ??
          (set! afkoeling #f)))

    (define (afkoeling?) afkoeling)

    (define (dispatch msg)
      (cond
        ((eq? msg 'stop-afkoeling) stop-afkoeling)
        ((eq? msg 'activeer!) activeer!)
        ((eq? msg 'afkoeling?) afkoeling?)
        (else
         "maak-power-up-adt: Ongeldig bericht")))
    dispatch))
          