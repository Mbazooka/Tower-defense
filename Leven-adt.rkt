;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                 Leven ADT                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (maak-leven-adt aantal)
  (define (levens-verminder!)
    (if (> aantal 0)
        (set! aantal (- aantal 1))
        "Einde spel"))

  (define (dood?)
    (= aantal 0))

  (define (reset!)
    (set! aantal *levens-hoeveelheid*))

  (define (dispatch msg)
    (cond
      ((eq? msg 'levens-verminder!) levens-verminder!)
      ((eq? msg 'dood?) dood?)
      ((eq? msg 'reset!) reset!)
      (else "maak-leven-adt: ongeldig bericht")))
  dispatch)

    
