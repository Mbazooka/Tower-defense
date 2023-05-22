;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                 Levens ADT                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; !!!!! Op basis van mosnter type, levens veranderen !!!!!
(define (maak-leven-adt aantal)
  
  (define (levens-verminder! hoeveel)
    (set! aantal (- aantal hoeveel)))

  (define (dood?)
    (<= aantal 0))

  (define (reset!)
    (set! aantal *levens-hoeveelheid*))

  (define (dispatch msg)
    (cond
      ((eq? msg 'levens-verminder!) levens-verminder!)
      ((eq? msg 'dood?) dood?)
      ((eq? msg 'status) aantal)
      ((eq? msg 'soort) 'levens)
      ((eq? msg 'reset!) reset!)
      (else "maak-leven-adt: ongeldig bericht")))
  dispatch)

    
