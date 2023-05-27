;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                 Levens ADT                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (maak-levens-adt aantal)

  ;; Volgende code bepaalt afhankelijk van monster hoeveel levens afgetrokken worden
  (define (bepaal-hoeveel type)
    (cond
      ((or (eq? type 'rood) (eq? type 'groen)) *rood&&groen-monster-levens-verlies*)
      ((eq? type 'geel) *geel-monster-levens-verlies*)
      ((eq? type 'paars) *paars-monster-levens-verlies*)))

  (define (levens-verminder! monster-lijst)
    (let ((hoeveel (accumulate + 0
                               (map (lambda (monster)
                                      (bepaal-hoeveel (monster 'type)))
                                    monster-lijst))))
      (if (>= hoeveel aantal)
          (set! aantal 0)
          (set! aantal (- aantal hoeveel)))))

  (define (dood?)
    (<= aantal 0))

  (define (reset!)
    (set! aantal *levens-hoeveelheid*))

  (define (dispatch msg)
    (cond
      ((eq? msg 'levens-verminder!) levens-verminder!)
      ((eq? msg 'dood?) dood?)
      ((eq? msg 'status) aantal)
      ((eq? msg 'reset!) reset!)
      (else "maak-leven-adt: ongeldig bericht")))
  dispatch)

    
