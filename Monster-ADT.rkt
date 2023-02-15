;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                               Monster ADT                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (maak-monster-adt positie type einde index-midden-pad) ;; Index stelt hoever in midden van het pad hij als is
  (let ((levens #f)) 

    (define (bepaal-levens-type!)
      (cond
        ((eq? type 'basis) (set! levens 1))
        (else
         "Probleem met code")))

    (bepaal-levens-type!)

    ;; Volgende code gaat na als het monstertje op het eind van het pad is
    (define (einde?)
      (>= index-midden-pad einde))

    ;; Volgende code zal het monstertje op de volgende positie zetten
    (define (volgende-positie! nieuw-positie)
      (set! positie nieuw-positie)
      (set! index-midden-pad (+ index-midden-pad 1)))

    (define (dispatch msg)
      (cond
        ((eq? msg 'positie) positie)
        ((eq? msg 'type) type)
        ((eq? msg 'index) index-midden-pad)
        ((eq? msg 'volgende-positie!) volgende-positie!)
        ((eq? msg 'einde?) einde?)      
        (else "maak-monster-adt: ongeldig bericht")))
    dispatch))


  