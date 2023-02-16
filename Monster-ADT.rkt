;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                               Monster ADT                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (maak-monster-adt positie type einde index-midden-pad) ;; Index stelt hoever in midden van het pad hij als is
  (let ((levens #f))

    ;; Voglende code gaat na hoeveel levens het monster mag hebben (op basis van type)
    (define (bepaal-levens!)
      (cond
        ((eq? type 'rood) (set! levens 1))
        (else
         "Geen correcte type")))

    (bepaal-levens!)

    ;; Volgende code gaat na als het monstertje op het eind van het pad is
    (define (einde?)
      (>= index-midden-pad einde))

    ;; Volgende code zal het monstertje op de volgende positie zetten
    (define (volgende-positie! nieuw-positie)
      (set! positie nieuw-positie)
      (set! index-midden-pad (+ index-midden-pad 1)))

    ;; Volgende code zal het leven van het monstertje verminderen met 1.
    (define (verander-levens!)
      (set! levens (- levens 1)))

    (define (dispatch msg)
      (cond
        ((eq? msg 'positie) positie)
        ((eq? msg 'type) type)
        ((eq? msg 'index) index-midden-pad)
        ((eq? msg 'volgende-positie!) volgende-positie!)
        ((eq? msg 'einde?) einde?)
        ((eq? msg 'gestorven?) (<= levens 0))
        ((eq? msg 'verander-levens!) verander-levens!)
        (else "maak-monster-adt: ongeldig bericht")))
    dispatch))


  