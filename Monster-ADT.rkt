;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                               Monster ADT                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (maak-monster-adt positie type pad index-midden-pad) ;; Index stelt hoever in midden van het pad hij als is

  (define (einde?)
    (= (pad 'einde) index-midden-pad))
  
  (define (volgende-positie! nieuw-positie)
    (set! positie nieuw-positie)
    (set! index-midden-pad (+ index-midden-pad 1)))
    
  (define (dispatch msg)
    (cond
      ((eq? msg 'type) type)
      ((eq? msg 'index) index-midden-pad)
      ((eq? msg 'einde?) einde?)
      ((eq? msg 'volgende-positie!) volgende-positie!)
      (else "maak-monster-adt: ongeldig bericht")))
  dispatch)


  