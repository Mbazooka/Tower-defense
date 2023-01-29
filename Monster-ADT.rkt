;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                               Monster ADT                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (maak-monster-adt positie type index-midden-pad) ;; Index stelt hoever in midden van het pad hij als is

  (define (positie! x y)
    ((positie 'x!) x)
    ((positie 'y!) y))
    
  (define (dispatch msg)
    (cond
      ((eq? msg 'positie) positie)
      ((eq? msg 'type) type)
      ((eq? msg 'positie!) positie!)
      (else "maak-monster-adt: ongeldig bericht")))
  dispatch)


  