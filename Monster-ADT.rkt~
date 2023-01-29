(define (maak-monster-adt positie)

  (define (positie! x y)
    ((positie 'x!) x)
    ((positie 'y!) y))
    
  (define (dispatch msg)
    (cond
      ((eq? msg 'positie) positie)
      ((eq? msg 'positie!) positie!)
      (else "maak-monster-adt: ongeldig bericht")))
  dispatch)
  