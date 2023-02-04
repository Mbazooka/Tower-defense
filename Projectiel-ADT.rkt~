;maak hier een projectiel ADT
(define (maak-projectiel-adt positie)

  (define (positie! x y)
    ((positie 'x!) x)
    ((positie 'y!) y))
          
  (define (dispatch msg)
    (cond
      ((eq? msg 'positie) positie)
      ((eq? msg 'positie!) positie!)
      (else "maak-projectiel-adt: ongeldig bericht")))
  dispatch)