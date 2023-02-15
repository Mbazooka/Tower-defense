;;maak hier een projectiel ADT
(define (maak-projectiel-adt positie bestemming)

  (define (positie! nieuw-positie)
    (set! positie nieuw-positie))

  (define (dispatch msg)
    (cond
      ((eq? msg 'positie) positie)
      ((eq? msg 'positie!) positie!)
      ((eq? msg 'bestemming-bereikt?) ((positie 'gelijk?) bestemming))
      (else "maak-projectiel-adt: ongeldig bericht")))
  dispatch)