;;maak hier een projectiel ADT
(define (maak-projectiel-adt positie bestemming)

  (define (positie! nieuw-positie)
    (set! positie nieuw-positie))

  (define (bestemming-bereikt?)
    (((positie 'ceil) 'gelijk?)  bestemming))

  (define (volgende-positie! update-hoeveelheid-x update-hoeveelheid-y)
    (if (not (bestemming-bereikt?))
        (begin
          ((positie 'x!) (+ (positie 'x) update-hoeveelheid-x))
          ((positie 'y!) (+ (positie 'y) update-hoeveelheid-y)))))

  (define (dispatch msg)
    (cond
      ((eq? msg 'positie) positie)    
      (else "maak-projectiel-adt: ongeldig bericht")))
  dispatch)