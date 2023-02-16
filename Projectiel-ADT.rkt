;;maak hier een projectiel ADT
(define (maak-projectiel-adt positie te-raken-monster)
  (let ((bestemming (te-raken-monster 'positie)))

  ;; Volgende code gaat na als het projectiel zijn positie bereikt heeft.
  (define (bestemming-bereikt?)
    ((((positie 'ceil)) 'gelijk?)  bestemming))

  ;; Volgende code zal de positie van het projectiel updaten
  (define (volgende-positie! dt)
    (if (not (bestemming-bereikt?))
        (let ((x-pos-proj (positie 'x))
              (y-pos-proj (positie 'y))          
              (x-pos-bestemming  (bestemming 'x))
              (y-pos-bestemming (bestemming 'y)))
          ((positie 'x!) (+ (* x-pos-bestemming *projectiel-afvuur-snelheid* dt) x-pos-proj))
          ((positie 'y!) (+ (* y-pos-bestemming *projectiel-afvuur-snelheid* dt) y-pos-proj)))))

  (define (dispatch msg)
    (cond
      ((eq? msg 'positie) positie)
      ((eq? msg 'bestemming) bestemming)
      ((eq? msg 'te-raken-monster) te-raken-monster)
      ((eq? msg 'bestemming-bereikt?) bestemming-bereikt?)
      ((eq? msg 'volgende-positie!) volgende-positie!)
      (else "maak-projectiel-adt: ongeldig bericht")))
  dispatch))