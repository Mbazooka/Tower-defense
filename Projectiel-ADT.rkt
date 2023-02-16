;;maak hier een projectiel ADT
(define (maak-projectiel-adt positie te-raken-monster)
  (let* ((bestemming (te-raken-monster 'positie))
        (positie-update-hoeveelheid-x (- (bestemming 'x) (positie 'x)))
        (positie-update-hoeveelheid-y (- (bestemming 'y) (positie 'y))))

  ;; Volgende code gaat na als het projectiel zijn positie bereikt heeft.
  (define (bestemming-bereikt?)
    ((((positie 'ceil)) 'gelijk?)  ((bestemming 'ceil))))

  ;; Volgende code zal de positie van het projectiel updaten
  (define (volgende-positie!)
    (if (not (bestemming-bereikt?))
        (let ((x-pos-proj (positie 'x))
              (y-pos-proj (positie 'y)))
          ((positie 'x!) (+ positie-update-hoeveelheid-x x-pos-proj))
          ((positie 'y!) (+ positie-update-hoeveelheid-y y-pos-proj)))))

  (define (dispatch msg)
    (cond
      ((eq? msg 'positie) positie)
      ((eq? msg 'bestemming) bestemming)
      ((eq? msg 'te-raken-monster) te-raken-monster)
      ((eq? msg 'bestemming-bereikt?) bestemming-bereikt?)
      ((eq? msg 'volgende-positie!) volgende-positie!)
      (else "maak-projectiel-adt: ongeldig bericht")))
  dispatch))