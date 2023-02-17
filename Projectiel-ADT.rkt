;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                 Projectiel ADT                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (maak-projectiel-adt initiele-positie positie te-raken-monster) ;; Initiele positie nodig om te weten van waar hij komt
  (let* ((bestemming (te-raken-monster 'positie))
        (positie-update-hoeveelheid-x (- (bestemming 'x) (initiele-positie 'x))) ;; Dit zijn positie update constanten om smoothe beweging te hebben
        (positie-update-hoeveelheid-y (- (bestemming 'y) (initiele-positie 'y))))

  ;; Volgende code gaat na als het projectiel zijn positie bereikt heeft.
  (define (bestemming-bereikt?)
    (or
     ((((positie 'ceil)) 'gelijk?)  ((bestemming 'ceil)))
     ((((positie 'flo)) 'gelijk?) ((bestemming 'flo)))
     ((((positie 'ceil)) 'gelijk?)  ((bestemming 'flo)))
     ((((positie 'flo)) 'gelijk?)  ((bestemming 'ceil)))))

  ;; Volgende code zal de positie van het projectiel updaten
  (define (volgende-positie!)
    (if (not (bestemming-bereikt?))
        (let ((x-pos-proj (positie 'x))
              (y-pos-proj (positie 'y)))
          ((positie 'x!) (+ (* positie-update-hoeveelheid-x *projectiel-afvuur-snelheid*) x-pos-proj))
          ((positie 'y!) (+ (* positie-update-hoeveelheid-y *projectiel-afvuur-snelheid*) y-pos-proj)))))

  (define (dispatch msg)
    (cond
      ((eq? msg 'positie) positie)
      ((eq? msg 'bestemming) bestemming)
      ((eq? msg 'te-raken-monster) te-raken-monster)
      ((eq? msg 'bestemming-bereikt?) bestemming-bereikt?)
      ((eq? msg 'volgende-positie!) volgende-positie!)
      (else "maak-projectiel-adt: ongeldig bericht")))
  dispatch))