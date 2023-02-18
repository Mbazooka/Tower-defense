;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                 Projectiel ADT                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (maak-projectiel-adt initiele-positie positie te-raken-monster type) ;; Initiele positie nodig om te weten van waar hij komt
  (let* ((bestemming (te-raken-monster 'positie))
         (bestemming-extra-1 (maak-positie-adt (+ (bestemming 'x) 1) (+ (bestemming 'y) 1))) ;; Dit zijn extras omdat projectiel soms besteming voor bij gaat
         (bestemming-extra-2 (maak-positie-adt (+ (bestemming 'x) 1) (- (bestemming 'y) 1)))
         (bestemming-extra-3 (maak-positie-adt (- (bestemming 'x) 1) (+ (bestemming 'y) 1)))
         (bestemming-extra-4 (maak-positie-adt (- (bestemming 'x) 1) (- (bestemming 'y) 1)))
         (bestemming-lijst (list bestemming bestemming-extra-1 bestemming-extra-2 bestemming-extra-3 bestemming-extra-4)) 
         (positie-update-hoeveelheid-x (- (bestemming 'x) (initiele-positie 'x))) ;; Dit zijn positie update constanten om smoothe beweging te hebben
         (positie-update-hoeveelheid-y (- (bestemming 'y) (initiele-positie 'y))))

    ;; Volgende code gaat na als het projectiel de bestemming of het extra bestemming positie bereikt heeft.    
    (define (bestemming-bereikt?)
      (define (positie-bereikt bestemming-positie)
        (or ((((positie 'ceil)) 'gelijk?)  ((bestemming-positie 'ceil)))
            ((((positie 'flo)) 'gelijk?) ((bestemming-positie 'flo)))
            ((((positie 'ceil)) 'gelijk?)  ((bestemming-positie 'flo)))
            ((((positie 'flo)) 'gelijk?)  ((bestemming-positie 'ceil)))))
      (accumulate (lambda (x y) (or x y)) #f (map positie-bereikt bestemming-lijst)))

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
        ((eq? msg 'type) type)
        ((eq? msg 'bestemming) bestemming)
        ((eq? msg 'te-raken-monster) te-raken-monster)
        ((eq? msg 'bestemming-bereikt?) bestemming-bereikt?)
        ((eq? msg 'volgende-positie!) volgende-positie!)
        (else "maak-projectiel-adt: ongeldig bericht")))
    dispatch))