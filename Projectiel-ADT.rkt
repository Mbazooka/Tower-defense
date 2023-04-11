;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                 Projectiel ADT                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (maak-projectiel-adt positie type te-raken-monster) 
  (let* ((bestemming (((te-raken-monster 'positie) 'positie-copieer)))
         (bestemming-x (bestemming 'x)) ;; Vaak nodig dus 1 maal berekent
         (bestemming-y (bestemming 'y))
         (initiele-positie ((positie 'positie-copieer)))
         (bestemming-extra-1 (maak-positie-adt (+ bestemming-x 1) (+ bestemming-y 1))) ;; Dit zijn extras omdat projectiel soms besteming voor bij gaat
         (bestemming-extra-2 (maak-positie-adt (+ bestemming-x 1) (- bestemming-y 1)))
         (bestemming-extra-3 (maak-positie-adt (- bestemming-x 1) (+ bestemming-y 1)))
         (bestemming-extra-4 (maak-positie-adt (- bestemming-x 1) (- bestemming-y 1)))
         (bestemming-lijst (list bestemming bestemming-extra-1 bestemming-extra-2 bestemming-extra-3 bestemming-extra-4)) 
         (positie-update-hoeveelheid-x (- bestemming-x (initiele-positie 'x))) ;; Dit zijn positie update constanten om gewicht te introduceren en ze zo smooth naar hun eindbestemming te brengen
         (positie-update-hoeveelheid-y (- bestemming-y (initiele-positie 'y))))

    ;; Volgende code gaat na als het projectiel de bestemming of de extra bestemming posities bereikt heeft.    
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

    ;; Volgende code voert actie uit op monster (afhankelijk van het type projectiel)
    (define (actie-te-raken-monster!)
      (cond
        ((eq? type 'steen) ((te-raken-monster 'actie-monster-levend!) 'verminder))
        ((eq? type 'net) ((te-raken-monster 'actie-monster-levend!) 'vertraag))
        (else "Projectiel: ongeldig type")))

    (define (dispatch msg)
      (cond
        ((eq? msg 'positie) positie)
        ((eq? msg 'type) type)
        ((eq? msg 'te-raken-monster) te-raken-monster)
        ((eq? msg 'bestemming-bereikt?) bestemming-bereikt?)
        ((eq? msg 'volgende-positie!) volgende-positie!)
        ((eq? msg 'actie-te-raken-monster!) actie-te-raken-monster!)
        ((eq? msg 'soort) 'projectiel)
        (else "maak-projectiel-adt: ongeldig bericht")))
    dispatch))