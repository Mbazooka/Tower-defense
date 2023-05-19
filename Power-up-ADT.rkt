;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                               Power-up ADT                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (maak-power-up-adt pad type)
  (let ((positie ((((pad 'begin)) 'positie-copieer)))
        (einde ((pad 'einde)))
        (pad-lengte ((pad 'lengte)))
        (keer-punten (pad 'keer-punten))
        (keer-tekens (pad 'keer-tekens))
        (beweging-richting-x #t)
        (beweging-zin +)
        (tijd 0)
        (bommen '()))

    ;; Volgende code gaat na indien de power-up het einde van het pad bereikt heeft
    (define (einde?)
      (>= (positie 'x) (einde 'x)))
        
    ;; Volgende code zal de tank op de volgende positie zetten
    (define (volgende-positie!)
      (define (teken-bepaling!) ;; Zal nagaan bij het veranderen van bewegingsdimensie in welke zin verandert moet worden.
        (cond
          ((and (null? keer-tekens) (eq? beweging-zin +)) (set! beweging-zin -))
          ((and (null? keer-tekens) (eq? beweging-zin -)) (set! beweging-zin +))
          ((eq? (car keer-tekens) '-) (set! beweging-zin -) (set! keer-tekens (cdr keer-tekens)))
          ((eq? (car keer-tekens) '+) (set! beweging-zin +) (set! keer-tekens (cdr keer-tekens)))
          (else
           "Doe niets")))
              
      (define (richting-verandering!) ;; Zal bij het bereiken van een keerpunt, veranderen van bewegingsrichting 
        (if (not (null? keer-punten))
            (if ((((positie 'ceil)) 'gelijk?) (car keer-punten)) ;; keerpunt bereikt?
                (begin
                  (set! beweging-richting-x (not beweging-richting-x))
                  (set! keer-punten (cdr keer-punten))
                  (teken-bepaling!)))))     
      (richting-verandering!)      
      (if beweging-richting-x
          ((positie 'x!) (+ (positie 'x) *tank-rijd-snelheid*))
          ((positie 'y!) (beweging-zin (positie 'y) *tank-rijd-snelheid*))))

    ;; Volgende code update de power-up op een gegeven manier
    (define (update! dt)
      (cond
        ((eq? type 'tank)
         (volgende-positie!))        
        ((eq? type 'bommen-regen) (set! tijd (+ tijd dt)))))

    (define (maak-bommen! aantal)
      (define (maak-hulp ctr)
        (let ((num (random pad-lengte)))
    
    (define (dispatch msg)
      (cond
        ((eq? msg 'positie) positie)
        ((eq? msg 'einde?) einde?)
        ((eq? msg 'update!) update!)
        ((eq? msg 'activeer!) activeer!)
        ((eq? msg 'geactiveerd?) geactiveerd?)
        ((eq? msg 'tijd) tijd)
        ((eq? msg 'bommen) bommen)
        ((eq? msg 'type) type)
        ((eq? msg 'soort) 'power-up)
        (else
         "maak-power-up-adt: Ongeldig bericht")))
    dispatch))
