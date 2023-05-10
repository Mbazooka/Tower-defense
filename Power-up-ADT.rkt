;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                               Power-up ADT                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (maak-power-up-adt pad type)
  (let ((einde ((pad 'einde)))
        (inflectie-punten (if bool (pad 'inflectie-punten) (list-ref opt 1)))
        (inflectie-tekens (if bool (pad 'inflectie-tekens) (list-ref opt 2)))
        (beweging-richting-x (if bool #t (list-ref opt 3)))
        (beweging-zin (if bool + (list-ref opt 4))))

    ;; Volgende code gaat na indien de power-up het einde van het pad bereikt heeft
    (define (einde?)
      (>= (positie 'x) (einde 'x)))
        
    ;; Volgende code zal de tank op de volgende positie zetten
    (define (volgende-positie!)
      (define (teken-bepaling!) ;; Zal nagaan bij het veranderen van bewegingsdimensie in welke zin verandert moet worden.
        (cond
          ((and (null? inflectie-tekens) (eq? beweging-zin +)) (set! beweging-zin -))
          ((and (null? inflectie-tekens) (eq? beweging-zin -)) (set! beweging-zin +))
          ((eq? (car inflectie-tekens) '-) (set! beweging-zin -) (set! inflectie-tekens (cdr inflectie-tekens)))
          ((eq? (car inflectie-tekens) '+) (set! beweging-zin +) (set! inflectie-tekens (cdr inflectie-tekens)))
          (else
           "Doe niets")))
              
      (define (richting-verandering!) ;; Zal bij het bereiken van een inflectie punt, veranderen van bewegingsrichting 
        (if (not (null? inflectie-punten))
            (if ((((positie 'ceil)) 'gelijk?) (car inflectie-punten)) ;; inflectie punt bereikt?
                (begin
                  (set! beweging-richting-x (not beweging-richting-x))
                  (set! inflectie-punten (cdr inflectie-punten))
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
        ((eq? type 'bommen-regen) #f)))

    (define (dispatch msg)
      (cond
        ((eq? msg 'einde?) einde?)
        ((eq? msg 'update!) update!)
        (else
         "maak-power-up-adt: Ongeldig bericht")))
    dispatch))
