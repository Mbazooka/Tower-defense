;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                               Monster ADT                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;s
(define (maak-monster-adt positie type einde inflectie-punten inflectie-tekens) ;; Index stelt hoever in midden van het pad hij als is
  (let ((levens #f)
        (beweging-richting-x #t)
        (beweging-zin +)) ;; #t beweeg x-richting, #f betekent beweeg y richting

    ;; Voglende code gaat na hoeveel levens het monster mag hebben (op basis van type)
    (define (bepaal-levens!)
      (cond
        ((eq? type 'rood) (set! levens 1))
        (else
         "Geen correcte type")))

    (bepaal-levens!)

    ;; Volgende code gaat na als het monstertje op het eind van het pad is
    (define (einde?)
      (>= (positie 'x) (einde 'x)))

    ;; Volgende code zal het monstertje op de volgende positie zetten
    (define (volgende-positie!)
      (define (teken-bepaling!) ;; Zal nagaan bij het veranderen van bewegingsdimensie in welke zin verandert moet worden.
        (cond
          ((and (null? inflectie-tekens) (eq? beweging-zin +)) (set! beweging-zin -))
          ((and (null? inflectie-tekens) (eq? beweging-zin -)) (set! beweging-zin +))
          ((eq? (car inflectie-tekens) '-) (set! beweging-zin -) (set! inflectie-tekens (cdr inflectie-tekens)))
          ((eq? (car inflectie-tekens) '+) (set! beweging-zin +) (set! inflectie-tekens (cdr inflectie-tekens)))))
              
      (define (richting-verandering!) ;; Zal bij het bereiken van een inflectie punt, veranderen van bewegingsrichting 
        (if (not (null? inflectie-punten))
            (if ((((positie 'ceil)) 'gelijk?) (car inflectie-punten))
                (begin
                  (set! beweging-richting-x (not beweging-richting-x))
                  (set! inflectie-punten (cdr inflectie-punten))
                  (teken-bepaling!)))))     
      (richting-verandering!)      
      (if beweging-richting-x
          ((positie 'x!) (+ (positie 'x) *monster-loop-snelheid*))
          ((positie 'y!) (beweging-zin (positie 'y) *monster-loop-snelheid*))))
      
      ;; Volgende code zal het leven van het monstertje verminderen met 1.
      (define (verander-levens!)
        (set! levens (- levens 1)))

      (define (dispatch msg)
        (cond
          ((eq? msg 'positie) positie)
          ((eq? msg 'type) type)
          ((eq? msg 'volgende-positie!) volgende-positie!)
          ((eq? msg 'einde?) einde?)
          ((eq? msg 'gestorven?) (<= levens 0))
          ((eq? msg 'verander-levens!) verander-levens!)
          (else "maak-monster-adt: ongeldig bericht")))
      dispatch))


  