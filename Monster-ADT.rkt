;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                               Monster ADT                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Optionele parameters zijn nodig om huidige stand van zaken over te zetten van groen monster naar rood monster
;; Zoals de Inflectie-punten, tekens, enzo.
(define (maak-monster-adt type pad . opt) 
  (let* ((bool (null? opt)) ;; Gedaan vermits dit vaak nodig is (efficientie)
        (positie (if bool (((pad 'begin) 'positie-copieer)) (list-ref opt 0)))
        (levens #f)
        (monster-loop-snelheid *rood&&groen&&paars-monster-loop-snelheid*)
        (schild #f)
        (einde (pad 'einde))
        (inflectie-punten (if bool (pad 'inflectie-punten) (list-ref opt 1)))
        (inflectie-tekens (if bool (pad 'inflectie-tekens) (list-ref opt 2)))
        (beweging-richting-x (if bool #t (list-ref opt 3)))
        (beweging-zin (if bool + (list-ref opt 4)))) ;; #t beweeg x-richting, #f betekent beweeg y richting

    ;; Voglende code zet de initiele dingen klaar  
    (define (bepaal-initieel!)
      (cond
        ((eq? type 'rood) (set! levens *levens-rood-monster*))
        ((eq? type 'groen) (set! levens *levens-groen-monster*))
        ((eq? type 'geel) (set! levens *levens-geel-monster*)
                          (set! monster-loop-snelheid *geel-monster-loop-snelheid*) ;; Enkel hier geassigned voor efficiente
                          (set! schild *schild-geel-monster*))        
        ((eq? type 'paars) (set! levens *levens-paars-monster*))
        (else
         "Geen correcte type")))
    (bepaal-initieel!)

    ;; Volgende code zal het monstertje op de volgende positie zetten
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
          ((positie 'x!) (+ (positie 'x) monster-loop-snelheid))
          ((positie 'y!) (beweging-zin (positie 'y) monster-loop-snelheid))))

    ;; Volgende code zal na gaan indien het monster aan het einde is van het pad
    (define (einde?)
      (>= (positie 'x) (einde 'x)))

    ;; Volgende code gaat na indien monster gestorven is of niet
    (define (gestorven?)
      (<= levens 0))
      
    ;; Volgende code zal het leven van het monstertje verminderen met 1.
    ;; De optionele parameter activeer zorgt ervoor dat het groen monster rood monster kan komen
    ;; Meer bepaald, het groen monster word vermoord om bij het level adt als dood beschouwd te worden
    ;; Zo kan men dat monster vast pakken, wetende dat hij geraakt is, en kan men dan het monster omvormen naar rood
    ;; Deze activeer neemt enkel de waarde #t. 
    (define (verander-levens! . activeer) 
      (cond 
        ((eq? type 'rood)  (set! levens (- levens 1)))
        ((eq? type 'groen) (if (eq? activeer #t) (maak-monster 'rood pad positie inflectie-punten inflectie-tekens beweging-richting-x beweging-zin)
                                                      (set! levens 0))) ;; !!!! Verander hier nog de procedure !!!! 
        ((eq? type 'geel) (if (= schild 0) (set! levens (- levens 1)) (set! schild (- schild 1)))) ;; !!!!Moet nog veranderen normaal!!!!
        ((eq? type 'paars) (set! levens (- levens 1))) ;; !!!!Meer monster levens!!!!
        (else
         "monster-type: ongeldig type")))    
        
    (define (dispatch msg)
      (cond
        ((eq? msg 'positie) positie)
        ((eq? msg 'type) type)
        ((eq? msg 'volgende-positie!) volgende-positie!)
        ((eq? msg 'einde?) einde?)
        ((eq? msg 'gestorven?) gestorven?)
        ((eq? msg 'verander-levens!) verander-levens!)
        ((eq? msg 'soort) 'monster) ;; Toegevoegd om code duplicatie bij teken-adt te vermijden
        (else "maak-monster-adt: ongeldig bericht")))
    dispatch))


  
