;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                               Monster ADT                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (maak-monster-adt type pad) 
  (let ((positie (((pad 'begin) 'positie-copieer)))
        (levens #f)
        (monster-loop-snelheid *rood&&groen&&paars-monster-loop-snelheid*)
        (schild #f)
        (einde (pad 'einde))
        (inflectie-punten (pad 'inflectie-punten))
        (inflectie-tekens (pad 'inflectie-tekens))
        (beweging-richting-x #t)
        (beweging-zin +)) ;; #t beweeg x-richting, #f betekent beweeg y richting

    ;; Voglende code gaat na ... !!!!  
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
        ((eq? type 'groen) (if activeer (begin (set! levens 1) (set! type 'rood)) (set! levens 0)))
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


  
