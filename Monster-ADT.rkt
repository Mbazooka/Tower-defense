;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                               Monster ADT                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Optionele parameters zijn nodig om huidige stand van zaken over te zetten van groen monster naar rood monster
;; Zoals de keer-punten, tekens, enzo.
(define (maak-monster-adt type pad . opt) 
  (let* ((bool (null? opt)) ;; Gedaan vermits dit vaak nodig is (efficientie)
         (positie (if bool ((((pad 'begin)) 'positie-copieer)) (bereikte-positie opt)))
         (levens #f)
         (monster-loop-snelheid *rood&&groen&&paars-monster-loop-snelheid*)
         (schild #f)
         (einde ((pad 'einde)))
         (keer-punten (if bool (pad 'keer-punten) (bereikte-keer-punten opt)))
         (keer-tekens (if bool (pad 'keer-tekens) (bereikte-keer-tekens opt)))
         (beweging-richting-x (if bool #t (bereikte-beweging-richting-x opt)))
         (beweging-zin (if bool + (bereikte-beweging-zin opt))) ;; #t beweeg x-richting, #f betekent beweeg y richting
         (net-projectielen (cons 'projectiel '()))) ;; Lijst van van net-projectielen die het monster vertraagt hebben

    ;; Voglende code zet de initiele dingen klaar  
    (define (bepaal-initieel!)
      (cond
        ((eq? type 'rood) (set! levens *levens-rood-monster*))
        ((eq? type 'groen) (set! levens (- *levens-groen-monster* 1))) ;; - 1 door special handeling van groen monster
        ((eq? type 'geel) (set! levens *levens-geel-monster*)
                          (set! monster-loop-snelheid *geel-monster-loop-snelheid*) 
                          (set! schild *schild-geel-monster*))        
        ((eq? type 'paars) (set! levens *levens-paars-monster*))
        (else
         "Geen correcte type")))
    (bepaal-initieel!)

    ;; Volgende zijn hulpprocedures voor volgende-positie!
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

    ;; Volgende code zal het monstertje op de volgende positie zetten
    (define (volgende-positie!)    
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

    ;; Volgende code gaat na indien groen-monster een actie moet doen (indien levens verminderen > 1)
    (define (geen-actie-groen-monster?)
      (< levens 0))

    ;; Volgende code is abstractie en een hulpprocedure
    (define type-vermindering car)
    
    (define (verminder-levens-hoeveelheid type)
      (cond
        ((eq? type 'bomwerp) *bomwerp-levens-verminder*)
        ((eq? type 'bom) (random *bom-levens-verminder-min* *bom-levens-verminder-max*))
        (else
         *standaard-levens-verminder*)))
      
    ;; Volgende code zal het leven van het monstertje aanpassen afhankelijk van het soort    
    (define (verminder-levens! . object)                               
      (let* ((test (and (pair? object) (or (eq? (type-vermindering object) 'bomwerp)
                                           (eq? (type-vermindering object) 'bom)))) ;; Ga na indien bomwerp-projectiel/bom de levens zal verminderen
             (levens-vermindering (if test
                                      (verminder-levens-hoeveelheid (type-vermindering object))
                                      (verminder-levens-hoeveelheid 'standaard-vermindering))))
                        
        (cond
          ((eq? type 'rood) (set! levens (- levens *standaard-levens-verminder*)))
          ((eq? type 'groen) (set! levens (- levens levens-vermindering)))
          ((eq? type 'paars)  (if (<= levens levens-vermindering)
                                  (set! levens *dood*)
                                  (set! levens (- levens levens-vermindering))))
                              
          ((eq? type 'geel)
           (cond
             ((>= schild levens-vermindering)
              (set! schild (- schild levens-vermindering)))
             ((= schild 0)
              (if (<= levens levens-vermindering)
                  (set! levens *dood*)
                  (set! levens (- levens levens-vermindering))))
             (else 
              (let ((rest (- levens-vermindering schild)))
                (set! schild 0)
                (if (<= levens rest)
                    (set! levens *dood*)
                    (set! levens (- levens rest)))))))                                                      
          (else
           "monster-type: ongeldig type"))))

    ;; Volgende code zal de levens van een monster met 1 verhogen
    (define (verhoog-levens!)
      (set! levens (+ levens 1)))

    ;; Volgende code zal een monster vertragen
    (define (vertraag-monster!)
      (let ((nieuwe-snelheid (- monster-loop-snelheid *net-projectiel-vertaging*)))
        (if (>= nieuwe-snelheid 0)
            (set! monster-loop-snelheid nieuwe-snelheid)
            (set! monster-loop-snelheid 0))))
    
    ;; Volgende code zal een actie uitvoeren als een monster gestorven is (als die een actie hoeft te doen)
    (define (actie-monster-sterven!)
      (cond
        ((eq? type 'groen) (maak-monster-adt 'rood pad positie keer-punten keer-tekens beweging-richting-x beweging-zin))
        ((eq? type 'paars)
         (let ((rand-paars-monster (make-vector 4)))
           (positie->rand! positie *paars-monster-rand-afstand* rand-paars-monster) ;; Maakt rand dat level kan gebruiken om alle monster in de buurt met levens te verhogen
           rand-paars-monster))
        (else
         "monster-type: ongeldig type")))

    ;; Volgende code zal een monster een actie laten doen wanneer de projectiel de bestemming bereikt heeft maar nog niks heeft gedaan
    (define (actie-monster-levend! actie . projectiel/levens) ;; Neemt optionele projectiel of levens variabele mee
      (cond
        ((eq? actie 'vertraag) (vertraag-monster!)
                               (voeg-net-projectiel-toe! (car projectiel/levens)))
        ((eq? actie 'verminder) (if (and (pair? projectiel/levens) (or (eq? (car projectiel/levens) 'bomwerp) (eq? (car projectiel/levens) 'bom)))                                    
                                    (verminder-levens! (car projectiel/levens))
                                    (verminder-levens!)))
        (else "Ongeldige actie")))

    ;; Volgende code zal een net-projectiel toevoegen aan de lijst (neemt constante 0 binnen die tijd voorstelt)
    (define (voeg-net-projectiel-toe! net-projectiel)
      (insert! net-projectiel 0 net-projectielen)) 

    ;; Volgende code zal na gaan als een net-projectiel een monster al vertraagd heeft
    (define (net-al-vetraagd? net-projectiel)
      (assq net-projectiel (rest-dict net-projectielen)))

    ;; Volgende code zijn hulpprocedures om de net-projectiel vertragings tijd te bekomen
    (define proj car)
    (define tijd cdr)

    ;; Volgende code zal de net-projectielen hun vertragings actie tijd update
    (define (update-tijd-net-projectielen! dt)
      (if (not (null? net-projectielen))
          (for-each (lambda (projectiel)              
                      (set-cdr! projectiel (+ (tijd projectiel) dt)))
                    (rest-dict net-projectielen)))) ;; rest-dict gedaan vermits het een getagde associate lijst is

    ;; Volgende code zal de net-projectielen waarvan hun vertragingstijd verlopen is weghalen (garbage collection)
    (define (haal-weg-verlopen-net-projectielen!)
      (for-each (lambda (projectiel)
                  (if (>= (tijd projectiel) *net-vertraag-tijd*)
                      (begin
                        (set! monster-loop-snelheid (+  monster-loop-snelheid *net-projectiel-vertaging*))
                        (delete! (proj projectiel) net-projectielen))))
                (rest-dict net-projectielen)))
                 
    (define (dispatch msg)
      (cond
        ((eq? msg 'snelheid) monster-loop-snelheid)
        ((eq? msg 'positie) positie)
        ((eq? msg 'type) type)
        ((eq? msg 'volgende-positie!) volgende-positie!)
        ((eq? msg 'einde?) einde?)
        ((eq? msg 'gestorven?) gestorven?)
        ((eq? msg 'geen-actie-groen-monster?) geen-actie-groen-monster?)
        ((eq? msg 'actie-monster-sterven!) actie-monster-sterven!)
        ((eq? msg 'actie-monster-levend!) actie-monster-levend!)
        ((eq? msg 'verhoog-levens!) verhoog-levens!)
        ((eq? msg 'voeg-net-projectiel-toe!) voeg-net-projectiel-toe!)
        ((eq? msg 'update-tijd-net-projectielen!) update-tijd-net-projectielen!)
        ((eq? msg 'net-al-vetraagd?) net-al-vetraagd?)
        ((eq? msg 'haal-weg-verlopen-net-projectielen!) haal-weg-verlopen-net-projectielen!) 
        ((eq? msg 'soort) 'monster)
        (else "maak-monster-adt: ongeldig bericht")))
    dispatch))