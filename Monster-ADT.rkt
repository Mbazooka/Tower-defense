;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                               Monster ADT                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Optionele parameters zijn nodig om huidige stand van zaken over te zetten van groen monster naar rood monster
;; Zoals de Inflectie-punten, tekens, enzo.
(define (maak-monster-adt type pad . opt) 
  (let* ((bool (null? opt)) ;; Gedaan vermits dit vaak nodig is (efficientie)
         (positie (if bool ((((pad 'begin)) 'positie-copieer)) (list-ref opt 0)))
         (levens #f)
         (monster-loop-snelheid *rood&&groen&&paars-monster-loop-snelheid*)
         (schild #f)
         (einde ((pad 'einde)))
         (inflectie-punten (if bool (pad 'inflectie-punten) (list-ref opt 1)))
         (inflectie-tekens (if bool (pad 'inflectie-tekens) (list-ref opt 2)))
         (beweging-richting-x (if bool #t (list-ref opt 3)))
         (beweging-zin (if bool + (list-ref opt 4))) ;; #t beweeg x-richting, #f betekent beweeg y richting
         (net-projectielen (cons 'projectiel '()))) ;; Lijst van van net-projectielen die het monster vertraagt hebben

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
      
    ;; Volgende code zal het leven van het monstertje aanpassen afhankelijk van het soort
    (define (verminder-levens! . bomwerp)
      (let ((test (and (pair? bomwerp) (eq? (car bomwerp) 'bomwerp)))) ;; Ga na indien bomwerp-projectiel de levens zal verminderen
        (cond
          ((eq? type 'rood) (set! levens (- levens 1)))
          ((eq? type 'groen) (set! levens 0))
          ((eq? type 'paars) (display levens) (display " : ") (cond                               
                                                                ((and test (<= levens *bomwerp-projectiel-schade*))
                                                                 (set! levens 0))
                                                                ((and test (> levens *bomwerp-projectiel-schade*))
                                                                 (set! levens (- levens *bomwerp-projectiel-schade*)))
                                                                (else
                                                                 (set! levens (- levens 1))))
                             (display levens) (display " : "))
          ((eq? type 'geel) (cond
                              ((and test (> schild 2))
                               (set! schild 0))
                              ((and test (= schild 0))
                               (set! levens 0))
                              (test 
                               (let ((rest (- *bomwerp-projectiel-schade* schild)))
                                 (set! schild 0)
                                 (if (<= levens rest)
                                     (set! levens 0)
                                     (set! levens (- levens 1)))))
                              (else
                               (if (= schild 0)
                                   (set! levens (- levens 1))
                                   (set! schild (- schild 1))))))                                                       
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
        ((eq? type 'groen) (maak-monster-adt 'rood pad positie inflectie-punten inflectie-tekens beweging-richting-x beweging-zin))
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
        ((eq? actie 'verminder) (if (and (pair? projectiel/levens) (eq? (car projectiel/levens) 'bomwerp))                                    
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
        ((eq? msg 'actie-monster-sterven!) actie-monster-sterven!)
        ((eq? msg 'actie-monster-levend!) actie-monster-levend!)
        ((eq? msg 'voeg-net-projectiel-toe!) voeg-net-projectiel-toe!)
        ((eq? msg 'update-tijd-net-projectielen!) update-tijd-net-projectielen!)
        ((eq? msg 'net-al-vetraagd?) net-al-vetraagd?)
        ((eq? msg 'haal-weg-verlopen-net-projectielen!) haal-weg-verlopen-net-projectielen!) 
        ((eq? msg 'soort) 'monster) ;; Toegevoegd om code duplicatie bij teken-adt te vermijden
        (else "maak-monster-adt: ongeldig bericht")))
    dispatch))