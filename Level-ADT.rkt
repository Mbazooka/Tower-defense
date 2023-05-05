;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                 Level ADT                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (maak-level-adt monster-rij geld levens . vorige-torens) ;; Neemt een rij van monster in die gereleased zullen zijn op het pad, (alsook geld en levens) optionele parameter torens om torens vorige level mee te nemen
  (let ((pad (maak-pad-adt vector-1))
        (torens (if (not (null? vorige-torens)) (car vorige-torens) vorige-torens))
        (monsters '()) ;; Lijst omdat elk element bewerken gemakkelijk is (for-each)
        (net-projectielen '())) ;; Alle net-projectielen die op het pad liggen
    
    ;; Abstracties om type, eerste monster en rest uit lijst te krijgen
    (define type car)
    (define eerste car)
    (define rest cdr)
        
    ;; Volgende code voegt een toren toe tot het spel wereld
    (define (voeg-toren-toe! toren)
      (set! torens (cons toren torens)))
    
    ;; Volgende code update de monsters die op het pad lopen
    (define (update-monsters! dt . update-teken) ;; !!!! Kijk naar volgerde voor efficientie
      (define (zet-terug-monster-lijst! zoeken-monster nieuw-monster monsters) ;; Hulp procedure om monsters te verwisselen (nodig om groen monster te switchen met rood monster zodat torens in juiste volgorde schieten)
        (cond
          ((null? monsters) "Error: Iets misgegaan")
          ((eq? (eerste monsters) zoeken-monster)
           (set-car! monsters nieuw-monster))
          (else
           (zet-terug-monster-lijst! zoeken-monster nieuw-monster (rest monsters)))))

      (define (verhoog-levens-paars-monster! rand-paars-monster) ;; Hulp procedure om alle monsters in buurt van dode paarse monster, hun levens te verhogen
        (for-each (lambda (monster)
                    (if (and (not (eq? (monster 'type) 'paars)) (in-rand? (monster 'positie) rand-paars-monster))
                        ((monster 'verhoog-levens!))))
                  monsters))  
      
      (define (update-vertragings-tijd-monsters!)
        (for-each (lambda (monster)
                    ((monster 'update-tijd-net-projectielen!) dt)
                    ((monster 'haal-weg-verlopen-net-projectielen!)))
                  monsters))
      
      (define (levens-verminder!) ;; Telt aantal monsters aan het einde en vermindert levens
        ((levens 'levens-verminder!) (length (filter (lambda (monster) ((monster 'einde?))) monsters))))

      (define (afhandeling-net-projectielen!)
        (for-each (lambda (projectiel) ;;Vertraagd de monster in de rand van het net-projectiel
                    (for-each (lambda (monster)
                                (if (and ((projectiel 'in-rand?) monster) (not ((monster 'net-al-vetraagd?) projectiel)))
                                    (begin
                                      ((monster 'voeg-net-projectiel-toe!) projectiel)
                                      ((monster 'actie-monster-levend!) 'vertraag)))) 
                              monsters))
                  net-projectielen)
        (set! net-projectielen (filter ;; Nodig anders zal een bepaald net, tot het eind van het spel blijven vertragen
                                (lambda (projectiel)
                                  ((projectiel 'niet-bereikt&&afgehandelt?)))
                                net-projectielen)))

      (define (geld-en-sterven-acties!)
        (for-each (lambda (monster)
                    (if (not (eq? (monster 'type) 'groen))
                        ((geld 'voeg-geld-toe!) (monster 'type))) ;; Zal geld updaten, en indien het een groen monster is, een rood monster spawnen
                    (cond
                      ((eq? (monster 'type) 'groen) (zet-terug-monster-lijst! monster ((monster 'actie-monster-sterven!)) monsters)) ;; Zal rood monster doen spawnen van groen monster
                      ((eq? (monster 'type) 'paars) (verhoog-levens-paars-monster! ((monster 'actie-monster-sterven!))))))                    
                  (filter (lambda (monster) ((monster 'gestorven?))) monsters)))

      (define (overblijvende-monsters!)
        (set! monsters (filter 
                        (lambda (monster)
                          (and (not ((monster 'einde?)))
                               (not ((monster 'gestorven?)))))
                        monsters))) ;; Overblijvende monsters te vermoorden

      (define (monsters-voort-bewegen!)
        (for-each (lambda (monster) ((monster 'volgende-positie!))) monsters)) ;; Overblijvende monster verder laten wandelen
      (monsters-voort-bewegen!)

      (define (volgend-monster-vrijlaten!)
        (if (and (not (null? update-teken)) (eq? (car update-teken) 'toevoegen) (not (null? monster-rij))) ;; Het toevoegen van monsters gedeelte van de procedure
            (begin
              (set! monsters (cons (maak-monster-adt (type monster-rij) pad) monsters))
              (set! monster-rij (rest monster-rij)))))

      ;; Uitvoeren van alle acties 
      (levens-verminder!)
      (afhandeling-net-projectielen!)
      (update-vertragings-tijd-monsters!)
      (geld-en-sterven-acties!)
      (monsters-voort-bewegen!)
      (volgend-monster-vrijlaten!))
    
    ;; Volgende code update de projectielen die door torens werden afgeschoten
    (define (update-torens-projectielen-positie! dt)
      (for-each
       (lambda (toren)         
         ((toren 'projectiel-update!) dispatch dt))        
       torens))

    ;; Volgende code zal projectielen afschieten naar een monster 
    (define (update-torens-projectielen-afschieten! pad) 
      (define (eerste-monster mons)
        (if (null? (cdr mons))
            (car mons)
            (eerste-monster (cdr mons))))

      (define (laatste-monster-weglaten mons)
        (reverse (cdr (reverse mons)))) 
      
      (define (toren-schiet-y/n toren monsters) ;; Procedure die monster zal vinden waarnaar de toren kan schieten (indien monsters in buurt)
        (let ((monster (eerste-monster monsters)))
          (if ((toren 'in-buurt?) monster)
              ((toren 'schiet!) monster pad)
              (if (not (null? (cdr monsters)))
                  (toren-schiet-y/n toren (laatste-monster-weglaten monsters))))))    
      (if (not (null? monsters))
          (for-each
           (lambda (toren)
             (toren-schiet-y/n toren monsters))
           torens)))

    ;; Volgende code zoekt het monster die volgt op het gegeven monster
    (define (monster-na-monster monster)
      (define (hulp-procedure monsters)
        (cond
          ((null? monsters) #f)
          ((null? (rest monsters)) #f)
          ((eq? monster (eerste (rest monsters))) (eerste monsters))
          (else
           (hulp-procedure (rest monsters)))))
      (hulp-procedure monsters))

    ;; Volgende code voegt een net projectiel toe aan de lijst van net projectielen
    (define (voeg-net-projectiel-toe! projectiel)
      (set! net-projectielen (cons projectiel net-projectielen)))

    ;; Volgende code bomwerpt alle monsters in de buurt
    (define (explodeer-monsters-in-buurt! rand)
      (for-each (monster)
                (if (in-rand? rand)
                    ((monster 'actie-monster-levend!) 'bomwerp)
                monsters)))
                         
    ;; Volgende code is om de projectielen van alle torens te verkrijgen (haal weg, maak beter)
    (define (verkrijg-projectielen)
      (flatten
       (map (lambda (toren)
              (toren 'projectielen))
            torens)))

    ;; Volgende code is om te zien als het level aan het einde gekomen is
    (define (einde?)
      (and (null? monster-rij) (null? monsters)))
          
    ;; Volgende code is om de level te skippen naar het einde
    (define (level-einde!)
      (if (not (einde?))
          (begin
            (set! monster-rij '())
            (set! monsters '()))))
                                   
    (define (dispatch msg)
      (cond
        ((eq? msg 'pad) pad)
        ((eq? msg 'monsters) monsters)
        ((eq? msg 'torens) torens)        
        ((eq? msg 'voeg-toren-toe!) voeg-toren-toe!)
        ((eq? msg 'update-monsters!) update-monsters!)
        ((eq? msg 'update-torens-projectielen-positie!) update-torens-projectielen-positie!)
        ((eq? msg 'update-torens-projectielen-afschieten!) update-torens-projectielen-afschieten!)
        ((eq? msg 'monster-na-monster) monster-na-monster)
        ((eq? msg 'voeg-net-projectiel-toe!) voeg-net-projectiel-toe!)
        ((eq? msg 'verkrijg-projectielen) verkrijg-projectielen)
        ((eq? msg 'explodeer-monsters-in-buurt!) explodeer-monsters-in-buurt!)
        ((eq? msg 'einde?) einde?)
        ((eq? msg 'level-einde!) level-einde!)
        ((eq? msg 'soort) 'level)
        (else
         "maak-level-adt: ongeldig bericht")))
    dispatch))