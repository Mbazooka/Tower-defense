;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                 Level ADT                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (maak-level-adt monster-rij geld levens . vorige-torens) ;; Neemt een rij van monster in die gereleased zullen zijn op het pad, (alsook geld en levens) optionele parameter torens om torens vorige level mee te nemen
  (let ((pad (maak-pad-adt vector-1))
        (torens (if (not (null? vorige-torens)) (car vorige-torens) vorige-torens))
        (monsters '()) ;; Lijst omdat elk element bewerken gemakkelijk is (for-each)
        (activeerde-tank '()) ;; Lijst om extra proceduredefinities uit te sparen
        (tank-power-up-monsters '()) ;; Dat zijn de monsters waarop de tank een invloed zal hebben 
        (activeerde-bommen-regen '())
        (net-projectielen '())) ;; Alle net-projectielen die op het pad liggen
    
    ;; Abstracties om type, eerste monster en rest uit lijst te krijgen
    (define type car)
    (define eerste car)
    (define rest cdr)
        
    ;; Volgende code voegt een toren toe tot het spel wereld
    (define (voeg-toren-toe! toren)
      (set! torens (cons toren torens)))

    (define (overblijvende-monsters!)
      (set! monsters (filter 
                      (lambda (monster)
                        (and (not ((monster 'einde?)))
                             (not ((monster 'gestorven?)))))
                      monsters))) ;; Overblijvende monsters te vermoorden
    
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
                                (if (and ((projectiel 'binnen-rand?) monster) (not ((monster 'net-al-vetraagd?) projectiel)))
                                    (begin
                                      ((monster 'voeg-net-projectiel-toe!) projectiel)
                                      ((monster 'actie-monster-levend!) 'vertraag projectiel)))) 
                              monsters))
                  net-projectielen)
        (set! net-projectielen (filter ;; Nodig anders zal een bepaald net, tot het eind van het spel blijven vertragen
                                (lambda (projectiel)
                                  ((projectiel 'niet-bereikt&&afgehandelt?)))
                                net-projectielen)))

      (define (geld-en-sterven-acties!)
        (for-each (lambda (monster)                    
                    ((geld 'voeg-geld-toe!) (monster 'type) #f) ;; Zal geld updaten, en indien het een groen monster is, een rood monster spawnen
                    (cond                      
                      ((eq? (monster 'type) 'groen) (zet-terug-monster-lijst! monster ((monster 'actie-monster-sterven!)) monsters)) ;; Zal rood monster doen spawnen van groen monster
                      ((eq? (monster 'type) 'paars) (verhoog-levens-paars-monster! ((monster 'actie-monster-sterven!))))))                    
                  (filter (lambda (monster) ((monster 'gestorven?))) monsters)))

      (define (monsters-voort-bewegen!)
        (for-each (lambda (monster) ((monster 'volgende-positie!))) monsters)) ;; Overblijvende monster verder laten wandelen
      (monsters-voort-bewegen!)

      (define (volgend-monster-vrijlaten!)
        (if (and (not (null? update-teken)) (eq? (car update-teken) 'toevoegen) (not (null? monster-rij))) ;; Het toevoegen van monsters gedeelte van de procedure
            (begin
              (set! monsters (cons (maak-monster-adt (type monster-rij) pad) monsters))
              (set! monster-rij (rest monster-rij)))))

      ;; Uitvoeren van alle acties van update-monsters!
      (levens-verminder!)
      (afhandeling-net-projectielen!)
      (update-vertragings-tijd-monsters!)
      (geld-en-sterven-acties!)
      (overblijvende-monsters!)
      (monsters-voort-bewegen!)
      (volgend-monster-vrijlaten!))
    
    ;; Volgende code update de projectielen die door torens werden afgeschoten
    (define (update-torens-projectielen-positie! dt)
      (for-each
       (lambda (toren)         
         ((toren 'projectiel-update!) dispatch dt))        
       torens))

    ;; Volgende code zal projectielen afschieten naar een monster 
    (define (update-torens-projectielen-afschieten! pad dt) 
      (define (eerste-monster mons)
        (if (null? (cdr mons))
            (car mons)
            (eerste-monster (cdr mons))))

      (define (laatste-monster-weglaten mons)
        (reverse (cdr (reverse mons)))) 
      
      (define (toren-schiet-y/n toren monsters) ;; Procedure die monster zal vinden waarnaar de toren kan schieten (indien monsters in buurt)
        (let ((monster (eerste-monster monsters)))
          (cond
            ((and ((toren 'in-buurt?) monster) (eq? (toren 'type) 'bomwerp-toren))
             (let ((aantal (length (filter (lambda (monster)
                                             ((toren 'in-buurt?) monster))
                                           monsters))))
               (if (> aantal *meerdere-monsters*)
                   ((toren 'schiet!) monster pad))))
            (((toren 'in-buurt?) monster) ((toren 'schiet!) monster pad))
            (else
             (if (not (null? (cdr monsters)))
                 (toren-schiet-y/n toren (laatste-monster-weglaten monsters)))))))    
      (if (not (null? monsters))
          (for-each
           (lambda (toren)
             (if ((toren 'schieten?))
                 (begin 
                   (toren-schiet-y/n toren monsters)
                   ((toren 'update-afvuur-tijd!) dt)) ;; Na het schieten, moet tijd up gedate worden
                 ((toren 'update-afvuur-tijd!) dt)))
           torens)))

    ;; Volgende code zal de toren-afvuur-tijden op 0 zetten
    (define (initialiseer-toren-tijden!)
      (for-each (lambda (toren) ((toren 'initialiseer-tijd!))) torens))

    ;; Volgende zal power-ups hun staat updaten
    (define (update-power-ups! dt)
      (tanken-verminder-monster-levens! activeerde-tank)
      (set! activeerde-tank (filter (lambda (tank) (not ((tank 'einde?)))) activeerde-tank)) ;; Haalt alle voorbijgegaande tanken weg
      (for-each (lambda (tank) ((tank 'update!) dt)) activeerde-tank)
      (for-each (lambda (bom-regen) ((bom-regen 'update!) dt)) activeerde-bommen-regen)
      (for-each (lambda (bom-regen)
                  (if ((bom-regen 'tijd-afgelopen?))
                      ((bom-regen 'bom-explosie!) explodeer-monsters-in-buurt!)))
                  activeerde-bommen-regen)
      (set! activeerde-bommen-regen (filter (lambda (bom-regen)
                                              (not ((bom-regen 'tijd-afgelopen?)))) ;; Haalt alle afgehandelte bom-regens
                                              activeerde-bommen-regen)))

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

    ;; Volgende code voegt een geactiveerde power-up toe aan de lijst van power-ups
    (define (voeg-power-up-toe! type power-up)
      (if (eq? type 'tank)
          (begin
            (set! activeerde-tank (cons power-up activeerde-tank))
            (set! tank-power-up-monsters monsters))
          (set! activeerde-bommen-regen (cons power-up activeerde-bommen-regen))))

    ;; Volgende code voegt een net projectiel toe aan de lijst van net projectielen
    (define (voeg-net-projectiel-toe! projectiel)
      (set! net-projectielen (cons projectiel net-projectielen)))

    ;; Volgende code bomwerpt/bomt alle monsters in de buurt
    (define (explodeer-monsters-in-buurt! rand type-vermindering)
      (for-each (lambda (monster)
                  (if (in-rand? (monster 'positie) rand)
                      ((monster 'actie-monster-levend!) 'verminder type-vermindering)))
                monsters))

    ;; Volgende code vermindert alle monster levens met 1
    (define (tanken-verminder-monster-levens! tanken)
      (define (in? monster monsters) ;; Hulpprocedure om na te gaan als men het monster al vermoord heeft door een toren 
        (cond
          ((null? monsters) #f)
          ((eq? monster (eerste monsters)) #t)
          (else
           (in? monster (rest monsters)))))        
        
      (for-each (lambda (tank)
                  (if (pair? tank-power-up-monsters)
                      (let ((mons (eerste tank-power-up-monsters)))
                        ((mons 'actie-monster-levend!) 'verminder)
                        (set! tank-power-up-monsters (cdr tank-power-up-monsters))
                        (if (and ((mons 'gestorven?)) (in? mons monsters))
                            ((geld 'voeg-geld-toe!) (mons 'type) #t)))))
                tanken)
      (if (not (null? tanken))
          (overblijvende-monsters!)))
                         
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
        ((eq? msg 'initialiseer-toren-tijden!) initialiseer-toren-tijden!)
        ((eq? msg 'update-power-ups!) update-power-ups!)
        ((eq? msg 'monster-na-monster) monster-na-monster)
        ((eq? msg 'voeg-net-projectiel-toe!) voeg-net-projectiel-toe!)
        ((eq? msg 'voeg-power-up-toe!) voeg-power-up-toe!)
        ((eq? msg 'verkrijg-projectielen) verkrijg-projectielen)
        ((eq? msg 'verkrijg-tank-power-ups) activeerde-tank)
        ((eq? msg 'verkrijg-bommen-regen-power-ups) activeerde-bommen-regen)
        ((eq? msg 'explodeer-monsters-in-buurt!) explodeer-monsters-in-buurt!)
        ((eq? msg 'einde?) einde?)
        ((eq? msg 'level-einde!) level-einde!)
        ((eq? msg 'soort) 'level)
        (else
         "maak-level-adt: ongeldig bericht")))
    dispatch))