;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                 Level ADT                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (maak-level-adt monster-rij geld levens . vorige-torens) ;; Neemt een rij van monster in die gereleased zullen zijn op het pad, (alsook geld en levens) optionele parameter torens om torens vorige level mee te nemen
  (let ((pad (maak-pad-adt vector-1))
        (torens (if (not (null? vorige-torens)) (car vorige-torens) vorige-torens))
        (monsters '())) ;; Lijst omdat elk element bewerken gemakkelijk is (for-each)
    
    ;; Abstracties om type en rest uit lijst te krijgen
    (define type car)
    (define rest cdr)
        
    ;; Volgende code voegt een toren toe tot het spel wereld
    (define (voeg-toren-toe! toren)
      (set! torens (cons toren torens)))
    
    ;; voglende code update de monsters die op het pad lopen
    (define (update-monsters! . update-teken)
      (define (zet-terug-monster-lijst! zoeken-monster nieuw-monster monsters) ;; Hulp procedure om monsters te verwisselen (nodig om groen monster te switchen met rood monster)
        (cond
          ((null? monsters) "Error: Iets misgegaan")
          ((eq? (car monsters) zoeken-monster)
           (set-car! monsters nieuw-monster))
          (else
           (zet-terug-monster-lijst! zoeken-monster nieuw-monster (cdr monsters)))))
      
      ((levens 'levens-verminder!) (length (filter (lambda (monster) ((monster 'einde?))) monsters))) ;; Telt aantal monsters aan het einde en vermindert levens
      (for-each (lambda (monster)
                  (if (not (eq? (monster 'type) 'groen))
                      ((geld 'voeg-geld-toe!) (monster 'type))) ;; Zal geld updaten, en indien het een groen monster is, een rood monster spawnen
                  (if (eq? (monster 'type) 'groen)
                      (zet-terug-monster-lijst! monster ((monster 'verander-levens!) #t) monsters)))
                (filter (lambda (monster) ((monster 'gestorven?))) monsters))
      (set! monsters (filter 
                      (lambda (monster)
                        (and (not ((monster 'einde?)))
                             (not ((monster 'gestorven?)))))
                      monsters)) ;; Overblijvende monsters te vermoorden
      (for-each (lambda (monster) ((monster 'volgende-positie!))) monsters) ;; Overblijvende monster verder laten wandelen
      (if (and (not (null? update-teken)) (eq? (car update-teken) 'toevoegen) (not (null? monster-rij))) ;; Het toevoegen van monsters gedeelte van de procedure
          (begin
            (set! monsters (cons (maak-monster-adt (type monster-rij) pad) monsters))
            (set! monster-rij (rest monster-rij)))))

    ;; Volgende code update de projectielen die door torens werden afgeschoten
    (define (update-torens-projectielen-positie!)
      (for-each
       (lambda (toren)         
         ((toren 'projectiel-update!)))        
       torens))

    ;; Volgende code zal projectielen afschieten naar een monster
    (define (update-torens-projectielen-afschieten!) 
      (define (eerste-monster mons)
        (if (null? (cdr mons))
            (car mons)
            (eerste-monster (cdr mons))))

      (define (laatste-monster-weglaten mons)
        (reverse (cdr (reverse mons)))) 
      
      (define (toren-schiet-y/n toren monsters) ;; Procedure die monster zal vinden waarnaar de toren kan schieten (indien monsters in buurt)
        (let ((monster (eerste-monster monsters)))
          (if ((toren 'in-buurt?) monster)
              ((toren 'schiet!) monster)
              (if (not (null? (cdr monsters)))
                  (toren-schiet-y/n toren (laatste-monster-weglaten monsters))))))
      
      (if (not (null? monsters))
          (for-each
           (lambda (toren)
             (toren-schiet-y/n toren monsters))
           torens)))
      
    ;; Volgende code is om de projectielen van alle torens te verkrijgen
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
        ((eq? msg 'verkrijg-projectielen) verkrijg-projectielen)
        ((eq? msg 'einde?) einde?)
        ((eq? msg 'level-einde!) level-einde!)
        ((eq? msg 'soort) 'level)
        (else
         "maak-level-adt: ongeldig bericht")))
    dispatch))