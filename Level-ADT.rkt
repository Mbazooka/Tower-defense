;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                 Level ADT                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (maak-level-adt pad monster-rij) ;; Neemt een rij van monster in die gereleased zullen zijn op het pad
  (let ((torens '())
        (monsters '()) ;; Lijst omdat elk element bewerken gemakkelijk is
        (inflectie-punten (pad 'inflectie)) 
        (update-type #t)) ;; Monster bijvoegen of niet? (om ze niet allemaal te snel te releasen)
    
    ;; Abstracties om type en rest uit lijst te krijgen
    (define type car)
    (define rest cdr)
   
    ;; Volgende code voegt een toren toe tot het spel wereld
    (define (voeg-toren-toe! toren)
      (set! torens (cons toren torens)))
    
    ;; voglende code update de monsters die op het pad lopen
    (define (update-monsters!)
      (set! monsters (filter
                      (lambda (monster)
                        (and (not ((monster 'einde?)))
                             (not (monster 'gestorven?))))
                      monsters)) ;; Overblijvende monsters te vermoorden
      (for-each (lambda (monster) ((monster 'volgende-positie!))) monsters)
      (if update-type
          (if (not (null? monster-rij))
              (let ((copy (pad 'inflectie-copy)))
                (set! monsters (cons (maak-monster-adt (((pad 'begin) 'positie-copieer)) (type monster-rij) (pad 'einde) (copy 'punten) (copy 'tekens)) monsters))
                (set! monster-rij (rest monster-rij))
                (set! update-type #f)))
          (set! update-type #t)))

    ;; Volgende code update de projectielen die door torens werden afgeschoten up te daten
    (define (update-torens-projectielen-positie!)
      (for-each
       (lambda (toren)         
         ((toren 'projectiel-update!)))        
       torens))

    ;; Volgende code zal de projectielen afschieten naar een monster
    (define (update-torens-projectielen-afschieten!)
      (define (eerste-monster mons)
        (if (null? (cdr mons))
            (car mons)
            (eerste-monster (cdr mons))))
      (if (not (null? monsters))
          (for-each
           (lambda (toren)
             (let ((monster (eerste-monster monsters)))
               (if ((toren 'in-buurt?) monster)
                   ((toren 'schiet!) monster))))               
           torens)))
      
    ;; Volgende code is om de projectielen van alle torens te verkrijgen
    (define (verkrijg-projectielen)
      (flatten
       (map (lambda (toren)
              (toren 'projectielen))
            torens)))

    ;;Volgende code is om de level te skippen naar het einde
                  
    (define (dispatch msg)
      (cond
        ((eq? msg 'monsters) monsters)
        ((eq? msg 'torens) torens)
        ((eq? msg 'voeg-toren-toe!) voeg-toren-toe!)
        ((eq? msg 'update-monsters!) update-monsters!)
        ((eq? msg 'update-torens-projectielen-positie!) update-torens-projectielen-positie!)
        ((eq? msg 'update-torens-projectielen-afschieten!) update-torens-projectielen-afschieten!)
        ((eq? msg 'verkrijg-projectielen) verkrijg-projectielen)
        (else
         "maak-level-adt: ongeldig bericht")))
    dispatch))