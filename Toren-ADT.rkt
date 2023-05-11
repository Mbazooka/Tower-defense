;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                 Toren ADT                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (maak-toren-adt centraal-positie type) ;; Positie stelt midden van de toren voor, type is het type toren
  (let ((toren-rand (make-vector 4)) ;; Stelt werkelijke posities toren voor (enkel 4 punten van rand, voor geheugenvriendelijkheid)
        (buurt-rand (make-vector 4)) ;; Stelt buurt voor (geheugenvriendelijker om met 4 te werken)
        (afvuur-frequentie #f)
        (afvuur-tijd 0) ;; Stelt de verlopen tijd sinds laatste afvuur moment
        (projectielen '()))

    ;; Volgende code gaat de grootte van de rand na afhankelijk van de toren alsook hun afvuur-frequentie
    (define (bepaal-buurt-rand/afvuur-frequentie!)
      (cond
        ((eq? type 'basis-toren) (set! afvuur-frequentie *basis-toren-afvuur-frequentie*)
                                 *basis-toren-buurt-rand-afstand*)
        ((eq? type 'net-toren) (set! afvuur-frequentie *net-toren-afvuur-frequentie*)
                               *net-toren-buurt-rand-afstand*)                             
        ((eq? type 'vuurbal-toren) (set! afvuur-frequentie *vuurbal-toren-afvuur-frequentie)
                                   *vuurbal-toren-buurt-rand-afstand*)                                  
        ((eq? type 'bomwerp-toren) (set! afvuur-frequentie *bomwerp-toren-afvuur-frequentie*)
                                   *bomwerp-toren-buurt-rand-afstand*)
        (else "Ongeldige type")))
             
    ;; Maakt de werkelijke 4-punt randen aan
    (positie->rand! centraal-positie *toren-rand-afstand* toren-rand)
    (positie->rand! centraal-positie (bepaal-buurt-rand/afvuur-frequentie!) buurt-rand)
    
    ;; Gaat na als de ingegeven toren op de beshouwde toren staat
    (define (in-toren? toren)
      (let ((posities (toren 'toren-posities)))
        (or (in-rand? (vector-ref posities 0) toren-rand)
            (in-rand? (vector-ref posities 1) toren-rand)
            (in-rand? (vector-ref posities 2) toren-rand)
            (in-rand? (vector-ref posities 3) toren-rand))))

    ;; Gaat na als een monster in een de buurt van een toren zit   
    (define (in-buurt? object) 
      (in-rand? (object 'positie) buurt-rand))

    ;; Volgende geeft het type projectiel dat een toren afschiet afhankelijk van het type toren
    (define (projectiel-type-toren)
      (cond
        ((eq? type 'basis-toren) 'steen)
        ((eq? type 'net-toren) 'net)
        ((eq? type 'vuurbal-toren) 'vuurbal)
        ((eq? type 'bomwerp-toren) 'bomwerp)
        (else
         "Ongeldig toren type")))

    ;; Volgende code laat toe om projectielen te schieten naar een bepaald monster
    (define (schiet! obj pad)
      (if (eq? type 'bomwerp-toren)
          (set! obj ((pad 'dichste-punt) centraal-positie))) 
      (let ((projectiel (maak-projectiel-adt
                         ((centraal-positie 'positie-copieer))
                         (projectiel-type-toren)
                         obj)))
        (set! projectielen (cons projectiel projectielen))))

    ;; Volgende code laat toe om de projectielen hun posities up te daten
    (define (projectiel-update! level dt) ;; Verander zodat overzichtelijker wordt
      (define (acties-bestemming-bereikt!)
        (for-each
         (lambda (projectiel)
           (let ((type-var (projectiel 'type)))
             (if (not (eq? type-var 'bomwerp))
                 ((projectiel 'actie-te-raken-monster!))) 
             (cond ;; Volgende actie na monster geraakt is
               ((eq? 'vuurbal type-var)
                (let ((actie ((projectiel 'actie-na-monster-raak!) level dt)))
                  (if actie ;; Gaat na als er een actie gedaan moet worden of niet
                      (set! projectielen (cons actie projectielen)))))
               ((eq? 'net type-var)
                ((projectiel 'maak-rand!) *net-projectiel-rand-afstand* level)
                ((projectiel 'actie-na-monster-raak!) level dt))
               ((eq? 'bomwerp type-var)
                ((projectiel 'actie-na-monster-raak!) level dt)))))
         (filter
          (lambda (project) ((project 'bestemming-bereikt?)))
          projectielen)))

      (define (haal-projectielen-weg!)
        (set! projectielen (filter
                            (lambda (projectiel)
                              (and (not (and ((projectiel 'bestemming-bereikt?)) ((projectiel 'afgehandelt?))))
                                  (in-buurt? projectiel))) ;; Gaat na als een projectiel nog in de rand zit
                            projectielen)))
      
      (define (beweeg-projectielen-voort!)
        (for-each (lambda (projectiel)
                    (if (not ((projectiel 'bestemming-bereikt?))) ;; Nodig want soms bestemming bereikt en dus wil je niet dat je ze verder bewegen (vb. net)
                        ((projectiel 'volgende-positie!))))
                  projectielen))

      ;; Uitvoeren van alle acties
      (acties-bestemming-bereikt!)
      (haal-projectielen-weg!)
      (beweeg-projectielen-voort!))

    ;; Volgende code gaat na als een projectiel in een de lijst van nog niet bereikte of afgehandelte projectielen zit
    ;; Indien hij er niet in zit, wil dat zeggen dat je dit projectiel mag op kuizen uit level-adt (garbage collection, geheugenvriendelijker)
    (define (niet-bereikt&&afgehandelt? projectiel)
      (if (memq projectiel projectielen)
          #t
          #f))

    ;; Volgende code updaten toren hun afvuurtijd
    (define (update-afvuur-tijd! dt)
      (if (>= afvuur-tijd afvuur-frequentie)
          (set! afvuur-tijd 0)
          (set! afvuur-tijd (+ afvuur-tijd dt))))

    ;; Volgende code gaat na als de toren mag schieten
    (define (schieten?)
      (= afvuur-tijd 0))
                                          
    (define (dispatch msg)
      (cond
        ((eq? msg 'positie) centraal-positie)
        ((eq? msg 'type) type)
        ((eq? msg 'toren-posities) toren-rand) 
        ((eq? msg 'in-toren?) in-toren?)
        ((eq? msg 'in-buurt?) in-buurt?)
        ((eq? msg 'schiet!) schiet!)
        ((eq? msg 'projectiel-update!) projectiel-update!)
        ((eq? msg 'projectielen) projectielen)
        ((eq? msg 'niet-bereikt&&afgehandelt?) niet-bereikt&&afgehandelt?)
        ((eq? msg 'update-afvuur-tijd!) update-afvuur-tijd!)
        ((eq? msg 'schieten?) schieten?)
        ((eq? msg 'soort) 'toren)
        (else "maak-toren-adt: ongeldig bericht")))
    dispatch))