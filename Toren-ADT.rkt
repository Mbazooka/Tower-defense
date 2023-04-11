;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                 Toren ADT                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (maak-toren-adt centraal-positie type) ;; Positie stelt midden van de toren voor, type voor uitbreidbaarheid (later)
  (let ((toren-rand (make-vector 4)) ;; Stelt werkelijke posities toren voor (enkel 4 punten van rand, voor geheugenvriendelijkheid)
        (buurt-rand (make-vector 4)) ;; Stelt buurt voor (geheugenvriendelijker om met 4 te werken)
        (projectielen '()))

    ;; Maakt de werkelijke 4-punt randen aan
    (positie->rand! centraal-positie *toren-rand-afstand* toren-rand) 
    (positie->rand! centraal-positie *buurt-rand-afstand* buurt-rand)
    
    ;; Gaat na als de ingegeven toren op de beshouwde toren staat
    (define (in-toren? toren)
      (let ((posities (toren 'toren-posities)))
        (or (in-rand? (vector-ref posities 0) toren-rand)
            (in-rand? (vector-ref posities 1) toren-rand)
            (in-rand? (vector-ref posities 2) toren-rand)
            (in-rand? (vector-ref posities 3) toren-rand))))

    ;; Gaat na als een monster in een de buurt van een toren zit   
    (define (in-buurt? monster) 
      (in-rand? (monster 'positie) buurt-rand))

    ;; Volgende code laat toe om projectielen te schieten naar een bepaald monster
    (define (schiet! monster)
      (let ((projectiel (maak-projectiel-adt
                          ((centraal-positie 'positie-copieer))
                          'steen
                          monster
                          )))
        (set! projectielen (cons projectiel projectielen))))

    ;; Volgende code laat toe om de projectielen hun posities up te daten
    (define (projectiel-update!)
      (for-each
       (lambda (projectiel)
         ((projectiel 'actie-te-raken-monster!)))
       (filter
        (lambda (project) ((project 'bestemming-bereikt?)))
        projectielen))
      (set! projectielen (filter
                          (lambda (projectiel)
                            (not ((projectiel 'bestemming-bereikt?))))
                          projectielen))
      (for-each (lambda (projectiel)
                  ((projectiel 'volgende-positie!)))
                projectielen))
                                          
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
        ((eq? msg 'soort) 'toren)
        (else "maak-toren-adt: ongeldig bericht")))
    dispatch))