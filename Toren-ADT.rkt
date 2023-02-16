;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                 Toren ADT                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (maak-toren-adt centraal-positie type) ;; Positie stelt midden van de toren voor, type voor uitbreidbaarheid
  (let ((toren-rand (make-vector 4)) ;; Stelt werkelijke posities toren voor (enkel 4 punten van rand, voor geheugenvriendelijkheid)
        (buurt-rand (make-vector 4)) ;; Stelt buurt voor (geheugenvriendelijk)
        (projectielen '()))

    ;; Geeft de rand van een bepaald abstract object a.d.h.v 4 posities (bv toren rand)
    (define (positie->rand! afstand vector)
      (let ((x-pos-cent (centraal-positie 'x))
            (y-pos-cent (centraal-positie 'y)))
        (vector-set! vector 0 (maak-positie-adt (- x-pos-cent afstand) (+ y-pos-cent afstand)))
        (vector-set! vector 1 (maak-positie-adt (+ x-pos-cent afstand) (+ y-pos-cent afstand)))
        (vector-set! vector 2 (maak-positie-adt (- x-pos-cent afstand) (- y-pos-cent afstand)))
        (vector-set! vector 3 (maak-positie-adt (+ x-pos-cent afstand) (- y-pos-cent afstand)))))
    
    ;; Kijkt als positie in een bepaalde rand zit 
    (define (in-rand? positie rand)
      (let ((x-pos (positie 'x))
            (y-pos (positie 'y)))
        (and (>= x-pos ((vector-ref rand 0) 'x))
             (<= y-pos ((vector-ref rand 0) 'y))
             (<= x-pos ((vector-ref rand 3) 'x))
             (>= y-pos ((vector-ref rand 3) 'y)))))
    
    ;; Maakt de werkelijke 4-punt randen aan
    (positie->rand! *toren-rand-afstand* toren-rand) ;; Hier werd speling gebruikt om te zorgen bitmap niet op pad komt (zorgt voor meer afstand tussen torens en pad)
    (positie->rand! *buurt-rand-afstand* buurt-rand)

    ;; Gaat na als een positie werkelijk in zo'n rand zit
    (define (in-toren? toren)
      (let ((posities (toren 'toren-posities)))
        (or (in-rand? (vector-ref posities 0) toren-rand)
            (in-rand? (vector-ref posities 1) toren-rand)
            (in-rand? (vector-ref posities 2) toren-rand)
            (in-rand? (vector-ref posities 3) toren-rand))))

    ;; Gaat na als een monster in een de buurt van een toren zit   
    (define (in-buurt? monster) 
      (in-rand? (monster 'positie) buurt-posities))

    ;; Volgende code laat toe om projectielen te schieten naar een bepaald monster
    (define (schiet! monster)
      (let ((projectiel (maak-projectiel centraal-positie (monster 'positie))))
        (set! projectielen (cons projectiel projectielen))))

    ;; Volgende code laat toe om de projectielen hun posities up te daten
    (define (projectiel-update! dt)
      (set! projectielen (filter
                          (lambda (projectiel)
                            (not (projectiel 'bestemming-bereikt?)))
                          projectielen))
      (for-each (lambda (projectiel)
                  ((projectiel 'volgende-positie) dt))
                projectielen))
                                          
    (define (dispatch msg)
      (cond
        ((eq? msg 'positie) centraal-positie)
        ((eq? msg 'toren-posities) toren-rand) ;; Nodig om toren overlap na te kijken
        ((eq? msg 'in-toren?) in-toren?)
        ((eq? msg 'in-buurt?) in-buurt?)
        ((eq? msg 'schiet!) schiet!)
        ((eq? msg 'projectiel-update!) projectiel-update!)
        (else "maak-toren-adt: ongeldig bericht")))
    dispatch))