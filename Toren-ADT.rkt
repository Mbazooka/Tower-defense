;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                 Toren ADT                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Voeg type toe voor uitbreidbaarheid
(define (maak-toren-adt centraal-positie type) ;; Positie stelt midden van de toren voor
  (let ((toren-posities (make-vector 2)) ;; Stelt werkelijke posities toren voor (enkel 2 punten van rand, voor geheugenvriendelijkheid)
        (buurt-posities (make-vector 2))) ;; Stelt buurt voor (geheugenvriendelijk)

    ;; Geeft de rand van een bepaald abstract object a.d.h.v 2 posities (bv toren rand)
    (define (positie->rand! afstand vector)
      (let ((x-pos-cent (centraal-positie 'x))
            (y-pos-cent (centraal-positie 'y)))
        (vector-set! vector 0 (maak-positie-adt (- x-pos-cent afstand) (+ y-pos-cent afstand)))
        (vector-set! vector 1 (maak-positie-adt (+ x-pos-cent afstand) (- y-pos-cent afstand)))))
        
    
    ;; Maakt de werkelijke 4-punt randen aan
    (positie->rand! 1 toren-posities)
    (positie->rand! 9 buurt-posities)

    ;; Gaat na als een positie werkelijk in zo'n rand zit
    (define (in-rand? positie rand)
      (let ((x-pos (positie 'x))
            (y-pos (postitie 'y)))
        (and (>= x-pos ((vector-ref 0 rand) 'x))
             (<= y-pos ((vector-ref 0 rand) 'y))
             (<= x-pos ((vector-ref 1 rand) 'x))
             (>= y-pos ((vector-ref 1 rand) 'y)))))

    (define (in-toren? positie)
      (in-rand? positie toren-posities))

    (define (in-buurt? positie)
      (in-rand? positie buurt-posities))

    (define (dispatch msg)
      (cond
        ((eq? msg 'positie) centraal-positie)
        ((eq? msg 'in-toren?) in-toren?)
        ((eq? msg 'in-buurt?) in-buurt?)
        (else "maak-toren-adt: ongeldig bericht")))
    dispatch))