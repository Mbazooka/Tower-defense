;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                   PAD ADT                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (maak-pad-adt lijst) 
  (let* ((vector-posities (neem-vector lijst))
         (lengte (vector-length vector-posities))
         (lijst-van-posities (vector->list vector-posities))
         (inflectie-counter (neem-inflectie-counter lijst))
         (inflectie-tekens (neem-inflectie-tekens lijst)) ;; Verander naam
         (inflectie-punten '())) 

    ;; Maakt de inflectie punten lijst van de pad
    (define (maak-inflectie-lijst)
      (define (iter ctr res)
        (if (< ctr inflectie-counter)               
            (iter (+ ctr 1) (cons (vector-ref vector-posities ctr) res))
            (reverse res)))
      (iter 0 inflectie-punten))

    (set! inflectie-punten (maak-inflectie-lijst))
          
    ;; Gaat na als toren in pad zit 
    (define (toren-in-pad? toren)
      (let ((toren-rand (toren 'toren-posities)))
        
        (define (in-pad? positie)
          (let ((afgeronde-pos ((positie 'ceil)))) ;; nodig want pad posities zijn discreet
            (accumulate (lambda (x y) (or x y)) #f (map (lambda (p) ((p 'gelijk?) afgeronde-pos)) lijst-van-posities))))

        (define (overlopen-torens ctr)
          (if (>= ctr 4)
              #f
              (or (in-pad? (vector-ref toren-rand ctr))
                  (overlopen-torens (+ ctr 1)))))
        (overlopen-torens 0)))

    ;; Dichtsbijzijnde punt op pad relatief tot een rand en een andere positie
    (define (dichtse-punt rand centraal-positie) 
      (let ((huidige-dichtse #f))
        (for-each (lambda (positie) ;; Beter om tweemalig de afstand te berekenen voor een positie dan voor elke positie die mogelijks niet in de rand zit
                    (if (and (in-rand? positie in-rand)
                             (> huidige-dichste ((positie 'afstand) centraal-positie)))
                        (set! huidige-dichtse centraal-positie)))
                  lijst-van-posities)
        huidige-dichste))

    ;; Begin van het pad
    (define (begin)
      (vector-ref vector-posities (+ inflectie-counter 1)))
    
    ;; Einde van het pad
    (define (einde)
      (vector-ref vector-posities (- lengte 2)))     
                   
    (define (dispatch msg)
      (cond
        ((eq? msg 'posities) vector-posities)
        ((eq? msg 'lengte) lengte)
        ((eq? msg 'inflectie-punten) inflectie-punten)
        ((eq? msg 'inflectie-tekens) inflectie-tekens)
        ((eq? msg 'begin) begin) ;; + 1, begin te zetten in midden van pad
        ((eq? msg 'einde) einde) 
        ((eq? msg 'toren-in-pad?) toren-in-pad?)
        ((eq? msg 'dichtse-punt) dichtse-punt)
        (else "maak-pad-adt: ongeldig bericht")))
    dispatch))
