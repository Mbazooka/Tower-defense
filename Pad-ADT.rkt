;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                   PAD ADT                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Veel overlappende rekenwerk, verander misschienn naar car en cdr rest 
(define (maak-pad-adt vector-van-posities) ;; Vector gebruikt, gemakkelijk acceseren
  (let* ((lengte (vector-length (neem-vector vector-van-posities)))
         (inflectie-counter (neem-inflectie-counter vector-van-posities))
         (inflectie-tekens (neem-inflectie-tekens vector-van-posities))
         (vector-posities (neem-vector vector-van-posities))
         (inflectie-punten '())) ;; Want lengte pad is altijd veelvoud van 3 per constructie

    ;; Maakt het midden van de pad
    (define (maak-inflectie-lijst)
      (define (rec ctr res)
        (if (< ctr inflectie-counter)               
            (rec (+ ctr 1) (cons (vector-ref vector-posities ctr) res))
            (reverse res)))
      (rec 0 inflectie-punten))

    ;; Maakt werkelijke het midden van de pad (moet van 1 beginnen om iedere keer de middenste tegel te nemen)
    (set! inflectie-punten (maak-inflectie-lijst))

    ;; Maakt copy van lijst, is nodig omdat monster ADT deze lijst nodig heeft maar niet exact dezelfde lijst
    (define (inflectie-copy punten/tekens)
      (cond
        ((eq? punten/tekens 'punten)
         (map (lambda (positie) ((positie 'positie-copieer))) inflectie-punten))
        ((eq? punten/tekens 'tekens)
         (map (lambda (tekens) tekens) inflectie-tekens))))
          
    ;; Gaat na als toren in pad zit (werkt niet)
    (define (toren-in-pad? toren)
      (let ((lijst-van-posities (vector->list vector-posities))
            (toren-rand (toren 'toren-posities)))
        
        (define (in-pad? positie)
          (let ((afgeronde-pos ((positie 'ceil)))) ;; nodig want pad posities zijn discreet
            (accumulate (lambda (x y) (or x y)) #f (map (lambda (p) ((p 'gelijk?) afgeronde-pos)) lijst-van-posities))))

        (define (overlopen-torens ctr)
          (if (>= ctr 4)
              #f
              (or (in-pad? (vector-ref toren-rand ctr))
                  (overlopen-torens (+ ctr 1)))))
        (overlopen-torens 0)))
                   
    (define (dispatch msg)
      (cond
        ((eq? msg 'posities) vector-posities)
        ((eq? msg 'lengte) lengte)
        ((eq? msg 'begin) (vector-ref vector-posities (+ inflectie-counter 1))) ;; + 1 begin te zetten in midden van pad
        ((eq? msg 'einde) (vector-ref vector-posities (- lengte 2))) 
        ((eq? msg 'inflectie-copy) inflectie-copy) 
        ((eq? msg 'toren-in-pad?) toren-in-pad?)
        (else "maak-pad-adt: ongeldig bericht")))
    dispatch))
