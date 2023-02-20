;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                   PAD ADT                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (maak-pad-adt lijst) 
  (let* ((vector-posities (neem-vector lijst))
         (lengte (vector-length vector-posities))
         (inflectie-counter (neem-inflectie-counter lijst))
         (inflectie-tekens (neem-inflectie-tekens lijst))
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
        ((eq? msg 'inflectie-punten) inflectie-punten)
        ((eq? msg 'inflectie-tekens) inflectie-tekens)
        ((eq? msg 'begin) (vector-ref vector-posities (+ inflectie-counter 1))) ;; + 1, begin te zetten in midden van pad
        ((eq? msg 'einde) (vector-ref vector-posities (- lengte 2))) 
        ((eq? msg 'toren-in-pad?) toren-in-pad?)
        (else "maak-pad-adt: ongeldig bericht")))
    dispatch))
