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

    ;; Maakt de inflectie punten lijst van het pad
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
    
    ;; Volgende code vind het dichtsbijzijnd punt op het pad relatief tot een andere positie 
    (define (dichste-punt centraal-positie)  ;; Aan te passen
      (let ((huidige-dichste #f)
            (huidige-dichste-afstand #f))
        (for-each
         (lambda (positie)
           (cond
             ((eq? huidige-dichste #f)
              (set! huidige-dichste positie)
              (set! huidige-dichste-afstand ((huidige-dichste 'afstand) centraal-positie)))
             (else
              (let ((afstand ((positie 'afstand) centraal-positie)))
                (cond
                  ((< afstand huidige-dichste-afstand)
                   (set! huidige-dichste positie)
                   (set! huidige-dichste-afstand afstand)))))))
         lijst-van-posities)
        huidige-dichste))

    ;; Begin van het pad (eerste pad positie), dit is soms nodig 
    (define (begin-alternatief)
      (vector-ref vector-posities inflectie-counter))
              
    ;; Begin van het pad (midden pad)
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
        ((eq? msg 'begin) begin)
        ((eq? msg 'einde) einde) 
        ((eq? msg 'toren-in-pad?) toren-in-pad?)
        ((eq? msg 'dichste-punt) dichste-punt)
        (else "maak-pad-adt: ongeldig bericht")))
    dispatch))
