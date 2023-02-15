;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                   PAD ADT                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Veel overlappende rekenwerk, verander misschienn naar car en cdr rest 
(define (maak-pad-adt vector-van-posities) ;; Vector gebruikt, gemakkelijk acceseren
  (let* ((lengte (vector-length (cdr vector-van-posities))) 
         (midden (make-vector (car vector-van-posities)))) ;; Want lengte pad is altijd veelvoud van 3 per constructie

    ;; Maakt het midden van de pad
    (define (maak-midden-vector!)
      (let ((eindconditie (car vector-van-posities)))
        (define (hulp ctr)
          (if (< ctr eindconditie)
              (begin
                (vector-set! midden ctr (vector-ref (cdr vector-van-posities) ctr))
                (hulp (+ ctr 1)))))
        (hulp 0)))

    ;; Maakt werkelijke het midden van de pad (moet van 1 beginnen om iedere keer de middenste tegel te nemen)
    (maak-midden-vector!)
          
    ;; Gaat na als toren in pad zit (werkt niet)
    (define (toren-in-pad? toren)
      (let ((lijst-van-posities (vector->list (cdr vector-van-posities)))
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
        ((eq? msg 'posities) vector-van-posities)
        ((eq? msg 'lengte) lengte)
        ((eq? msg 'begin) (vector-ref midden 0))
        ((eq? msg 'midden) midden)
        ((eq? msg 'einde) (- (vector-length midden) 1)) ;; einde pad in termen van indexen
        ((eq? msg 'toren-in-pad?) toren-in-pad?)
        (else "maak-pad-adt: ongeldig bericht")))
    dispatch))
