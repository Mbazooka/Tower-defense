;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                   PAD ADT                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (maak-pad-adt vector-van-posities) ;; Vector gebruikt, gemakkelijk acceseren
  (let* ((lengte (vector-length vector-van-posities)) 
         (midden (make-vector (/ lengte 3)))) ;; Want lengte pad is altijd veelvoud van 3 per constructie

    ;; Maakt het midden van de pad
    (define (maak-midden-vector! ctr-pad ctr-midden)
      (if (not (>= ctr-pad (vector-length vector-van-posities)))
          (begin
            (vector-set! midden ctr-midden (vector-ref vector-van-posities ctr-pad))
            (maak-midden-vector! (+ ctr-pad 3) (+ ctr-midden 1)))))

    ;; Maakt werkelijke het midden van de pad
    (maak-midden-vector! 0 0)
          
    ;; Gaat na als toren in pad zit
    (define (toren-in-pad? toren)
      (let ((lijst-van-posities (vector->list vector-van-posities))
            (toren-rand (toren 'toren-posities)))
        
        (define (in-pad? positie)
          (let ((afgeronde-pos (maak-positie-adt (floor (positie 'x)) (floor (positie 'y)))))
            (accumulate (lambda (x y) (or x y)) #f (map (lambda (p) ((p 'gelijk?) afgeronde-pos)) lijst-van-posities))))

        (define (overlopen-torens ctr)
          (if (= ctr 4)
              #f
              (or (in-pad? (vector-ref toren-rand ctr))
                  (overlopen-torens (+ ctr 1)))))
        (overlopen-torens 0)))
                   
    (define (dispatch msg)
      (cond
        ((eq? msg 'posities) vector-van-posities)
        ((eq? msg 'lengte) lengte)
        ((eq? msg 'midden) midden)
        ((eq? msg 'einde) (- (vector-length midden) 1)) ;; einde pad in termen van indexen
        ((eq? msg 'toren-in-pad?) toren-in-pad?)
        (else "maak-pad-adt: ongeldig bericht")))
    dispatch))
