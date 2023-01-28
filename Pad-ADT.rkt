;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                   PAD ADT                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Maak file met vector-van-posities (zoek code die dit produceert)
(load "Positie-ADT.rkt")
(load "vectoren-van-posities.rkt")
(load "Toren-ADT.rkt")
(define (maak-pad-adt vector-van-posities)
  (let* ((lengte (vector-length vector-van-posities)) 
         (begin (vector (vector-ref vector-van-posities 0)
                        (vector-ref vector-van-posities 1)
                        (vector-ref vector-van-posities 2)));; De eerste 3 posities zijn begin pad
         (einde (vector (vector-ref vector-van-posities (- lengte 3))
                        (vector-ref vector-van-posities (- lengte 2))
                        (vector-ref vector-van-posities (- lengte 1)))))
   
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
        ((eq? msg 'begin) begin)
        ((eq? msg 'einde) einde)
        ((eq? msg 'toren-in-pad?) toren-in-pad?)
        (else "maak-pad-adt: ongeldig bericht")))
    dispatch))
