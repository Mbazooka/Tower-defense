;; Maak file met vector-van-posities (zoek code die dit produceert) 
(define (maak-pad-adt vector-van-posities)
  (let* ((lengte (vector-length vector-van-posities)) 
         (begin (vector (vector-ref vector-van-posities 0)
                        (vector-ref vector-van-posities 1)
                        (vector-ref vector-van-posities 2)));; De eerste 3 posities zijn begin pad
         (einde (vector (vector-ref vector-van-posities (- lengte 3))
                        (vector-ref vector-van-posities (- lengte 2))
                        (vector-ref vector-van-posities (- lengte 1)))))
   
    ;; Gaat na als positie in pad zit
    (define (in-pad? positie)
      (let ((conscellen-van-posities (map (lambda (p) (cons (p 'x) (p 'y))) (vector->list vector-van-posities))))
        (if (member (cons (positie 'x) (positie 'y)) conscellen-van-posities)
            #t
            #f)))
               
    (define (dispatch msg)
      (cond
        ((eq? msg 'posities) vector-van-posities)
        ((eq? msg 'lengte) lengte)
        ((eq? msg 'begin) begin)
        ((eq? msg 'einde) einde)
        ((eq? msg 'in-pad?) in-pad?)
        (else "maak-pad-adt: ongeldig bericht")))
    dispatch))
