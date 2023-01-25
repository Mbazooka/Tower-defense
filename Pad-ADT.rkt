;; Maak file met vector-van-posities (zoek code die dit produceert) 
(define (maak-pad-adt vector-van-posities)
  (let* ((lengte (vector-length vector-van-posities)) ;;Lengte vaak gebruikt vandaar
         (begin (vector (vector-ref vector-van-posities 0)
                        (vector-ref vector-van-posities 1)
                        (vector-ref vector-van-posities 2)));; De eerste 3 posities zijn begin pad
         (einde (vector (vector-ref vector-van-posities (- lengte 3))
                        (vector-ref vector-van-posities (- lengte 2))
                        (vector-ref vector-van-posities (- lengte 1)))));; Lengte van vector want algemeen
   
    ;; Gaat na als positie in pad zit
    (define (in? positie)
      ((positie 'in-vector?) vector-van-posities))
               
    (define (dispatch msg)
      (cond
        ((eq? msg 'posities) vector-van-posities)
        ((eq? msg 'lengte) lengte)
        ((eq? msg 'begin) begin)
        ((eq? msg 'einde) einde)
        ((eq? msg 'in?) in?)
        (else "maak-pad-adt: ongeldig bericht")))
    dispatch))

;; Comments verander begin,einde