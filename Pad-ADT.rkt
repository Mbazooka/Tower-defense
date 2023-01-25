;; Maak file met vector-van-posities (zoek code die dit produceert)
;; Vector van posities zullen 
(define (maak-pad-adt vector-van-posities)
  (let* ((lengte (vector-length vector-van-posities))
         (begin (vector-ref vector-van-posities 0))
         (einde (vector-ref vector-van-posities (- lengte 1)))) ;; Lengte van vector want algemeen
    
    (define (in? positie)
      (positie 'in-vector?) vector-van-posities)
               
    (define (dispatch msg)
      (cond
        ((eq? msg 'posities) vector-van-posities)
        ((eq? msg 'lengte) lengte)
        ((eq? msg 'begin) begin)
        ((eq? msg 'einde) einde)
        ((eq? msg 'in?) in?)
        (else "maak-pad-adt: ongeldig bericht")))
    dispatch))