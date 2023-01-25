;maak hier een toren ADT
(define (maak-toren-adt vector-van-posities)

  (define (in-buurt? positie)
    ((positie 'in-vector?) vector-van-posities))

  (define (dispatch msg)
    (cond
      ((eq? msg 'posities) vector-van-posities)
      ((eq? msg 'in-buurt?) in-buurt?)
      (else "maak-toren-adt: ongeldig bericht")))
  dispatch)