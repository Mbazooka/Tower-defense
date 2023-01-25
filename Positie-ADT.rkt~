(define (maak-positie-adt x y)
    
  (define (x! nieuw-x)
    (set! x nieuw-x))

  (define (y! nieuw-y)
    (set! y nieuw-y))

  (define (afstand positie2);misschien een round nodig voor later op het resultaat
    (define (square x)
      (* x x))

    (sqrt
     (+ (square (- x (positie2 'x))) (square (- y (positie2 'y))))))

  (define (gelijk? positie2)
    (and (= x (positie2 'x)) (= y (positie2 'y))))

  ;; Zal na gaan of positie in een lijst van vectoren zit 
  (define (in-vector? vector)
    (let ((conscellen-van-posities (map (lambda (pos) (cons (pos 'x) (pos 'y))) (vector->list vector))))
      (if (member (cons x y) conscellen-van-posities)
          #t
          #f)))
               
  (define (beweeg! nieuw-x nieuw-y)
    (x! nieuw-x)
    (y! nieuw-y))


  (define (dispatch msg)
    (cond
      ((eq? msg 'x) x)
      ((eq? msg 'y) y)
      ((eq? msg 'x!) x!)
      ((eq? msg 'y!) y!)
      ((eq? msg 'afstand) afstand)
      ((eq? msg 'gelijk?) gelijk?)
      ((eq? msg 'in-vector?) in-vector?)
      ((eq? msg 'beweeg!) beweeg!)
      (else "maak-positie-adt: ongeldig bericht")))
  dispatch)
