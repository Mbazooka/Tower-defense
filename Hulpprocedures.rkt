;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                 Hulpprocedures                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Misschien weg doen indien niet gebruikt word 
;(define (vector-for-each proc vector)
;  (let ((lengte (vector-length vector)))
;    (define (hulp-vector-for-each ctr)
;      (if (not (= ctr (vector-length vector)))
;          (begin
;            (proc (vector-ref vector ctr))
;            (hulp-vector-for-each (+ ctr 1)))))
;    (hulp-vector-for-each 0)))

(define (accumulate operator null-waarde lijst)
  (if (null? lijst)
      null-waarde
      (operator (car lijst) (accumulate operator null-waarde (cdr lijst)))))

(define (associatie dict)
  (car dict))

(define (rest-dict dict)
  (cdr dict))

(define (sleutel associatie)
  (car associatie))

(define (waarde associatie)
  (cdr associatie))

(define (delete! sleut dict) 
  (define (delete-hulp huidige vorige)
    (cond
      ((null? (rest-dict huidige))
       (if (eq? (sleutel (associatie huidige)) sleut)
           (set-cdr! vorige '())
           #f))
      ((eq? (sleutel (associatie huidige)) sleut)
       (set-cdr! vorige (rest-dict huidige)))
      (else
       (delete-hulp (rest-dict huidige) huidige))))
  (delete-hulp (cdr dict) dict))


