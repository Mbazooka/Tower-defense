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

(define (filter pred? lijst)
  (cond
    ((null? lijst) '())
    ((pred? (car lijst))
     (cons (car lijst) (filter pred? (cdr lijst))))
    (else
     (filter pred? (cdr lijst)))))


