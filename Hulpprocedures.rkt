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
;          


            