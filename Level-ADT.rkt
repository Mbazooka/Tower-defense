;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                               Monster ADT                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (maak-level-adt pad monster-rij) ;; Neemt een rij van monster in die gereleased zullen zijn op het pad
  (let ((monsters '()) ;; Lijst omdat elk element bewerken gemakkelijk is
        (midden (pad 'midden))) ;; Zorgt voor minder computaties

    (define (update!)
      (set! monsters (filter (lambda (monster) (not (monster 'einde?))) monsters)) ;; Overblijvende monsters te vermoorden
      (for-each (lambda (monster) ((monster 'volgende-positie!) (vector-ref midden (+ (monster 'index) 1)))) monsters)
      (if (not (null? monster-rij))
            (set! monsters (cons (maak-monster (pad 'begin) (car monster-rij) pad 0) monsters))))

    (define (dispatch msg)
      (cond
        ((eq? msg 'update!) update!)
        ((eq? msg 'monsters) monsters)
        (else
         "maak-level-adt: ongeldig bericht")))
    dispatch))