;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                               Monster ADT                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (maak-level-adt pad monster-rij) ;; Neemt een rij van monster in die gereleased zullen zijn op het pad
  (let ((monsters '()) ;; Lijst omdat elk element bewerken gemakkelijk is
        (midden (pad 'midden)) ;; Zorgt voor minder computaties
        (update-type #t)) ;; Monster bijvoegen of niet?

    (define (update!)
      (set! monsters (filter (lambda (monster) (not ((monster 'einde?)))) monsters)) ;; Overblijvende monsters te vermoorden
      (for-each (lambda (monster) ((monster 'volgende-positie!) (vector-ref midden (+ (monster 'index) 1)))) monsters)
      (if update-type
          (if (not (null? monster-rij))
              (begin
                (set! monsters (cons (maak-monster-adt (pad 'begin) (car monster-rij) (pad 'einde) 0) monsters))
                (set! monster-rij (cdr monster-rij))
                (set! update-type #f)))
          (set! update-type #t)))              

    (define (dispatch msg)
      (cond
        ((eq? msg 'update!) update!)
        ((eq? msg 'monsters) monsters)
        (else
         "maak-level-adt: ongeldig bericht")))
    dispatch))