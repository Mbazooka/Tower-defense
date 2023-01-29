;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                               Monster ADT                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (maak-level-adt pad monster-rij) ;; Neemt een rij van monster in die gereleased zullen zijn op het pad
  (let ((monsters '()) ;; Lijst omdat elk element bewerken gemakkelijk is
        (midden (pad 'midden))) ;; Zorgt voor minder computaties

    (define (update!)
      (let ((nieuw-monsters (filter (lambda (monster) (not (monster 'einde?))) monsters)))
        (set! monsters nieuw-monsters)
        (for-each (lambda (monster) ((monster 'volgende-positie!) (vector-ref midden (+ (monster 'index) 1)))) monsters))
      (if (not (null? monster-rij))
          (let ((nieuw-monster (maak-monster (pad 'begin) (car monster-rij) 0)))
            (set! monsters (cons nieuw-monster monsters)))))

    (define (dispatch msg)
      (cond
        ((eq? msg 'update!) update!)
        (else
         "maak-level-adt: ongeldig bericht")))
    dispatch))