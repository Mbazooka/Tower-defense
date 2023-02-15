;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                               Monster ADT                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (maak-level-adt pad monster-rij) ;; Neemt een rij van monster in die gereleased zullen zijn op het pad
  (let ((torens '())
        (monsters '()) ;; Lijst omdat elk element bewerken gemakkelijk is
        (midden (pad 'midden)) ;; Zorgt voor minder computaties
        (update-type #t)) ;; Monster bijvoegen of niet?
    

    (define (update!)
      (set! monsters (filter
                      (lambda (monster)
                        (and (not ((monster 'einde?)))
                            (not (monster 'gestorven?))))
                      monsters)) ;; Overblijvende monsters te vermoorden
      (for-each (lambda (monster) ((monster 'volgende-positie!) (vector-ref midden (+ (monster 'index) 1)))) monsters)
      (if update-type
          (if (not (null? monster-rij))
              (begin
                (set! monsters (cons (maak-monster-adt (pad 'begin) (car monster-rij) (pad 'einde) 0) monsters))
                (set! monster-rij (cdr monster-rij))
                (set! update-type #f)))
          (set! update-type #t)))

    (define (voeg-toren-toe! toren)
      (set! torens (cons toren torens)))

    (define (dispatch msg)
      (cond
        ((eq? msg 'update!) update!)
        ((eq? msg 'monsters) monsters)
        ((eq? msg 'torens) torens)
        ((eq? msg 'voeg-toren-toe!) voeg-toren-toe!)
        (else
         "maak-level-adt: ongeldig bericht")))
    dispatch))