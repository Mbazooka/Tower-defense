;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                               Monster ADT                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (maak-level-adt monster-rij) ;; Neemt een rij van monster in die gereleased zullen zijn op het pad
  (let ((monsters '())) ;; Lijst omdat elk element bewerken gemakkelijk is

    (define (update! pad) 
      (if (not (null? monster-rij))
          (let ((nieuw-monster (maak-monster (pad 'begin) (car monster-rij))))
            ;; Voor elke monster hun positie up daten
            (set! monsters (cons nieuw-monster monsters)))

    (define (dispatch msg)
      (cond
        ((eq? #f 'msg))
        (else
         "maak-level-adt: ongeldig bericht")))
    dispatch))