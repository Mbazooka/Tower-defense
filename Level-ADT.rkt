;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                 Level ADT                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load "positie-adt.rkt")
(load "constanten.rkt")
(load "Hulpprocedures.rkt")
(load "vectoren-van-posities.rkt")
(load "Monster-lijst.rkt")
(load "pad-adt.rkt")
(load "Projectiel-ADT.rkt")
(load "Toren-adt.rkt")
(load "Monster-ADT.rkt")

(define (maak-level-adt pad monster-rij) ;; Neemt een rij van monster in die gereleased zullen zijn op het pad
  (let ((torens '())
        (monsters '()) ;; Lijst omdat elk element bewerken gemakkelijk is
        (midden (pad 'midden)) ;; Zorgt voor minder computaties
        (update-type #t)) ;; Monster bijvoegen of niet? (om ze niet allemaal te snel te releasen)
    
    ;; Volgende code voegt een toren toe tot het spel wereld
    (define (voeg-toren-toe! toren)
      (set! torens (cons toren torens)))
    
    ;; voglende code update de monsters die op het pad lopen
    (define (update-monsters!)
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

    ;; Volgende code update de projectielen die door torens worden afgeschoten
    (define (update-torens-projectielen!)
      (for-each
       (lambda (toren)         
         ((toren 'projectiel-update!))
         (for-each
          (lambda (monster)
            (if  ((toren 'in-buurt?) monster)
                 ((toren 'schiet!) monster)))
          monsters))         
       torens))

    ;; volgende code is om de projectielen van alle torens te verkrijgen
    (define (verkrijg-projectielen)
      (flatten
       (map (lambda (toren)
              (toren 'projectielen))
            torens)))
                  
    (define (dispatch msg)
      (cond
        ((eq? msg 'monsters) monsters)
        ((eq? msg 'torens) torens)
        ((eq? msg 'voeg-toren-toe!) voeg-toren-toe!)
        ((eq? msg 'update-monsters!) update-monsters!)
        ((eq? msg 'update-torens-projectielen!) update-torens-projectielen!)
        ((eq? msg 'verkrijg-projectielen) verkrijg-projectielen)
        (else
         "maak-level-adt: ongeldig bericht")))
    dispatch))

(define pad (maak-pad-adt vector-1))
> (define level-1 (maak-level-adt pad voorbeeld-lijst))
> (define toren-1 (maak-toren-adt (maak-positie-adt 10 12) 'basis))
> ((level-1 'voeg-toren-toe!) toren-1)
> (define toren-2 (maak-toren-adt (maak-positie-adt 10 30) 'basis))
> ((level-1 'voeg-toren-toe!) toren-2)
> (define toren-3 (maak-toren-adt (maak-positie-adt 15 30) 'basis))
> ((level-1 'voeg-toren-toe!) toren-3)

(define (trace-test proj)
  (let ((proj-pos (proj 'positie))
        (proj-best (proj 'bestemming)))
    (display "Bestemming: ")
    (display (proj-best 'x))
    (display " ")
    (display (proj-best 'y))
    (newline)
    ((level-1 'update-torens-projectielen!))
    ((level-1 'update-torens-projectielen!))
    ((level-1 'update-torens-projectielen!))
    (display "Huidige positie: ")
    (display (proj-pos 'x))
    (display " ")
    (display (proj-pos 'y))
    (newline)))

((level-1 'update-monsters!))
> ((level-1 'update-monsters!))
> ((level-1 'update-monsters!))
> ((level-1 'update-monsters!))
> ((level-1 'update-monsters!))
> ((level-1 'update-monsters!))
> ((level-1 'update-monsters!))
> ((level-1 'update-torens-projectielen!))
> (define test-1 (car ((level-1 'verkrijg-projectielen))))
> (define test-2 (car (cddddr ((level-1 'verkrijg-projectielen)))))
> (define test-3 (cadr (cddddr ((level-1 'verkrijg-projectielen)))))
> (trace-test test-1) (trace-test test-2) (trace-test test-3)