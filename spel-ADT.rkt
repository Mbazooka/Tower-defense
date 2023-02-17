;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                  Spel ADT                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (maak-spel-adt)
  (let* ((pad (maak-pad-adt vector-1)) ;; maak de fundamenten van het spel
         (teken-adt (maak-teken-adt (+ *menu-breedte-px* *spel-breedte-px*) *spel/menu-hoogte-px*))
         (level (maak-level-adt pad voorbeeld-lijst))
         (toren-type #f)
         (monster-tijd 0))

    ;; Maakt basis compenenten van het spel
    ((teken-adt 'teken-pad!) pad) 

    ;; Start de dynamische werking van het spel
    (define (start!)
      ((teken-adt 'set-muis-toets!) muis-klik-procedure)
      ((teken-adt 'set-toets-procedure!) toets-procedure))   

    ;; De procedure die het klikken van muis op scherm voorstelt    
    (define (muis-klik-procedure toets toestand x y)
      (cond
        ((and (eq? toets 'left) (eq? toestand 'pressed) (>= x *toren-knop-breedte-start*) (<= x *toren-knop-breedte-einde*) (>= y *toren-1-knop-hoogte-start*) (<= y *toren-1-knop-hoogte-einde*)) ;; Initialiseert toren type
         (set! toren-type 'basis))
        ((eq? toren-type #f) "Beweging niet mogelijk") ;; Indien nog niks in de menu gekozen is
        ((and (eq? toets 'left) (eq? toestand 'pressed)
              (<= x (- *start-x-pos-menu* (* 2 *px-breedte*))) ;; Plaats toren buiten menu. De constante 2 is om speling te vermijden en niks op menu te hebben terwijl positie van toren er toch buiten zit
              (not (and (<= x *beperking-1-breedte*) (<= y *beperking-1-hoogte*)))
              (not (and (>= x *beperking-1-breedte*) (<= x *beperking-2-breedte*) (<= y *beperking-2-hoogte*)))
              (not (and (>= x *beperking-3-breedte*) (<= y *beperking-3-hoogte*))))
        
         (let ((toren (maak-toren-adt (maak-positie-adt (/ x *px-breedte*) (/ y *px-hoogte*)) toren-type)))
           (cond
             ((and (null? (level 'torens)) (not ((pad 'toren-in-pad?) toren)))
              ((level 'voeg-toren-toe!) toren)
              ((teken-adt 'teken-toren!) toren))
             ((and (not (accumulate (lambda (x y) (or x y)) #f (map (lambda (t) ((t 'in-toren?) toren)) (level 'torens))))
                   (not ((pad 'toren-in-pad?) toren)))
              ((level 'voeg-toren-toe!) toren)
              ((teken-adt 'teken-toren!) toren))
             (else "Beweging niet mogelijk"))))))
    
    ;; Volgende code implementeert de spel lus van het spel
    (define (spel-lus-procedure dt)
      (if (>= monster-tijd *monster-beweeg-snelheid*) ;; Zal monsters op scherm updaten na 0,1 seconde
          (begin
            ((level 'update-monsters!))
            ((teken-adt 'teken-monsters!) (level 'monsters))
            (set! monster-tijd 0)
            ((level 'update-torens-projectielen!))
            ((teken-adt 'teken-projectielen!) ((level 'verkrijg-projectielen)))))
      (set! monster-tijd (+ monster-tijd dt)))     

  ;;Volgende code implementeert een toets om het spel de laten starten
  (define (toets-procedure toestand toets)
    (if (and (eq? toestand 'pressed) (eq? toets #\space))
        ((teken-adt 'set-spel-lus!) spel-lus-procedure)))
            
  (define (dispatch msg)
    (cond 
      ((eq? msg 'start!) (start!))
      (else
       "maak-spel-adt: ongeldig bericht")))
  dispatch))