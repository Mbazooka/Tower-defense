;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                  Spel ADT                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (maak-spel-adt)
  (let* ((geld (maak-geld-adt *geld-begin-bedrag*))
         (levens (maak-leven-adt *levens-hoeveelheid*))
         (level (maak-level-adt level-1 geld levens))
         (pad (level 'pad))
         (teken-adt (maak-teken-adt (+ *menu-breedte-px* *spel-breedte-px*) *spel/menu-hoogte-px*));; maak de fundamenten van het spel
         (toren-type #f) ;; Om torens te plaatsen veranderen we dit om te weten welk type te plaatsen.
         (monster-tijd 0) ;; Tijd afgelopen sinds vorige monster op pad
         (projectiel-tijd 0)) ;; Tijd afgelopen sinds vorige projectiel die geschoten werd

    ;; Tekent pad van het spel
    ((teken-adt 'teken-pad!) pad) 

    ;; Start het van het spel door toetsen te initialiseren
    (define (start!)
      ((teken-adt 'set-muis-toets!) muis-klik-procedure)
      ((teken-adt 'set-toets-procedure!) toets-procedure))

    ;; De procedure die het klikken van muis op scherm voorstelt    
    (define (muis-klik-procedure toets toestand x y)
      (cond ;; !!!!!Zet dit in de het Teken-adt!!!!!
        ((and (eq? toets 'left) (eq? toestand 'pressed) (>= x *start-data-menu*) (<= x *toren-knop-breedte-einde*) (>= y *toren-1-knop-hoogte-start*) (<= y *toren-1-knop-hoogte-einde*)) ;; Initialiseert toren type
         (set! toren-type 'basis))
        ((eq? toren-type #f) "Beweging niet mogelijk") ;; Indien nog geen toren gekozen is
        ((and (eq? toets 'left) (eq? toestand 'pressed)
              (<= x (- *start-x-pos-menu* (* 2 *px-breedte*))) ;; Plaats toren buiten menu. De constante 2 is om speling te vermijden en niks op menu te hebben terwijl positie van toren er toch buiten zit (stukje van toren in menu)
              (not (and (<= x *beperking-1-breedte*) (<= y *beperking-1-hoogte*)))
              (not (and (>= x *beperking-1-breedte*) (<= x *beperking-2-breedte*) (<= y *beperking-2-hoogte*)))
              (not (and (>= x *beperking-2-breedte*) (<= x *beperking-3-breedte*) (<= y *beperking-3-hoogte*)))
              ((geld 'voldoende-geld?) toren-type))
        
         (let ((toren (maak-toren-adt (maak-positie-adt (/ x *px-breedte*) (/ y *px-hoogte*)) toren-type)))
           (cond
             ((and (not (accumulate (lambda (x y) (or x y)) #f (map (lambda (t) ((t 'in-toren?) toren)) (level 'torens))))
                   (not ((pad 'toren-in-pad?) toren)))
              ((geld 'verwijder-geld!) toren-type)
              ((level 'voeg-toren-toe!) toren)
              ((teken-adt 'teken-toren!) toren)
              ((teken-adt 'update-tekst-teken!) geld))
             (else "Beweging niet mogelijk"))))))
    
    ;; Volgende code implementeert de spel lus van het pel
    (define (spel-lus-procedure dt)
      (if ((levens 'dood?))
          ((level 'level-einde!)))
      (if (>= monster-tijd *monster-spawn-frequentie*) ;; Zal monsters op scherm updaten na ongeveer 2 seconden
          (begin
            ((level 'update-monsters!) 'toevoegen)
            (set! monster-tijd 0))
          ((level 'update-monsters!)))
      ((teken-adt 'teken-monsters!) (level 'monsters))
      (set! monster-tijd (+ monster-tijd dt))
      (if (>= projectiel-tijd *toren-afvuur-frequentie*)
          (begin
            ((level 'update-torens-projectielen-afschieten!))
            (set! projectiel-tijd 0)))
      (set! projectiel-tijd (+ projectiel-tijd dt))
      ((level 'update-torens-projectielen-positie!))
      ((teken-adt 'teken-projectielen!) ((level 'verkrijg-projectielen)))
      ((teken-adt 'update-tekst-teken!) levens))
      
    ;;Volgende code implementeert een toets om het spel de laten starten
    (define (toets-procedure toestand toets)
      (cond
        ((and (eq? toestand 'pressed) (eq? toets #\space) ((level 'einde?))) 
         (set! level (maak-level-adt level-1 geld levens (level 'torens))))
        ((and (eq? toestand 'pressed) (eq? toets #\space))
         ((teken-adt 'set-spel-lus!) spel-lus-procedure))
        ((and (eq? toestand 'pressed) (eq? toets 'escape))
         ((level 'level-einde!)))))
            
    (define (dispatch msg)
      (cond 
        ((eq? msg 'start!) (start!))
        (else
         "maak-spel-adt: ongeldig bericht")))
    dispatch))
