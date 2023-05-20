;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                  Spel ADT                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (maak-spel-adt)
  (let* ((spel-lus-gestart? #f) ;; Nuttig voor bepaalde elementen beter te doen functioneren
         (geld (maak-geld-adt *geld-begin-bedrag*))
         (levens (maak-leven-adt *levens-hoeveelheid*))
         (level (maak-level-adt level-1 geld levens))                  
         (pad (level 'pad))
         (teken-adt (maak-teken-adt (+ *menu-breedte-px* *spel-breedte-px*) *spel/menu-hoogte-px*));; maak de fundamenten van het spel
         (toren-type #f) ;; Om torens te plaatsen veranderen we dit om te weten welk type toren te plaatsen.
         (monster-tijd 0) ;; Tijd afgelopen sinds vorige monster op pad
         (tank-power-up '())
         (bommen-regen-power-up '())
         (level-teller 1)
         (ronde-teller 1)
         (tank-cooldown-teller 0)
         (bommen-regen-cooldown-teller 0))

    ;; Tekent pad van het spel
    ((teken-adt 'teken-pad!) pad) 

    ;; Start het van het spel door toetsen te initialiseren
    (define (start!)
      ((teken-adt 'set-muis-toets!) muis-klik-procedure)
      ((teken-adt 'set-toets-procedure!) toets-procedure))

    ;; De procedure die het klikken van muis op scherm voorstelt    
    (define (muis-klik-procedure toets toestand x y)
      (let ((geselecteerde-toren ((teken-adt 'toren-selectie) x y))
            (geselecteerde-power-up ((teken-adt 'power-up-selectie) x y)))
        (cond 
          ((and (eq? toets 'left) (eq? toestand 'pressed) geselecteerde-toren) ;; Initialiseert toren type          
           (set! toren-type geselecteerde-toren))
          ((and (eq? toets 'left) (eq? toestand 'pressed) geselecteerde-power-up) ;; Koopt power-up
           (if ((geld 'voldoende-geld?) geselecteerde-power-up)
               (begin
                 ((geld 'verwijder-geld!) geselecteerde-power-up)
                 (if (eq? geselecteerde-power-up 'tank)
                     (set! tank-power-up (cons (maak-power-up-adt pad 'tank) tank-power-up))
                     (set! bommen-regen-power-up (cons (maak-power-up-adt pad 'bommen-regen) bommen-regen-power-up)))
                 ((teken-adt 'update-tekst-teken!) 'geld (geld 'status)))))
          ((eq? toren-type #f) "Kies een toren") ;; Indien nog geen toren gekozen is dan moet
          ((and (eq? toets 'left) (eq? toestand 'pressed)
                ((teken-adt 'buiten-beperking?) x y)
                ((geld 'voldoende-geld?) toren-type))
        
           (let ((toren (maak-toren-adt (maak-positie-adt (/ x *px-breedte*) (/ y *px-hoogte*)) toren-type)))
             (cond
               ((and (not (accumulate (lambda (x y) (or x y)) #f (map (lambda (t) ((t 'in-toren?) toren)) (level 'torens))))
                     (not ((pad 'toren-in-pad?) toren)))
                ((geld 'verwijder-geld!) toren-type)
                ((level 'voeg-toren-toe!) toren)
                ((teken-adt 'teken-toren!) toren)
                ((teken-adt 'update-tekst-teken!) 'geld (geld 'status)))
               (else "Beweging niet mogelijk")))))))
    
    ;; Volgende code implementeert de spel lus van het spel
    (define (spel-lus-procedure dt)
      (set! spel-lus-gestart? #t)
      (if ((levens 'dood?))
          ((level 'level-einde!)))
      (if (>= monster-tijd *monster-spawn-frequentie*) ;; Zal monsters op scherm updaten na ongeveer 2 seconden
          (begin
            ((level 'update-monsters!) dt 'toevoegen)
            (set! monster-tijd 0))
          ((level 'update-monsters!) dt))
      ((teken-adt 'teken-monsters!) (level 'monsters))
      (set! monster-tijd (+ monster-tijd dt))
      ((level 'update-power-ups!) dt)
      ((level 'update-torens-projectielen-afschieten!) pad dt)
      ((level 'update-torens-projectielen-positie!) dt)        
      ((teken-adt 'teken-projectielen!) ((level 'verkrijg-projectielen)))
      ((teken-adt 'teken-tank-power-up!) (level 'verkrijg-tank-power-ups))
      ((teken-adt 'update-tekst-teken!) 'geld (geld 'status))
      ((teken-adt 'update-tekst-teken!) 'levens (levens 'status))
      ((teken-adt 'update-tekst-teken!) 'level level-teller))
      
    ;;Volgende code implementeert een toets om het spel de laten starten
    (define (toets-procedure toestand toets)
      (cond
        ((and (eq? toestand 'pressed) (eq? toets #\space) ((level 'einde?)))
         ((geld 'voeg-geld-toe!) 'level #f)
         ((teken-adt 'update-tekst-teken!) 'geld (geld 'status))
         ((level 'initialiseer-toren-tijden!))
         (set! level (maak-level-adt level-1 geld levens (level 'torens)))
         (set! level-teller (+ level-teller 1))
         (set! ronde-teller 1)
         (set! spel-lus-gestart? #f)
         ((teken-adt 'update-tekst-teken!) 'level level-teller))
        ((and (eq? toestand 'pressed) (eq? toets #\space))
         ((teken-adt 'set-spel-lus!) spel-lus-procedure))
        ((and (eq? toestand 'pressed) (eq? toets 'escape))
         ((level 'level-einde!)))
        ((and (eq? toestand 'pressed) (eq? toets #\t) spel-lus-gestart?)
         (power-up-handelingen! 'tank tank-power-up))
        ((and (eq? toestand 'pressed) (eq? toets #\b) spel-lus-gestart?)
         (power-up-handelingen! 'bommen-regen bommen-regen-power-up))))

    ;; Volgende code zijn abstracties
    (define volgende-power-up car)
    (define rest-power-ups cdr)
    
    ;; Volgende code is algemene code om power-up te activeren en te tekenen
    (define (power-up-handelingen! power-up-type power-up-lijst)
      (let*  ((bool-1 (pair? power-up-lijst)) ; #t
              (bool-2 (eq? power-up-type 'tank)) ; #f
              (power-up (if bool-1
                            (volgende-power-up power-up-lijst)
                            #f))
              (teken-bericht (if bool-2 'teken-tank-power-up! 'teken-bommen-regen-power-up!))
              (verkrijg-objecten-bericht (if bool-2 'verkrijg-tank-power-ups 'verkrijg-bommen-regen-power-ups)))
        
        (if (and bool-1 power-up)
            (if bool-2
                (set! tank-power-up (rest-power-ups tank-power-up))
                (set! bommen-regen-power-up (rest-power-ups bommen-regen-power-up))))
        (if power-up
            (begin
              ((level 'voeg-power-up-toe!) power-up-type power-up)
              ((teken-adt teken-bericht) (level verkrijg-objecten-bericht))))))
       
                   
    (define (dispatch msg)
      (cond 
        ((eq? msg 'start!) (start!))
        (else
         "maak-spel-adt: ongeldig bericht")))
    dispatch))
