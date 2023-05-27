;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                  Spel ADT                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (maak-spel-adt)
  (let* ((spel-lus-gestart? #f) ;; Nuttig voor bepaalde elementen beter te doen functioneren
         (level-teller 1)
         (ronde-teller 1)
         (pad-teller 1)
         (geld (maak-geld-adt *geld-begin-bedrag*))
         (levens (maak-levens-adt *levens-hoeveelheid*))
         (level (maak-level-adt pad-teller (monster-vector-verkrijg level-teller ronde-teller) geld levens))                 
         (pad (level 'pad))
         (teken-adt (maak-teken-adt (+ *menu-breedte-px* *spel-breedte-px*) *spel/menu-hoogte-px*));; maak de fundamenten van het spel
         (toren-type #f) ;; Om torens te plaatsen veranderen we dit om te weten welk type toren te plaatsen.
         (monster-tijd 0) ;; Tijd afgelopen sinds vorige monster op pad
         (tank-power-up '())
         (bommen-regen-power-up '())
         (power-up-tijd-actief 0)
         (power-up-afkoeling 0)
         (start? #t)
         (game-over? #f)
         (game-gewonnen? #f))

    ;; Tekent initiele zaken van het spel
    ((teken-adt 'teken-pad!) pad)
    ((teken-adt 'teken-begin-scherm!))

    ;; Start het van het spel door toetsen te initialiseren
    (define (start!)
      ((teken-adt 'set-muis-toets!) muis-klik-procedure)
      ((teken-adt 'set-toets-procedure!) toets-procedure))

    ;; Volgende code zijn abstracties
    (define tank car)
    (define bommen-regen cdr)

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
                     (begin
                       (set! tank-power-up (cons (maak-power-up-adt pad 'tank) tank-power-up))
                       ((teken-adt 'update-tekst-teken!) 'tank (length tank-power-up)))
                     (begin
                       (set! bommen-regen-power-up (cons (maak-power-up-adt pad 'bommen-regen) bommen-regen-power-up))
                       ((teken-adt 'update-tekst-teken!) 'bommen-regen (length bommen-regen-power-up))))
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
               (else "Beweging niet mogelijk"))))
          (else
           (let* ((opgenomen-power-ups ((level 'drop-opraap!) x y))
                  (opgenomen-tanks (tank opgenomen-power-ups))
                  (opgenomen-bommen-regen (bommen-regen opgenomen-power-ups)))
             (if (not (null? opgenomen-tanks))
                 (set! tank-power-up (append opgenomen-tanks tank-power-up)))
             (if (not (null? opgenomen-bommen-regen))
                 (set! bommen-regen-power-up (append opgenomen-bommen-regen bommen-regen-power-up)))))                                                   
          )))
    
    ;; Volgende code implementeert de spel lus van het spel
    (define (spel-lus-procedure dt)
      (set! spel-lus-gestart? #t)
      (if ((levens 'dood?))
          (begin
            ((level 'ronde-einde!))
            ((teken-adt 'teken-game-over!))
            (set! game-over? #t)))
      (if (= monster-tijd 0) ;; Zal monsters op scherm updaten na ongeveer 2 seconden
          (begin
            ((level 'update-monsters!) dt 'toevoegen)
            (set! monster-tijd (+ monster-tijd dt)))
          (begin
            ((level 'update-monsters!) dt)
            (if (>= monster-tijd *monster-spawn-frequentie*)
                (set! monster-tijd 0)
                (set! monster-tijd (+ monster-tijd dt)))))
      ((teken-adt 'teken-monsters!) (level 'monsters))
      ((level 'update-power-ups!) dt)
      ((level 'update-torens-projectielen-afschieten!) pad dt)
      ((level 'update-torens-projectielen-positie!) dt)
      (if (afkoeling?)
          (begin
            (set! power-up-afkoeling (+ power-up-afkoeling dt))
            (if (>= power-up-afkoeling *power-up-afkoel-tijd*)
                (begin
                  (set! power-up-afkoeling 0)
                  ((teken-adt 'teken-afkoeling-acties!) 'verwijderen)))))
      (if (not (<= power-up-tijd-actief 0))
          (if (> power-up-tijd-actief dt)            
              (set! power-up-tijd-actief (- power-up-tijd-actief dt))
              (begin
                (set! power-up-tijd-actief 0)
                ((teken-adt 'verwijder-bommen!)))))
      ((teken-adt 'teken-projectielen!) ((level 'verkrijg-projectielen)))
      ((teken-adt 'teken-tank-power-up!) (level 'verkrijg-tank-power-ups))
      ((teken-adt 'update-tekst-teken!) 'geld (geld 'status))
      ((teken-adt 'update-tekst-teken!) 'levens (levens 'status))
      ((teken-adt 'update-tekst-teken!) 'level level-teller)
      ((teken-adt 'update-tekst-teken!) 'tank (length tank-power-up))
      ((teken-adt 'update-tekst-teken!) 'bommen-regen (length bommen-regen-power-up))
      (if (afkoeling?)
          ((teken-adt 'update-tekst-teken!) 'afkoel  (ms->s (- *power-up-afkoel-tijd* power-up-afkoeling)))
          ((teken-adt 'update-tekst-teken!) 'afkoel  (ms->s *geen-afkoel-tijd*)))
      ((teken-adt 'update-tekst-teken!) 'actief-tijd  (ms->s power-up-tijd-actief))
      ((teken-adt 'teken-gedropte-power-ups!) (level 'verkrijg-gedropte-power-ups)))
      
    ;;Volgende code implementeert een toets om het spel de laten starten
    (define (toets-procedure toestand toets)
      (cond
        ((and (eq? toestand 'pressed) (eq? toets #\return) start?)
         (set! start? #f)
         ((teken-adt 'verwijder-scherm!)))
        ((and (eq? toestand 'pressed) (eq? toets #\return) (or game-over? game-gewonnen?))
         (reset-game!)
         ((teken-adt 'verwijder-scherm!)))
        ((and (eq? toestand 'pressed) (eq? toets #\space) (level-einde?))
         (next-level!))
        ((and (eq? toestand 'pressed) (eq? toets #\space) ((level 'ronde-einde?)))
         (next-ronde!))
        ((and (eq? toestand 'pressed) (eq? toets #\space) (not start?))
         ((teken-adt 'set-spel-lus!) spel-lus-procedure))
        ((and (eq? toestand 'pressed) (eq? toets 'escape) (not start?))
         ((level 'ronde-einde!)))
        ((and (eq? toestand 'pressed) (eq? toets #\t) spel-lus-gestart?  (not (afkoeling?)) (not (null? tank-power-up)))
         (power-up-handelingen! 'tank tank-power-up)         
         (set! power-up-tijd-actief *tank-actief-tijd*))
        ((and (eq? toestand 'pressed) (eq? toets #\b) spel-lus-gestart? (not (afkoeling?)) (not (null? bommen-regen-power-up)))
         (power-up-handelingen! 'bommen-regen bommen-regen-power-up)
         (set! power-up-tijd-actief *bommen-regen-actief-tijd*))))

    ;; Volgende code zijn abstracties
    (define volgende-power-up car)
    (define rest-power-ups cdr)
    
    ;; Volgende code is algemene code om power-ups te activeren en te tekenen
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
              ((teken-adt teken-bericht) (level verkrijg-objecten-bericht))
              ((teken-adt 'teken-afkoeling-acties!) 'toevoegen)
              (set! power-up-afkoeling (+ power-up-afkoeling 1)))))) ;; Trucje om power-up-afkoeling verschillend van 0 te maken (voor in afkoeling staat te komen-

    ;; Volgende code gaat na als de power-ups in cool-down zijn
    (define (afkoeling?)
      (not (= power-up-afkoeling 0)))

    ;; Volgende code bepaalt als een level tot zijn einde gekomen is
    (define (level-einde?)
      (and ((level 'ronde-einde?)) (>= ronde-teller *aantal-ronden*)))

    ;; Volgende doet het spel naar de volgende ronde gaan
    (define (next-ronde!)
      (set! monster-tijd 0)
      ((geld 'voeg-geld-toe!) 'ronde #f)
      (set! ronde-teller (+ ronde-teller 1))
      ((teken-adt 'update-tekst-teken!) 'level level-teller)
      ((teken-adt 'update-tekst-teken!) 'ronde ronde-teller)
      ((level 'zet-monster-lijst!) (monster-vector-verkrijg level-teller ronde-teller)))

    ;; Volgende doet het spel naar de volgende ronde gaan
    (define (next-level!)
      (set! level-teller (+ level-teller 1))
      (if (> level-teller *aantal-levels*)
          (begin
            ((teken-adt 'teken-eind-scherm!))
            (set! game-gewonnen? #t))
          (begin
            (set! ronde-teller 1)
            (set! monster-tijd 0)
            ((geld 'voeg-geld-toe!) 'level #f)
            ((teken-adt 'update-tekst-teken!) 'geld (geld 'status))
            ((level 'initialiseer-toren-tijden!))     
            (set! pad-teller (+ pad-teller 1))
            (set! tank-power-up '())
            ((teken-adt 'update-tekst-teken!) 'tank (length tank-power-up))        
            (set! bommen-regen-power-up '())
            ((teken-adt 'update-tekst-teken!) 'bommen-regen (length bommen-regen-power-up))
            ((teken-adt 'verwijder-torens!))
            ((teken-adt 'verwijder-pad!))      
            (set! level (maak-level-adt pad-teller (monster-vector-verkrijg level-teller ronde-teller) geld levens))
            (set! pad (level 'pad))
            ((teken-adt 'teken-pad!) pad)
            (set! spel-lus-gestart? #f)
            ((teken-adt 'update-tekst-teken!) 'level level-teller)
            ((teken-adt 'update-tekst-teken!) 'ronde ronde-teller))))

    ;; Volgende zal het spel restarten
    (define (reset-game!)      
      ((levens 'reset!))
      ((teken-adt 'update-tekst-teken!) 'levens (levens 'status))
      ((geld 'reset!))
      ((teken-adt 'update-tekst-teken!) 'geld (geld 'status))
      (set! toren-type #f)
      ((teken-adt 'verwijder-torens!))
      (set! monster-tijd 0)
      (set! tank-power-up '())
      ((teken-adt 'update-tekst-teken!) 'tank (length tank-power-up))
      (set! bommen-regen-power-up '())
      ((teken-adt 'update-tekst-teken!) 'bommen-regen (length bommen-regen-power-up))
      ((teken-adt 'verwijder-gedropte-power-ups!))
      (set! level-teller 1)
      ((teken-adt 'update-tekst-teken!) 'level level-teller)
      (set! ronde-teller 1)
      ((teken-adt 'update-tekst-teken!) 'ronde ronde-teller)
      (set! pad-teller 1)
      ((teken-adt 'verwijder-pad!))
      (if (afkoeling?)
          ((teken-adt 'teken-afkoeling-acties!) 'verwijderen))
      (set! power-up-tijd-actief 0)
      ((teken-adt 'update-tekst-teken!) 'actief-tijd  (ms->s power-up-tijd-actief))
      (set! power-up-afkoeling 0)
      ((teken-adt 'update-tekst-teken!) 'afkoel  (ms->s *geen-afkoel-tijd*))
      (set! game-over? #f)
      (set! game-gewonnen? #f)
      (set! spel-lus-gestart? #f)                    
      (set! level (maak-level-adt pad-teller (monster-vector-verkrijg level-teller ronde-teller) geld levens))
      (set! pad (level 'pad))
      ((teken-adt 'teken-pad!) pad))

    ;; Volgende code zal microseconden omvormen naar seconden
    (define (ms->s getal)
      (quotient getal *seconde*))
                                    
    (define (dispatch msg)
      (cond 
        ((eq? msg 'start!) (start!))
        (else
         "maak-spel-adt: ongeldig bericht")))
    dispatch))
