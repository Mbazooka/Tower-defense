;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                  Spel ADT                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (maak-spel-adt)
  (let* ((geld (maak-geld-adt *geld-begin-bedrag*))
         (levens (maak-leven-adt *levens-hoeveelheid*))
         (level (maak-level-adt level-1 geld levens))
         (pad (level 'pad))
         (teken-adt (maak-teken-adt (+ *menu-breedte-px* *spel-breedte-px*) *spel/menu-hoogte-px*));; maak de fundamenten van het spel
         (toren-type #f) ;; Om torens te plaatsen veranderen we dit om te weten welk type toren te plaatsen.
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
      (let ((geselecteerde-toren ((teken-adt 'toren-selectie) x y)))
        (cond 
          ((and (eq? toets 'left) (eq? toestand 'pressed) geselecteerde-toren) ;; Initialiseert toren type
           (set! toren-type geselecteerde-toren))
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
                ((teken-adt 'update-tekst-teken!) geld))
               (else "Beweging niet mogelijk")))))))
    
    ;; Volgende code implementeert de spel lus van het pel
    (define (spel-lus-procedure dt)
      (if ((levens 'dood?))
          ((level 'level-einde!)))
      (if (>= monster-tijd *monster-spawn-frequentie*) ;; Zal monsters op scherm updaten na ongeveer 2 seconden
          (begin
            ((level 'update-monsters!) dt 'toevoegen)
            (set! monster-tijd 0))
          ((level 'update-monsters!) dt))
      ((teken-adt 'teken-monsters!) (level 'monsters))
      (set! monster-tijd (+ monster-tijd dt))
      (if (>= projectiel-tijd *toren-afvuur-frequentie*)
          (begin
            ((level 'update-torens-projectielen-afschieten!))
            (set! projectiel-tijd 0)))
      (set! projectiel-tijd (+ projectiel-tijd dt))
      ((level 'update-torens-projectielen-positie!) dt)
      ((teken-adt 'teken-projectielen!) ((level 'verkrijg-projectielen)))
      ((teken-adt 'update-tekst-teken!) geld)
      ((teken-adt 'update-tekst-teken!) levens))
      
    ;;Volgende code implementeert een toets om het spel de laten starten
    ;; !!!! Te veranderen, naar betere vorm van next-level! !!!!!
    (define (toets-procedure toestand toets)
      (cond
        ((and (eq? toestand 'pressed) (eq? toets #\space) ((level 'einde?)))
         ((geld 'voeg-geld-toe!) 'level)
         ((teken-adt 'update-tekst-teken!) geld)
         (set! level (maak-level-adt level-1 geld levens (level 'torens)))
         ((teken-adt 'update-tekst-teken!) level))
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
