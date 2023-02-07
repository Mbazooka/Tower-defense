;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                 Teken ADT                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Doel van dit ADT is om alles gemakkelijk te teken gebruikmakend van de grafische bibliotheek (dit zal gebruikt worden door spel ADT)
(define (maak-teken-adt horizontale-pixels verticale-pixels)
  (let ((venster (make-window horizontale-pixels verticale-pixels "Tower Defense"))
        (monster-tiles-dict (cons 'tegels '()))) ;; Tagged omdat 1ste conscell veranderd moet worden/ Dit zijn monster-tegel associaties
    
    ((venster 'set-background!) "black")

    ;; Volgende code is om een achtergrond te hebben waarop een pad gemaakt wordt
    (define laag-achtergrond ((venster 'new-layer!)))
    (define achtergrond-tegel (make-bitmap-tile "Images/Lava-Achtergrond.png"))
    ((laag-achtergrond 'add-drawable!) achtergrond-tegel)

    ;; Volgende code is om een menu te maken 
    (define laag-menu ((venster 'new-layer!)))
    (define menu-tegel (make-tile *menu-breedte-px* *spel/menu-hoogte-px*))
    ((menu-tegel 'draw-rectangle!) 0 0 *menu-breedte-px* *spel/menu-hoogte-px* "black")
    ((menu-tegel 'draw-rectangle!) 0 0 5 *spel/menu-hoogte-px* "darkorange") ;; Voegt lijntje van 5px groot toe bij menu (maakt het stijlvoller)
    ((menu-tegel 'draw-text!) "Torens" 12 *tekst-toren-breedte* *tekst-toren-hoogte* "darkorange")
    ((menu-tegel 'set-x!) *spel-breedte-px*)
    ((laag-menu 'add-drawable!) menu-tegel)

    ;; Volgende code is om de user-interface van de menu te maken
    (define user-interface ((venster 'new-layer!)))
    (define toren-1-tegel (make-bitmap-tile "Images/Toren-1-Game.png" "Images/Toren-1-game-mask.png"))
    ((toren-1-tegel 'set-x!) *toren-knop-breedte-start*) ;; Dit is een keuze om 1 px breed van start van menu, een "button" image te zetten
    ((toren-1-tegel 'set-y!) *toren-1-knop-hoogte-start*)
    ((user-interface 'add-drawable!) toren-1-tegel)
      
    ;; Laag waarop pad getekent word
    (define laag-pad ((venster 'new-layer!))) 

    ;; Procedure die tegel op juiste pixel positie zet
    ;;met positie gedaan (niet object als formele parameter) want pad geeft meerdere posities, code kan enkel 1 positie/object per keer doen
    (define (bepaal-tegel-px-positie! positie tegel) ;; Misschien later als hulpprocedure definieren (afhankelijk implementatie dynamische zaken)
      (let* ((obj-x-pos (positie 'x))
             (obj-y-pos (positie 'y))
             (scherm-x (* obj-x-pos *px-breedte*))
             (scherm-y (* obj-y-pos *px-hoogte*)))
        ((tegel 'set-x!) scherm-x)
        ((tegel 'set-y!) scherm-y)))

    ;; Maakt tegel en zet tegel op laag op juiste plaats (voor een statisch object) !!!!Verander naam!!!!
    (define (initialiseer-statisch-posities-scherm! positie object-bitmap object-mask object-laag) 
      (let ((tegel-van-object (make-bitmap-tile object-bitmap object-mask)))
        (bepaal-tegel-px-positie! positie tegel-van-object)
        ((object-laag 'add-drawable!) tegel-van-object)
        tegel-van-object)) ;; Tegel teruggeven want later nodig om dictionary van monsters-tegel te maken
    
    ;; Pakt elke pad positie en maakt een tegel en zet die op juiste plaats  
    (define (teken-pad! pad)
      (let ((pad-posities (cdr (pad 'posities))))
        (define (hulp-teken-pad! ctr)
          (if (not (= ctr (pad 'lengte)))
              (begin
                (initialiseer-statisch-posities-scherm! (vector-ref pad-posities ctr) "Images/lava.jpeg" "Images/Lava-mask.png" laag-pad)
                (hulp-teken-pad! (+ ctr 1)))))
        (hulp-teken-pad! 0)))

    ;; Volgende code is een venster om torens op te plaatsen
    (define laag-toren ((venster 'new-layer!)))

    ;; Tekent toren op het scherm gegeven een toren
    (define (teken-toren! toren)
      (let ((toren-positie (toren 'positie)))              
        (initialiseer-statisch-posities-scherm! (maak-positie-adt (- (toren-positie 'x) 1) (- (toren-positie 'y) 1)) "Images/Toren-1-game.png" "Images/Toren-1-game-mask.png" laag-toren))) ;; nieuwe positie om toren te centreren

    ;Volgende code zijn abstracties om met dictionaries te werken
    (define (associatie dict)
      (car dict))

    (define rest-dict cdr)

    (define (sleutel associatie)
      (car associatie))

    (define (waarde associatie)
      (cdr associatie))

    ;; Steekt een associatie in de dictionary
    (define (insert! sleut value tagged-dict)
      (let ((toe-te-voegen (list (cons sleut value))))
        (set-cdr! toe-te-voegen (rest-dict tagged-dict))
        (set-cdr! tagged-dict toe-te-voegen)))   

    ;; Delete een bepaalde sleutel uit de dictionary
    (define (delete! sleut dict) 
      (define (delete-hulp huidige vorige)
        (cond
          ((null? (rest-dict huidige))
           (if (eq? (sleutel (associatie huidige)) sleut)
               (set-cdr! vorige '())
               #f))
          ((eq? (sleutel (associatie huidige)) sleut)
           (set-cdr! vorige (rest-dict huidige)))
          (else
           (delete-hulp (rest-dict huidige) huidige))))
      (delete-hulp (cdr dict) dict))
    
    ;; Volgende code is een venster om monsters te plaatsen
    (define laag-monster ((venster 'new-layer!)))
    
    ;; Tekent bestaande monsters op het scherm gegeven een lijst monsters
    (define (teken-monsters! monsters) 
      (define (haal-weg-monster-tiles-dict! diction)
        (if (null? diction)
            #f
            (let ((te-zoeken (sleutel (associatie diction))))
              (if (not (memq te-zoeken monsters)) ;; Dit klopt niet!!!
                  (begin
                    ((laag-monster 'remove-drawable!) (waarde (associatie diction)))
                    (delete! te-zoeken monster-tiles-dict))
                  (haal-weg-monster-tiles-dict! (rest-dict diction))))))             
      
      (define (voeg-toe-monster-tiles-dict! huidige-monster) ;; Gaat mogelijks nieuwe tiles toevoegen en tekenen (1 per keer)
        (if (null? huidige-monster)
            #f
            (let ((monster (car huidige-monster)))
              (if (not (assq monster (rest-dict monster-tiles-dict)))
                  (insert! monster (initialiseer-statisch-posities-scherm! (monster 'positie) "Images/Rood-monster.jpg" "Images/Rood-monster-mask.png" laag-monster) monster-tiles-dict)
                  (voeg-toe-monster-tiles-dict! (cdr huidige-monster))))))

      (haal-weg-monster-tiles-dict! (rest-dict monster-tiles-dict)) 
      (for-each ;; Gaat elke tile updaten 
       (lambda (ass) 
         (bepaal-tegel-px-positie! ((sleutel ass) 'positie) (waarde ass))) 
       (rest-dict monster-tiles-dict))
      (voeg-toe-monster-tiles-dict! monsters))
           
    ;; Als hij in de monsters lijst zit maar niet in de dictionary dan moet je hem toevoegen. (Voeg dit toe voor meer informatie)
          
    ;; OPTIE: Probeer te veranderen zodat argument "pad" weg is!!!!!!!!!
    (define (teken-spel! pad) 
      (teken-pad! pad))

    ;; Volgende code is om muis klikken te implementeren
    (define (set-muis-toets-procedure! proc)
      ((venster 'set-mouse-click-callback!) proc))

    ;; Volgende code is om een spellus te implementeren
    (define (set-spel-lus-procedure! proc)
      ((venster 'set-update-callback!) proc))

    ;; Voglende code is om een knop in te voegen
    (define (set-toets-procedure! proc)
      ((venster 'set-key-callback!) proc))
              
    (define (dispatch msg)
      (cond
        ((eq? msg 'teken-spel!) teken-spel!)
        ((eq? msg 'teken-toren!) teken-toren!)
        ((eq? msg 'teken-toren!) teken-toren!)
        ((eq? msg 'teken-monsters!) teken-monsters!)
        ((eq? msg 'set-muis-toets!) set-muis-toets-procedure!)
        ((eq? msg 'set-spel-lus!) set-spel-lus-procedure!)
        ((eq? msg 'set-toets-procedure!) set-toets-procedure!)
        (else "maak-teken-adt: ongeldig bericht")))
    dispatch))