;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                 Teken ADT                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Doel van dit ADT is om alles gemakkelijk te teken gebruikmakend van de grafische bibliotheek (dit zal gebruikt worden door spel ADT)
(define (maak-teken-adt horizontale-pixels verticale-pixels)
  (let ((venster (make-window horizontale-pixels verticale-pixels "Tower Defense"))
        (monster-tiles '()))
    
    ((venster 'set-background!) "black")

    ;; Volgende code is om een achtergrond te hebben waarop een pad gemaakt wordt
    (define laag-gras ((venster 'new-layer!)))
    (define gras-tegel (make-tile *spel-breedte-px* *spel/menu-hoogte-px*))
    ((gras-tegel 'draw-rectangle!) 0 0 *spel-breedte-px* *spel/menu-hoogte-px* "black")
    ((laag-gras 'add-drawable!) gras-tegel)

    ;; Volgende code is om een menu te maken 
    (define laag-menu ((venster 'new-layer!)))
    (define menu-tegel (make-tile *menu-breedte-px* *spel/menu-hoogte-px*))
    ((menu-tegel 'draw-rectangle!) 0 0 *menu-breedte-px* *spel/menu-hoogte-px* "black")
    ((menu-tegel 'draw-rectangle!) 0 0 5 *spel/menu-hoogte-px* "darkorange") ;; Voegt lijntje van 5px groot toe bij menu 
    ((menu-tegel 'set-x!) *spel-breedte-px*)
    ((laag-menu 'add-drawable!) menu-tegel)

    ;; Volgende code is om de user-interface van de menu te maken
    (define user-interface ((venster 'new-layer!)))
    (define toren-1-tegel (make-bitmap-tile "Images/Toren-1-Game.png"))
    ((toren-1-tegel 'set-x!) (+ *spel-breedte-px* *px-breedte*)) ;; Dit is een keuze om 1 px breed van start van menu, een "button" image te zetten
    ((toren-1-tegel 'set-y!) *toren-1-knop-hoogte-start*)
    ((user-interface 'add-drawable!) toren-1-tegel)
      
    ;; Laag waarop pad getekent word
    (define laag-pad ((venster 'new-layer!))) 

    ;; Procedure die tegel op juiste pixel positie zet 
    (define (bepaal-tegel-px-positie! positie tegel) ;; Misschien later als hulpprocedure definieren (afhankelijk implementatie dynamische zaken)
      (let* ((obj-x-pos (positie 'x))
             (obj-y-pos (positie 'y))
             (scherm-x (* obj-x-pos *px-breedte*))
             (scherm-y (* obj-y-pos *px-hoogte*)))
        ((tegel 'set-x!) scherm-x)
        ((tegel 'set-y!) scherm-y)))

    ;; Maakt tegel en zet tegel op laag op juiste plaats (voor een statisch object)
    (define (initialiseer-statisch-posities-scherm! positie object-bitmap object-laag) 
      (let ((tegel-van-object (make-bitmap-tile object-bitmap)))
        (bepaal-tegel-px-positie! positie tegel-van-object)
        ((object-laag 'add-drawable!) tegel-van-object)))
    
    ;; Pakt elke pad positie en maakt een tegel en zet die op juiste plaats  
    (define (teken-pad! pad)
      (let ((pad-posities (pad 'posities)))
        (define (hulp-teken-pad! ctr)
          (if (not (= ctr (pad 'lengte)))
              (begin
                (initialiseer-statisch-posities-scherm! (vector-ref pad-posities ctr) "Images/lava.jpeg" laag-pad)
                (hulp-teken-pad! (+ ctr 1)))))
        (hulp-teken-pad! 0)))

    ;; Volgende code is een venster om torens op te plaatsen
    (define laag-toren ((venster 'new-layer!)))

    ;; Tekent toren op het scherm gegeven een toren
    (define (teken-toren! toren)
      (let ((x-pos ((toren 'positie) 'x))
            (y-pos ((toren 'positie) 'y)))                
        (initialiseer-statisch-posities-scherm! (maak-positie-adt (- x-pos 1) (- y-pos 1)) "Images/Toren-1.jpg" laag-toren))) ;; nieuwe positie om toren te centreren
                     
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
      ((vesnter 'set-key-callback!) proc))
              
    (define (dispatch msg)
      (cond
        ((eq? msg 'teken-spel!) teken-spel!)
        ((eq? msg 'teken-toren!) teken-toren!)
        ((eq? msg 'teken-toren!) teken-toren!)
        ((eq? msg 'set-muis-toets!) set-muis-toets-procedure!)
        ((eq? msg 'set-spel-lus!) set-spel-lus-procedure!)
        (else "maak-teken-adt: undefined message")))
    dispatch))