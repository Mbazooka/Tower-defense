;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                 Teken ADT                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Doel van dit ADT is om alles gemakkelijk te teken gebruikmakend van de grafische bibliotheek (dit zal gebruikt worden door spel ADT)
(define (maak-teken-adt horizontale-pixels verticale-pixels)
  (let ((venster (make-window horizontale-pixels verticale-pixels "Tower Defense")))
    
    ((venster 'set-background!) "black")

    ;; Volgende code is om een achtergrond te hebben waarop een pad gemaakt wordt
    (define laag-gras ((venster 'new-layer!)))
    (define gras-tegel (make-tile 800 600 "Images/Gras-3.jpg"))
    (define random-tegel (make-tile 90 90))
    ((random-tegel 'draw-rectangle!) 7 7 90 90 "blue")
    ((laag-gras 'add-drawable!) gras-tegel)
    ((laag-gras 'add-drawable!) random-tegel)

    ;; Volgende code is om een menu te maken 
    (define laag-menu ((venster 'new-layer!)))
    (define menu-tegel (make-tile 200 600))
    ((menu-tegel 'draw-rectangle!) 0 0 200 600 "lightblue")
    ((menu-tegel 'set-x!) 800)
    ((laag-menu 'add-drawable!) menu-tegel)

    ;; Volgende code is om de user-interface van de menu te maken
    (define user-interface ((venster 'new-layer!)))
    (define toren-1-tegel (make-tile 60 60 *bitmap-toren-1*))
    ((toren-1-tegel 'set-x!) 840)
    ((toren-1-tegel 'set-y!) 40)
    ((user-interface 'add-drawable!) toren-1-tegel)
      
    ;; Laag waarop pad getekent word
    (define laag-pad ((venster 'new-layer!))) 

    ;; Procedure die tegel op juiste pixel positie zet 
    (define (bepaal-tegel-px-positie! positie tegel) ;; Misschien later als hulpprocedure definieren (afhankelijk implementatie dynamische zaken)
      (let* ((pad-x-pos (positie 'x))
             (pad-y-pos (positie 'y))
             (scherm-x (* pad-x-pos *px-breedte*))
             (scherm-y (* pad-y-pos *px-hoogte*)))
        ((tegel 'set-x!) scherm-x)
        ((tegel 'set-y!) scherm-y)))

    ;; Maakt tegel en zet tegel op laag op juiste plaats
    (define (initialiseer-statisch-posities-scherm! dimensie-x dimensie-y positie object-bitmap object-laag) 
      (let ((tegel-van-object (make-tile dimensie-x dimensie-y object-bitmap)))
        (bepaal-tegel-px-positie! positie tegel-van-object)
        ((object-laag 'add-drawable!) tegel-van-object)))
    
    ;; Pakt elke pad positie en maakt een tegel en zet die op juiste plaats  
    (define (teken-pad! pad)
      (let ((pad-posities (pad 'posities)))
        (define (hulp-teken-pad! ctr)
          (if (not (= ctr (pad 'lengte)))
              (begin
                (initialiseer-statisch-posities-scherm! 20 20 (vector-ref pad-posities ctr) *bitmap-pad* laag-pad)
                (hulp-teken-pad! (+ ctr 1)))))
        (hulp-teken-pad! 0)))

    ;; Volgende code is een venster voor torens op te plaatsen
    (define laag-toren ((venster 'new-layer!)))

    ;; Tekent toren op het scherm gegeven een toren
    (define (teken-toren! toren)
      (let ((x-pos ((toren 'positie) 'x))
            (y-pos ((toren 'positie) 'y)))                
        (initialiseer-statisch-posities-scherm! 60 60 (maak-positie-adt (- x-pos 1) (- y-pos 1)) *bitmap-toren-1* laag-toren))) ;; nieuwe positie om toren te centreren
                     
    ;; OPTIE: Probeer te veranderen zodat argument "pad" weg is
    (define (teken-spel! pad) 
      (teken-pad! pad))

    ;; Volgende code is om muis klikken te implementeren
    (define (set-muis-toets! proc)
      ((venster 'set-mouse-click-callback!) proc))
              
    (define (dispatch msg)
      (cond
        ((eq? msg 'teken-spel!) teken-spel!)
        ((eq? msg 'teken-toren!) teken-toren!)
        ((eq? msg 'teken-toren!) teken-toren!)
        ((eq? msg 'set-muis-toets!) set-muis-toets!)
        (else "maak-teken-adt: undefined message")))
    dispatch))