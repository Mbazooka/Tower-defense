;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                 Teken ADT                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Doel van dit ADT is om alles gemakkelijk te teken gebruikmakend van de grafische bibliotheek (dit zal gebruikt worden door spel ADT)
;; ADT dient om alles op te roepen, dus het tekenen ook
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
      
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                       
    (define (beweeg-tegel-object! object tegel) ;; Operatie beweegt tegel van object naar gewenste positie 
      (let* ((obj-x-pos ((object 'positie) 'x)) ;; initialisatie positie om te blijven of continue beweging,
             (obj-y-pos ((object 'positie) 'y)) ;;  uit veronderstellen posities al veranderd zijn (werkt niet voor pad en toren)
             (scherm-x (* obj-y-pos px-breedte))
             (scherm-y (* obj-y-px-hoogte)))
        ((tegel 'set-x!) scherm-x)
        ((tegel 'set-y!) scherm-y)))
    
    (define (teken-dynamisch-object! object object-bitmap object-laag)
      (let ((tegel-van-object (make-tile 20 20 object-bitmap)))
        (beweeg-tegel! object tegel-van-object)
        ((object-laag 'add-drawable!) tegel-van-object)))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
    ;; Laag waarop pad getekent word
    (define laag-pad ((venster 'new-layer!))) 

    ;; Procedure die tegel op juiste pixel positie zet 
    (define (bepaal-tegel-px-positie! positie tegel) ;; Misschien later als hulpprocedure definieren (afhankelijk implementatie dynamische zaken)
      (let* ((pad/rand-x-pos (positie 'x))
             (pad/rand-y-pos (positie 'y))
             (scherm-x (* pad/rand-x-pos *px-breedte*))
             (scherm-y (* pad/rand-y-pos *px-hoogte*)))
        ((tegel 'set-x!) scherm-x)
        ((tegel 'set-y!) scherm-y)))

    ;; Maakt tegel en zet tegel op laag op juiste plaats
    (define (initialiseer-statisch-posities-scherm! positie object-bitmap object-laag) 
      (let ((tegel-van-object (make-tile 20 20 object-bitmap)))
        (bepaal-tegel-px-positie! positie tegel-van-object)
        ((object-laag 'add-drawable!) tegel-van-object)))
    
    ;; Pakt elke pad positie en maakt een tegel en zet die op juiste plaats  
    (define (teken-pad! pad)
      (let ((pad-posities (pad 'posities)))
        (define (hulp-teken-pad! ctr)
          (if (not (= ctr (pad 'lengte)))
              (begin
                (initialiseer-statisch-posities-scherm! (vector-ref pad-posities ctr) *bitmap-pad* laag-pad)
                (hulp-teken-pad! (+ ctr 1)))))
        (hulp-teken-pad! 0)))

    ;; Tekent toren op het scherm gegeven een positie (die positie is gegeven door de muis callback functie)
    (define laag-toren ((vesnter 'new-layer!)))
    
    (define (teken-toren! toren positie)
      (let ((toren-tegel (make-tile 60 60 *bitmap-toren-1*)))
        
      
      
      

      
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
            ((eq? msg 'set-muis-toets!) set-muis-toets!)
            (else "Teken-adt: undefined message")))
        dispatch))