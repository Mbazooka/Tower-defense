;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                 Teken ADT                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Doel van dit ADT is om alles gemakkelijk te teken gebruikmakend van de grafische bibliotheek (dit zal gebruikt worden door spel ADT)
(define (maak-teken-adt horizontale-pixels verticale-pixels)
  (let ((venster (make-window horizontale-pixels verticale-pixels "Tower Defence"))
        (monster-tiles-dict (cons 'tegels '())) ;; Tagged omdat 1ste conscell veranderd moet worden/ Dit zijn monster-tegel associaties
        (projectielen-tiles-dict (cons 'tegels '()))) ;; Dit zijn projectiet-tegel associaties

    ;; Volgende code is om een achtergrond te hebben waarop een pad gemaakt wordt
    (define laag-achtergrond ((venster 'new-layer!)))
    (define achtergrond-tegel (make-bitmap-tile "Images/Lava-Achtergrond.png"))
    ((laag-achtergrond 'add-drawable!) achtergrond-tegel)

    ;; Volgende code is om een menu te maken !!!!(Proberen geld layer hier toe te voegen)!!!!
    (define laag-menu ((venster 'new-layer!)))
    (define menu-tegel (make-tile *menu-breedte-px* *spel/menu-hoogte-px*))
    ((menu-tegel 'draw-rectangle!) 0 0 *menu-breedte-px* *spel/menu-hoogte-px* "black")
    ((menu-tegel 'draw-rectangle!) 0 0 (/ *px-breedte* 2) *spel/menu-hoogte-px* "darkorange") ;; Voegt lijntje van om stijlvoller te maken
    ((menu-tegel 'draw-text!) "Torens" *tekst-font* *tekst-toren-breedte* *tekst-toren-hoogte* "darkorange")
    ((menu-tegel 'set-x!) *spel-breedte-px*)
    ((laag-menu 'add-drawable!) menu-tegel)

    ;; Volgende code is om de user-interface van de menu te maken
    (define laag-user-interface ((venster 'new-layer!)))
    (define toren-1-tegel (make-bitmap-tile "Images/Toren-1-Game.png" "Images/Toren-1-game-mask.png"))
    ((toren-1-tegel 'set-x!) *start-data-menu*) 
    ((toren-1-tegel 'set-y!) *toren-1-knop-hoogte-start*)
    ((laag-user-interface 'add-drawable!) toren-1-tegel)

    ;; Volgende is om het geld en de levens van de speler voor te stellen.
    (define laag-geld&&levens ((venster 'new-layer!)))
    (define geld-tegel (make-bitmap-tile "Images/geld.png" "Images/geld-mask.png"))
    (define geld-tekst-tegel (make-tile *algemeen-tekst-breedte* *algemeen-tekst-hoogte*)) ;; Waarop de hoeveelheid zal staan

    (define levens-tegel (make-bitmap-tile "Images/levens.png" "Images/levens-mask.png"))
    (define levens-tekst-tegel (make-tile *algemeen-tekst-breedte* *algemeen-tekst-hoogte*))
    
    ((geld-tegel 'set-x!) *start-data-menu*) ;; Bitmap verplaatsen
    ((geld-tegel 'set-y!) 575) ;; Verander naar constante
    ((laag-geld&&levens 'add-drawable!) geld-tegel)
    
    ((geld-tekst-tegel 'draw-text!) (number->string *geld-begin-bedrag*) *tekst-font* 0 0 "white") ;; Tekst naast bitmap
    ((geld-tekst-tegel 'set-x!) (+ *start-data-menu* *px-breedte*)) 
    ((geld-tekst-tegel 'set-y!) 577) ;; Verander naar constante
    ((laag-geld&&levens 'add-drawable!) geld-tekst-tegel)

    ((levens-tegel 'set-x!) (+ *start-data-menu* (* 4 *px-breedte*))) ;; Bitmap verplaatsen
    ((levens-tegel 'set-y!) 575) ;; Verander naar constante
    ((laag-geld&&levens 'add-drawable!) levens-tegel)

    ((levens-tekst-tegel 'draw-text!) (number->string *levens-hoeveelheid*) *tekst-font* 0 0 "white") ;; Tekst naast bitmap
    ((levens-tekst-tegel 'set-x!) (+ *start-data-menu* (* 5 *px-breedte*)))
    ((levens-tekst-tegel 'set-y!) 577) ;; Verander naar constante
    ((laag-geld&&levens 'add-drawable!) levens-tekst-tegel)
      
    ;; Laag waarop pad getekent word
    (define laag-pad ((venster 'new-layer!)))

    ;; Volgende code is om de tekst delen van het spel up te daten
    (define (update-tekst-teken! object)
      (define (update-tekst-hulp! tegel)
        ((tegel 'clear!))
        ((tegel 'draw-text!) (number->string (object 'status)) *tekst-font* 0 0 "white"))
        
      (cond
        ((eq? 'geld (object 'soort)) (update-tekst-hulp! geld-tekst-tegel))
        ((eq? 'levens (object 'soort)) (update-tekst-hulp! levens-tekst-tegel))
        (else
         "Ongeldig object ingegeven")))

    ;; Procedure die tegel op juiste pixel positie zet (vanaf hiet beginnen de procedures voor de spelelementen)
    ;;met positie gedaan (niet object als formele parameter) want pad geeft meerdere posities, code kan enkel 1 positie per keer doen (zo hebben we maar 1 procedure voor alle px posities te bepalen)
    (define (bepaal-tegel-px-positie! positie tegel)
      (let ((scherm-x (* (positie 'x) *px-breedte*))
            (scherm-y (* (positie 'y) *px-hoogte*)))
        ((tegel 'set-x!) scherm-x)
        ((tegel 'set-y!) scherm-y)))

    ;; Maakt tegel en zet tegel op juiste plaats op laag  
    (define (teken-object-scherm! positie object-bitmap object-mask object-laag) 
      (let ((tegel-van-object (make-bitmap-tile object-bitmap object-mask)))
        (bepaal-tegel-px-positie! positie tegel-van-object)
        ((object-laag 'add-drawable!) tegel-van-object)
        tegel-van-object)) ;; Tegel teruggeven want later nodig om dictionary van monsters-tegel of projectielen-tegel te maken
    
    ;; Pakt elke pad positie en maakt een tegel en zet die op juiste plaats  
    (define (teken-pad! pad)
      (let ((pad-posities (pad 'posities))
            (lengte-pad (pad 'lengte)))
        (define (hulp-teken-pad! ctr)
          (if (not (= ctr lengte-pad))
              (begin
                (teken-object-scherm! (vector-ref pad-posities ctr) "Images/lava.jpeg" "Images/Lava-mask.png" laag-pad)
                (hulp-teken-pad! (+ ctr 1)))))
        (hulp-teken-pad! 0)))

    ;; Volgende code is om de correctie bitmap te verkrijgen afhankelijk van het type van het object (voor algemeenheid van sommige code)
    (define (bitmap-type object object-type)
      (cond
        ((eq? object 'toren)
         (cond
           ((eq? object-type 'basis) *basis-toren-bitmap&&mask*)))
        ((eq? object 'monster)
         (cond
           ((eq? object-type 'rood) *rood-monster-bitmap&&mask*)))
        ((eq? object 'projectiel)
         (cond
           ((eq? object-type 'steen) *steen-projectiel-bitmap&&mask*)))))

    ;; Volgende code is een venster om torens op te plaatsen
    (define laag-toren ((venster 'new-layer!)))

    ;; Tekent toren op het scherm gegeven een toren
    (define (teken-toren! toren)
      (let ((toren-positie (toren 'positie)))
        (let ((bitmap-adressen (bitmap-type (toren 'soort) 'basis)))
          (teken-object-scherm! (maak-positie-adt (- (toren-positie 'x) 1) (- (toren-positie 'y) 1)) (bitmap bitmap-adressen) (mask bitmap-adressen) laag-toren)))) ;; nieuwe positie om toren te centreren

    ;; Volgende code zijn abstracties om met dictionaries te werken (hier gezet want enkel hier gebruikt)
    (define (associatie dict)
      (car dict))

    (define (rest-dict dict)
      (cdr dict))

    (define (sleutel associatie)
      (car associatie))

    (define (waarde associatie)
      (cdr associatie))

    ;; Volgende code steekt een associatie in de dictionary
    (define (insert! sleut value tagged-dict)
      (let ((toe-te-voegen (list (cons sleut value))))
        (set-cdr! toe-te-voegen (rest-dict tagged-dict))
        (set-cdr! tagged-dict toe-te-voegen)))   

    ;; Volgende code delete een bepaalde sleutel uit de dictionary
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

    ;; Volgende code is om tiles weg te halen van het scherm die niet meer nodig zijn
    (define (haal-weg-tiles-dict! objecten diction diction-te-verwijderen laag) ;; Zit het in de dictionary maar niet in de lijst van objecten dan moet hij weg
      (if (null? diction)
          #f
          (let ((te-zoeken (sleutel (associatie diction))))
            (if (not (memq te-zoeken objecten))
                (begin
                  ((laag 'remove-drawable!) (waarde (associatie diction)))
                  (delete! te-zoeken diction-te-verwijderen))
                (haal-weg-tiles-dict! objecten (rest-dict diction) diction-te-verwijderen laag)))))

    ;; Volgende code is om tiles op het scherm te voegen die er nog niet op stonden
    (define (voeg-toe-tiles-dict! huidige-object diction-toevoegen laag) ;; Zit het in de lijst van objecten maar niet in de dictionary dan moet je tiles bijvoegen
      (if (null? huidige-object)
          #f
          (let ((object (car huidige-object)))
            (if (not (assq object (rest-dict diction-toevoegen)))
                (let ((bitmap-adressen (bitmap-type (object 'soort) (object 'type))))
                  (insert! object (teken-object-scherm! (object 'positie) (bitmap bitmap-adressen) (mask bitmap-adressen) laag) diction-toevoegen))
                (voeg-toe-tiles-dict! (cdr huidige-object) diction-toevoegen laag)))))
    
    ;; Volgende code is om dynamische objecten te tekenen (objecten waarvan ze moeten tevoorschijn komen, een positie bereiken en dan verdwijnen)
    (define (teken-dynamisch-object! objecten tiles laag)
      (haal-weg-tiles-dict! objecten (rest-dict tiles) tiles laag) 
      (for-each ;; Gaat elke tile van objecte  updaten 
       (lambda (ass) 
         (bepaal-tegel-px-positie! ((sleutel ass) 'positie) (waarde ass))) 
       (rest-dict tiles))
      (voeg-toe-tiles-dict! objecten tiles laag))
    
    ;; Volgende code is een venster om monsters te plaatsen
    (define laag-monster ((venster 'new-layer!)))

    ;; Volgende code is om monsters te tekenen
    (define (teken-monsters! monsters)
      (teken-dynamisch-object! monsters monster-tiles-dict laag-monster))

    ;; Volgende code is een venster om projectielen te plaatsen
    (define laag-projectiel ((venster 'new-layer!)))

    ;; Volgende code is om projectielen te tekenen
    (define (teken-projectielen! projectielen)
      (teken-dynamisch-object! projectielen projectielen-tiles-dict laag-projectiel))
           
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
        ((eq? msg 'teken-pad!) teken-pad!)
        ((eq? msg 'teken-toren!) teken-toren!)
        ((eq? msg 'teken-monsters!) teken-monsters!)
        ((eq? msg 'teken-projectielen!) teken-projectielen!)
        ((eq? msg 'set-muis-toets!) set-muis-toets-procedure!)
        ((eq? msg 'set-spel-lus!) set-spel-lus-procedure!)
        ((eq? msg 'set-toets-procedure!) set-toets-procedure!)
        ((eq? msg 'update-tekst-teken!) update-tekst-teken!)
        (else "maak-teken-adt: ongeldig bericht")))
    dispatch))