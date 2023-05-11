;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                 Teken ADT                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Doel van dit ADT is om alles gemakkelijk te teken gebruikmakend van de grafische bibliotheek (dit zal gebruikt worden door spel ADT)
(define (maak-teken-adt horizontale-pixels verticale-pixels)
  (let ((venster (make-window horizontale-pixels verticale-pixels "Tower Defence"))
        (monster-tiles-dict (cons 'tegels '())) ;; Tagged omdat 1ste conscell veranderd moet worden/ Dit zijn monster--tegel associaties
        (projectielen-tiles-dict (cons 'tegels '())) ;; Dit zijn projectiel--tegel associaties
        (tank-power-ups-tiles-dict (cons 'tegels '()))) ;; Dit zijn tank-power-up--tegel associaties

    ;; Volgende code is om een achtergrond te hebben waarop een pad gemaakt wordt
    (define laag-achtergrond ((venster 'new-layer!)))
    (define achtergrond-tegel (make-bitmap-tile "Images/Lava-Achtergrond.png"))
    ((laag-achtergrond 'add-drawable!) achtergrond-tegel)

    ;; Volgende code is om een menu te maken !!!!(Proberen geld layer hier toe te voegen)!!!!
    (define laag-menu ((venster 'new-layer!)))
    (define menu-tegel (make-tile *menu-breedte-px* *spel/menu-hoogte-px*))
    ((menu-tegel 'draw-rectangle!) 0 0 *menu-breedte-px* *spel/menu-hoogte-px* "black")
    ((menu-tegel 'draw-rectangle!) 0 0 (/ *px-breedte* 2) *spel/menu-hoogte-px* "darkorange") ;; Voegt lijntje van om stijlvoller te maken
    (define GUI (make-bitmap-tile "Images/Text.png"))
    ((menu-tegel 'set-x!) *spel-breedte-px*)
    ((GUI 'set-x!) (+ *spel-breedte-px* 10))
    ((laag-menu 'add-drawable!) GUI)
    ((laag-menu 'add-drawable!) menu-tegel)

    ;; Volgende code is om de user-interface van de menu te maken
    (define laag-user-interface ((venster 'new-layer!)))
    (define toren-1-tegel (make-bitmap-tile "Images/Toren-1-Game.png" "Images/Toren-1-game-mask.png"))
    (define toren-2-tegel (make-bitmap-tile "Images/Toren-2-Game.png" "Images/Toren-2-game-mask.png"))
    (define toren-3-tegel (make-bitmap-tile "Images/Toren-3-Game.png" "Images/Toren-3-game-mask.png"))
    (define toren-4-tegel (make-bitmap-tile "Images/Toren-4-Game.png" "Images/Toren-4-game-mask.png"))
    (define power-up-1-tegel (make-bitmap-tile "Images/Tank-knop.png"))
    (define power-up-2-tegel (make-bitmap-tile "Images/Bomregen-knop.png"))
    ((toren-1-tegel 'set-x!) *start-data-menu*) 
    ((toren-1-tegel 'set-y!) *toren-1-knop-hoogte-start*)
    ((toren-2-tegel 'set-x!) *start-data-menu*) 
    ((toren-2-tegel 'set-y!) *toren-2-knop-hoogte-start*)
    ((toren-3-tegel 'set-x!) *start-data-menu*) 
    ((toren-3-tegel 'set-y!) *toren-3-knop-hoogte-start*)
    ((toren-4-tegel 'set-x!) *start-data-menu*) 
    ((toren-4-tegel 'set-y!) *toren-4-knop-hoogte-start*)
    ((power-up-1-tegel 'set-x!) *start-data-menu-power-up*)
    ((power-up-1-tegel 'set-y!) *power-up-1-knop-hoogte-start*)
    ((power-up-2-tegel 'set-x!) *start-data-menu-power-up*)
    ((power-up-2-tegel 'set-y!) *power-up-2-knop-hoogte-start*)
    ((laag-user-interface 'add-drawable!) toren-1-tegel)
    ((laag-user-interface 'add-drawable!) toren-2-tegel)
    ((laag-user-interface 'add-drawable!) toren-3-tegel)
    ((laag-user-interface 'add-drawable!) toren-4-tegel)
    ((laag-user-interface 'add-drawable!) power-up-1-tegel)
    ((laag-user-interface 'add-drawable!) power-up-2-tegel)    

    ;; Volgende is om het geld en de levens van de speler voor te stellen.
    (define laag-geld&&levens&&level ((venster 'new-layer!)))
    
    (define geld-tegel (make-bitmap-tile "Images/geld.png" "Images/geld-mask.png"))
    (define geld-tekst-tegel (make-tile *algemeen-tekst-breedte* *algemeen-tekst-hoogte*)) ;; Waarop de hoeveelheid zal staan

    (define levens-tegel (make-bitmap-tile "Images/levens.png" "Images/levens-mask.png"))
    (define levens-tekst-tegel (make-tile *algemeen-tekst-breedte* *algemeen-tekst-hoogte*))

    (define level-tekst-tegel-statisch (make-tile *algemeen-tekst-breedte* *algemeen-tekst-hoogte*))
    (define level-tekst-tegel-dynamisch (make-tile *algemeen-tekst-breedte* *algemeen-tekst-hoogte*))
      
    ((geld-tegel 'set-x!) *start-data-menu*) ;; Bitmap verplaatsen naar juiste plaats
    ((geld-tegel 'set-y!) *geld&&levens-tegel-px-hoogte*) 
    ((laag-geld&&levens&&level 'add-drawable!) geld-tegel)
    
    ((geld-tekst-tegel 'draw-text!) (number->string *geld-begin-bedrag*) *tekst-font* 0 0 "white") ;; Tekst naast bitmap
    ((geld-tekst-tegel 'set-x!) (+ *start-data-menu* *px-breedte*)) 
    ((geld-tekst-tegel 'set-y!) *tekst-geld&&levens-px-hoogte*) 
    ((laag-geld&&levens&&level 'add-drawable!) geld-tekst-tegel)

    ((levens-tegel 'set-x!) (+ *start-data-menu* (* 4 *px-breedte*))) ;; Bitmap verplaatsen naar juiste plaats
    ((levens-tegel 'set-y!) *geld&&levens-tegel-px-hoogte*) 
    ((laag-geld&&levens&&level 'add-drawable!) levens-tegel)

    ((levens-tekst-tegel 'draw-text!) (number->string *levens-hoeveelheid*) *tekst-font* 0 0 "white") ;; Tekst naast bitmap
    ((levens-tekst-tegel 'set-x!) *tekst-levens-px-breedte*)
    ((levens-tekst-tegel 'set-y!) *tekst-geld&&levens-px-hoogte*) 
    ((laag-geld&&levens&&level 'add-drawable!) levens-tekst-tegel)

    ((level-tekst-tegel-statisch 'draw-text!) "Level" *tekst-font* 0 0 "white")
    ((level-tekst-tegel-statisch 'set-y!) (- verticale-pixels *algemeen-tekst-hoogte*)) 
    ((laag-geld&&levens&&level 'add-drawable!) level-tekst-tegel-statisch)

    ((level-tekst-tegel-dynamisch 'draw-text!) "1" *tekst-font* 0 0 "white")
    ((level-tekst-tegel-dynamisch 'set-x!) *algemeen-tekst-breedte*)
    ((level-tekst-tegel-dynamisch 'set-y!) (- verticale-pixels *algemeen-tekst-hoogte*))
    ((laag-geld&&levens&&level 'add-drawable!) level-tekst-tegel-dynamisch)
    
    ;; Laag waarop pad getekent word
    (define laag-pad ((venster 'new-layer!)))

    ;; Volgende code is om de tekst delen van het spel up te daten
    (define (update-tekst-teken! object . level-tal) ;; Dit werd gedaan omdat het level object zijn level niet bijhoud
      (define (update-tekst-hulp! tegel)
        ((tegel 'clear!))
        ((tegel 'draw-text!) (number->string (object 'status)) *tekst-font* 0 0 "white"))
        
      (cond
        ((eq? 'geld (object 'soort)) (update-tekst-hulp! geld-tekst-tegel))
        ((eq? 'levens (object 'soort)) (update-tekst-hulp! levens-tekst-tegel))
        ;        ((eq? 'level (object 'soort)) (update-tekst-hulp! level-tekst-tegel-dynamisch))
        (else
         "Update-tekst-teken!: Ongeldig object ingegeven")))

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
           ((eq? object-type 'basis-toren) *basis-toren-bitmap&&mask*)
           ((eq? object-type 'net-toren) *net-toren-bitmap&&mask*)
           ((eq? object-type 'vuurbal-toren) *vuurbal-toren-bitmap&&mask*)
           ((eq? object-type 'bomwerp-toren) *bomwerp-toren-bitmap&&mask*)))
        ((eq? object 'monster)
         (cond
           ((eq? object-type 'rood) *rood-monster-bitmap&&mask*)
           ((eq? object-type 'groen) *groen-monster-bitmap&&mask*)
           ((eq? object-type 'geel) *geel-monster-bitmap&&mask*)
           ((eq? object-type 'paars) *paars-monster-bitmap&&mask*)))
        ((eq? object 'projectiel)
         (cond
           ((eq? object-type 'steen) *steen-projectiel-bitmap&&mask*)
           ((eq? object-type 'net) *net-projectiel-bitmap&&mask*)
           ((eq? object-type 'vuurbal) *vuurbal-projectiel-bitmap&&mask*)
           ((eq? object-type 'bomwerp) *bomwerp-projectiel-bitmap&&mask*)))
        ((eq? object 'power-up)
         (cond
           ((eq? object-type 'tank) *tank-power-up-bitmap&&mask*)))))

    ;; Volgende code is een venster om torens op te plaatsen
    (define laag-toren ((venster 'new-layer!)))

    ;; Tekent toren op het scherm gegeven een toren
    (define (teken-toren! toren)
      (let ((toren-positie (toren 'positie)))
        (let ((bitmap-adressen (bitmap-type (toren 'soort) (toren 'type))))
          (teken-object-scherm! (maak-positie-adt (- (toren-positie 'x) 1) (- (toren-positie 'y) 1)) (bitmap bitmap-adressen) (mask bitmap-adressen) laag-toren)))) ;; nieuwe positie om toren te centreren

    ;; Volgende code gaat na welke toren geselecteerd werd van de menu
    (define (toren-selectie x y)
      (cond
        ((and (>= x *start-data-menu*) (<= x *toren-knop-breedte-einde*) (>= y *toren-1-knop-hoogte-start*) (<= y *toren-1-knop-hoogte-einde*))
         'basis-toren)
        ((and (>= x *start-data-menu*) (<= x *toren-knop-breedte-einde*) (>= y *toren-2-knop-hoogte-start*) (<= y *toren-2-knop-hoogte-einde*))
         'net-toren)
        ((and (>= x *start-data-menu*) (<= x *toren-knop-breedte-einde*) (>= y *toren-3-knop-hoogte-start*) (<= y *toren-3-knop-hoogte-einde*))
         'vuurbal-toren)
        ((and (>= x *start-data-menu*) (<= x *toren-knop-breedte-einde*) (>= y *toren-4-knop-hoogte-start*) (<= y *toren-4-knop-hoogte-einde*))
         'bomwerp-toren)
        (else
         #f)))

    ;; Volgende code
    (define (power-up-selectie x y)
      (cond
        ((and (>= x *start-data-menu-power-up*) (<= x *power-up-knop-breedte-einde*) (>= y *power-up-1-knop-hoogte-start*) (<= y *power-up-1-knop-hoogte-einde*))
         'tank)
        ((and (>= x *start-data-menu-power-up*) (<= x *power-up-knop-breedte-einde*) (>= y *power-up-2-knop-hoogte-start*) (<= y *power-up-2-knop-hoogte-einde*))
         'bom-regen)
        (else #f)))
        
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
      (for-each ;; Gaat elke tile van objecte updaten 
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

    ;; Volgende code is een venster om tank-power-ups te plaatsen
    (define laag-tank-po ((venster 'new-layer!)))

    ;; Volgende code is om tank-power-ups te tekenen
    (define (teken-tank-power-up! tank-power-ups)
      (teken-dynamisch-object! tank-power-ups tank-power-ups-tiles-dict laag-tank-po))
           
    ;; Volgende code is om muis klikken te implementeren
    (define (set-muis-toets-procedure! proc)
      ((venster 'set-mouse-click-callback!) proc))

    ;; Volgende code is om een spellus te implementeren
    (define (set-spel-lus-procedure! proc)
      ((venster 'set-update-callback!) proc))

    ;; Volgende code is om een knop in te voegen
    (define (set-toets-procedure! proc)
      ((venster 'set-key-callback!) proc))

    ;; Volgende code is om na te gaan als object geplaatst worden bij een beperking
    (define (buiten-menu? x y)
      (<= x (- *start-x-pos-menu* (* 2 *px-breedte*))))

    (define (buiten-lava? x y)
      (and 
      (not (and (<= x *beperking-1-breedte*) (<= y *beperking-1-hoogte*))) 
      (not (and (>= x *beperking-1-breedte*) (<= x *beperking-2-breedte*) (<= y *beperking-2-hoogte*)))
      (not (and (>= x *beperking-2-breedte*) (<= x *beperking-3-breedte*) (<= y *beperking-3-hoogte*)))))
      
    (define (buiten-beperking? x y)
      (and (buiten-menu? x y)
           (buiten-lava? x y)))
              
      (define (dispatch msg)
        (cond
          ((eq? msg 'teken-pad!) teken-pad!)
          ((eq? msg 'teken-toren!) teken-toren!)
          ((eq? msg 'toren-selectie) toren-selectie)
          ((eq? msg 'power-up-selectie) power-up-selectie)
          ((eq? msg 'teken-monsters!) teken-monsters!)
          ((eq? msg 'teken-projectielen!) teken-projectielen!)
          ((eq? msg 'teken-tank-power-up!) teken-tank-power-up!)
          ((eq? msg 'set-muis-toets!) set-muis-toets-procedure!)
          ((eq? msg 'set-spel-lus!) set-spel-lus-procedure!)
          ((eq? msg 'set-toets-procedure!) set-toets-procedure!)
          ((eq? msg 'update-tekst-teken!) update-tekst-teken!)
          ((eq? msg 'buiten-beperking?) buiten-beperking?)
          (else "maak-teken-adt: ongeldig bericht")))
      dispatch))