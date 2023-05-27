;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                 Teken ADT                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Doel van dit ADT is om alles gemakkelijk te teken gebruikmakend van de grafische bibliotheek (dit zal gebruikt worden door spel ADT)
(define (maak-teken-adt horizontale-pixels verticale-pixels)
  (let ((venster (make-window horizontale-pixels verticale-pixels "Tower Defence"))
        (pad-tegels '())
        (toren-tegels '())
        (monster-tiles-dict (cons 'tegels '())) ;; Tagged omdat 1ste conscell veranderd moet worden/ Dit zijn monster--tegel associaties
        (projectielen-tiles-dict (cons 'tegels '())) ;; Dit zijn projectiel--tegel associaties
        (tank-power-ups-tiles-dict (cons 'tegels '())) ;; Dit zijn tank-power-up--tegel associaties
        (bommen-regen-power-ups-tiles '())
        (gedropte-power-ups-tiles-dict (cons 'tegel '()))) ;; Dit zijn gedropte-power-up--tegel associaties

    ;; Volgende code is om een achtergrond te hebben waarop een pad gemaakt wordt
    (define laag-achtergrond ((venster 'new-layer!)))
    (define achtergrond-tegel (make-bitmap-tile "Images/Lava-Achtergrond.png"))
    ((laag-achtergrond 'add-drawable!) achtergrond-tegel)

    ;; Volgende code is om een menu te maken 
    (define laag-menu ((venster 'new-layer!)))
    (define menu-tegel (make-tile *menu-breedte-px* *spel/menu-hoogte-px*))
    (define boord-tegel (make-bitmap-tile "Images/boord.png")) ;; Voor stijlvoller Menu
    ((menu-tegel 'draw-rectangle!) 0 0 *menu-breedte-px* *spel/menu-hoogte-px* "black")
    (define GUI (make-bitmap-tile "Images/Text.png"))
    ((menu-tegel 'set-x!) *spel-breedte-px*)
    ((GUI 'set-x!) (+ *spel-breedte-px* 10))
    ((boord-tegel 'set-x!) *spel-breedte-px*)
    ((laag-menu 'add-drawable!) GUI)
    ((laag-menu 'add-drawable!) boord-tegel)
    ((laag-menu 'add-drawable!) menu-tegel)

    ;; Volgende code is om de user-interface van de menu te maken
    (define laag-user-interface ((venster 'new-layer!)))
    (define toren-1-tegel (make-bitmap-tile "Images/Toren-1-Game.png" "Images/Toren-1-game-mask.png"))
    (define toren-2-tegel (make-bitmap-tile "Images/Toren-2-Game.png" "Images/Toren-2-game-mask.png"))
    (define toren-3-tegel (make-bitmap-tile "Images/Toren-3-Game.png" "Images/Toren-3-game-mask.png"))
    (define toren-4-tegel (make-bitmap-tile "Images/Toren-4-Game.png" "Images/Toren-4-game-mask.png"))
    (define tank-tegel (make-bitmap-tile "Images/Tank-knop.png"))
    (define bommen-regen-tegel (make-bitmap-tile "Images/Bommen-regen-knop.png"))
    (define tank-afkoel-tegel (make-bitmap-tile "Images/afkoeling.png"))
    (define bommen-regen-afkoel-tegel (make-bitmap-tile "Images/afkoeling.png"))
    ((toren-1-tegel 'set-x!) *start-data-menu*) 
    ((toren-1-tegel 'set-y!) *toren-1-knop-hoogte-start*)
    ((toren-2-tegel 'set-x!) *start-data-menu*) 
    ((toren-2-tegel 'set-y!) *toren-2-knop-hoogte-start*)
    ((toren-3-tegel 'set-x!) *start-data-menu*) 
    ((toren-3-tegel 'set-y!) *toren-3-knop-hoogte-start*)
    ((toren-4-tegel 'set-x!) *start-data-menu*) 
    ((toren-4-tegel 'set-y!) *toren-4-knop-hoogte-start*)
    ((tank-tegel 'set-x!) *start-data-menu-power-up*)
    ((tank-tegel 'set-y!) *tank-knop-hoogte-start*)
    ((tank-afkoel-tegel 'set-x!) *start-data-menu-power-up*)
    ((tank-afkoel-tegel 'set-y!) *tank-knop-hoogte-start*)
    ((bommen-regen-tegel 'set-x!) *start-data-menu-power-up*)
    ((bommen-regen-tegel 'set-y!) *bommen-regen-knop-hoogte-start*)
    ((bommen-regen-afkoel-tegel 'set-x!) *start-data-menu-power-up*)
    ((bommen-regen-afkoel-tegel 'set-y!) *bommen-regen-knop-hoogte-start*)
    ((laag-user-interface 'add-drawable!) toren-1-tegel)
    ((laag-user-interface 'add-drawable!) toren-2-tegel)
    ((laag-user-interface 'add-drawable!) toren-3-tegel)
    ((laag-user-interface 'add-drawable!) toren-4-tegel)
    ((laag-user-interface 'add-drawable!) tank-tegel)
    ((laag-user-interface 'add-drawable!) bommen-regen-tegel)

    ;; Volgende code is om tekst op user interface te plaatsen om
    (define toren-1-naam&&prijs-tegel (make-tile *toren-benaming/prijs-tekst-breedte* *algemeen-tekst-hoogte*))
    (define toren-2-naam&&prijs-tegel (make-tile *toren-benaming/prijs-tekst-breedte* *algemeen-tekst-hoogte*))
    (define toren-3-naam&&prijs-tegel (make-tile *toren-benaming/prijs-tekst-breedte* *algemeen-tekst-hoogte*))
    (define toren-4-naam&&prijs-tegel (make-tile *toren-benaming/prijs-tekst-breedte* *algemeen-tekst-hoogte*))
    (define tank-pu-naam&&prijs-tegel (make-tile *toren-benaming/prijs-tekst-breedte* *algemeen-tekst-hoogte*))
    (define bommen-regen-pu-naam&&prijs-tegel (make-tile *toren-benaming/prijs-tekst-breedte* *algemeen-tekst-hoogte*))

    ((toren-1-naam&&prijs-tegel 'draw-text!) (string-append "Basis (" (number->string *basis-toren-kost*) ")")  *tekst-font* 0 0 "orange") ;; Algemeen vandaar met append
    ((toren-1-naam&&prijs-tegel 'set-x!) *start-data-menu*)
    ((toren-1-naam&&prijs-tegel 'set-y!) *toren-1-knop-hoogte-einde*)
    ((laag-user-interface 'add-drawable!) toren-1-naam&&prijs-tegel)
    
    
    ((toren-2-naam&&prijs-tegel 'draw-text!) (string-append "Net (" (number->string *net-toren-kost*) ")")  *tekst-font* 0 0 "orange")
    ((toren-2-naam&&prijs-tegel 'set-x!) *start-data-menu*)
    ((toren-2-naam&&prijs-tegel 'set-y!) *toren-2-knop-hoogte-einde*)
    ((laag-user-interface 'add-drawable!) toren-2-naam&&prijs-tegel)
    
    ((toren-3-naam&&prijs-tegel 'draw-text!) (string-append "Vuurbal (" (number->string *vuurbal-toren-kost*) ")")  *tekst-font* 0 0 "orange")
    ((toren-3-naam&&prijs-tegel 'set-x!) *start-data-menu*)
    ((toren-3-naam&&prijs-tegel 'set-y!) *toren-3-knop-hoogte-einde*)
    ((laag-user-interface 'add-drawable!) toren-3-naam&&prijs-tegel)
    
    ((toren-4-naam&&prijs-tegel 'draw-text!) (string-append "Bomwerp (" (number->string *bomwerp-toren-kost*) ")")  *tekst-font* 0 0 "orange")
    ((toren-4-naam&&prijs-tegel 'set-x!) *start-data-menu*)
    ((toren-4-naam&&prijs-tegel 'set-y!) *toren-4-knop-hoogte-einde*)
    ((laag-user-interface 'add-drawable!) toren-4-naam&&prijs-tegel)

    ((tank-pu-naam&&prijs-tegel 'draw-text!) (string-append "Tank (" (number->string *tank-kost*) ")")  *tekst-font* 0 0 "orange") ;; Algemeen vandaar met append
    ((tank-pu-naam&&prijs-tegel 'set-x!) *start-data-menu-power-up*)
    ((tank-pu-naam&&prijs-tegel 'set-y!) *tank-knop-hoogte-einde*)
    ((laag-user-interface 'add-drawable!) tank-pu-naam&&prijs-tegel)
    
    ((bommen-regen-pu-naam&&prijs-tegel 'draw-text!) (string-append "Bom (" (number->string *bommen-regen-kost*) ")")  *tekst-font* 0 0 "orange") ;; Algemeen vandaar met append
    ((bommen-regen-pu-naam&&prijs-tegel 'set-x!) *start-data-menu-power-up*)
    ((bommen-regen-pu-naam&&prijs-tegel 'set-y!) *bommen-regen-knop-hoogte-einde*)
    ((laag-user-interface 'add-drawable!) bommen-regen-pu-naam&&prijs-tegel)

    ;; Volgende is om het geld en de levens van de speler voor te stellen.
    (define laag-geld&&levens&&level ((venster 'new-layer!)))
    
    (define geld-tegel (make-bitmap-tile "Images/geld.png" "Images/geld-mask.png"))
    (define geld-tekst-tegel (make-tile *algemeen-tekst-breedte* *algemeen-tekst-hoogte*)) ;; Waarop de hoeveelheid zal staan

    (define levens-tegel (make-bitmap-tile "Images/levens.png" "Images/levens-mask.png"))
    (define levens-tekst-tegel (make-tile *algemeen-tekst-breedte* *algemeen-tekst-hoogte*))

    (define tank-tegel-data (make-bitmap-tile "Images/Tank-mini.png"))
    (define tank-tekst-data-tegel (make-tile *algemeen-tekst-breedte* *algemeen-tekst-hoogte*))

    (define bommen-regen-tegel-data (make-bitmap-tile "Images/Bommen-regen-mini.png"))
    (define bommen-regen-tekst-data-tegel (make-tile *algemeen-tekst-breedte* *algemeen-tekst-hoogte*))

    (define afkoeling-tegel-statisch (make-tile *algemeen-tekst-breedte* *algemeen-tekst-hoogte*))
    (define afkoeling-tegel-dynamisch (make-tile *algemeen-tekst-breedte* *algemeen-tekst-hoogte*))

    (define tijd-actief-tegel-statisch (make-tile *algemeen-tekst-breedte* *algemeen-tekst-hoogte*))
    (define tijd-actief-tegel-dynamisch (make-tile *algemeen-tekst-breedte* *algemeen-tekst-hoogte*))
   
    (define level-tekst-tegel-statisch (make-tile *algemeen-tekst-breedte* *algemeen-tekst-hoogte*))
    (define level-tekst-tegel-dynamisch (make-tile *algemeen-tekst-breedte* *algemeen-tekst-hoogte*))

    (define ronde-tekst-tegel-dynamisch (make-tile *algemeen-tekst-breedte* *algemeen-tekst-hoogte*))
      
    ((geld-tegel 'set-x!) *start-data-menu*) ;; Bitmap verplaatsen naar juiste plaats
    ((geld-tegel 'set-y!) *geld&&levens-tegel-px-hoogte*) 
    ((laag-geld&&levens&&level 'add-drawable!) geld-tegel)
    
    ((geld-tekst-tegel 'draw-text!) (number->string *geld-begin-bedrag*) *tekst-font* 0 0 "orange") ;; Tekst naast bitmap
    ((geld-tekst-tegel 'set-x!) (+ *start-data-menu* *px-breedte*)) 
    ((geld-tekst-tegel 'set-y!) *tekst-geld&&levens-px-hoogte*) 
    ((laag-geld&&levens&&level 'add-drawable!) geld-tekst-tegel)

    ((levens-tegel 'set-x!) (+ *start-data-menu* (* 4 *px-breedte*))) ;; Bitmap verplaatsen naar juiste plaats
    ((levens-tegel 'set-y!) *geld&&levens-tegel-px-hoogte*) 
    ((laag-geld&&levens&&level 'add-drawable!) levens-tegel)

    ((levens-tekst-tegel 'draw-text!) (number->string *levens-hoeveelheid*) *tekst-font* 0 0 "orange") ;; Tekst naast bitmap
    ((levens-tekst-tegel 'set-x!) *tekst-levens-px-breedte*)
    ((levens-tekst-tegel 'set-y!) *tekst-geld&&levens-px-hoogte*) 
    ((laag-geld&&levens&&level 'add-drawable!) levens-tekst-tegel)

    ((tank-tegel-data 'set-x!) *start-data-menu*)
    ((tank-tegel-data 'set-y!) (- *geld&&levens-tegel-px-hoogte* (* *px-hoogte* 2)))
    ((laag-geld&&levens&&level 'add-drawable!) tank-tegel-data)

    ((tank-tekst-data-tegel 'draw-text!) "0" *tekst-font* 0 0 "orange") ;; Tekst naast bitmap
    ((tank-tekst-data-tegel 'set-x!) (+ *start-data-menu* (* 1.2 *px-breedte*)))
    ((tank-tekst-data-tegel 'set-y!) (- *geld&&levens-tegel-px-hoogte* (* *px-hoogte* 2))) 
    ((laag-geld&&levens&&level 'add-drawable!) tank-tekst-data-tegel)    

    ((bommen-regen-tegel-data 'set-x!) (+ *start-data-menu* (* 4 *px-breedte*)))
    ((bommen-regen-tegel-data 'set-y!) (- *geld&&levens-tegel-px-hoogte* (* *px-hoogte* 2)))
    ((laag-geld&&levens&&level 'add-drawable!) bommen-regen-tegel-data)

    ((bommen-regen-tekst-data-tegel 'draw-text!) "0" *tekst-font* 0 0 "orange") ;; Tekst naast bitmap
    ((bommen-regen-tekst-data-tegel 'set-x!) (+ *start-data-menu* (* 5.2 *px-breedte*)))
    ((bommen-regen-tekst-data-tegel 'set-y!) (- *geld&&levens-tegel-px-hoogte* (* *px-hoogte* 2))) 
    ((laag-geld&&levens&&level 'add-drawable!) bommen-regen-tekst-data-tegel)

    ((afkoeling-tegel-statisch 'draw-text!) "Afkoel" *tekst-font* 0 0 "orange")
    ((afkoeling-tegel-statisch 'set-x!) *start-data-menu*)
    ((afkoeling-tegel-statisch 'set-y!) (- *geld&&levens-tegel-px-hoogte* (* *px-hoogte* 3.2))) 
    ((laag-geld&&levens&&level 'add-drawable!) afkoeling-tegel-statisch)

    ((afkoeling-tegel-dynamisch 'draw-text!) "0" *tekst-font* 0 0 "orange")
    ((afkoeling-tegel-dynamisch 'set-x!) (- 1000 100))
    ((afkoeling-tegel-dynamisch 'set-y!) (- *geld&&levens-tegel-px-hoogte* (* *px-hoogte* 3.2))) 
    ((laag-geld&&levens&&level 'add-drawable!) afkoeling-tegel-dynamisch)

    ((tijd-actief-tegel-statisch 'draw-text!) "Tijd" *tekst-font* 0 0 "orange")
    ((tijd-actief-tegel-statisch 'set-x!) *start-data-menu*)
    ((tijd-actief-tegel-statisch 'set-y!) (- *geld&&levens-tegel-px-hoogte* (* *px-hoogte* 4.2))) 
    ((laag-geld&&levens&&level 'add-drawable!) tijd-actief-tegel-statisch)

    ((tijd-actief-tegel-dynamisch 'draw-text!) "0" *tekst-font* 0 0 "orange")
    ((tijd-actief-tegel-dynamisch 'set-x!) (- 1000 100))
    ((tijd-actief-tegel-dynamisch 'set-y!) (- *geld&&levens-tegel-px-hoogte* (* *px-hoogte* 4.2))) 
    ((laag-geld&&levens&&level 'add-drawable!) tijd-actief-tegel-dynamisch)

    (define level/ronde-tekst-hoogte (- verticale-pixels *algemeen-tekst-hoogte*)) ;; Hoogte voor level-tekst (kan niet globaal)

    ((level-tekst-tegel-statisch 'draw-text!) "Level" *tekst-font* 0 0 "orange")
    ((level-tekst-tegel-statisch 'set-y!) level/ronde-tekst-hoogte) 
    ((laag-geld&&levens&&level 'add-drawable!) level-tekst-tegel-statisch)
      
    ((level-tekst-tegel-dynamisch 'draw-text!) "1" *tekst-font* 0 0 "orange")
    ((level-tekst-tegel-dynamisch 'set-x!) *dynamisch-tekst-level-begin*) 
    ((level-tekst-tegel-dynamisch 'set-y!) level/ronde-tekst-hoogte)
    ((laag-geld&&levens&&level 'add-drawable!) level-tekst-tegel-dynamisch)

    ((ronde-tekst-tegel-dynamisch 'draw-text!) "1" *tekst-font* 0 0 "orange")
    ((ronde-tekst-tegel-dynamisch 'set-x!) *dynamisch-tekst-ronde-begin*)
    ((ronde-tekst-tegel-dynamisch 'set-y!) level/ronde-tekst-hoogte)
    ((laag-geld&&levens&&level 'add-drawable!) ronde-tekst-tegel-dynamisch)   
    
    ;; Laag waarop pad getekent word
    (define laag-pad ((venster 'new-layer!)))

    ;; Volgende code is om de tekst delen van het spel up te daten
    (define (update-tekst-teken! type getal) ;; Dit werd gedaan omdat het level object zijn level niet bijhoud
      (define (update-tekst-hulp! tegel)
        ((tegel 'clear!))
        ((tegel 'draw-text!) (number->string getal) *tekst-font* 0 0 "orange"))
        
      (cond
        ((eq? type 'geld) (update-tekst-hulp! geld-tekst-tegel))
        ((eq? type 'levens) (update-tekst-hulp! levens-tekst-tegel))
        ((eq? type 'tank) (update-tekst-hulp! tank-tekst-data-tegel))
        ((eq? type 'bommen-regen) (update-tekst-hulp! bommen-regen-tekst-data-tegel))
        ((eq? type 'level) (update-tekst-hulp! level-tekst-tegel-dynamisch))
        ((eq? type 'ronde) (update-tekst-hulp! ronde-tekst-tegel-dynamisch))
        ((eq? type 'afkoel) (update-tekst-hulp! afkoeling-tegel-dynamisch))
        ((eq? type 'actief-tijd) (update-tekst-hulp! tijd-actief-tegel-dynamisch))
        (else
         "Update-tekst-teken!: Ongeldig object ingegeven")))

    ;; Procedure die tegel op juiste pixel positie zet (vanaf hiet beginnen de procedures voor de spelelementen)
    ;;met positie gedaan (niet object als formele parameter) want pad geeft meerdere posities, code kan enkel 1 positie per keer doen (zo hebben we maar 1 procedure voor alle px posities te bepalen)
    (define (bepaal-tegel-px-positie! positie tegel tank?) ;; Tank? gaat na als het een tank object is en verschuift het tank bitmap voor mooiheid
      (let* ((x-pos (positie 'x))
             (y-pos (if tank? (- (positie 'y) 1) (positie 'y))) ;; Om netter op scherm te zetten
             (scherm-x (* x-pos *px-breedte*))
             (scherm-y (* y-pos *px-hoogte*)))
        ((tegel 'set-x!) scherm-x)
        ((tegel 'set-y!) scherm-y)))

    ;; Maakt tegel en zet tegel op juiste plaats op laag  
    (define (teken-object-scherm! positie object-bitmap object-mask object-laag tank?) 
      (let ((tegel-van-object (make-bitmap-tile object-bitmap object-mask)))
        (bepaal-tegel-px-positie! positie tegel-van-object tank?)
        ((object-laag 'add-drawable!) tegel-van-object)
        tegel-van-object)) ;; Tegel teruggeven want later nodig om dictionary van monsters-tegel of projectielen-tegel te maken
    
    ;; Pakt elke pad positie en maakt een tegel en zet die op juiste plaats  
    (define (teken-pad! pad)
      (let ((pad-posities (pad 'posities))
            (lengte-pad (pad 'lengte)))
        (define (hulp-teken-pad! ctr)
          (if (not (= ctr lengte-pad))
              (let ((tegel (teken-object-scherm! (vector-ref pad-posities ctr) "Images/lava.jpeg" "Images/Lava-mask.png" laag-pad #f)))
                (set! pad-tegels (cons tegel pad-tegels))
                (hulp-teken-pad! (+ ctr 1)))))
        (hulp-teken-pad! 0)))

    ;; Volgende code zal een pad weghalen van het scherm
    (define (verwijder-pad!)
      (for-each (lambda (tegel)
                  ((laag-pad 'remove-drawable!) tegel))
                pad-tegels)
      (set! toren-tegels '()))

    ;; Volgende code is een venster om bommen-regen-power-ups te plaatsen
    (define laag-bommen-regen-pu ((venster 'new-layer!)))

    ;; Volgende code is abstractie
    (define neem-power-up car)
    (define (neem-positie-lbh bom) (vector-ref bom 2)) ;; Positie linker boven hoek

    ;; Volgende code is om bommen-regen-power-ups te tekenen
    (define (teken-bommen-regen-power-up! bommen-regen-power-up)
      (let ((bommen-lijst ((neem-power-up bommen-regen-power-up) 'bommen)))
        (for-each (lambda (bom)
                    (let* ((aan-te-passen-positie (neem-positie-lbh bom))
                           (teken-positie (maak-positie-adt (+ (aan-te-passen-positie 'x) 2) (+ (aan-te-passen-positie 'y) 2)))) ;; Om mooier te tekenen op het scherm
                      (set! bommen-regen-power-ups-tiles (cons (teken-object-scherm! teken-positie "Images/bomwerp.png" "Images/bomwerp-mask.png" laag-bommen-regen-pu #f)
                                                               bommen-regen-power-ups-tiles))))
                  bommen-lijst)))

    ;; Volgende code haalt de bommen weg van het scherm
    (define (verwijder-bommen!)
      (for-each (lambda (tile)
                  ((laag-bommen-regen-pu 'remove-drawable!) tile))
                bommen-regen-power-ups-tiles)
      (set! bommen-regen-power-ups-tiles '()))
                      
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
           ((eq? object-type 'tank) *tank-power-up-bitmap&&mask*)))
        ((eq? object 'drop-power-up)
         (cond
           ((eq? object-type 'tank) *drop-tank-bitmap&&mask*)
           ((eq? object-type 'bommen-regen) *drop-bommen-regen-bitmap&&mask*)))
        (else
         "Ongeldig object")))

    ;; Volgende code is een venster om torens op te plaatsen
    (define laag-toren ((venster 'new-layer!)))

    ;; Tekent toren op het scherm gegeven een toren
    (define (teken-toren! toren)
      (let ((toren-positie (toren 'positie)))
        (let* ((bitmap-adressen (bitmap-type (toren 'soort) (toren 'type)))
               (tegel (teken-object-scherm! (maak-positie-adt (- (toren-positie 'x) 1) (- (toren-positie 'y) 1)) (bitmap bitmap-adressen) (mask bitmap-adressen) laag-toren #f))) ;; nieuwe positie om toren te centreren
          (set! toren-tegels (cons tegel toren-tegels)))))

    ;; volgende neemt de torens weg van het scherm
    (define (verwijder-torens!)
      (for-each (lambda (tegel)
                  ((laag-toren 'remove-drawable!) tegel))
                toren-tegels)
      (set! toren-tegels '()))

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

    ;; Volgende code gaat na welke power-up geselecteerd werd van de menu:
    (define (power-up-selectie x y)
      (cond
        ((and (>= x *start-data-menu-power-up*) (<= x *power-up-knop-breedte-einde*) (>= y *tank-knop-hoogte-start*) (<= y *tank-knop-hoogte-einde*))
         'tank)
        ((and (>= x *start-data-menu-power-up*) (<= x *power-up-knop-breedte-einde*) (>= y *bommen-regen-knop-hoogte-start*) (<= y *bommen-regen-knop-hoogte-einde*))
         'bommen-regen)
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
    (define (voeg-toe-tiles-dict! huidige-object diction-toevoegen laag tank?) ;; Zit het in de lijst van objecten maar niet in de dictionary dan moet je tiles bijvoegen
      (if (null? huidige-object)
          #f
          (let ((object (car huidige-object)))
            (if (not (assq object (rest-dict diction-toevoegen)))
                (let* ((obj-soort (object 'soort))
                       (bitmap-adressen (bitmap-type obj-soort (object 'type))))
                  (insert! object (teken-object-scherm! (if (eq? obj-soort 'drop-power-up) (object 'drop-positie) (object 'positie)) (bitmap bitmap-adressen) (mask bitmap-adressen) laag tank?) diction-toevoegen))
                (voeg-toe-tiles-dict! (cdr huidige-object) diction-toevoegen laag tank?)))))
    
    ;; Volgende code is om dynamische objecten te tekenen (objecten waarvan ze moeten tevoorschijn komen, een positie bereiken en dan verdwijnen)
    (define (teken-dynamisch-object! objecten tiles laag tank?)
      (haal-weg-tiles-dict! objecten (rest-dict tiles) tiles laag) 
      (for-each ;; Gaat elke tile van objecte updaten 
       (lambda (ass)
         (let ((sleut (sleutel ass)))
           (bepaal-tegel-px-positie! (if (eq? (sleut 'soort) 'drop-power-up) (sleut 'drop-positie) (sleut 'positie)) (waarde ass) tank?))) 
       (rest-dict tiles))
      (voeg-toe-tiles-dict! objecten tiles laag tank?)) 
    
    ;; Volgende code is een venster om monsters te plaatsen
    (define laag-monster ((venster 'new-layer!)))

    ;; Volgende code is om monsters te tekenen
    (define (teken-monsters! monsters)
      (teken-dynamisch-object! monsters monster-tiles-dict laag-monster #f))

    ;; Volgende code is een venster om projectielen te plaatsen
    (define laag-projectiel ((venster 'new-layer!)))

    ;; Volgende code is om projectielen te tekenen
    (define (teken-projectielen! projectielen)
      (teken-dynamisch-object! projectielen projectielen-tiles-dict laag-projectiel #f))

    ;; Volgende code is een venster om tank-power-ups te plaatsen
    (define laag-tank-pu ((venster 'new-layer!)))

    ;; Volgende code is om tank-power-ups te tekenen
    (define (teken-tank-power-up! tank-power-ups)
      (teken-dynamisch-object! tank-power-ups tank-power-ups-tiles-dict laag-tank-pu #t))

    ;; Volgende code is om gedropte power-ups te tekenen
    (define laag-drops ((venster 'new-layer!)))

    ;; Volgende code is om de gredopte power-ups te tekenen
    (define (teken-gedropte-power-ups! gedropte-power-ups)
      (teken-dynamisch-object! gedropte-power-ups gedropte-power-ups-tiles-dict laag-drops #f))

    ;; Volgende code verwijdert gedropte-power-ups van het scherm
    (define (verwijder-gedropte-power-ups!)
      (for-each (lambda (ass)
                  ((laag-drops 'remove-drawable!) (waarde ass)))
                (rest-dict gedropte-power-ups-tiles-dict))
      (set-cdr! gedropte-power-ups-tiles-dict '()))

    ;; Volgende code is om een afkoeling te tekenen
    (define (teken-afkoeling-acties! actie)
      (cond
        ((eq? actie 'toevoegen)
         ((laag-user-interface 'add-drawable!) tank-afkoel-tegel)
         ((laag-user-interface 'add-drawable!) bommen-regen-afkoel-tegel)
         ((laag-user-interface 'remove-drawable!) tank-tegel)
         ((laag-user-interface 'remove-drawable!) bommen-regen-tegel))
        ((eq? actie 'verwijderen)
         ((laag-user-interface 'remove-drawable!) tank-afkoel-tegel)
         ((laag-user-interface 'remove-drawable!) bommen-regen-afkoel-tegel)
         ((laag-user-interface 'add-drawable!) tank-tegel)
         ((laag-user-interface 'add-drawable!) bommen-regen-tegel))
        (else
         "Ongeldige actie")))

    ;; Vogende code stelt startscherm en eindeschermen voor (game over, en win)
    (define laag-schermen ((venster 'new-layer!)))
    (define game-over-tegel (make-bitmap-tile "Images/game-over.jpeg"))
    (define begin-scherm-tegel (make-bitmap-tile "Images/begin-scherm.jpeg"))
    (define eind-scherm-tegel (make-bitmap-tile "Images/eind-scherm.jpg"))

    ;; Volgende code is om het game-over scherm te plaatsen
    (define (teken-game-over!)
      ((laag-schermen 'add-drawable!) game-over-tegel))

    ;; Volgende code is om het begin van het scherm te tekenen
    (define (teken-begin-scherm!)
      ((laag-schermen 'add-drawable!) begin-scherm-tegel))

    ;; Volgende code is om het eind van het scherm te tekenen
    (define (teken-eind-scherm!)
      ((laag-schermen 'add-drawable!) eind-scherm-tegel))

    ;; Volgende code is om het game-over scherm te verwijderen
    (define (verwijder-scherm!)
      ((laag-schermen 'empty!)))
                 
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
        ((eq? msg 'verwijder-pad!) verwijder-pad!)
        ((eq? msg 'teken-toren!) teken-toren!)
        ((eq? msg 'toren-selectie) toren-selectie)
        ((eq? msg 'verwijder-torens!) verwijder-torens!)
        ((eq? msg 'power-up-selectie) power-up-selectie)
        ((eq? msg 'teken-monsters!) teken-monsters!)
        ((eq? msg 'teken-projectielen!) teken-projectielen!)
        ((eq? msg 'teken-tank-power-up!) teken-tank-power-up!)
        ((eq? msg 'teken-bommen-regen-power-up!) teken-bommen-regen-power-up!)
        ((eq? msg 'teken-gedropte-power-ups!) teken-gedropte-power-ups!)
        ((eq? msg 'verwijder-gedropte-power-ups!) verwijder-gedropte-power-ups!)
        ((eq? msg 'verwijder-bommen!) verwijder-bommen!)
        ((eq? msg 'teken-afkoeling-acties!) teken-afkoeling-acties!)
        ((eq? msg 'teken-begin-scherm!) teken-begin-scherm!)
        ((eq? msg 'teken-eind-scherm!) teken-eind-scherm!)
        ((eq? msg 'teken-game-over!) teken-game-over!)
        ((eq? msg 'verwijder-scherm!) verwijder-scherm!)
        ((eq? msg 'set-muis-toets!) set-muis-toets-procedure!)
        ((eq? msg 'set-spel-lus!) set-spel-lus-procedure!)
        ((eq? msg 'set-toets-procedure!) set-toets-procedure!)
        ((eq? msg 'update-tekst-teken!) update-tekst-teken!)
        ((eq? msg 'buiten-beperking?) buiten-beperking?)
        (else "maak-teken-adt: ongeldig bericht")))
    dispatch))