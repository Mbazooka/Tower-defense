;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                 Constanten                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; De constant hieronder zullen de grootte van de pixels voorstellen
;; Voor het omzetten van spelposities naar tekenposities
(define *px-breedte* 20)
(define *px-hoogte* 20)

(define *spel-breedte* 40)
(define *spel-hoogte* 30)

(define *menu-breedte* 10)

;; Kaart beperkingen (contraints)
(define *beperking-1-breedte* 305) 
(define *beperking-1-hoogte* 255)

(define *beperking-2-breedte* 655) 
(define *beperking-2-hoogte* 170)

(define *beperking-3-breedte* 800) 
(define *beperking-3-hoogte* 220)

;; Constanten die de totale spelgrootte voorstellen (met verschillende componenten)
(define *menu-breedte-px* (* *menu-breedte* *px-breedte*))
(define *spel-breedte-px* (* *spel-breedte* *px-breedte*))
(define *spel/menu-hoogte-px* (* *spel-hoogte* *px-hoogte*))

;; Menu constante (stelt de x coordinaat voor die de start van de menu voorstelt, nodig voor torens te plaatsen)
(define *start-x-pos-menu* (* *spel-breedte* *px-breedte*)) 
(define *start-data-menu* (+ *spel-breedte-px* *px-breedte*))
(define *toren-knop-breedte-einde* (+ *spel-breedte-px* (* 4 *px-breedte*))) ;; zijn keuzes om knopen hier te starten en daar te eindigen
(define *toren-1-knop-hoogte-start* (* 2 *px-hoogte*))
(define *toren-1-knop-hoogte-einde* (* 5 *px-hoogte*))

(define *toren-2-knop-hoogte-start* (* 7 *px-hoogte*))
(define *toren-2-knop-hoogte-einde* (* 10 *px-hoogte*))

(define *toren-3-knop-hoogte-start* (* 12 *px-hoogte*))
(define *toren-3-knop-hoogte-einde* (* 15 *px-hoogte*))

(define *toren-4-knop-hoogte-start* (* 17 *px-hoogte*))
(define *toren-4-knop-hoogte-einde* (* 20 *px-hoogte*))

(define *start-data-menu-power-up* (+ *start-data-menu* (* 5 *px-breedte*)))
(define *power-up-knop-breedte-einde* (+ *start-data-menu-power-up* (* 5 *px-hoogte*)))

(define *power-up-1-knop-hoogte-start* (* 2.3 *px-hoogte*))
(define *power-up-1-knop-hoogte-einde* (* 5 *px-hoogte*))

(define *power-up-2-knop-hoogte-start* (* 7.3 *px-hoogte*))
(define *power-up-2-knop-hoogte-einde* (* 9 *px-hoogte*))

;; Tekst constanten voor op menu
(define *tekst-toren-breedte* (+ *px-breedte* 10)) ;; + 5 want oranje rand centreert de tekst niet
(define *tekst-toren-hoogte* *px-hoogte*)
(define *tekst-font* 13)
(define *algemeen-tekst-breedte* (* 2 *px-breedte*))
(define *algemeen-tekst-hoogte* *px-hoogte*)
(define *tekst-geld-px-breedte* (+ *start-data-menu* *px-breedte*))
(define *tekst-levens-px-breedte* (+ *start-data-menu* (* 5 *px-breedte*)))
(define *tekst-geld&&levens-px-hoogte* (- *spel/menu-hoogte-px* 23)) ;; 23 Zo gekozen voor stijlvolle manier
(define *dynamisch-tekst-level-begin* (* 2 *px-breedte*))
(define *dynamisch-tekst-ronde-begin* (* 3 *px-breedte*))

;; Abstracties om bitmap en mask te acesseren
(define bitmap car)
(define mask cdr)

;; Constanten voor bitmap van torens
(define *basis-toren-bitmap&&mask* (cons "Images/Toren-1-game.png" "Images/Toren-1-game-mask.png"))
(define *net-toren-bitmap&&mask* (cons "Images/Toren-2-game.png" "Images/Toren-2-game-mask.png"))
(define *vuurbal-toren-bitmap&&mask* (cons "Images/Toren-3-game.png" "Images/Toren-3-game-mask.png"))
(define *bomwerp-toren-bitmap&&mask* (cons "Images/Toren-4-game.png" "Images/Toren-4-game-mask.png"))

;; Constanten voor bitmap van monsters (geheugen vriendelijker)
(define *rood-monster-bitmap&&mask* (cons "Images/Rood-monster.jpg" "Images/Rood-monster-mask.png"))
(define *groen-monster-bitmap&&mask* (cons "Images/Groen-monster.png" "Images/Groen-monster-mask.png"))
(define *geel-monster-bitmap&&mask* (cons "Images/Geel-monster.png" "Images/Geel-monster-mask.png"))
(define *paars-monster-bitmap&&mask* (cons "Images/Paars-monster.png" "Images/Paars-monster-mask.png"))

;; Constanten voor bitmap van projectielen
(define *steen-projectiel-bitmap&&mask* (cons "Images/projectiel.png" "Images/projectiel-mask.png")) ;; Veranderen naam
(define *net-projectiel-bitmap&&mask* (cons "Images/Net.png" "Images/Net-mask.png"))
(define *vuurbal-projectiel-bitmap&&mask*  (cons "Images/Vuurbal.png" "Images/Vuurbal-mask.png"))
(define *bomwerp-projectiel-bitmap&&mask* (cons "Images/bomwerp.png" "Images/bomwerp-mask.png"))

;; Constanten voor bitmap van power-ups
(define *tank-power-up-bitmap&&mask* (cons "Images/Tank.png" "Images/tank-mask.png"))
(define *bommen-regen-bitmap&&mask* (cons "Images/bomwerp.png" "Images/bomwerp-mask.png"))

;; Constanten voor tegel-positie van bitmap van geld en levens
(define *geld&&levens-tegel-px-hoogte* (- *spel/menu-hoogte-px* 25)) ;; 23 Zo gekozen voor stijlvolle manier

;; Toren constanten (tijd en geen tijd constanten)
(define *toren-rand-afstand* 1.6) ;; Hier werd speling gebruikt om te zorgen bitmap niet op pad komt (zorgt voor meer afstand tussen torens en pad)
(define *basis-toren-buurt-rand-afstand* 10)
(define *net-toren-buurt-rand-afstand* 12)
(define *vuurbal-toren-buurt-rand-afstand* 8)
(define *bomwerp-toren-buurt-rand-afstand* 7)
(define *basis-toren-afvuur-frequentie* 3000) ;; Toren schiet frequentie (2000)
(define *net-toren-afvuur-frequentie* 6000) ;; Toren schiet frequentie voor net toren
(define *vuurbal-toren-afvuur-frequentie 3000)
(define *bomwerp-toren-afvuur-frequentie* 10000)
(define *meerdere-monsters* 2)

;; Projectiel constanten (tijd en geen tijd constanten)
(define *projectiel-afvuur-snelheid-steen-net-vuurbal* 0.1) ;; Hoe snel het steen projectiel afgevuurd word
(define *net-projectiel-vertaging* 0.05) ;; Vertraagt bepaald monster met 20% van zijn huidige snelheid
(define *net-blijf-liggen-tijd* 5000)
(define *net-vertraag-tijd* 5000)
(define *vuurbal-hits-snelheid-verander* 0.025) ;; Vertraging vuurbal-hits
(define *bomwerp-projectiel-ligtijd* 3000)
(define *net-projectiel-rand-afstand* 2)
(define *bomwerp-projectiel-rand-afstand* 4)

;; Monster tijd constanten
(define *monster-spawn-frequentie* 3000) ;; Monster spawn frequentie (te veranderen)
(define *rood&&groen&&paars-monster-loop-snelheid* 0.1) ;; Monster beweeg snelheid
(define *geel-monster-loop-snelheid* 0.05)
(define *monster-vetraag-tijd* 5000) ;; Hoelang een monster vertraagd word door een net

;; Power-up tijd en andere constanten
(define *tank-rijd-snelheid* 1)
(define *power-afkoel-tijd* 15000)
(define *bomregen-aftel-tijd 3000)
(define *bomregen-aantal-bommen* 5)
(define *bomregen-rand-afstand* 2.5)

;; Constanten voor het begin geld bedrag en het begin levens hoeveelheid
(define *geld-begin-bedrag* 5000) ;; Verander achteraf
(define *levens-hoeveelheid* 5)

;; Constanten die geld van speler doet veranderen
(define *basis-toren-kost* 500)
(define *net-toren-kost* 500)
(define *vuurbal-toren-kost* 1000)
(define *bomwerp-toren-kost* 1500)
(define *tank-kost* 700)
(define *bommen-regen-kost* 1000)
(define *rood-groen-monster-winst* 20)
(define *geel-monster-winst* 50)
(define *paars-monster-winst* 30)
(define *level-winst* 500)
(define *extra-tank-winst* 30)

;; Constanten die levens (en ook rand) van monsters voorstellen 
(define *levens-rood-monster* 1)
(define *levens-groen-monster* 2)
(define *levens-geel-monster* 2)
(define *schild-geel-monster* 3)
(define *levens-paars-monster* 4)
(define *paars-monster-rand-afstand* 4)
(define *dood* 0)
(define *standaard-levens-verminder* 1)
(define *bomwerp-levens-verminder* 3)
(define *bom-levens-verminder-min* 1)
(define *bom-levens-verminder-max* 3) ;; Dit is 3 vermits het maar tot de waarde 2 gaat bekijken in random functie