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

;; Toren constanten
(define *toren-rand-afstand* 1.6) ;; Hier werd speling gebruikt om te zorgen bitmap niet op pad komt (zorgt voor meer afstand tussen torens en pad)
(define *buurt-rand-afstand* 10)

;; Tijd constanten
(define *monster-spawn-frequentie* 3000) ;; Monster spawn frequentie (te veranderen)
(define *monster-loop-snelheid* 0.5) ;; Monster beweeg snelheid
(define *toren-afvuur-frequentie* 2000) ;; Toren schiet frequentie
(define *projectiel-afvuur-snelheid* 0.1) ;; Hoe snel het projectiel afgevuurd word

;; Constanten die de totale spelgrootte voorstellen (met verschillende componenten)
(define *menu-breedte-px* (* *menu-breedte* *px-breedte*))
(define *spel-breedte-px* (* *spel-breedte* *px-breedte*))
(define *spel/menu-hoogte-px* (* *spel-hoogte* *px-hoogte*))

;; Menu constante (stelt de x coordinaat voor die de start van de menu voorstelt, nodig voor torens te plaatsen)
(define *start-x-pos-menu* (* *spel-breedte* *px-breedte*))
(define *toren-1-knop-hoogte-start* (* 2 *px-hoogte*)) ;; zijn keuzes om knopen hier te starten en daar te eindigen
(define *toren-knop-breedte-start* (+ *spel-breedte-px* *px-breedte*))
(define *toren-1-knop-hoogte-einde* (* 5 *px-hoogte*))
(define *toren-knop-breedte-einde* (* *spel-breedte* (* 4 *px-breedte*)))

;; Tekst constanten voor op menu
(define *tekst-toren-breedte* (+ *px-breedte* 10)) ;; + 5 want orange rand centreert de tekst niet
(define *tekst-toren-hoogte* *px-hoogte*)

;; Abstracties om bitmap en mask te acesseren
(define bitmap car)
(define mask cdr)

;; Constanten voor bitmap van torens
(define *basis-toren-bitmap&&mask* (cons "Images/Toren-1-game.png" "Images/Toren-1-game-mask.png"))

;; Constanten voor bitmap van monsters (geheugen vriendelijker)
(define *rood-monster-bitmap&&mask* (cons "Images/Rood-monster.jpg" "Images/Rood-monster-mask.png"))

;; Constanten voor bitmap van projectielen
(define *steen-projectiel-bitmap&&mask* (cons "Images/projectiel.png" "Images/projectiel-mask.png"))

;; Constanten voor het begin geld bedrag en het begin levens hoeveelheid
(define *geld-begin-bedrag* 1000)
(define *levens-hoeveelheid* 5)

;; Constanten die geld van speler voorstelt
(define *basis-toren-kost* 500)

;; Constanten die levens van monsters voorstellen
(define *levens-rood-monster* 1)
(define *levens-groen-monster* 2)
(define *levens-geel-monster* 2)
(define *levens-paars-monster* 4)




