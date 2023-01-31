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

;; Tijd constanten
(define *monster-beweeg-snelheid* 200)

;; Constanten die de totale spelgrootte voorstellen (met verschillende componenten)
(define *menu-breedte-px* (* *menu-breedte* *px-breedte*))
(define *spel-breedte-px* (* *spel-breedte* *px-breedte*))
(define *spel/menu-hoogte-px* (* *spel-hoogte* *px-hoogte*))

;; Menu constante (stelt de x coordinaat voor die de start van de menu voorstelt, nodig voor torens te plaatsen)
(define *start-x-pos-menu* (* *spel-breedte* *px-breedte*))
(define *toren-1-knop-hoogte-start* (* 2 *px-hoogte*)) ;; zijn keuzes om knopen hier te starten en daar te eindigen
(define *toren-1-knop-breedte-start* (+ *spel-breedte* *px-breedte*))
(define *toren-1-knop-hoogte-einde* (* 5 *px-hoogte*))
(define *toren-1-knop-breedte-einde* (* *spel-breedte* (* 4 *px-breedte*)))



