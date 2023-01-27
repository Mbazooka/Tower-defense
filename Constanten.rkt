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

;; Bitmap constanten die de code netter maken (indien meerdere keren gebruikt wordt)
(define *bitmap-pad* "Images/Aarde.png")
(define *bitmap-rood-monster* #f) ;; Verander later
(define *bitmap-toren-1* "Images/Toren-1-Game.png") ;; Verander later

;; Menu constante (stelt de x coordinaat voor die de start van de menu voorstelt, nodig voor torens te plaatsen)
(define *start-x-pos-menu* *menu-breedte*)


