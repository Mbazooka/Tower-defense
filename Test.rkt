;; Basis idee om spel te implementeren
(#%require "Graphics.rkt")

(define grond (make-window 800 600 "Tower Defense"))
((grond 'set-background!) "black")

(define laag-1 ((grond 'new-layer!)))

(define tegel (make-tile 20 20 "Images/Aarde.png"))
((tegel 'set-x!) 0)
((tegel 'set-y!) 0)
((laag-1 'add-drawable!) tegel)



;; maken van een pad
