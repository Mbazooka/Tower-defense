;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                  Geld ADT                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load "Constanten.Rkt")
(define (maak-geld-adt begin-bedrag)

  ;; Volgende procedure gaat na indien de speler voldoende geld heeft voor een power-up of toren te kopen
  (define (voldoende-geld? type)
    (cond
      ((eq? type 'basis) (>= begin-bedrag *basis-toren-kost*))
      (else "Ongeldig type")))
  
  (define (verwijder-geld! bedrag)
    (set! begin-bedrag (- begin-bedrag bedrag)))

  (define (voeg-geld-toe! bedrag)
    (set! begin-bedrag (+ begin-bedrag bedrag)))

  (define (reset!)
    (set! begin-bedrag *geld-bedrag*))

  (define (dispatch msg)
    (cond
      ((eq? msg 'voldoende-geld?) voldoende-geld?)
      ((eq? msg 'verwijder-geld!) verwijder-geld!)
      ((eq? msg 'voeg-geld-toe!) voeg-geld-toe!)
      ((eq? msg 'reset!) reset!)
      (else "maak-geld-adt: ongeldig bericht")))
  dispatch)
