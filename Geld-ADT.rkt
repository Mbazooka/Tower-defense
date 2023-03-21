;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                  Geld ADT                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (maak-geld-adt begin-bedrag)
  (define (verwijder-geld! bedrag)
    (if (< bedrag begin-bedrag)
        (set! begin-bedrag (- begin-bedrag bedrag))
        "Niet genoeg funds"))

  (define (voeg-geld-toe! bedrag)
    (set! begin-bedrag (+ begin-bedrag bedrag)))

  (define (reset!)
    (set! begin-bedrag *geld-bedrag*))

  (define (dispatch msg)
    (cond 
      ((eq? msg 'verwijder-geld!) verwijder-geld!)
      ((eq? msg 'voeg-geld-toe!) voeg-geld-toe!)
      ((eq? msg 'reset!) reset!)
      (else "maak-geld-adt: ongeldig bericht")))
  dispatch)
