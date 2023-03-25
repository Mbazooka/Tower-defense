;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                  Geld ADT                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (maak-geld-adt begin-bedrag)

  ;; Volgende code heeft het bedrag terug naargelang het type-object
  (define (bedrag type)
    (cond
      ((eq? type 'basis) *basis-toren-kost*)
      (else "Ongeldig type")))

  ;; Volgende code gaat na als de speler genoeg geld heeft
  (define (voldoende-geld? type)
    (>= begin-bedrag (bedrag type)))

  ;; Volgende code zaar na gelang de type van object het juiste bedrag aftrekken
  (define (verwijder-geld! type)
    (set! begin-bedrag (- begin-bedrag (bedrag type))))

  (define (voeg-geld-toe! bedrag)
    (set! begin-bedrag (+ begin-bedrag bedrag)))

  (define (reset!)
    (set! begin-bedrag *geld-bedrag*))

  (define (dispatch msg)
    (cond
      ((eq? msg 'voldoende-geld?) voldoende-geld?)
      ((eq? msg 'verwijder-geld!) verwijder-geld!)
      ((eq? msg 'voeg-geld-toe!) voeg-geld-toe!)
      ((eq? msg 'status) status)
      ((eq? msg 'reset!) reset!)
      (else "maak-geld-adt: ongeldig bericht")))
  dispatch)
