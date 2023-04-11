;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                  Geld ADT                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (maak-geld-adt begin-bedrag)

  ;; Volgende code heeft het bedrag terug naargelang het type-object (zijn disjunct)
  (define (bedrag type)
    (cond
      ((eq? type 'basis-toren) *basis-toren-kost*)
      ((eq? type 'rood) *rood-groen-monster-winst*)
      ((eq? type 'geel) *geel-monster-winst*)
      ((eq? type 'paars) *paars-monster-winst*)
      ((eq? type 'level) *level-winst*)
      (else "Ongeldig type")))

  ;; Volgende code gaat na als de speler genoeg geld heeft
  (define (voldoende-geld? type)
    (>= begin-bedrag (bedrag type)))

  ;; Volgende code zal naar gelang de type van object het juiste bedrag aftrekken
  (define (verwijder-geld! type)
    (set! begin-bedrag (- begin-bedrag (bedrag type))))

  (define (voeg-geld-toe! type)
    (set! begin-bedrag (+ begin-bedrag (bedrag type))))

  (define (reset!)
    (set! begin-bedrag *geld-bedrag*))

  (define (dispatch msg)
    (cond
      ((eq? msg 'voldoende-geld?) voldoende-geld?)
      ((eq? msg 'verwijder-geld!) verwijder-geld!)
      ((eq? msg 'voeg-geld-toe!) voeg-geld-toe!)
      ((eq? msg 'status) begin-bedrag)
      ((eq? msg 'soort) 'geld)
      ((eq? msg 'reset!) reset!)
      (else "maak-geld-adt: ongeldig bericht")))
  dispatch)
