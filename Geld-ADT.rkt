;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                  Geld ADT                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (maak-geld-adt begin-bedrag)

  ;; Volgende code heeft het bedrag terug naargelang het type-object (zijn disjunct)
  (define (bedrag type extra)
    (let ((kost-winst #f))
      (cond
        ((eq? type 'basis-toren) (set! kost-winst *basis-toren-kost*))
        ((eq? type 'net-toren) (set! kost-winst *net-toren-kost*))
        ((eq? type 'vuurbal-toren) (set! kost-winst *vuurbal-toren-kost*))
        ((eq? type 'bomwerp-toren) (set! kost-winst *bomwerp-toren-kost*))
        ((eq? type 'tank) (set! kost-winst *tank-kost*))
        ((eq? type 'bommen-regen) (set! kost-winst *bommen-regen-kost*))
        ((eq? type 'rood) (set! kost-winst *rood-groen-monster-winst*))
        ((eq? type 'geel) (set! kost-winstst *geel-monster-winst*))
        ((eq? type 'paars) (set! kost-winst *paars-monster-winst*))
        ((eq? type 'level) (set! kost-winst *level-winst*))  
        (else "Ongeldig type"))

      (cond
        ((and extra kost-winst)
         (let ((nieuwe-kost-winst (+ kost-winst *extra-tank-winst*)))
           (set! kost-winst #f)
           nieuwe-kost-winst))
        (kost-winst
         (let ((nieuwe-kost-winst kost-winst))
           (set! kost-winst #f)
           nieuwe-kost-winst))
        (else
         "Ongeldig type"))))
          

  ;; Volgende code gaat na als de speler genoeg geld heeft
  (define (voldoende-geld? type)
    (>= begin-bedrag (bedrag type #f)))

  ;; Volgende code zal naar gelang de type van object het juiste bedrag aftrekken
  (define (verwijder-geld! type)
    (set! begin-bedrag (- begin-bedrag (bedrag type #f))))

  (define (voeg-geld-toe! type extra)
    (if (not (eq? type 'groen))
        (set! begin-bedrag (+ begin-bedrag (bedrag type extra)))))

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
