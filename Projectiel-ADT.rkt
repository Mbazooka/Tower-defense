;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                 Projectiel ADT                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (maak-projectiel-adt positie type te-raken-monster . afvuur-snelheid) 
  (let* ((bestemming (((te-raken-monster 'positie) 'positie-copieer)))
         (bestemming-x (bestemming 'x)) ;; Vaak nodig dus 1 maal berekent
         (bestemming-y (bestemming 'y))
         (initiele-positie ((positie 'positie-copieer)))
         (bestemming-extra-1 (maak-positie-adt (+ bestemming-x 1) (+ bestemming-y 1))) ;; Dit zijn extras omdat projectiel soms besteming voor bij gaat
         (bestemming-extra-2 (maak-positie-adt (+ bestemming-x 1) (- bestemming-y 1)))
         (bestemming-extra-3 (maak-positie-adt (- bestemming-x 1) (+ bestemming-y 1)))
         (bestemming-extra-4 (maak-positie-adt (- bestemming-x 1) (- bestemming-y 1)))
         (bestemming-lijst (list bestemming bestemming-extra-1 bestemming-extra-2 bestemming-extra-3 bestemming-extra-4)) 
         (positie-update-hoeveelheid-x (- bestemming-x (initiele-positie 'x))) ;; Dit zijn positie update constanten om gewicht te introduceren en ze zo smooth naar hun eindbestemming te brengen
         (positie-update-hoeveelheid-y (- bestemming-y (initiele-positie 'y)))
         (projectiel-afvuur-snelheid (if (pair? afvuur-snelheid) (car afvuur-snelheid) *projectiel-afvuur-snelheid-vuurbal*)) ;; Verander voor algemeenheid
         (lig-tijd 0) ;; Is de tijd dat een projectiel al blijft liggen (voor net)
         (toegevoegd #f) ;; Is om na te gaan als het net-projectiel toegevoegd is aan het level-adt (om te zien als monster over netten lopen)
         (vertraag #f)) ;; Is om na te gaan indien een projectiel een monster al vertraag heeft (zodat niet in elke loop het monster vertraag)

    ;; Volgende code gaat na als het projectiel de bestemming of de extra bestemming posities bereikt heeft.    
    (define (bestemming-bereikt?)
      (define (positie-bereikt bestemming-positie)
        (or ((((positie 'ceil)) 'gelijk?)  ((bestemming-positie 'ceil)))
            ((((positie 'flo)) 'gelijk?) ((bestemming-positie 'flo)))
            ((((positie 'ceil)) 'gelijk?)  ((bestemming-positie 'flo)))
            ((((positie 'flo)) 'gelijk?)  ((bestemming-positie 'ceil)))))
      (accumulate (lambda (x y) (or x y)) #f (map positie-bereikt bestemming-lijst)))

    ;; Volgende code zal na gaan indien alle acties afgehandelt zijn van een projectiel
    (define (afgehandelt?)
      (cond
        ((or (eq? type 'steen) (eq? type 'vuurbal)) #t)
        ((eq? type 'net) (>= lig-tijd *net-blijf-liggen-tijd*)) ;; Maak constanten 
        (else
         "Ongeldig type projectiel")))                             

    ;; Volgende code zal de positie van het projectiel updaten
    (define (volgende-positie!)
      (if (not (bestemming-bereikt?))
          (let ((x-pos-proj (positie 'x))
                (y-pos-proj (positie 'y)))
            ((positie 'x!) (+ (* positie-update-hoeveelheid-x projectiel-afvuur-snelheid) x-pos-proj))
            ((positie 'y!) (+ (* positie-update-hoeveelheid-y projectiel-afvuur-snelheid) y-pos-proj)))))

    ;; Volgende code voert actie uit op monster (afhankelijk van het type projectiel)
    (define (actie-te-raken-monster!)
      (cond
        ((or (eq? type 'steen) (eq? type 'vuurbal) (eq? type 'bom))
         ((te-raken-monster 'actie-monster-levend!) 'verminder))
        ((eq? type 'net) ((te-raken-monster 'actie-monster-levend!) 'vertraag))
        (else "Projectiel: ongeldig type")))

    ;; Volgende code zal een projectiel een actie doen uitvoeren na dat hij een monster heeft geraakt
    (define (actie-na-monster-raak! level dt) ;; Voeg andere dingen toe
      (cond
        ((eq? type 'vuurbal)
         (let ((snelheid (- projectiel-afvuur-snelheid *vuurbal-hits-snelheid-verander*))
               (volgend-monster ((level 'monster-na-monster) te-raken-monster)))
           (if (and (> snelheid 0)  volgend-monster)         
               (maak-projectiel-adt positie type
                                    volgend-monster
                                    snelheid)
               #f)))
        ((eq? type 'net) (set! lig-tijd (+ lig-tijd dt)))
        (else
         "Heeft geen actie na het raken van monsters")))

    ;; Volgende code zal het toegevoegd Lokale Variabele aanpassen naar #t
    (define (toegevoegd!)
      (set! toegevoegd #t))

    ;; Volgende code gaat na als het projectiel toegevoegd is aan het level-adt
    (define (toegevoegd?) toegevoegd)
     
    (define (dispatch msg)
      (cond
        ((eq? msg 'positie) positie)
        ((eq? msg 'type) type)
        ((eq? msg 'te-raken-monster) te-raken-monster)
        ((eq? msg 'bestemming-bereikt?) bestemming-bereikt?)
        ((eq? msg 'afgehandelt?) afgehandelt?)
        ((eq? msg 'volgende-positie!) volgende-positie!)
        ((eq? msg 'actie-te-raken-monster!) actie-te-raken-monster!)
        ((eq? msg 'actie-na-monster-raak!) actie-na-monster-raak!)
        ((eq? msg 'toegevoegd!) toegevoegd!)
        ((eq? msg 'toegevoegd?) toegevoegd?)
        ((eq? msg 'soort) 'projectiel)
        (else "maak-projectiel-adt: ongeldig bericht")))
    dispatch))