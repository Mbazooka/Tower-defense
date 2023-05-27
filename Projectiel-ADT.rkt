;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                 Projectiel ADT                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (maak-projectiel-adt positie type te-raken . afvuur-snelheid) 
  (let* ((monster-of-niet (monster? te-raken)) ;; boolean die vaak herhaald word (#t dan monster, #f dan positie)
         (bestemming (if monster-of-niet (((te-raken 'positie) 'positie-copieer)) te-raken))
         (bestemming-x (bestemming 'x)) ;; Vaak nodig dus 1 maal berekent
         (bestemming-y (bestemming 'y))
         (initiele-positie ((positie 'positie-copieer)))
         (bestemming-extra-1 (maak-positie-adt (+ bestemming-x 1) (+ bestemming-y 1))) ;; Dit zijn extras omdat projectiel soms bestemming voor bij gaat
         (bestemming-extra-2 (maak-positie-adt (+ bestemming-x 1) (- bestemming-y 1)))
         (bestemming-extra-3 (maak-positie-adt (- bestemming-x 1) (+ bestemming-y 1)))
         (bestemming-extra-4 (maak-positie-adt (- bestemming-x 1) (- bestemming-y 1)))
         (bestemming-lijst (list bestemming bestemming-extra-1 bestemming-extra-2 bestemming-extra-3 bestemming-extra-4)) 
         (positie-update-hoeveelheid-x (- bestemming-x (initiele-positie 'x))) ;; Dit zijn positie update constanten om gewicht te introduceren en ze zo smooth naar hun eindbestemming te brengen
         (positie-update-hoeveelheid-y (- bestemming-y (initiele-positie 'y)))
         (projectiel-afvuur-snelheid (if (pair? afvuur-snelheid) (car afvuur-snelheid) *projectiel-afvuur-snelheid-steen-net-vuurbal*)) ;; Verander voor algemeenheid
         (lig-tijd 0) ;; Is de tijd dat een projectiel al blijven liggen is (voor net)         
         (vertraagd #f) ;; Is om na te gaan indien een projectiel een monster al vertraagd heeft (zodat niet in elke loop het monster vertraag)
         (projectiel-rand #f)) ;; Is de rand van een projectiel (indien nodig bv bij een net-projectiel of bomwerp)

    ;; Volgende code is een hulpprocedure voor bestemming-bereikt?
    (define (positie-bereikt bestemming-positie)
      (or ((((positie 'ceil)) 'gelijk?)  ((bestemming-positie 'ceil)))
          ((((positie 'flo)) 'gelijk?) ((bestemming-positie 'flo)))
          ((((positie 'ceil)) 'gelijk?)  ((bestemming-positie 'flo)))
          ((((positie 'flo)) 'gelijk?)  ((bestemming-positie 'ceil)))))
         
    ;; Volgende code gaat na als het projectiel de bestemming of de extra bestemming posities bereikt heeft.    
    (define (bestemming-bereikt?)
      (accumulate (lambda (x y) (or x y)) #f (map positie-bereikt bestemming-lijst)))

    ;; Volgende code zal na gaan indien alle acties afgehandelt zijn van een projectiel
    (define (afgehandelt?)
      (cond
        ((or (eq? type 'steen) (eq? type 'vuurbal)) #t)
        ((eq? type 'net) (>= lig-tijd *net-blijf-liggen-tijd*)) ;; Maak constanten
        ((eq? type 'bomwerp) (>= lig-tijd *bomwerp-projectiel-ligtijd*))))                            

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
        ((or (eq? type 'steen) (eq? type 'vuurbal))
         ((te-raken 'actie-monster-levend!) 'verminder))
        ((eq? type 'net) (if (not vertraagd) ;; Gaat na als het te raken monster al vertraagd is geweest
                             (begin
                               ((te-raken 'actie-monster-levend!) 'vertraag dispatch)
                               (set! vertraagd #t))))))

    ;; Volgende code zal een projectiel een actie doen uitvoeren na dat hij een monster heeft geraakt
    (define (actie-na-positie-bereik! level dt) 
      (cond
        ((eq? type 'vuurbal)
         (let ((snelheid (- projectiel-afvuur-snelheid *vuurbal-hits-snelheid-verander*))
               (volgend-monster ((level 'monster-na-monster) te-raken)))
           (if (and (> snelheid 0)  volgend-monster)         
               (maak-projectiel-adt positie type
                                    volgend-monster
                                    snelheid)
               #f)))
        ((eq? type 'net) (maak-rand! *net-projectiel-rand-afstand* level) (set! lig-tijd (+ lig-tijd dt)))
        ((eq? type 'bomwerp) (set! lig-tijd (+ lig-tijd dt))
                             (if (>= lig-tijd *bomwerp-projectiel-ligtijd*)
                                 (explodeer! level (maak-rand! *bomwerp-projectiel-rand-afstand* level))))))

    ;; Volgende code maakt een rand voor een net-projectiel
    (define (maak-rand! afstand level)
      (if (not projectiel-rand)
          (let ((vec (make-vector 4)))
            (positie->rand! positie afstand vec)
            (set! projectiel-rand vec)
            (if (eq? type 'net)
                ((level 'voeg-net-projectiel-toe!) dispatch) ;; Moet zo gedaan worden, zodat net-projectiel niet meermaals aan level word toegevoegd
                vec))))

    ;; Volgende code gaat na als een monster in de rand van een net-projectiel zit
    (define (binnen-rand? monster)
      (in-rand? (monster 'positie) projectiel-rand))

    ;; Volgende code explodeert de bom
    (define (explodeer! level rand)
      ((level 'explodeer-monsters-in-buurt!) rand 'bomwerp))
    
    ;; Volgende code gaat na als een projectiel niet bereikt en afgehandelt is
    (define (niet-bereikt&&afgehandelt?)
      (not (and (bestemming-bereikt?) (afgehandelt?))))
              
    (define (dispatch msg)
      (cond
        ((eq? msg 'positie) positie)
        ((eq? msg 'type) type)
        ((eq? msg 'bestemming-bereikt?) bestemming-bereikt?)
        ((eq? msg 'afgehandelt?) afgehandelt?)
        ((eq? msg 'volgende-positie!) volgende-positie!)
        ((eq? msg 'actie-te-raken-monster!) actie-te-raken-monster!)
        ((eq? msg 'actie-na-positie-bereik!) actie-na-positie-bereik!)
        ((eq? msg 'binnen-rand?) binnen-rand?)
        ((eq? msg 'niet-bereikt&&afgehandelt?) niet-bereikt&&afgehandelt?)
        ((eq? msg 'soort) 'projectiel)
        (else "maak-projectiel-adt: ongeldig bericht")))
    dispatch))
