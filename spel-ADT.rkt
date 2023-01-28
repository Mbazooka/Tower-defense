;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                  Spel ADT                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (maak-spel-adt)
  (let ((pad (maak-pad-adt vector-1)) ;; maak de fundamenten van het spel
        (teken-adt (maak-teken-adt (+ *menu-breedte-px* *spel-breedte-px*) *spel/menu-hoogte-px*))
        (toren-type #f)
        (torens '())) 

    ;; Maakt basis compenenten van het spel
    ((teken-adt 'teken-spel!) pad) 

    ;; Start de dynamische werking van het spel
    (define (start!)
      ((teken-adt 'set-muis-toets!) muis-klik-procedure))

    ;; De procedure die het klikken van muis op scherm voorstelt    
    (define (muis-klik-procedure toets toestand x y)
      (cond
        ((and (eq? toets 'left) (eq? toestand 'pressed) (>= x *toren-1-knop-breedte-start*) (<= x *toren-1-knop-breedte-einde*) (>= y *toren-1-knop-hoogte-start*) (<= y *toren-1-knop-hoogte-einde*)) ;; Initialiseert toren type
         (set! toren-type 'basis))
        ((eq? toren-type #f) "Beweging niet mogelijk")
        ((and (eq? toets 'left) (eq? toestand 'pressed)
              (<= x (- *start-x-pos-menu* (* 2 *px-breedte*)))) ;; Plaats toren buiten menu. De constante 2 is om speling te vermijden en niks op menu te hebben                          
         (let ((toren (maak-toren-adt (maak-positie-adt (/ x *px-breedte*) (/ y *px-hoogte*)) toren-type)))
           (cond
             ((null? torens)
              (set! torens (cons toren torens))
              ((teken-adt 'teken-toren!) toren))
             ((and (not (accumulate (lambda (x y) (or x y)) #f (map (lambda (t) ((t 'in-toren?) toren)) torens)))
                   (not ((pad 'toren-in-pad?) toren)))
              (set! torens (cons toren torens))
              ((teken-adt 'teken-toren!) toren))
             (else
              "Beweging niet mogelijk"))))
        (else
         "Beweging niet mogelijk")))
                 
    (define (dispatch msg)
      (cond 
        ((eq? msg 'start!) (start!))
        (else
         "maak-spel-adt: undefined message")))
    dispatch))