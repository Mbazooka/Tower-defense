;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              Monster lijst                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Volgende code is om een level op te stellen met bepaalde monsters die eruit komen
(define ronde-1-1 (list 'rood 'rood 'groen 'rood 'groen
                      'paars 'groen 'rood 'geel 'paars
                      'geel 'rood 'rood 'rood 'rood )) 

(define ronde-1-2 (list 'rood 'rood 'groen 'groen 'groen
                      'geel 'rood 'geel 'paars 'paars
                      'rood 'rood 'geel 'groen 'rood))

(define ronde-1-3 (list 'groen 'rood 'groen 'paars 'rood
                      'geel 'geel 'groen 'rood 'paars
                      'paars 'rood 'groen 'geel 'rood))

(define ronde-2-1 (list 'groen 'rood 'rood 'paars 'geel
                        'paars 'rood 'rood 'groen 'rood
                        'geel  'groen 'paars 'groen 'geel
                        'paars 'groen 'rood 'geel 'groen))

(define ronde-2-2 (list 'groen 'groen 'paars 'geel 'groen
                        'rood 'geel 'groen 'geel 'rood
                        'groen 'rood 'rood 'groen 'geel
                        'paars 'paars 'paars 'groen 'groen))

(define ronde-2-3 (list 'paars 'geel 'groen 'rood 'rood
                        'paars 'groen 'groen 'geel 'groen
                        'groen 'rood 'paars 'geel 'paars
                        'paars 'geel 'paars 'groen 'geel))

(define ronde-3-1 (list 'rood 'paars 'paars 'geel 'paars
                        'groen 'rood 'geel  'paars 'groen
                        'paars 'paars 'geel 'rood 'geel
                        'groen 'rood 'rood 'paars 'rood
                        'groen 'rood 'rood 'rood 'paars))

(define ronde-3-2 (list 'paars 'paars 'geel 'groen 'paars
                        'rood 'paars 'paars 'geel 'geel
                        'geel 'paars 'geel 'groen 'rood
                        'geel 'paars 'paars 'groen 'paars
                        'groen 'paars 'geel 'paars 'rood))

(define ronde-3-3 (list 'rood 'groen 'paars 'paars 'geel
                        'geel 'geel 'geel 'paars 'paars
                        'paars 'groen 'paars 'geel 'geel
                        'paars 'geel 'paars 'paars 'geel
                        'paars 'paars 'geel 'paars 'paars))

;; Hieronder vind je de vector van vectoren om een (en volgende) ronde te verkrijgen
(define monster-vector (vector (vector ronde-1-1 ronde-1-2 ronde-1-3)
                               (vector ronde-2-1 ronde-2-2 ronde-2-3)
                               (vector ronde-3-1 ronde-3-2 ronde-3-3)))