;; Hieronder vind je vectoren waarmee we paden maken
;; ;m Staat voor midden in een pad
(load "positie-adt.rkt")
(define vector-1
  (cons 44
        (vector
         (maak-positie-adt 0 16) ;m
         (maak-positie-adt 1 16) ;m
         (maak-positie-adt 2 16) ;m
         (maak-positie-adt 3 16) ;m
         (maak-positie-adt 4 16) ;m
         (maak-positie-adt 5 16) ;m
         (maak-positie-adt 6 16) ;m
         (maak-positie-adt 7 16) ;m
         (maak-positie-adt 8 16) ;m
         (maak-positie-adt 9 16) ;m
         (maak-positie-adt 10 16) ;m
         (maak-positie-adt 11 16) ;m
         (maak-positie-adt 12 16) ;m
         (maak-positie-adt 13 16) ;m
         (maak-positie-adt 14 16) ;m         
         (maak-positie-adt 15 16) ;m
         (maak-positie-adt 16 16) ;m
         (maak-positie-adt 16 15) ;m
         (maak-positie-adt 16 14) ;m
         (maak-positie-adt 16 13) ;m         
         (maak-positie-adt 16 12) ;m
         (maak-positie-adt 16 11) ;m
         (maak-positie-adt 16 10) ;m
         (maak-positie-adt 17 10) ;m
         (maak-positie-adt 18 10) ;m
         (maak-positie-adt 19 10) ;m
         (maak-positie-adt 20 10) ;m         
         (maak-positie-adt 21 10) ;m
         (maak-positie-adt 22 10) ;m
         (maak-positie-adt 23 10) ;m
         (maak-positie-adt 24 10) ;m
         (maak-positie-adt 25 10) ;m
         (maak-positie-adt 26 10) ;m
         (maak-positie-adt 27 10) ;m
         (maak-positie-adt 28 10) ;m
         (maak-positie-adt 29 10) ;m
         (maak-positie-adt 30 10) ;m
         (maak-positie-adt 31 10) ;m
         (maak-positie-adt 32 10) ;m
         (maak-positie-adt 33 10) ;m
         (maak-positie-adt 34 10) ;m
         (maak-positie-adt 35 10) ;m
         (maak-positie-adt 36 10) ;m
         (maak-positie-adt 37 10) ;m
         (maak-positie-adt 38 10) ;m
         (maak-positie-adt 39 10) ;m         
         (maak-positie-adt 0 15)
         (maak-positie-adt 0 17)
         (maak-positie-adt 1 15)
         (maak-positie-adt 1 17)
         (maak-positie-adt 2 15)
         (maak-positie-adt 2 17)
         (maak-positie-adt 3 15)
         (maak-positie-adt 3 17)
         (maak-positie-adt 4 15)
         (maak-positie-adt 4 17)
         (maak-positie-adt 5 15)
         (maak-positie-adt 5 17)
         (maak-positie-adt 6 15)
         (maak-positie-adt 6 17)
         (maak-positie-adt 7 15)
         (maak-positie-adt 7 17)
         (maak-positie-adt 8 15)
         (maak-positie-adt 8 17)
         (maak-positie-adt 9 15)
         (maak-positie-adt 9 17)
         (maak-positie-adt 10 15)  
         (maak-positie-adt 10 17)
         (maak-positie-adt 11 15)
         (maak-positie-adt 11 17)
         (maak-positie-adt 12 15)
         (maak-positie-adt 12 17)
         (maak-positie-adt 13 15)        
         (maak-positie-adt 13 17)
         (maak-positie-adt 14 15)
         (maak-positie-adt 14 17)
         (maak-positie-adt 15 15)
         (maak-positie-adt 15 17)
         (maak-positie-adt 16 17)
         (maak-positie-adt 17 15)
         (maak-positie-adt 17 16)
         (maak-positie-adt 17 17) 
         (maak-positie-adt 15 14)
         (maak-positie-adt 17 14)
         (maak-positie-adt 15 13)
         (maak-positie-adt 17 13)   
         (maak-positie-adt 15 12)
         (maak-positie-adt 17 12)
         (maak-positie-adt 15 11)
         (maak-positie-adt 17 11)
         (maak-positie-adt 15 10)           
         (maak-positie-adt 15 9)
         (maak-positie-adt 16 9) 
         (maak-positie-adt 17 9)   
         (maak-positie-adt 18 9)
         (maak-positie-adt 18 11)
         (maak-positie-adt 19 9)
         (maak-positie-adt 19 11)
         (maak-positie-adt 20 9) 
         (maak-positie-adt 20 11)
         (maak-positie-adt 21 9) 
         (maak-positie-adt 21 11)
         (maak-positie-adt 22 9) 
         (maak-positie-adt 22 11) 
         (maak-positie-adt 23 9) 
         (maak-positie-adt 23 11)
         (maak-positie-adt 24 9)
         (maak-positie-adt 24 11)
         (maak-positie-adt 25 9)
         (maak-positie-adt 25 11)
         (maak-positie-adt 26 9)
         (maak-positie-adt 26 11)
         (maak-positie-adt 27 9)
         (maak-positie-adt 27 11)
         (maak-positie-adt 28 9)
         (maak-positie-adt 28 11)
         (maak-positie-adt 29 9)
         (maak-positie-adt 29 11)
         (maak-positie-adt 30 9)
         (maak-positie-adt 30 11)
         (maak-positie-adt 31 9)
         (maak-positie-adt 31 11)
         (maak-positie-adt 32 9)
         (maak-positie-adt 32 11)
         (maak-positie-adt 33 9)        
         (maak-positie-adt 33 11)
         (maak-positie-adt 34 9)
         (maak-positie-adt 34 11)
         (maak-positie-adt 35 9)
         (maak-positie-adt 35 11)
         (maak-positie-adt 36 9)
         (maak-positie-adt 36 11)
         (maak-positie-adt 37 9)
         (maak-positie-adt 37 11)
         (maak-positie-adt 38 9)
         (maak-positie-adt 38 11)
         (maak-positie-adt 39 9)
         (maak-positie-adt 39 11))))
  
