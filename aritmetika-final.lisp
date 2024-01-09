;;;; Poznamka: 
;;;;   Nasledujuce priklady je mozne riesit pomocou rekurzie 
;;;;   (pocitanie smerom dnu aj von) alebo pomocou iteracie


;;; Sucet dvoch nezapornych celociselnych scitancov
;;; 
;;;   (op-plus 3 5) -> 8
;;;
;;; Napoveda: riesenie zalozte na pouziti '1+' a/alebo '1-'
(defun op-plus (x y) ;;; Do vnútra rekurzie -> ak je x=0 vrati sa ypsilon ku ktoremu sa rekurzivne pripočitavalo +1 tolko krat kolko bolo x (+1 do parametra rekurzie)
  (if (zerop x) y (op-plus (1- x) (1+ y)))
  )

(defun op-plus (x y) ;;; Von z rekurzie -> ak je x =0 vrati sa ypsilon, inač sa rekurzivne k ypsilonu z konca rekurzie pripočíta +1(+1 do vysledku rekurzie) (tolko krat ake velke je x)
  (if (zerop x) y (1+ (op-plus (1- x) y)))
  )

(defun op-plus (x y) ;;; Pomocou cyklu
  (do ((i x (1- i)) ;;; inicializujeme symbol i na hodnotu x, odpočitavame od i kazdou iteraciou
       (j y (1+ j))) ;;; inicializujeme symbol j na hodnotu y, pripočitavame k j kazdou iteraciou
      ((zerop i) j) ;;; ukončovacia podmienka -> ak i=0 vratime j
    )
  )

;;; Sucet dvoch celociselnych scitancov
;;; 
;;;   (op-plus  3  5) ->  8
;;;   (op-plus -3  5) ->  2
;;;   (op-plus -3 -5) -> -8
;;;
;;; Napoveda: riesenie zalozte na pouziti '1+' a/alebo '1-'

(defun op-plus (x y) ;;; Pomocou rekurzie von
  (cond 
    ((> 0 x) (1- (op-plus (1+ x) y)))
    ((> x 0) (1+ (op-plus (1- x) y)))
    (t y))
  )

(defun op-plus (x y) ;;; Pomocou do vnutra rekurzie
  (cond
    ((> 0 x) (op-plus (1+ x) (1- y)))
    ((> x 0) (op-plus (1- x) (1+ y)))
    (t y))
  )

;;; Rozdiel dvoch nezapornych celych cisel
;;;
;;;   (op-minus 5 3) ->  2
;;;   (op-minus 3 5) -> -2
;;;
;;; Napoveda: riesenie zalozte na pouziti '1+' a/alebo '1-'

(defun op-minus (x y) ;;; Pomocou von z rekurzie
  (if (zerop y) x (1- (op-minus x (1- y)))))

(defun op-minus (x y) ;;; Pomocou dnu do rekurzie
  (if (zerop y) x (op-minus (1- x) (1- y))))

;;; Rozdiel dvoch celych cisel
;;;
;;;   (op-minus  3  5) -> -2
;;;   (op-minus  3 -5) ->  8
;;;   (op-minus -3 -5) ->  2
;;;
;;; Napoveda: riesenie zalozte na pouziti '1+' a/alebo '1-'

(defun op-minus (x y) ;;; Von z rekurzie 
  (cond ((> 0 y) (1+ (op-minus x (1+ y))))
        ((> y 0) (1- (op-minus x (1- y))))
        (t x)))


;;; Sucin dvoch celociselnych sucinitelov
;;;
;;;   (op-krat  3  4) -> 12
;;;   (op-krat -3  4) -> -12
;;;   (op-krat -3 -4) ->  12
;;;
;;; Napoveda: riesenie zalozte na pouziti '+'

(defun op-krat (x y &optional (accum 0)) ;;; Pomocou akumulatora cez optional premennú
  (cond ((> 0 x) (op-krat (1+ x) y (- accum y)))
        ((> x 0) (op-krat (1- x) y (+ accum y)))
        (t accum))
  )

(defun op-krat-wrap (x y accum) ;;; Pomomcou "wrapper" funkcie namiesto optional
  (cond ((> 0 x) (op-krat-wrap (1+ x) y (- accum y)))
        ((> x 0) (op-krat-wrap (1- x) y (+ accum y)))
        (t accum))
  )
(defun op-krat (x y)
  (op-krat-wrap x y 0)
  )

;;; Kontrola delitelnosti dvoch celych cisel
;;;
;;;   (p-del  12  4) -> t
;;;   (p-del -12  4) -> t
;;;   (p-del -12 -4) -> t
;;;   (p-del  13 -4) -> nil
;;;
;;; Napoveda: riesenie zalozte na pouziti '-'

(defun p-del (x y)
  (cond
        ((< x 0) (p-del (- x)  (if (> 0 y) (- y) y) )) ;;; ak je x (cislo ktore delime) mensie ako nula, zmenime znamienko, ak je y < 0 taktiez zmenime znamienko a volame rekurzivne p-del
        ((< x y) (zerop (- x))) ;;; Vysledna podmienka, ak je cislo menšie ako delitel testujeme či je rovne 0, ak je vrati sa T (automaticky z podmienky) inak nil
        (t (p-del (- x (if (> 0 y) (- y) y) ) (if (> 0 y) (- y) y) ) ) ;;; Od čisla x sa odpočíta delitel (y) a rekurzívne sa volá funkcia
   )
  )
;;; Celociselne delenie dvoch celych cisel
;;;
;;;   (op-celdel 12 4) -> 3
;;;   (op-celdel -13  4) -> -3
;;;   (op-celdel -13 -4) ->  3
;;;
;;; Napoveda: riesenie zalozte na pouziti '-'

(defun op-celdel (x y)
  (cond ((< y 0) (op-celdel (- x) (- y))) ;;; Ak je delitel menší ako nula zmeníme obe znamienka (aby sme vždy odčítavali od čísla ktoré delíme
        ((< x 0) (- (op-celdel (- x) y))) ;;; ak je následne x menšie ako 0 tak dáme - pred celé dalšie delenie a otočíme znamienko pri x
        ((< x y) 0) ;;; ak je x menšie ako y znamená, že už sa dalej deliť nedá a vrátime nulu 
        ((= x y) 1) ;;; ak sa čísla rovnajú tak sa deliť dá ešte raz (zv. 0) preto vrátime 1
        (t (1+ (op-celdel (- x y) y))))) ;;; rekurzivne pripočítavame jedna pred rekurziu a odpočítavame od čísla x y

;;; Umocnovanie pomocou nezaporneho celociselneho exponentu
;;;
;;;   (op-mocn  2 4) -> 16
;;;   (op-mocn -3 2) ->  9
;;;   (op-mocn -3 3) -> -27
;;;
;;; Napoveda: riesenie zalozte na pouziti '*'

(defun op-mocn (x y &optional (acc 1))
  (cond ((= y 0) acc) ;;; ak y už je rovne nule vratime akumulator
        (t (op-mocn x (1- y) (* acc x))) ;;; inak rekurzívne dekrementujeme y a násobíme akumulátor *x
        )
  )

(defun op-mocn-itr (x y)
  (do ((acc 1 (* acc x)) ;;; inicializacia akumulatora na jedna a násobenie akumulátora *x pri každom kroku
       (y y (1- y))) ;;; inicializacia y na zadanu hodnotu a dekrementacia y pri každom kroku
      ((= y 0) acc)  ;;; testovacia podmienka, ak je y=0 vratime akumulátor
   )
)
;;; Celociselna odmocnina nezaporneho celeho cisla
;;;
;;;   (op-odm 16) -> 4
;;;   (op-odm 24) -> 4
;;;
;;; Napoveda: riesenie zalozte na pouziti '-' 

(defun op-odm (x &optional (acc 0) (odd 1))
  (cond ((= x 0) acc) ;;; vrátime akumulátor ak už od x sa nedá odpočítavať
        ((< x 0) (1- acc)) ;;; ak je x zaporne, vrátime akumulátor -1 (chceme celočíslené a x je už zaporne)
        (t (op-odm (- x odd) (1+ acc) (+ 2 odd))) ;;; rekurzívne odpočítavame odd od x (zmenšujeme x o odmocninu) a pripočítavame akumulátor +1 a odmocninu +2
        )
)

(defun op-odm-itr (x) 
  (do ((odd 1 (+ 2 odd)) ;;; odmocninu zvyšujeme +2 v každej it
       (x x (- x odd)) ;;; odpočítavame od x odmocninu v každej iteracii
       (acc 0 (1+ acc))) ;;; akumulátor inkrementujeme v každej iteracii
      ((<= x 0) (if (< x 0) (1- acc) acc)) ;;; ak je x zaporne vratime akumulátor -1 inak akumulátor (finalna podmienka cyklu)
  )
)
;;; Porovnavanie dvoch celych cisel
;;;
;;;   (p->  4  5) -> nil
;;;   (p->  5  4) -> t
;;;   (p-> -5 -4) -> nil
;;;
;;; Napoveda: riesenie zalozte na pouziti '1+' a/alebo '1-'
;;;
;;; Analogicky navrhnite predikaty 'p-<' a 'p-='
(defun p-> (a b) ;;; rekurzivne sa odpočítavajú čísla a a b o jedna 
  (cond ((and (< a 0) (>= b 0)) nil) ;;; ak je a záporné a b kladné, tak je a menšie ako b -> nil
        ((and (< b 0) (>= a 0)) t) ;;; ak je b záporné a a kladné, tak je a väčšie ako b -> t
        ((= a b) nil) ;;; ak sa rovnaju -> nil
        (t (p-> (1- a) (1- b))) ;;; ak sú obe kladné tak sa odočítajú o jedna a znova voláme funkciu

        )
  )
(defun p-< (a b)  ;;; Zavoláme funkciu p-> a výsledok negujeme, stači v prvych dvoch podmienkach zmeniť nil a t
  (if (p-> a b) nil t) 
  )

(defun p= (a b) 
  (cond ((and (= 0 a) (= 0 b)) t) ;;; ak sa rovna 0 a aj b -> t (tj zmenšili sme obe na 0 o rovnaký počet)
        ((or (= 0 a) (= 0 b)) nil) ;;; Ak sa iba jedno rovná nule vráť nil (nerovnajú sa)
        (t (p= (abs (1- a)) (abs (1- b)))) ;;; Ak sa nerovnajú, tak zmenšíme obe o jedna a zavoláme funkciu
        )
  )

(defun p-> (x y) ;;; Iteratívne riešenie
  (do ((x x (if (< x 0) (1+ x) (1- x))) ;;; Ak je x záporné, tak sa zväčší o jedna, inak sa zmenší o jedna
       (y y (if (< y 0) (1+ y) (1- y)))) ;;; Ak je y záporné, tak sa zväčší o jedna, inak sa zmenší o jedna
      ((or (and (< x 0) (>= y 0)) (and (< y 0) (>= x 0)) (and (= x 0) (= y 0))) (and (< y 0) (>= x 0))) ;;; podmienka ak je x zap a y klad alebo y zap a x klad alebo obe nula tak sa ukončí cyklus, nasledne ak je y zaporne a x kladné vráti sa T inak NIL
  )
)
;;; Test parnosti a neparnosti celych cisel
;;;
;;;   (p-neparne  4) -> nil
;;;   (p-neparne -3) -> t
;;;   (p-parne  4) -> t
;;;   (p-parne -3) -> nil
;;;
;;; Napoveda: riesenie zalozte na pouziti '1+' a/alebo '1-'

;;; volaju sa rekurzivne navzajom p-parne a p-neparne, ak na konci je x = 0 a začalo to p-parne tak je parne ak to začalo p-neparne tak je neparne

(defun p-neparne (x)
  (if (= x 0) nil (p-parne (1- (abs x)))) 
  )

(defun p-parne (x)
  (if (= x 0) t (p-neparne (1- (abs x))))
  )

(defun neparne-itr (x)
  (do ((x x (if (> x 0) (1- x) (1+ x))) ;;; x sa zmenšuje o jedna, ak je kladné, inak sa zväčšuje o jedna
       (odd nil (not odd))) ;;; neguje sa odd pri každej iteracii, inicializuje sa na NIL
      ((zerop x) odd) ;;; ak je x nula vráti sa odd
  )
)

(defun parne-itr (x) ;;; rovnako ako neparne akurat sa inicializuje even na T 
  (do ((x x (if (< x 0) (1+ x) (1- x)))
       (even T (not even)))
      ((zerop x) even)
  )
)

;;; Najvacsi spolocny delitel dvoch nezapornych celych cisel 
;;;
;;;    (op-maxsd 15 9) -> 3
;;;    (op-maxsd  9 9) -> 9
;;;    (op-mxsd  17 5) -> 1
;;;    
;;; Napoveda: riesenie zalozte na pouziti 'mod'

(defun nsd (x y div)
  (cond ((and (= (mod x div) 0) (= (mod y div) 0)) div) 
        (t (nsd x y (1- div)))
        )
  )
(defun op-maxsd (x y)
  (let (div)
    (if (< x y) (setf div x) (setf div y))
    (nsd x y div)
    )  
  )

(defun nsd-itr (x y)
  (do ((div (if (< x y) x y) (1- div)))
      ((and (zerop (mod x div)) (zerop (mod y div))) div)

  )
)

;;; Najmensi spolocny nasobok dvoch nezapornych celych cisel
;;;
;;;    (op-minsn 6 9) -> 18
;;;    (op-minsn 6 6) -> 6
;;;    
;;; Napoveda: riesenie zalozte na pouziti 'mod'
(defun op-minsn (x y &optional (acc 1))
  (cond ((= (mod (* x acc) y) 0) (* x acc))
        (t (op-minsn x y (1+ acc)))
        )
  )

(defun op-minsn (x y &optional (acc x))
  (cond ((= (mod acc y) 0) acc)
        (t (op-minsn x y (+ acc x)))
  )
)
;;; Odmocnina nezaporneho realneho cisla (s presnostou na 0.01)
;;;
;;;   (op-rodm 2.0) -> 1.4142
;;;
;;; Napoveda: riesenie zalozte na pouziti Newtonovej metody postupnych 
;;;   aproximacii:
;;;     Ak y je (nepresny) odhad korena cisla x, potom presnejsi 
;;;     odhad sa ziska ako priemer hodnot y a x/y.
;;;
;;; Alternativa: pozadovana presnost je vstupnym argumentom

(defun op-rodm (y &optional (acc 0.01) (x 1))
  (if (< (abs (- (* x x) y)) acc)
      (float x)
      (op-rodm y acc (/ (+ x (/ y x)) 2))
      )
  )

;;; Faktorial kladneho celeho cisla
;;;
;;;   (op-fakt 5) -> 120
;;;
;;; Napoveda: :-)

(defun op-fakt (x &optional (c (1- x)))
  (cond ((= x 0) 1) ;;; na konci rekurzie vratime 1 (ak x = 0)
        ((= c 0) x) ;;; ak c=0 vrátime x (výsledok)
        (t (op-fakt (* x c) (1- c)) ;;; vynasobime x a c a dekrementujeme c
           )
        )
  )
;;; Tlac rozkladu kladneho celeho cisla do suctu jednotlivych radov
;;;
;;;   (rozloz 2345) -> 2*10^3 + 3*10^2 + 4*10^1 + 5*10^0
;;;
;;; Napoveda: riesenie moze vyzadovat zistenie poctu cifier, z ktorych
;;;           sa cislo sklada
(defun rozloz (x &optional (stup 0) (digits ""))
  (cond ((zerop x) digits)
        ((= stup 0)
         (rozloz (floor (/ x 10))
                 (1+ stup)
                 (concatenate 'string
                              (format nil "~D*10^~D" (mod x 10) stup) digits))
         )
        (t (rozloz (floor (/ x 10))
                   (1+ stup)
                   (concatenate 'string
                                (format nil "~D*10^~D + " (mod x 10) stup) digits))
           ))
  )


;;; Tlac kladneho celeho cisla ako postupnosti jeho cifier
;;;
;;;   (cifry 2345) -> 2 3 4 5
;;;
;;; Napoveda: mozno budete potrebovat 'floor' a 'mod'
(defun cifry (x &optional (digits ""))
  (cond ((zerop x) digits)
        (t
         (cifry (floor (/ x 10))
                (concatenate 'string
                             (format nil "~D " (mod x 10)) digits))
         )
        )
  )

;;; Tlac kosostvorca v strede rastra rozmeru k x k (k je neparne
;;; kladne cele cislo)
;;;
;;;   (obraz 7) ->
;;;     . . . * . . .
;;;     . . * * * . .
;;;     . * * * * * .
;;;     * * * * * * *
;;;     . * * * * * .
;;;     . . * * * . .
;;;     . . . * . . .
;;;
;;; Napoveda: pocet hviezdiciek v riadku zavisi na cisle riadku
;;;           a celkovom pocte riadkov.

(defun obraz (num &optional (border (floor (/ num 2))) (og-num num))
  (cond ((= num 0) NIL)
        (t  (print  (concatenate 'string
                                 (make-string (abs border) :initial-element #\.)
                                 (make-string (- og-num (* 2 (abs border))) :initial-element #\*)
                                 (make-string (abs border) :initial-element  #\.)))
            (obraz (1- num) (1- border) og-num)
            )
        )
  )

;;; Rozklad nezaporneho celeho cisla na jeho sucinitele - sucinitele vytlacte
;;; do riadku za sebou
;;;
;;;   (rozklad 210)    -> 2 3 5 7
;;;   (rozklad 95095)  -> 5 7 11 13 19
;;;   (rozklad 123456) -> 2 2 2 2 2 2 3 643
;;;
;;; Napoveda: - pre tlac pouzite format
;;;           - pre kontrolu delitelnosti pouzite (mod 5 3) -> 2

(defun rozklad (x &optional (d 2) (res ""))
  (cond ((= x 1) res)
        ((zerop (mod x d))
         (rozklad (/ x d) d (concatenate 'string res
                                         (format nil "~D " d))))
        (t (rozklad x (1+ d) res))
        )
  )
