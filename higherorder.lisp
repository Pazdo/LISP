;;;; Poznamka:
;;;;   Nasledujuce priklady je mozne riesit pomocou pouzitia 
;;;;   funkcii vyssieho radu (bez nutnosti explicitneho pouzitia
;;;;   rekurzie alebo iteracie)
;;;;
;;;;   Alternativne riesenie je pouzit rekurziu alebo iteraciu
;;;;   bez nutnosti pouzitia funkcii vyssieho radu


;;; Generovanie postupnosti
;;;
;;;   (postupnost 5) -> (1 2 3 4 5)
;;;
;;; Napoveda: v rieseni nepouzivajte rekurziu ani iteraciu
;;;
;;; Alternativa: - generujte rad 1/1,1/2,...
;;;              - generujte rad c^(n-1),c^(n-2),...,c^1,c^0
;;;              - generujte postupnost (min, min+1, ..., max-1, max)
(reverse (maplist #'length (make-list 5))) ;;; -> Pomocou maplist a length vieme vytvoriť postupnosť 1 2 3 4 5, make-list vytvori zoznam 5 prvkov NIL, a maplist bude postupne pridavať dlžky podzoznamov. tj 5 4 3 2 1 -> potom musime dať reverse

(reverse (mapcar #'(lambda (x) (/ 1 x)) (maplist #'length (make-list 5)))) ;;; -> pomocou mapcar každý prvok predelíme 1/x (mapcar prechadza prvky a vrati novy zoznam premapovanych prvkov), reverse aby neboli opačne

(defun expseq (c n)
  (mapcar #'(lambda (x) (expt c (1- x))) ;;; rovnako ako predtym ale použijeme lambdu s expt a nepouživame reverse lebo ideme od najvyššieho čísla
          (maplist #'length (make-list n))
          )
  )
(defun minmaxseq (min max)
  (let ((min (1- min))) ;;; nastavime min na min-1 pretože mapcar zacina od 1
    (mapcar #'(lambda (x) (+ x min)) ;;; pridame ku kazdemu prvku min
            (reverse (maplist #'length (make-list (- max min))))) ;;; vygenerujeme prvky (1 2 3 4 5 ... n) kde n = (max - min) 
    )
  )
;;; Generovanie postupnosti (1 2 3 3 3 4 4 4 4 ...) podla jedneho
;;; zadaneho parametra
;;;
;;;   (postupnost 4) -> (1 2 2 3 3 3 4 4 4 4)
(defun postupnost (y)
  (mapcan #'(lambda (x) (make-list x :initial-element x)) ;;; -> Mapcan prejde prvky a pomocou maplist vytvorí zoznam s dlzkou x a elementami x, mapcan všetky podzoznami spojí do jedného
          (reverse (maplist #'length (make-list y)))) ;;; vygenerujeme zoznam dlzok (1 2 3 4... n)
  )

;;; Vypocet hodnoty polynomu
;;;
;;;   (polynom 2 '(1 3 0 2)) -> 22
;;;
;;; Napoveda: riesenie zalozte na pouziti 'reduce'

(defun polynom (c zoz)
  (reduce #'+  (mapcar (lambda (x y) (* c (expt x y))) ;;; 2 x (expt 1 3) + 2 x (expt 3 2) ... ;;; -> Tu sa využiva funkcionalita mapcar na viac zoznamoch, vezme sa pôvodny zoznam, novy zoznam exponentov a použije sa na nich lambda funkcia na vypočet mocniny
                       zoz ;;-> original list
                       (maplist (lambda (x) (1- (length x))) (make-list (length zoz)))) ;;;-> exponent list -> vytvorime zoznam exponentov, maplist zachovava strukturu pôvodneho zoznamu
          ))
;;; Zadany lubovolny pocet mnozin - najst prvky ktore su v kazdej mnozine
;;;
;;;   (op-intersection '((1 2 3 5) (2 3 4) (3 4 5 6))) -> (3)
;;;
;;; Napoveda: v rieseni nepouzivajte rekurziu ani iteraciu
;;;
;;; Alternativa: - najst prvky, ktore su iba v jednej z mnozin
;;;              - najst prvky, ktore su aspon v jednej z mnozin

(defun op-intersection (zoz)
  (remove-if-not (lambda (x) (every
                              (lambda (y) (member x y))
                              (cdr zoz)))
                 (car zoz))
  )
(defun op-union (zoz)
  (remove-if #'null
             (maplist (lambda (x) (if (null (member-if (lambda (y) (= (car x) y)) (cdr x)))
                            (car x)
                            NIL))
                      (apply #'append zoz)))
  
  )

(defun op-union (zoz)
  (remove-duplicates (apply #'append zoz))
  )

(defun op-xor (zoz)
  (let ((zoz (apply #'append zoz)))
    (remove-if (lambda (x) (> (count x zoz) 1)) zoz)
  )
)

;;; Je zadany zoznam bodov na ploche (reprezentovanych dvojprvkovymi 
;;; zoznamami). Najdite ten bod, ktory je najdalej od pociatku.
;;;
;;;   (bod-najdalej '((3 4)(1 2)(6 2)(4 2))) -> (6 2) 
;;;
;;; Napoveda: riesenie zalozte na pouziti 'reduce'
(defun dist (x y)
  (let ((a  (sqrt (+ (expt (first x) 2) (expt (second x) 2)))) ;;; vypočet vzdialenosti od bodu (0,0)
        (b  (sqrt (+ (expt (first y) 2) (expt (second y) 2))))
        )
    (if (> a b) x y) ;;; porovnavame vzdialenost prveho bodu s nasledujúcim, ak je vacsia vratime prvy bod, inak druhy, s tým bodom sa dalej pokracuje 
    ) 
  )
(defun bod-najdalej (body)
  (reduce #'dist body) ;;; použitie reduce na prejdenie všetkych bodov a porovnanie vzdialenosti
  )

;;; Urcenie hodnoty cisla z cifier pri zadanej ciselnej baze
;;;
;;;   (hodnota 10 '(2 0 1 1)) -> 2011
;;;
;;; Napoveda: v rieseni pouzivajte 'mapcar'
;;;           - pre mocnenie pouzite (expt 2 3) -> 8

(defun hodnota (base zoz)
  (let ((exps (maplist (lambda (x) (1- (length x)))  (make-list (length zoz))))) ;;; do exps sa uloží pole exponentov (n-1, n-2, ... 1, 0) kde n je dlžka zoznamu
    (reduce #'+ (mapcar (lambda (x y) (* x (expt base y))) zoz exps)) ;;; podobne ako pri polynomoch sa pomocou mapcar prechadzaju dve polia (zoz a exps) a pomocou lambdy sa vytvorí nový zoznam s mocninami, ktorý sa následne sčíta s využitim reduce
    
    )
  )

;;; Pre standardnu 'sudoku' plochu (9 riadkov, 9 stlpcov, 
;;; 9 stvorcov 3x3) generujte vsetky policka z ktorych
;;; pozostava n-ty riadok (cislovanie zacina od 0)
;;;
;;;   (op-riadok 3) -> ((3 0)(3 1)(3 2)(3 3)(3 4)(3 5)(3 6)(3 7)(3 8))
;;;
;;; Napoveda: riesenie zalozte na 'mapcar'
;;;
;;; Alternativa: policka stvorca x,y 
;;;   (op-stvorec 0 0) -> 
;;;       ((0 0)(0 1)(0 2)(1 0)(1 1)(1 2)(2 0)(2 1)(2 2))

(defun op-riadok (l)
  (mapcar #'(lambda (x) (list l (1- x))) (reverse (maplist #'length (make-list 9))))
  )

(defun op-stvorec (xs ys)
  (let ((x-vals  (reverse (maplist (lambda (x) (+ xs (1- (length x)))) (make-list 3))))
        (y-vals  (reverse (maplist (lambda (x) (+ ys (1- (length x)))) (make-list 3)))))
    (mapcan (lambda (x) (mapcar (lambda (y) (cons x (list y))) y-vals)) x-vals)
    )
  )

;;; Zo zoznamu navzajom roznych cisel odstrante tie cisla ktore kazia 
;;; vzostupne usporiadanie zoznamu
;;;
;;;   (urob-vzostup '(2 7 6 4 8 5 9)) -> (2 4 5 9)
;;;
;;; Napoveda: riesenie zalozte na pouziti 'remove-if' 
;;;           - na ziskanie zvysku zoznamu zacinajuceho nejakym 
;;;             cislom pouzite member
;;;

(defun urob-vzostup (zoz)
  (remove-if #'null
             (maplist (lambda (x) (if (null (member-if (lambda (y) (> (car x) y)) (cdr x)))
                                      (car x)
                                      NIL))
                      zoz))
  )
;; Alternativa: riesenie zalozte na 'remove-if-not'

(defun urob-vzostup2 (zoz)
  (remove-if-not #'numberp
                 (maplist (lambda (x) (if (null (member-if (lambda (y) (> (car x) y)) (cdr x)))
                                          (car x)
                                          NIL))
                          zoz))
  )

;;; Rozklad cisla na sucinitele, ak cislo pozostava z navzajom roznych
;;; sucinitelov
;;;
;;;   (rozklad 210) -> (2 3 5 7)
;;;
;;;
;;; Napoveda: v rieseni nepouzivajte rekurziu ani iteraciu
;;;           - pre kontrolu delitelnosti pouzite (mod 5 3) -> 2

(defun rozklad (cislo)
  (let ((cis cislo))
    (remove-if #'null
               (mapcar (lambda (x) (when (= (mod cis x) 0) (setf cis (/ cis x)) x))
                       (reverse (maplist (lambda (x) (1+ (length x))) (make-list (1- cislo)))))))
  )

;;; Zadane su dve mnoziny a (ne)rovnost - kontrola ci plati (ne)rovnost
;;;
;;;   (kontrola '(1 2 3 4 5) '(3 4 5 6 7) #'>)  -> nil
;;;   (kontrola '(4 5) '(3 4) #'>)              -> t
;;;
;;; Napoveda: riesenie zalozte na pouziti 'some' a 'every'  
(defun kontrola (zoz1 zoz2 fun)
  (every fun zoz1 zoz2) ;;; pomocou funkcie every sa prejdu všetky prvky zoznamu a porovnaju sa s druhym zoznamom pomocou fun (fun môže byť napr #'> kedy prvky musia byť väčšie ako v druhom zozname)
)

;;; Zadane dve mnoziny a (ne)rovnost - opravy aby platila (ne)rovnost
;;;
;;;  (oprava '(1 2 3 4 5) '(3 4 5 6 7) #'>)  -> (4 5) (3 4)
;;;  (oprava '(1 2 3 4 5) '(3 4 5 6 7) #'=)  -> (3 4 5) (3 4 5)
;;;
;;; Napoveda: riesenie zalozte na pouziti funkcii vyssieho radu
(defun oprava (zoz1 zoz2 fun)
  (values
   (remove-if  #'null (mapcar (lambda (x) (if (some (lambda (y) (funcall fun x y)) zoz2) x NIL)) zoz1))
   (remove-if #'null (mapcar (lambda (x) (if (some (lambda (y) (funcall fun y x)) zoz1) x NIL)) zoz2))
   )
  )
;;; Vytvarajte vsetky rozne dvojice
;;;
;;;   (dvojice '(a b c) '(1 2)) -> ((a 1) (a 2) (b 1) (b 2) (c 1) (c 2))
;;;
;;; Napoveda: riesenie zalozte na pouziti 'mapcar'

(defun dvojice (zoz1 zoz2)
  (mapcan (lambda (inner-val) (mapcar (lambda (outer-val) (cons inner-val (list outer-val))) zoz2)) zoz1)
  )

;;; Vytvarajte vsetky rozne kombinacie z prvkov zadaneho zoznamu
;;;
;;;   (kombinacie '(a b c)) -> ((a b c) (a b) (a c) (a) (b c) (b) (c) nil)
;;;   (kombinacie '())      -> (nil)
;;;
;;; Napoveda: riesenie zalozte na sucasnom pouziti 'mapcar' a rekurzie
;;; - pre odhalenie rekurzivneho pravidla porovnajte vystup
;;;     pre zoznam (a b c) a zoznam (b c)

;;v1 - z netu, pouziva mapcan, funguje ale dava ine poradie
(defun kombinacie (zoz)
  (if (null zoz) (list NIL)
      (mapcan (lambda (x)  (list (cons (car zoz) x) x)) (kombinacie (cdr zoz)))
      ) 
  )

;;v2, jano-inspired
(defun kombinacie-wrap (zoz)
  (let ((result NIL))
    (kombinacie zoz result)
    )
  )
(defun kombinacie (zoz result)
  (if (null zoz) (list NIL)
      (append (mapcar #'cons
                      (make-list (expt 2 (1- (length zoz))) :initial-element (car zoz))
                      (setf result (kombinacie (cdr zoz) result)))
              result)
      )
  )


;;; Otacanie matice zadanej po riadkoch o 90 stupnov v smere a 
;;; proti smeru hodinovych ruciciek
;;;
;;;    (otoc+ '((1 2 3)(4 5 6)(7 8 9))) ->
;;;        ((7 4 1)(8 5 2)(9 6 3))
;;;    (otoc- '((1 2 3)(4 5 6)(7 8 9))) ->
;;;        ((3 6 9)(2 5 8)(1 4 7))
;;;
;;; Napoveda: maticu najprv transponujte (vymente riadky za stlpce)
;;;           a nasledne upravte zmenou poradia prvkov
(defun transp (matica &optional res)
  (cond ((every #'null matica) res)
        (t (transp (mapcar #'cdr matica) (append res (list (mapcar #'car matica)))))
        )
)

(defun otoc+ (matica)
  (mapcar #'reverse (transp matica))
  )
(defun otoc- (matica)
  (transp (mapcar #'reverse matica))
  )
