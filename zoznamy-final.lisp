;;;; Poznamka:
;;;;   Nasledujuce priklady je mozne riesit pomocou rekurzie
;;;;   (mozne pocitanie smerom dnu aj von)
;;;;
;;;;   Alternativne riesenie je pouzit funkcie pre pracu so zoznamami
;;;;   bez rekurzivnej struktury.


;;; Pocetnost vyskytu prvku v zozname
;;;
;;;   (op-vyskyt 3 '(1 3 4 3 2)) -> 2
;;;   (op-vyskyt 5 '(1 3 4 3 2)) -> 0
;;;
;;; Napoveda: riesenie zalozte na pouziti 'car' a 'cdr'
(defun op-vyskyt (x zoz &optional (acc 0))
  (cond ((null zoz) acc) ;;; ak sme na konci zoznamu, vratime počet vyskytov (acc)
        ((= (car zoz) x) (op-vyskyt x (cdr zoz) (1+ acc))) ;;; car vráti prvý prvok zoznamu, ak sa prvok rovná x, zavoláme funkciu so zvyškom zoznamu (cdr zoz) a inkrementujeme akumulator
        (t (op-vyskyt x (cdr zoz) acc)) ;;; ak sa prvok nerovná, zavolame funkciu so zvyškom zoznamu a akumulator ostava pôvodný
  )
)
;;; Sucet cisel v zozname
;;;
;;;   (op-sucet '(1 2 3 4)) -> 10
;;;
;;; Napoveda: riesenie zalozte na pouziti 'car' a 'cdr'
;;;
;;; Alternativa: zoznam moze obsahovat aj neciselne prvky ktore sa do
;;;              suctu nezahrnaju

;;; sucet všetkych prvkov
(defun op-sucet (zoz &optional (counter 0))
  (if (null zoz) counter ;;; ak je zoznam prázdny vrátime counter
      (op-sucet (cdr zoz) (+ counter (car zoz))) ;;; zavolame funkciu so zvyškom zoznamu a pripočítame ku counteru prvok
  )
)

(defun op-sucet2 (zoz &optional (counter 0))
  (cond ((null zoz) counter)
        ((numberp (car zoz)) (op-sucet2 (cdr zoz) (+ counter (car zoz)))) ;;; zvýši counter iba ak platí podmienka numberp (prvok je číslo)
        (t (op-sucet2 (cdr zoz) counter)) ;;; ak neplatí tak volame funkciu s nezmeneným counterom
  )
)

;;; Test pritomnosti prvku v zozname
;;;
;;;   (op-member 3 '(1 2 3 4)) -> (3 4)
;;;   (op-member 5 '(1 2 3 4)) -> nil
;;;
;;; Napoveda: riesenie zalozte na pouziti 'car' a 'cdr'

(defun op-member (x zoz)
  (cond ((null zoz) nil) ;;; ak sme prešli cely zoznam vratime nil
        ((= x (car zoz)) zoz) ;;; ak je prvok v zozname vratime zbytok zoznamu vratane prveho vyskytu prvku
        (t (op-member x (cdr zoz))) ;;; ak prvko sa nerovná, volame funkciu so zvyškom zoznamu
  )
)

;;; Pristup k n-temu prvku zoznamu
;;;
;;;   (op-nth 3 '(1 2 3 4 5)) -> 4
;;;   (op-nth 5 '(1 2 3))     -> nil
;;;
;;; Napoveda: riesenie zalozte na pouziti 'car' a 'cdr'

(defun op-nth (x zoz &optional (acc 0))
  (cond ((null zoz) nil) ;;; vrati nil ak sme na konci zoznamu
        ((= acc x) (car zoz)) ;;; ak sme na požadovanej pozicii vratime prvok zoznamu
        (t (op-nth x (cdr zoz) (1+ acc))) ;;; ak nie, rekurzivne volame funkciu so zvyškom zoznamu a zväčšíme akumulator

  )
)

;;; Obratenie poradia prvkov zoznamu
;;;
;;;   (op-reverse '(1 2 3 4)) -> (4 3 2 1)
;;;
;;; Napoveda: riesenie zalozte na pouziti 'car', 'cdr' a 'cons'

(defun op-reverse (zoz &optional acc)
  (if (null zoz) acc
      (op-reverse (cdr zoz) (cons (car zoz) acc))) ;;; cons do zoznamu acc pridá na začiatok prvý prvok zoznamu zoz, tym že ideme do rekurzie, tak sa začína s prazdnym zoznamom v acc a postupne sa pridavaju prvky v obratenom poradí
  )

;;; Spojenie dvoch zoznamov do jedneho zoznamu
;;;
;;;   (op-append '(1 2 3) '(4 5 6 7)) -> (1 2 3 4 5 6 7)
;;;
;;; Napoveda: riesenie zalozte na pouziti 'car', 'cdr' a 'cons'

(defun op-append (arr1 arr2 &optional zoz)
  (cond ((and (null arr1) (null arr2)) (reverse zoz)) ;;; ak sme pridali všetky prvky, obratime zoznam pomocou reverse
        ((null arr1) (op-append arr1 (cdr arr2) (cons (car arr2) zoz))) ;;; ak je arr1 nulové rekurzivne pridávame prvky z arr 2
        (t (op-append (cdr arr1) arr2 (cons (car arr1) zoz))) ;;; ak sú obe naplnené, tak pridávame prvky z arr1 do vysledného zoznamu
  )
)

;;; Spristupnenie posledneho prvku zoznamu
;;;
;;;   (op-last '(1 2 3 4)) -> 4
;;;
;;; Napoveda: riesenie zalozte na pouziti 'car', 'cdr' a 'length'

(defun op-last (zoz)
  (cond ((= (length zoz) 1) (car zoz)) ;;; ak sme na poslednom prvku  zoznamu tak vrátime prvok
        (t (op-last (cdr zoz))) ;;; ak ešte niesme, tak rekurzivne volame funkciu so zvyškom zoznamu
  )
)
;;;
;;; Alternativa: bez pocitania dlzky zoznamu

(defun op-last2 (zoz &optional last)
  (cond ((null zoz) last) ;;; ak je pole prázdne (vybrali sme všetky prvky) vratime premennú last
        (t (op-last2 (cdr zoz) (car zoz))) ;;; do pomocnej premennej last si vždy ukladame posledny prvok zoznamu
  )
)
;;;
;;; Alternativa2: Spristupnenie zoznamu okrem posledneho prvku
;;;
;;;   (op-butlast '(1 2 3 4)) -> (1 2 3)

(defun op-butlast (zoz &optional rest)
  (cond ((= (length zoz) 1) (reverse rest)) ;;; ak sme na poslednom prvku zoznamu tak vrátime zoznam rest
        (t (op-butlast (cdr zoz) (cons (car zoz) rest))) ;;; do rest si postupne pridávame prvky zoznamu
  )
)

;;; Vlozenie prvku na n-tu poziciu
;;;
;;;   (op-vloz 0 2 '(1 3 5 7)) -> (1 3 0 5 7)
;;;
;;; Napoveda: riesenie zalozte na pouziti 'car', 'cdr' a 'cons'

(defun op-vloz (what where arr &optional zoz (acc 0))
  (cond ((null arr) (reverse zoz)) ;;; novy zoznam otočíme ak sme na konci pôvodného
        ((= where acc) (op-vloz what where arr (cons what zoz) (1+ acc))) ;;; ak sa pozícia rovná požadovanej pozícii pridáme prvok what do zoznamu pomocou cons
        (t (op-vloz what where (cdr arr) (cons (car arr) zoz) (1+ acc))) ;;; ak je pozícia iná ako požadovaná tak pridáme prvok z pôvodného zoznamu
  )
)

;;; Vlozenie prvku do zoznamu za/pred vzor
;;;
;;;   (op-vlozLR 0 3 :za '(1 2 3 4 3)) -> (1 2 3 0 4 3)
;;;   (op-vlozLR 0 3 :pred '(1 2 3 4)) -> (1 2 0 3 4)
;;;
;;; Napoveda: riesenie zalozte na pouziti 'car', 'cdr' a 'cons'
(defun op-vlozLR (x vzor how zoz &optional new)
  (cond ((null zoz) (reverse new))
        ((and (eq (car zoz) vzor) (eq how :pred)) ;;; ak prvok zoznamu je rovnaký ako vzor a how je :pred tak pridáme prvok x pred vzor
         (op-vlozLR x vzor :pred (cdr zoz) (append (list (car zoz) x) new)) ;;; append pridá prvok x na začiatok zoznamu new, append pridá všetky prvky
         )
        ((and (eq (car zoz) vzor) (eq how :za))
         (op-vlozLR x vzor :za (cdr zoz) (append (list x (car zoz)) new)) ;;; rovnako ako pred, iba sa v append vymenilo poradie
         )
        (t (op-vlozLR x vzor how (cdr zoz) (cons (car zoz) new))) ;;; default pridávame postupne všetky prvky zoznamu pomocou cons a car
  )
)
;;;
;;; Alternativa: - uvazovat vsetky vyskyty vzoru
;;;              - uvazovat iba urcity pocet prvych vyskytov
;;;              - uvazovat iba parne vyskyty
(defun op-vlozLR2)

;;; Redukcia viacnasobneho vyskytu prvkov v zozname
;;;
;;;   (op-redukcia '(1 2 1 4 2 3 5 1)) -> (1 2 4 3 5)
;;;
;;; Napoveda: riesenie zalozte na pouziti 'car' a 'cdr'

(defun op-redukcia (zoz &optional arr)
  (cond ((null zoz) (reverse arr))
        ((not (member (car zoz) arr)) ;;; ak prvok zoznamu nie je v zozname arr tak ho pridáme
         (op-redukcia (cdr zoz) (cons (car zoz) arr))) ;;; zavolame funkciu a prvok pridame do noveho arr pomocou cons
        (t (op-redukcia (cdr zoz) arr)) ;;; ak je prvok v zozname tak ho preskocime 
  )
)

;;; Prienik dvoch zoznamov (vysledny zoznam bude obsahovat iba
;;; tie prvky, ktore sa nachadzaju v oboch zoznamoch)
;;;
;;;   (op-prienik '(1 2 3) '(2 3 4)) -> (2 3)
;;;
;;; Napoveda: riesenie zalozte na pouziti 'car' a 'cdr'
;;;
;;; Alternativa: - zjednotenie (prvky nachadzajuce sa aspon v
;;;                    jednom zozname)
;;;              - xor (prvky nachadzajuce sa iba v jednom zozname)

(defun is-in (x zoz) 
  (cond ((null zoz) NIL) ;;; ak sme na konci tak nil
        ((= (car zoz) x) T) ;;; ak je prvok v zozname vratime T
        (t (is-in x (cdr zoz))) ;;; rekurzivne prechadzame prvyk zoznamu
  )
)
(defun op-prienik (zoz1 zoz2 &optional new)
  (cond ((null zoz1) (reverse new))
        ((is-in (car zoz1) zoz2) ;;; zistujeme ci je prvok zo zoz1 v zozname zoz2 ak ano tak pomocou cons pridáme do nového zoznamu
         (op-prienik (cdr zoz1) zoz2 (cons (car zoz1) new))
        )
        (t (op-prienik (cdr zoz1) zoz2 new)) ;;; ak prvok nie je druhom zozname, preskočíme
  )
  )

(defun op-zjednotenie (zoz1 zoz2 &optional new)
  (cond ((not (null zoz2)) (op-zjednotenie (append zoz1 zoz2) NIL new)) ;;; ak v zozname 2 mame prvky, pridame ich od zoz1 a zavolame funkciu
        ((null zoz1) (reverse new)) ;;; ak je zoz1 prazdne vratime nove pole
        ((not (is-in (car zoz1) new)) ;;; ak prvok pôvodných zoznamov ešte nie je v novom, tak ho pridáme pomocou cons
         (op-zjednotenie (cdr zoz1) zoz2 (cons (car zoz1) new))) ;;; (cdr zoz1) -> pokračujeme so zvyškom zoznamu
        (t (op-zjednotenie (cdr zoz1) zoz2 new)) ;;; ak sa prvok nachádza tak ho preskakujeme

  )
  )

(defun xor-helper (p z &optional res)
  (cond ((null z) res)
        ((is-in (car z) p) (xor-helper p (cdr z) res))
        (t (xor-helper p (cdr z) (cons (car z) res)))
  )
)

(defun xor (zoz1 zoz2)
  (reverse (xor-helper (op-prienik zoz1 zoz2) (op-zjednotenie zoz1 zoz2)))
)

;;; Opakovanie prvkov zoznamu
;;;
;;;   (op-opakovanie 3 '(1 2 3)) -> (1 1 1 2 2 2 3 3 3)
;;;
;;; Napoveda: riesenie zalozte na pouziti 'car' a 'cdr'

(defun op-opakovanie (x zoz &optional arr (counter 1)) 
  (cond ((null zoz) (reverse arr))
        ((= counter x) (op-opakovanie x (cdr zoz) (cons (car zoz) arr) 1)) ;;; ak sme prvok zopakovali x krat tak ideme na další a counter vyresetujeme 
        (t (op-opakovanie x zoz (cons (car zoz) arr) (1+ counter))) ;;; ak nie tak prvok pridáme do zoznamu a zvýšime counter
  )
)
;;;
;;; Alternativa:
;;;   (op-opakovanie2 3 '(1 2 3)) -> (1 2 3 1 2 3 1 2 3)

(defun op-opakovanie2 (x zoz &optional arr (counter 0) (backup zoz))
  (cond ((= counter x) (reverse arr))
        ((null zoz) (op-opakovanie2 x backup arr (1+ counter) backup))
        (t (op-opakovanie2 x (cdr zoz)
                           (cons (car zoz) arr) counter  backup))
  )
)

;;; Rozdelenie zoznamu na podzoznamy podla vzoru
;;;
;;;   (op-rozdel 3 '(1 2 3 4 3 2 1)) -> ((1 2) (4) (2 1))
;;;
;;; Napoveda: riesenie zalozte na pouziti 'car' a 'cdr'

(defun op-rozdel (vzor zoz &optional fin new) ;;; -> fin je finalny zoznam zoznamov, new je pomocny podzoznam
  (cond ((null zoz) (reverse (cons (reverse new) fin))) ;;; na konci vratime finalny zoznam ku ktoremu pridáme posledný podzoznam
        ((eq (car zoz) vzor) ;;; -> porovnávame či sa prvý prvok rovná vzoru
         (op-rozdel vzor (cdr zoz) (cons (reverse new) fin) NIL)) ;;; -> ak sa rovná do finalneho zoznamu pomocou cons pridame nový podzoznam a "new" inicializujeme na nil (prazdny zoznam)
        (t (op-rozdel vzor (cdr zoz) fin (cons (car zoz) new))) ;;; -> ak ešte nie sme pri vzore, do podzoznamu "new" pridávame prvky zaradom
  )
)

;;; Kompresia zoznamu
;;;
;;;   (op-kompres '(1 1 2 0 0 0 1 2 2)) -> ((2 1) 2 (3 0) 1 (2 2))
;;;
;;; Napoveda: riesenie zalozte na pouziti 'car' 'cdr'
;;;
;;; Alternativa: Dekompresia zoznamu (reverzna operacia)
(defun op-kompres (zoz &optional res (item (car zoz)) (count 0))
  (cond ((and (null zoz) (< count 2)) (reverse (cons item res)))
        ((null zoz) (reverse (cons (list count item) res)))
        ((= (car zoz) item) (op-kompres (cdr zoz) res item (1+ count)))
        ((= count 1) (op-kompres zoz (cons item res) (car zoz) 0))
        (t (op-kompres zoz (cons (list count item) res) (car zoz) 0))      
  )
)

;;; dekompresia
(defun add_to_list (x zoz times)
  (cond ((= times 0) zoz)
        (t (add_to_list x (cons x zoz) (1- times)))
  )
)
(defun op-dekompres (zoz &optional res)
  (cond ((null zoz) (reverse res))
        ((listp (car zoz))
         (op-dekompres (cdr zoz) (add_to_list (second (car zoz)) res (first (car zoz)))))
        (t (op-dekompres (cdr zoz) (cons (car zoz) res)))
  )
)
;;; Zotriedenie zoznamu
;;;
;;;   (op-sort '(3 2 4 1 5)) -> (1 2 3 4 5)
;;;
;;; Napoveda: riesenie zalozte na pouziti 'car' a 'cdr'
;;;
;;; Algoritmus:
;;;   Zo zonamu sa vyberie prvok P a zoznam sa rozdeli na dva
;;;   zoznamy: M (prvky mensie ako P) a V (prvky vacsie ako P).
;;;   M a V sa zotriedia a vysledny zoznam vznikne spojenim 
;;;   M + P + V


(defun select (zoz elem fun)
  (cond ((null zoz) NIL)
        ((funcall fun (car zoz) elem) (cons (car zoz) (select (cdr zoz) elem fun)))
        (t (select (cdr zoz) elem fun))
  )
)

(defun op-sort (zoz)
  (cond ((null zoz) NIL)
        (t (append
            (op-sort (select zoz (car zoz)  #'<))
            (select zoz (car zoz)  #'=)
            (op-sort (select zoz (car zoz)  #'>))
           ))
        )
)
;;; Transpozicia matice zadanej ako zoznam riadkov
;;;
;;;    (transp '((1 2 3)(4 5 6)(7 8 9))) -> ((1 4 7)(2 5 8)(3 6 9))
;;;
;;; Napoveda: riesenie zalozte na pouziti 'car' a 'cdr'
(defun getter (fun matica &optional res)
  (if (null matica)
      (reverse res)
      (getter fun (cdr matica) (cons (funcall fun (car matica)) res))
  )
)
(defun transp (matica &optional res)
  (cond ((null (car matica)) (reverse res))
        (t (transp (getter #'cdr matica) (cons (getter  #'car matica) res)))
  )
)

;;; Obojsmerny pohyb v zozname - prvy argument udava startovaciu 
;;; poziciu v zozname, selektovany prvok v zozname udava dalsi 
;;; posun v zozname
;;;
;;;   (vyber 3 '(2 9 4 1 6 3 2 7 6 5 2)) -> (1 6 2)
;;;   (vyber 0 '(2 9 4 1 6 3 2 7 6 5 2)) -> (2 4 2 6)
;;;   (vyber 2 '(2 9 4 1 6 -3 -2 7 6 5 -2)) -> (4 -2 6 -2 6)
;;;
;;; Napoveda: zoznam si reprezentujte ako trojprvkovu strukturu
;;;           (a b c), kde
;;;             - b je aktualny prvok 
;;;             - a je zoznam prvkov nachadzajucich sa pred
;;;                 aktualnym prvkom (v reverzovanom pradi)
;;;             - c je zoznam prvkov nachadzajucich sa za
;;;                 aktualnym prvkom
;;;           riesenie zalozte na pouziti 'car' a 'cdr'

(defun get-el (idx zoz &optional (act-idx 0))
  (cond ((= idx act-idx) (car zoz))
        (t (get-el idx (cdr zoz) (1+ act-idx)))
        )
)
(defun vyber (idx zoz &optional res)
  (let ((new (get-el idx zoz)))
    (cond ((null new) (reverse res))
          (t (vyber (+ idx new) zoz (cons new res)))
    )
  )
)

;;; Rozdelenie zoznamu na podzoznamy podla zotriedenia, kde 
;;; v kazdom podzozname cisla musia byt zotriedene vzostupne 
;;; alebo zostupne. Susedne podzoznamy sa prekryvaju v jednom
;;; prvku (posledny prvok podzoznamu je zaroven prvym prvkom 
;;; nasledujuceho podzoznamu) 
;;;
;;;   (rozdel '(1 2 3 -1 -3 6 2 9)) -> 
;;;                       ((1 2 3)(3 -1-3)(-3 6)(6 2)(2 9))
;;;
;;; Napoveda: riesenie zalozte na pouziti 'car' a 'cdr'

(defun rozdel (zoz &optional act res sign)
  (cond ((null zoz) (reverse (cons (reverse act) res)))
        ((and (null sign) (< (length act) 2))
         (rozdel (cdr zoz) (cons (car zoz) act) res sign))
        ((null sign)
         (if (< (first act) (second act))
             (rozdel zoz act res #'<)
             (rozdel zoz act res #'>)
         ))
        ((funcall sign (car zoz) (car act))
         (rozdel (cdr zoz) (cons (car zoz) act) res sign))
        (t (rozdel zoz (list (car act)) (cons (reverse act) res) NIL)

        )))
;;; Vyhladenie zoznamu s lubovolnym vnorenim podzoznamov 
;;; do tvaru zoznamu hodnot
;;;
;;;   (vyhlad '((1) 2 ((3 (4) ((5) 6)) 7))) -> (1 2 3 4 5 6 7)
;;;
;;; Napoveda: riesenie zalozte na pouziti 'car' a cdr'

;;;tree>list verzia
(defun vyhlad (zoz &optional res)
  (cond ((null zoz) res)
        ((listp zoz) (vyhlad (car zoz) (vyhlad (cdr zoz) res)))
        (t (vyhlad NIL (cons zoz res)))
        )
  )
;;; kinda weird
(defun vyhlad-worker (zoz &optional res)
  (cond ((null zoz) res)
        ((listp (car zoz))
         (vyhlad-worker (cdr zoz) (append (vyhlad-worker (car zoz) NIL) res))
        )
        (t (vyhlad-worker (cdr zoz) (append (list (car zoz)) res)))
  )
  )
(defun vyhlad (zoz)
  (reverse (vyhlad-worker zoz))
)
;;; Vytvarajte alternativne podoby zoznamu, kde vzdy pouzijete
;;; iba jednu z povolenych alternativ
;;;
;;;   (alternativy '(a (b1 b2) c (d1 d2 d3) f) ->
;;;          ((a b1 c d1 f) (a b1 c d2 f) (a b1 c d3 f)
;;;           (a b2 c d1 f) (a b2 c d2 f) (a b2 c d3 f))
;;;
;;; Napoveda: nesnazte sa ulohu vyriesit definovanim iba jednej
;;;           funkcie
;;;           riesenie zalozte na pouziti 'car' a 'cdr'

(defun get-count (zoz &optional (count 1))
  (cond ((null zoz) count)
        ((listp (car zoz)) (get-count (cdr zoz) (* count (length (car zoz)))))
        (t (get-count (cdr zoz) count))
  )
  )
(defun make-lists (count &optional res)
  (if (= count 0) res (make-lists (1- count) (cons (list) res))) 
  )

(defun add-element (elem zozs &optional res)
  (cond ((null zozs) res)
        (t (add-element elem (cdr zozs) (cons (cons elem (car zozs)) res)))
  )
 )

(defun get-nth (n zoz &optional (counter 0))
  (cond ((null zoz) NIL)
        ((= counter n) (car zoz))
        (t (get-nth n (cdr zoz) (1+ counter)))
  )
)
(defun add-alternatives (alts zozs &optional res (counter 0))
  (cond ((null zozs) res)
        ((= counter (length alts)) (add-alternatives alts zozs res 0))
        (t (add-alternatives alts (cdr zozs)
                             (cons (cons (get-nth counter alts) (car zozs)) res)
                             (1+ counter))))
)
(defun reverse-elems (zoz &optional res)
  (if (null zoz) res (reverse-elems (cdr zoz) (cons (reverse (car zoz)) res)))
)
(defun alternativy (zoz &optional (res (make-lists (get-count zoz))))
  (cond ((null zoz) (reverse-elems res))
        ((listp (car zoz)) (alternativy (cdr zoz) (add-alternatives (car zoz) res)))
        (t (alternativy (cdr zoz) (add-element (car zoz) res)))
   )
)




(defun make-all-list (l)
  (cond ((null l) nil)
        ((not (listp (car l))) (cons (list (car l)) (make-all-list (cdr l))))
        (t (cons (car l) (make-all-list (cdr l))))))


(defun alternativy (l)
  (let ((opts nil))
    (labels ((alt (z &optional (my-part (car z)) (prev-part nil))
               (cond ((null z) nil)
                     ((null my-part) nil)
                     ((null (cdr z)) (setf opts (append opts (list (reverse (cons (car my-part) prev-part))))) (alt z (cdr my-part) prev-part))
                     (t (alt (cdr z) (cadr z) (cons (car my-part) prev-part)) (alt z (cdr my-part) prev-part)))))
      (alt (make-all-list l))
      opts)))
