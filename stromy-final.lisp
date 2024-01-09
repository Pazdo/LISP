;;;; Poznamka:
;;;;   Nasledujuce priklady je mozne riesit pomocou pouzitia 
;;;;   rekurzie


;;; Maximalna hlbka listoveho uzlu v strome
;;;
;;;   (tree-hlbka '(((a b) c) (a (s (f) g) q))    ) -> 4
;;;   (tree-hlbka '(((a b) c) (a (s () g) q))     )  -> 3
;;; 
;;; Napoveda: - pre zistenie maximalnej hpodnoty pouzite 'max'
;;;           - zapis (((a b) c) (a (s (f) g) q)) reprezentuje strom
;;;
;;;             +
;;;             |
;;;         +---+---+
;;;         |       |
;;;       +-+-+  +--+--+
;;;       |   c  a  |  q
;;;     +-+-+     +-+-+ 
;;;     a   b     s | g
;;;                 +
;;;                 f

(defun tree-hlbka (strom &optional (h 0))
  (cond ((null strom) h) ;;; ak je strom prazdny, vrat hlbku
        ((listp strom) (max (tree-hlbka (car strom) (1+ h)) (tree-hlbka (cdr strom) h))) ;;; -> strom je zadany ako vnorene dvojprvkove zoznamy, tj vyskusame hladat hlbku na prvom prvku (car) a na druhom prvku (zbytku) (cdr), pomocou max sa potom porovna ktora je väčšia (pri ktorej vetve)
        (t (tree-hlbka NIL h)) ;;; Ak sme došli na hodnotu (tj sme na liste) tak vratime hlbku 
  )
)

;;; Vytvorenie zoznamu listovych uzlov stromu
;;;
;;;   (tree>list '(((a b) c) (a (s (f) g) q))) -> (a b c a s f g q)
;;;   (tree>list '(((a b) c) (a (s () g) q)))  -> (a b c a s g q)
;;;
;;; Napoveda: zapis (((a b) c) (a (s (f) g) q)) reprezentuje strom
;;;
;;;             +
;;;             |
;;;         +---+---+
;;;         |       |
;;;       +-+-+  +--+--+
;;;       |   c  a  |  q
;;;     +-+-+     +-+-+ 
;;;     a   b     s | g
;;;                 +
;;;                 f

(defun tree>list (strom &optional res)
  (cond ((null strom) res)
        ((listp strom) (tree>list (car strom) (tree>list (cdr strom) res))) ;;; ak sme na vetve tak do stromu pridame zoznam z vetvy, a do res pridame vysledok z volania funkcie na zvyšok
        (t (tree>list NIL (cons strom res))) ;;; ak sme na liste tak pridame do res hodnotu listu

  )
)

;;; Test pritomnosti prvku v strome
;;;
;;;   (tree-member 5 '(1 (2 (4 a b) c) (3 a (5 s (6 f) g) q))) -> (5 s (6 f) g)
;;;   (tree-member 0 '(1 (2 (4 a b) c) (3 a (5 s (6 f) g) q))) -> nil
;;;
;;; Napoveda: zapis (1 (2 (4 a b) c) (3 a (5 s (6 f) g) q)) reprezentuje strom
;;;               1
;;;          +----+----+
;;;          2         3
;;;        +-+-+   +---+---+
;;;        4   c   a   5   q
;;;      +-+-+       +-+-+
;;;      a   b       s 6 g
;;;                    +
;;;                    f

(defun tree-member (elem strom &optional lastup) 
  (cond ((null strom) NIL)
        ((listp strom) (or (tree-member elem (car strom) strom) (tree-member elem (cdr strom) strom))) 
        ((eq elem strom) lastup) ;;; ak je element v danom strome tak vratime vetvu ktorá začína v danom node (nody maju hodnoty)
  )
)
  
;;; Aritmeticke vypocty v infixnej notacii
;;;
;;;   (tree-calc '((3 * 5) + 9))             -> 24
;;;   (tree-calc '((3 * 5) + (9 - (2 * 6)))) -> 12
;;;  
;;; Napoveda: - pre testovanie ci objekt je cislo pouzite 'numberp'
;;;           - symboly '*', '-' a '*' referuju na prislusnu funkciu
;;;             svojim 'fvalue' slotom

(defun tree-calc (strom)
  (cond ((numberp strom) strom) ;;; -> ak sme na konci vyrazu vratime iba číslo (nie je to už zoznam (nr1 operation nr2))
        ((listp strom) (funcall (second strom) (tree-calc (first strom)) (tree-calc (third strom)))) ;;; zavolame funkciu ktora je na druhej pozicii zoznamu a ako parametre vysledok z funkcie kde ide prvy prvok stromu a vysledok funkcie kde ide druhy prvok
  )
)
  
;;; Prechadzanie uzlami binarneho stromu
;;;
;;;   (preorder '(1 2 (3 (5 8 9) (6 4 7)))  )  -> (1 2 3 5 8 9 6 4 7)
;;;   (inorder '(1 2 (3 (5 8 9) (6 4 7)))  )   -> (2 1 8 5 9 3 4 6 7)
;;;   (postorder '(1 2 (3 (5 8 9) (6 4 7)))  ) -> (2 8 9 5 4 7 6 3 1)
;;;
;;; Napoveda: zapis (1 2 (3 (5 8 9) (6 4 7))) reprezentuje strom
;;;   
;;;         1
;;;     +---+---+
;;;     2       3
;;;         +---+---+
;;;         5       6
;;;       +-+-+   +-+-+
;;;       8   9   4   7

(defun preorder (strom &optional res)
  (cond ((null strom) res) ;;; ak sme na konci stromu vratime res 
        ((numberp strom) (preorder NIL (cons strom res))) ;;; ak je hodnota v strome číslo (dalej sa nevetví) tak zavoláme preorder kde podstrom je NIL a res je hodnota z listu + zbytok
        (t (preorder (car strom) (preorder (cdr strom) res))) ;;; ak ešte niesme na leafe tak zavolame preorder na prvy prvok (leaf) a další preorder na zbytok stromu (mali by tam byť dve zoznamy (<- lava a -> prava strana stromu))
  )
  )

(defun inorder (strom &optional res)
  (cond ((null strom) res)
        ((listp strom) (inorder (third strom) (inorder (first strom) (inorder (second strom) res))))
        (t (inorder NIL (append res (list strom))))
  )
  )

(defun preorder (strom)
  (cond ((null strom) NIL)
        ((not (listp strom)) (list strom))
        (t (append (preorder (car strom))
                   (preorder (cdr strom)))
         )
  )
  )

(defun inorder (strom)
  (cond ((null strom) NIL)
        ((not (listp strom)) (list strom))
        (t (append (inorder (cadr strom))
                   (inorder (car strom))
                   (inorder (caddr strom))
           )
         )
  )
)
(defun postorder (strom)
  (cond ((null strom) NIL)
        ((not (listp strom)) (list strom))
        (t (append (postorder (cadr strom))
                   (postorder (caddr strom))
                   (postorder (car strom))
            )
        )
  )
)
;;; Transformacia stromu (zmena reprezentacie listovych uzlov
;;;
;;;   (dopln '(2 (1 nil nil) (6 (4 nil (5 nil nil)) (8 nil nil)))) ->
;;;         (2 1 (6 (4 nil 5) 8))
;;;   
;;;         2
;;;     +---+---+
;;;     1       6
;;;         +---+---+
;;;         4       8
;;;         +-+
;;;           5
;;;
;;; Napoveda: -
;;;
;;; Alternativa: opacna transformacia

;;;checks if we have an ending branch
(defun no-leafs (elem-cdr)
  (cond ((null elem-cdr) T)
        ((not (null (car elem-cdr))) NIL)
        (t (no-leafs (cdr elem-cdr)))
  )
)

(defun dopln (strom &optional res)
  (cond ((null strom) (reverse res))
        ((not (listp (car strom))) (dopln (cdr strom) (cons (car strom) res)))
        ((no-leafs (cdr (car strom))) (dopln (cdr strom) (cons (car (car strom)) res)))
        (t (dopln (cdr strom) (cons (dopln (car strom) NIL) res)))
        )
)

(defun oddopln (strom &optional (check -1))
  (cond ((null strom) nil)
        ((and (> check 0) (numberp strom)) (list strom nil nil))
        ((zerop check) strom)
        (t (list (oddopln (car strom) 0) (oddopln (second strom) 1) (oddopln (third strom) 1)))
        )
  )
;;; Budovanie binarneho stromu
;;;
;;;   (list>strom '(2 6 4 1 5 8)) ->
;;;            (2 (1 nil nil) (6 (4 nil (5 nil nil)) (8 nil nil)))  
;;;   
;;;         2
;;;     +---+---+
;;;     1       6
;;;         +---+---+
;;;         4       8
;;;         +-+
;;;           5
;;;
;;; Napoveda: - pouzite rekurziu
;;;           - rozlozte na dve podulohy: pridanie prvku do stromu
;;;             a vytvorenie stromu
;;;
;;; Alternativa: podulohu vytvorenia stromu rieste pomocou 'reduce'

(defun compare (zoz item fun &optional res)
  (cond ((null zoz) (reverse res))
        ((funcall fun (car zoz) item) (compare (cdr zoz) item fun (cons (car zoz) res)))
        (t (compare (cdr zoz) item fun res))
  )
)
(defun list>strom (strom)
  (cond ((null strom) NIL)
        (t (cons  (car strom)
                  (list (list>strom (compare strom (car strom) #'<))
                        (list>strom (compare strom (car strom) #'>))))
        )
  )
  )

;;; Zamena prvkov v strome - kazdy uzol, ktory splna zadanu podmienku,
;;; je potrebne zamenit za nil
;;;
;;;    (zamen #'evenp '((1 (4 6)) ((5 3) 2))) ->
;;;                                    ((1 (nil nil) ((5 3) nil))
;;;
;;; Napoveda: zapis ((1 (4 6)) ((5 3) 2))) reprezentuje strom
;;;
;;;               +
;;;               |
;;;       +-------+-------+
;;;       |               |
;;;    +--+--+         +--+--+
;;;    1     |         |     2
;;;       +--+--+   +--+--+
;;;       4     6   5     3
;;;
;;; Alternativa: strom ktory nesie informaciu aj v nelistovych
;;;              uzloch

(defun zamen (fun strom &optional res)
  (cond ((null strom) (reverse res))
        ((listp (car strom)) (zamen fun (cdr strom) (cons (zamen fun (car strom) NIL) res)))
        ((funcall fun (car strom)) (zamen fun (cdr strom) (cons NIL res)))
        (t (zamen fun (cdr strom) (cons (car strom) res)))

        )
  )

;;; Redukcia stromu - vypustit kazdu vetvu na konci ktorej je nil
;;;
;;;    (redukuj '((1 (nil nil)) ((5 3) nil))) -> (1 (5 3))
;;;
;;; Napoveda: zapis ((1 (nil nil)) ((5 3) nil))) reprezentuje strom
;;;
;;;               +
;;;               |
;;;       +-------+-------+
;;;       |               |
;;;    +--+--+         +--+--+
;;;    1     |         |    nil
;;;       +--+--+   +--+--+
;;;      nil   nil  5     3

(defun redukuj (strom)
  (cond ((null strom) NIL)
        ((listp strom)
         (let ((hlava (redukuj (car strom)))
               (telo  (redukuj (cdr strom))))
           (cond ((null hlava) telo)
                 ((null telo) hlava)
                 (t (cons hlava (list telo)))
           )
         ))
        (t strom)
  )
)

;;; Vyplnenie stromu - do zadaneho prazdneho stromu je potrebne vpisat
;;; informaciu k jednotlivym uzlom podla pravidiel:
;;;   - listove uzly budu cislovane postupnostou zacinajucou hodntou 1
;;;     smerom zlava doprava
;;;   - nelistovy uzol bude suctom hodnot svojich potomkov
;;;
;;;    (vypln '((nil (nil nil)) ((nil nil) nil (nil)))) -> 
;;;                       (28 (6 1 (5 2 3)) (22 (9 4 5) 6 (7 7)))
;;;
;;; Napoveda: - zapis (5 2 3) reprezentuje podstrom
;;;                5
;;;             +--+--+
;;;             2     3
;;;           - zapis ((nil (nil nil)) ((nil nil) nil (nil))) reprezentuje
;;;             strom
;;;                +
;;;                |
;;;       +--------+---------+
;;;       |                  |
;;;    +--+--+         +-----+-----+
;;;   nil    |         |    nil    |
;;;       +--+--+   +--+--+        +
;;;      nil   nil nil   nil      nil


(defparameter num 0)
(defun cisluj (strom)
  (cond ((null strom) (setf num (1+ num)))
        ((listp strom)
         (cons (cisluj (car strom))
               (when (> (length (cdr strom)) 0) (cisluj (cdr strom))))
        )
  )
  )

(defun checker (elem)
  (if (listp elem) (car elem) elem)
)
(defun pocitaj (strom)
  (cond ((null strom) NIL)
        ((listp strom)
         (let ((hlava (pocitaj (car strom)))
               (brucho (pocitaj (cadr strom)))
               (telo (pocitaj (caddr strom))))
           (cond ((null brucho) hlava)
                 ((null telo) (list (+ (checker hlava) (checker brucho)) hlava brucho))
                 (t (list (+ (checker hlava) (checker brucho) (checker telo))
                          hlava
                          brucho
                          telo))
           )))
        (t strom)
   )
)
(defun vypln (strom)
  (pocitaj (cisluj strom))
)

;;; Zo stromu je potrebne vybrat vsetky uzly, ktore sa nachadzaju v
;;; zadanej pozadovanej hlbke.
;;;   - korenovy uzol je v hlbke 1
;;;   - hlbka narasta smerom k listovym uzlom
;;; Pozadovana hlbka je dana dvomi hodnotami - minimalnou a maximalnou
;;; hlbkou.
;;;
;;;    (vyber 2 3 '(a (d 1 (k 4 6)) (b (g 5 3) (o 2)))) ->
;;;                                               (1 k d g o b)
;;;
;;; Napoveda: zapis (a (d 1 (k 4 6)) (b (g 5 3) (o 2)))) reprezentuje
;;;           strom
;;;
;;;               +
;;;               a
;;;       +-------+-------+
;;;       d               b
;;;    +--+--+         +--+--+
;;;    1     k         g     o
;;;       +--+--+   +--+--+  +
;;;       4     6   5     3  2

(defun vyber (minh maxh strom &optional (curh 1))
  (cond ((null strom) NIL)
        ((and (not (listp strom)) (<= minh curh) (>= maxh curh)) (list strom))
        ((listp strom)
         (append (vyber minh maxh (cadr strom) (1+ curh))
                 (vyber minh maxh (caddr strom) (1+ curh))
                 (vyber minh maxh (car strom) curh)
         )
        )

        ))
;;; Transformacia aritmetickych vyrazov - Binarne operatory
;;; '+' (scitanie) a '*' (nasobenie) vytvaraju spolu so svojimi
;;; argumentami binarny strom. Ulohou je transformovat tento strom
;;; do tvaru ineho binarneho stromu, kde v ziadnej ceste od
;;; korenoveho uzla k listovemu uzlu nie je operator scitania
;;; vo vacsej hlbke ako operator nasobenia (teda pri vyhodnocovani
;;; vyrazu je mozne najprv urobit vsetky nasobenia a az nasledne
;;; vsetky sucty).
;;;
;;;    (transf '((a + (b * c)) * ((d + e) + f))) ->
;;;       (((a * d) + (a * e)) + (a * f)) +
;;;            ((((b * c) * d) + ((b * c) * e)) + ((b * c) * f))
;;;
;;; Napoveda: zapis ((a + (b * c)) * ((d + e) + f))) reprezentuje
;;;           binarny strom
;;;
;;;               *
;;;       -----------------
;;;       +               +
;;;    -------         -------
;;;    a     *         +     f
;;;       -------   -------
;;;       b     c   d     e
;;;           doporucujeme odvodit transformacne pravidla z prikladov
;;;              (a * (b + c))  -> ((a * b) + (a * c))
;;;              ((a + b) * c)  -> ((a * c) + (b * c))
(defun transf)
