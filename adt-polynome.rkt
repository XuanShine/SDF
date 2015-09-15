#lang racket

;;; adt-polynome.rkt
;;; Un module contenant le type abstrait "polynome"
;;; PF2 - Fac. Sciences de Nice, 2014, Racket 6.1

(require "string2poly.rkt")       ; pour string->poly et poly->string
(require lang/posn)
(provide string->poly poly->string poly0 poly0? monome polynome degre coeff tete reste poly*ext
         poly+ poly- poly* poly-val poly-const poly-rac interpol)

(define-syntax show
  (syntax-rules ()
    ((show expr) (begin (printf "? ~s\n" 'expr) (printf "--> ~s\n" expr)))))

(define TEST? #f)        ; mettre #t pour effectuer tous les tests <--------------------------------

;;; Un polynome est une suite ordonnee de monomes ((c e) ...), suivant les puissances decroissantes. 

(define p (string->poly "2x^5+x^4+2x^3-2x+3"))
(define q (string->poly "x^4-2x^3+7x^2-x"))
(when TEST?
  (printf "p --> ~a   ; 2x^5+x^4+2x^3-2x+3\n" p)
  (printf "q --> ~a   ; x^4-2x^3+7x^2-x\n" q))

(define poly0 '())

(define (poly0? p)
  (null? p))

(define (monome c e)           ; (c e)
  (list c e))                  ; un monome N'EST PAS un polynome !

(define (polynome c e)         ; ((c e))
  (list (list c e)))

(define (degre mp)             ; p = ((cN eN) ...), retourne N
  (cond ((poly0? mp) -inf.0)
        ((pair? (car mp)) (cadar mp))   ; polynom
        (else (cadr mp))))              ; monome

(define (coeff mp)             ; le coefficient dominant cN
  (cond ((poly0? mp) 0)
        ((pair? (car mp)) (caar mp))   ; polynome
        (else (car mp))))              ; monome

(define (tete p)              ; le monome de tete ((cN eN))
  (if (poly0? p) 
      (error "Pas de monome de tete pour le polynome nul !")
      (car p)))

(define (reste p)              ; le polynome prive de (cN eN)
  (if (poly0? p) 
      (error "Pas de reste pour le polynome nul !")
      (cdr p)))

(define (poly*ext k p)    ; loi externe
  (if (= k 0)
      poly0
      (map (lambda (m) (monome (* k (coeff m)) (degre m))) p)))

(when TEST?
  (show (poly->string (poly*ext 2 p))))

(define (poly+ p1 p2)
  (cond ((poly0? p1) p2)
        ((poly0? p2) p1)
        ((> (degre p1) (degre p2)) (cons (car p1) (poly+ (cdr p1) p2)))
        ((< (degre p1) (degre p2)) (cons (car p2) (poly+ p1 (cdr p2))))
        (else (let ((c (+ (coeff p1) (coeff p2))))
                (if (zero? c)
                    (poly+ (cdr p1) (cdr p2))
                    (cons (list c (degre p1)) (poly+ (cdr p1) (cdr p2))))))))

(when TEST? (show (poly->string (poly+ p q))))

(define (poly- p1 p2)
  (poly+ p1 (poly*ext -1 p2)))

(when TEST? (show (poly->string (poly- p q))))

(define (poly++ c d p)    ; inserer le monome (c d) dans le polynome p
  (poly+ (polynome c d) p))

(when TEST? (show (poly->string (poly++ 3 2 p))))

(define (poly* p1 p2)
  (if (poly0? p1)
      poly0
      (let ((c1 (coeff p1)) (d1 (degre p1)))
        (poly+ (map (lambda (m) (monome (* (coeff m) c1) (+ (degre m) d1))) p2)
               (poly* (reste p1) p2)))))

(when TEST?
  (show (poly->string (poly* p p)))
  (show (poly->string (poly* (string->poly "2x^100-1") (string->poly "2x^100+1")))))

(define (poly** c d p)     ; p * cx^d
  (poly* (polynome c d) p))

(define (poly-val_ p x)
  (for/sum ([m (in-list p)]) (* (coeff m) (expt x (degre m)))))       ; for/sum <=> sum de Python

(when TEST?
  (show p)
  (show (poly-val_ p 1)))

(define (poly-val p x)
  (apply + (map (lambda (m) (* (coeff m) (expt x (degre m)))) p)))    ; plus classique...

(when TEST? (show (poly-val p 1)))

(define (poly-derive p)
  (if (<= (degre p) 0)
      poly0
      (let* ((m (tete p)) (c (coeff m)) (d (degre m)))
        (poly++ (* c d) (- d 1) (poly-derive (reste p))))))

(when TEST? (show (poly->string (poly-derive p))))

(define (poly/ p1 p2)      ; La division de polynomes. deg(q) >= 1. Retourne (quotient reste)
  (if (< (degre p1) (degre p2))
      (list poly0 p1)
      (let* ((c (/ (coeff p1) (coeff p2)))
             (d (- (degre p1) (degre p2)))
             (HR (poly/ (poly- p1 (poly** c d p2)) p2)))
        (list (poly++ c d (car HR)) (cadr HR)))))

(when TEST?
  (let* ((div (poly/ p q)) (quo (first div)) (rem (second div)))
    (printf "Je divise ~a par ~a :\n" (poly->string p) (poly->string q))
    (printf "~a = (~a)(~a)~a\n" (poly->string p) (poly->string q) (poly->string quo) (poly->string rem)))
  (let* ((p (string->poly "x^3-1")) (q (string->poly "2x+1")) (div (poly/ p q)))
    (printf "Je divise x^3-1 par 2x+1 :\n")
    (printf "+x^3-1 = (+2x+1)(~a)~a\n" (poly->string (first div)) (poly->string (second div)))))
  
(define (poly-modulo p1 p2)
  (cadr (poly/ p1 p2)))

(when TEST?
  (show (map poly->string (poly/ (string->poly "x^2+1") (string->poly "x+1"))))
  (show (poly->string (poly-modulo (string->poly "x^2+1") (string->poly "x+1")))))

(define (poly-pgcd a b)
  (if (poly0? b)
      (poly*ext (/ (coeff a)) a)
      (poly-pgcd b (poly-modulo a b))))

(when TEST?
  (show (poly->string (poly-pgcd (string->poly "x^8+x^6-3x^4-3x^3+8x^2+2x-5") 
                                 (string->poly "3x^6+5x^4-4x^2-9x+21")))))

;Polynome d'interpolation de LAGRANGE

(define (poly-const a) ; le polynome a
  (if (zero? a) poly0 `((,a 0))))

(define (poly-rac a)   ; le polynome X-a
  (if (zero? a) `((1 1)) `((1 1) (,(- a) 0))))

(when TEST?
  (show (poly-const 2))
  (show (poly-rac 2)))

(define (poly-lagrange xi Lx)           ; Li(x)=1 sur xi et 0 sur les points de Lx
  (define (iter Lx acc)
    (if (null? Lx)
        acc
        (let ((x1 (car Lx)))
          (iter (cdr Lx) (poly* (poly*ext (/ (- xi x1)) (poly-rac x1)) acc)))))
  (iter Lx (poly-const 1)))

(when TEST?
  (show (poly->string (poly-lagrange 2 '(3 5)))))

(define (interpol Lpoints)    ; Lpoints = (#<struct:posn x y> ...)
  (let ((Lx (map posn-x Lpoints)))
    (define (supprimer x L)
      (if (equal? x (car L)) (cdr L) (cons (car L) (supprimer x (cdr L)))))
    (define (iter Lpts acc)
      (if (null? Lpts)
          acc
          (iter (cdr Lpts) 
                (poly+ acc (poly*ext (posn-y (car Lpts)) 
                                     (poly-lagrange (posn-x (car Lpts)) (supprimer (posn-x (car Lpts)) Lx)))))))
    (iter Lpoints poly0)))

(when TEST?
  (define L (interpol (list (make-posn 3 -2) (make-posn 5 0) (make-posn 0 4))))
  (printf "\nSoit L le polynome d'interpolation de (3,-2), (5,0) et (0,4) :\n")
  (show (poly->string L))
  (show (map (lambda (x) (poly-val L x)) '(3 5 0)))
  
  (define L1 (interpol (list (make-posn 3 -2) (make-posn 6 -4) (make-posn -12 8))))
  (printf "\nSoit L1 le polynome d'interpolation de (3,-2), (6,-4) et (-12,8) :\n")
  (show (poly->string L1))
  (show (map (lambda (x) (poly-val L1 x)) '(3 6 -12)))
  (printf "\n"))
  
(printf "### adt-polynome.rkt : poly0, (poly0? p), (monome c e), (polynome c e), (degre mp), (coeff mp),
(tete p), (reste p), (poly*ext k p), (poly+ p1 p2), (poly- p1 p2), (poly* p1 p2), (poly-val p x), 
(poly-const a), (poly-rac a), (interpol Lpoints)\n")

; la suite dans "lagrange-gui-mouse.rkt"

