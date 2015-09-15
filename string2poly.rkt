#lang racket

;;; string2poly.rkt with the Lex/Yacc tools of Racket 6.1
;;; Option PF2, Fac. Sciences de Nice 2014
;;; (string->poly "-53x^4+256x-1")        --> ((-53 4)(256 1)(-1 0))
;;; (poly->string '((-53 4)(256 1)(-1 0))) --> "-53x^4+256x-1"

;;; Lexer and parser of polynomial strings. This file contains Scheme level 3 ;-) Difficult !...

(require parser-tools/lex parser-tools/yacc)
(provide string->poly poly->string)

;;; ====================== Definition of the lexer ====================
;;; The "lexer" finds tokens (words) from characters. Do not read...

(define-lex-abbrevs
  (digit (char-range "0" "9"))
  (int (repetition 1 +inf.0 digit))
  (simple-float (concatenation int "." int))
  (e-float (concatenation simple-float "e-" int))
  (float (union simple-float e-float))
  (num (union int float))
  (puiss int)
  ($coeff (concatenation (union "+" "-") num))
  (var (union "x" "X")))

(define-tokens value-tokens (puiss $coeff var))
(define-empty-tokens op-tokens (+ - ^ EOF))

(define get-lexeme
  (lexer
   [(eof) 'EOF]
   [(union "+" "-" "^") (string->symbol lexeme)]
   [var (token-var 'x)]
   [puiss (token-puiss (string->number lexeme))]
   [$coeff (token-$coeff (string->number lexeme))]))

; test the lexer

(define (test-lexer)
  (let ((str "+53x^4-256x-2"))
    (printf "Testing the lexer on the string ~s :\n" str)
    (let* ((p-in (open-input-string str))
           (lex (lambda () (get-lexeme p-in))))
      (do ((i 0 (+ i 1)) (obj (lex) (lex)))
        ((equal? obj 'EOF) (printf "No more tokens...\n\n"))
        (printf "~s\n" obj)))))

;(test-lexer)

;;; ====================== Definition of the parser ====================
;;; parser == analyseur syntaxique

(define parse
  (parser
   
   (start S)                       ; the so-called axiom
   (end EOF)                       ; end of analysis upon eof
   (tokens value-tokens op-tokens)
   (error (lambda (a b c) (printf "Parsing error with token ~a\n" b)))
   
   (precs)
   
   (grammar         ; grammar of polynomials, Yacc style
    
    (S     [(poly) $1])
    
    (poly [() '()]
          [(mono poly) `(,$1 ,@$2)])
    (mono (($coeff var ^ puiss) `(,$1 ,$4))
          (($coeff var) `(,$1 1))
          (($coeff) `(,$1 0))
          ((+ var) `(1 1))
          ((+ var ^ puiss) `(1 ,$4))
          ((- var) `(-1 1))
          ((- var ^ puiss) `(-1 ,$4))))))

(define (string->poly str)
  (let ((c (string-ref str 0)))
    (set! str (if (not (or (char=? c #\+) (char=? c #\-))) (string-append "+" str) str))
    (let* ((p-in (open-input-string str))
           (lex (lambda () (get-lexeme p-in)))
           (poly (parse lex)))
      (sort poly (lambda (monome1 monome2)
                   (>= (second monome1) (second monome2)))))))

(define (test:string->poly)
  (let* ((str "53.0x^2+x-256.789e-06x^3-x^8-1.5+x^4") (p (string->poly str)))
    (printf "Testing string->poly :\n~s --> ~a\n" str p)))

(define (poly->string p)
  (define (poly-write p . port)          ; a pretty printer for polynomials
    (let ((port (if (null? port) (current-output-port) (car port))))
      (define (mono-write c e)           ; c is not 0
        (case e
          ((0) (when (> c 0) (fprintf port "+")) (write c port))
          ((1) (fprintf port "~ax" (cond ((= c 1) "+")
                                         ((= c -1) "-")
                                         ((< c 0) c)
                                         (else (format "+~a" c)))))
          (else (fprintf port "~ax^~a" (cond ((= c 1) "+")
                                             ((= c -1) "-")
                                             ((< c 0) c)
                                             (else (format "+~a" c)))
                         e))))
      (if (null? p)
          (fprintf port "0")
          (for-each (lambda (x) (mono-write (car x) (cadr x))) p))))
  (let ((p-out (open-output-string)))
    (poly-write p p-out)
    (let ((str (get-output-string p-out)))
      (if (char=? (string-ref str 0) #\+)
          (substring str 1)
          str))))

(define (test:poly->string)
  (let* ((p '((-1 8) (1 4) (-0.000256789 3) (53.0 2) (1 1) (-1.5 0))) (str (poly->string p)))
    (printf "Testing poly->string :\n~a ---> ~s\n" p str)))

(define (test-all)
  (test:string->poly)
  (test:poly->string))

(printf "### string2poly.rkt : (poly->string L), (string->poly str)\n")

;(test-all)




