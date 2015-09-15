#lang racket

(require rsound rsound/piano-tones)
(require 2htdp/image 2htdp/universe)
(require rsound/draw)
(require lang/posn)
(require "adt-polynome.rkt")
(require "partition-2.0.rkt")


;; Réglages
(define tempo-clock (/ 1 30))  ;; 30 images par secondes par défaut;
(define mini 425); 528) ;; coordonnée x minimal du volcan
(define maxi 775) ;672) ;; coordonnée x maximal du volcan
(define list-note '(do do# re re# mi fa fa# sol sol# la la# si
                     do2 do2# re2 re2# mi2 fa2 fa2# sol2 sol2# la2 la2# si2))
(define nbr-note 24) ;; Calcul de l'unité de mesure pour la note de musique: il y a 24 notes de musiques
(define ptdepart '(600 340)) ;; Note au départ
(define bpm-base 200) ; bpm-base = vraibpm * 2 (environ)
(define vx-base 0.3) ; vx à la base du volcan (pour que les notes avancent plus ou moins vite.


;; Fond d'écran
(define TRIANGLE 
  (isosceles-triangle 350 60 'solid "lightgrey"))
(define CARREBLANC
  (rectangle 130 60 'solid "white"))
(define VOLCAN (underlay/xy TRIANGLE 100 0 CARREBLANC))
(define (dessinerfond a)
  (if (equal? 0 a)
      (empty-scene 1200 600)
      (scene+line (dessinerfond (- a 1)) 0 (* 100 a) 1200 (* 100 a) "black")))
(define FONDPARTITION (dessinerfond 5))
(define SCENE1 (place-image VOLCAN 600 460 FONDPARTITION))
(define (NOTE color)
  (underlay/xy (circle 9 'solid (make-color color color color))
               15 -19
               (underlay/xy (rectangle 3 30 'solid (make-color color color color)) 0 0
                            (rectangle 12 2 'solid (make-color color color color)))))
(define PIANO (text "a-z-e-r-t-y-u: la ~ sol     left: delete from the beginning     right: delete from the end     w: bouncing note    Space: random sound" 13 'white))
(define EXAMPLE (text "q: Test     s: Au clair de la lune     d: Fais dodo     f: Hymne à la joie     g ~ m: You can set customs sheets music on the file partition.rkt" 13 'white))
(define FLECHE (text "up: Increase bpm     down: Decrease bpm" 13 'white))
(define CREDIT (text "Projet d'animation en Scheme/Racket utilisant Rsound par les étudiants Nguyen Xuan Truong et Louis Brandon de la Team SDF (Sans Distribution Fixe)" 13 'red))
;(define SCENE (above SCENE1 (underlay (empty-scene 1200 100 'black) (above CREDIT PIANO EXAMPLE FLECHE))))
(define SCENE SCENE1)

;; supprime le dernier élément d'une liste
(define (remove-last l)
  (cond ((empty? l) null)
        ((empty? (cdr l)) null)
        (else (cons (car l) (remove-last (cdr l))))))

(define unite (/ mini nbr-note)) 
(define pxl (/ (- 1200 maxi) nbr-note))
(define mid-pxl (/ pxl 2))
(define (search-note note L) ; renvoi l'indice de la première occurence de note dans L
  (cond [(empty? L) (error "search-note error" note)]
        [(equal? (car L) note) 0]
        [else (+ 1 (search-note note (cdr L)))]))



(define (add-note note) ; note est une quote
  ; À partir de la quote de la note, on va le lier à un coté (gauche ou droite du volcan)
  ; pour ensuite lui affecter un vx (pour le déplacement horizontale)
  ; et à partir de là, il sera lié à une fonction (avec lagrange)
  (if (equal? (random 2) 1)
      (let* [(n (- nbr-note (search-note note list-note))) (vx (* vx-base (+ (/ (- maxi (car ptdepart)) pxl) n)))
                                                           (xA (+ (- (* pxl n) mid-pxl) maxi)) (xI (/ (+ (car ptdepart) xA) 2))
                                                           (lagrange (interpol (list (make-posn (car ptdepart) (cadr ptdepart)) (make-posn xI (random 200)) (make-posn xA 590))))]
        (list (car ptdepart) (cadr ptdepart) vx (λ (x) (apply + (map (λ (poly) (* (expt x (cadr poly)) (car poly))) lagrange))) note))
      (let* [(n (+ (search-note note list-note) 1)) (vx (- (* vx-base (+ (/ (- (car ptdepart) mini) pxl) (- nbr-note n)))))
                                              (xA (- (* pxl n) mid-pxl)) (xI (/ (+ (car ptdepart) xA) 2))
                                              (lagrange (interpol (list (make-posn (car ptdepart) (cadr ptdepart)) (make-posn xI (random 200)) (make-posn xA 590))))]
        (list (car ptdepart) (cadr ptdepart) vx (λ (x) (apply + (map (λ (poly) (* (expt x (cadr poly)) (car poly))) lagrange))) note))
  ))

;; joue un son en fonction de la position de x lorsque la note atteint le "plancher" (c'est-à-dire y > 580)
(define (jouer-son son)
  (play (clip (piano-tone (+ 60 (search-note son list-note))) 0 67000)))


;; Partitions de musique

(define au-clair-de-la-lune '(do2 do2 do2 re2 mi2 () re2 () do2 mi2 re2 re2 do2 () () ()
                               do2 do2 do2 re2 mi2 () re2 () do2 mi2 re2 re2 do2 () () ()))
(define fais-dodo '(mi2 () re2 do2 () do2 re2 do2 re2 mi2 () do2 mi2 () re2 do2 () do2 re2 mi2 re2 do2 ()
                       sol2 sol2 fa2 mi2 re2 () sol2 sol2 fa2 mi2 re2 () sol2 sol2 fa2 mi2 re2 () sol2 sol2 fa2 mi2 re2 ()
                       mi2 () re2 do2 () do2 re2 do2 re2 mi2 () do2 mi2 () re2 do2 () do2 re2 mi2 re2 do2 ()))
(define test '(la si  (la do2) () mi2 re2 mi2))
(define hymne-a-la-joie '((si mi2) (si mi2) (do2 fa2) (re2 sol2) (re2 sol2) (do2 fa2) (si mi2)
                                  (la re2) do2 do2 (la re2) (si mi2) (si mi2) () (la re2) (la re2) ()
                                  (si mi2) (si mi2) (do2 fa2) (re2 sol2) (re2 sol2) (do2 fa2) (si mi2)
                                  (la re2) do2 do2 (la re2) (si mi2) (la re2) () do2 do2))

;; Redéfinir la partition de l'utilisateur du fichier "partition.rkt"
(define particular-g partition-g)
(define particular-h partition-h)
(define particular-j partition-j)
(define particular-k partition-k)
(define particular-l partition-l)
(define particular-m partition-m)

;; Renvoi d'une note aléatoire
(define (note-aleatoire)
  (vector-ref (list->vector '(do do# re re# mi fa fa# sol sol# la la# si
                               do2 do2# re2 re2# mi2 fa2 fa2# sol2 sol2# la2 la2# si2))
              (random 24)))

(define (xrandom k)
  (* (- k (random 3)) (+ -1 (* 2 (random 2)))))

(define (volcan pile-note bpm)
  (define note-playing null)  ;; INIT du big-bang
  
  (define cpt 0) ;; compte le nombre de battements/image en fonction du ips et du bpm afin d'ajouter une note
  
  (define (suivant L)  ; les notes ne rebondissent pas et s'arretent hors ecran. Il doit renvoyer une liste
    ;; cette partie ajoute une note à la liste L
    (define new-list
      (if (< cpt (/ 60 bpm tempo-clock)) ; images/seconde, battements/minute => battements/image
          (begin (set! cpt (+ cpt 1)) L)
          (begin (set! cpt 0) (cond [(empty? pile-note) L]
                                    [(empty? (car pile-note)) (set! pile-note (cdr pile-note)) L]
                                    [(symbol? (car pile-note)) (let [(new (car pile-note))] (set! pile-note (cdr pile-note)) (cons (add-note new) L))]
                                    [(procedure? (car pile-note))
                                     (let [(new ((car pile-note)))] (set! pile-note (cdr pile-note))
                                       (if (symbol? new)
                                           (cons (add-note new) L)
                                           (append (for/list ([note (in-list new)]) (add-note note)) L)))]
                                    [else (let [(new (car pile-note))] (set! pile-note (cdr pile-note)) (append (for/list ([note (in-list new)]) (add-note note)) L))]
                                    ))))
    (define (iter L)
      (if (empty? L)
          null
          (let* [(note (car L)) (x (first note)) (y (second note)) (vx (third note)) (trajectoire (fourth note)) (son (fifth note))]
            (if (> y 590) 
                    (begin (jouer-son son) (iter (cdr L)))
                    (cons (list (+ x vx) (trajectoire x) vx trajectoire son) (iter (cdr L)))))))           
    (iter new-list))
  
  (define (dessiner l)
    (if (empty? l)
        SCENE
        (place-image (NOTE 10) (first (first l)) (second (first l))
                     (dessiner (rest l)))))
  
  (define (change m key)
    (cond
      ((key=? key "a")
       (cons (add-note 'do) m))
      ((key=? key "A")
       (cons (add-note 'do2) m))
      ((key=? key "z")
       (cons (add-note 're) m))
      ((key=? key "Z")
       (cons (add-note 're2) m))
      ((key=? key "e")
       (cons (add-note 'mi) m))
      ((key=? key "E")
       (cons (add-note 'mi2) m))
      ((key=? key "r")
       (cons (add-note 'fa) m))
      ((key=? key "t")
       (cons (add-note 'sol) m))
      ((key=? key "y")
       (cons (add-note 'la) m))
      ((key=? key "u")
       (cons (add-note 'si) m))
      ((key=? key "R")
       (cons (add-note 'fa2) m))
      ((key=? key "T")
       (cons (add-note 'sol2) m))
      ((key=? key "Y")
       (cons (add-note 'la2) m))
      ((key=? key "U")
       (cons (add-note 'si2) m))
      ((key=? key " ")
       (cons (add-note (note-aleatoire)) m))
      ((key=? key "q")
       (set! pile-note test) m)
      ((key=? key "s")
       (set! pile-note au-clair-de-la-lune) m)
      ((key=? key "d")
       (set! pile-note fais-dodo) m)
      ((key=? key "f")
       (set! pile-note hymne-a-la-joie) m)
      ((key=? key "g")
       (set! pile-note particular-g) m)
      ((key=? key "h")
       (set! pile-note particular-h) m)
      ((key=? key "j")
       (set! pile-note particular-j) m)
      ((key=? key "k")
       (set! pile-note particular-k) m)
      ((key=? key "l")
       (set! pile-note particular-l) m)
      ((key=? key "m")
       (set! pile-note particular-m) m)
      ((and (not (empty? m)) (key=? key "right"))
       (cdr m))
      ((and (not (empty? m)) (key=? key "left"))
       (remove-last m))
      ((key=? key "up")
       (set! bpm (+ bpm 25)) m)
      ((key=? key "down")
       (set! bpm (- bpm 25)) m)
      #|((key=? key "w")
       (cons (list (car ptdepart) (cadr ptdepart) (xrandom 5) (- (- (random 10)) 10)) m))|#
      (else m)))
  
  (big-bang note-playing
            (on-tick suivant tempo-clock)
            (on-key change)
            (on-draw dessiner)))

(volcan '() bpm-base)
