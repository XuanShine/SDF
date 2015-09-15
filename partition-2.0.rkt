#lang racket

(provide (all-defined-out))

(define bpm 200) ; Normalement, on peut redéfinir le bpm dans les partitions en faisant un (lambda () (set! bpm <nouvelle valeur>)
; <noteàrenvoyer> mais le bpm est local, et il n'est pas possible de modifier une variable qui est dans un module.
; Mais on peut mettre une fonction dans les partitions pour agir sur le programme. Il faut qu'elle renvoie une liste de valeur de note
; ou une note.

;; Set customs sheets music here.
;; Partition particulier à définir

(define partition-g '(la si do re mi fa sol))
(define partition-h '((la si) (do re) (sol re)))
(define partition-j '(la () si ()))
(define partition-k ; Fur Elise
  `(mi2 re2# mi2 re2# mi2 si re2 do2 la () () do mi la si () () mi sol# si do2 () () mi
    mi2 re2# mi2 re2# mi2 si re2 do2 la () () do mi la si () () mi do2  si la  () () ()
    mi2 re2# mi2 re2# mi2 si re2 do2 la () () do mi la si () () mi sol# si do2 () () mi
    mi2 re2  mi2 re2  mi2 si re2 do2 la () () do mi la si () () mi do2  si la  () ()
    
    si do2 re2 mi2 () sol fa2 mi2 re2 () fa mi2 re2 do2 () mi re2 do2 si ()
    mi mi2 () () mi2 si2 () () re2# mi2 re2#
    
    mi2 re2# mi2 re2# mi2 si re2 do2 la () () do mi la si () () mi sol# si do2 () () mi
    mi2 re2# mi2 re2# mi2 si re2 do2 la () () do mi la si () () mi do2 si la () ()
    
    si do2 re2 mi2 () sol fa2 mi2 re2 () fa mi2 re2 do2 () mi re2 do2 si ()
    mi mi2 () () mi2 si2 () () re2# mi2 re2#
    
    mi2 re2# mi2 re2# mi2 si re2 do2 la () () do mi la si () () mi sol# si do2 () () mi
    mi2 re2# mi2 re2# mi2 si re2 do2 la () () do mi la si () () mi do2 si la () ()
    
    (mi do2) (fa do2) (mi sol do2)
    fa la do2 fa2 mi2 mi2 re2 si2 la2 la2 sol2 fa2 mi2 re2
    do2 la# la la sol la si do2 () () re2 re2# mi2 mi2 fa2 la
    do2 () () re2 si
    ,(lambda () (set! bpm (* 2 bpm)) 'do)
    sol2 sol sol2 la sol2 si sol2 do2 sol2 re2 sol2 mi2 sol2 si2 si2 la2 sol2 fa2 mi2 re2 sol2 fa2 re2
    
do2 sol2 sol sol2 la sol2 si sol2 do2 sol2 re2 sol2 mi2 sol2 si2 si2 la2 sol2 fa2 mi2 re2 sol2 fa2 re2
    
    mi2 fa2 mi2 re2# mi2 si mi2 re2 mi2 si mi2 re2
    ,(lambda () (set! bpm (/ bpm 2)) 'mi2) ()
    si mi2 re2# mi2 () si mi2 () () re2# mi2 () () re2
    
    mi2 re2# mi2 si re2 do2 la () () do mi la si () () mi sol# si do2 () () mi
    mi2 re2# mi2 re2# mi2 si re2 do2 la () () do mi la si () () mi do2 si la () ()
    ))

(define partition-l '())
(define partition-m '())
