#lang racket
(require "suffix-tree.rkt")

(provide (all-defined-out))

; TODO 2
; Implementați o funcție care primește două cuvinte (liste
; de caractere) w1 și w2 și calculează cel mai lung prefix
; comun al acestora, împreună cu restul celor două cuvinte
; după eliminarea prefixului comun.
; ex:
; (longest-common-prefix '(#\w #\h #\y) '(#\w #\h #\e #\n))
; => '((#\w #\h) (#\y) (#\e #\n))
; Folosiți recursivitate pe coadă.
(define (longest-common-prefix w1 w2)
  
  (define (longest-common-prefix-helper w1 w2 acc)
    (cond
      ((or (null? w1) (null? w2)) (list acc w1 w2))
      ((equal? (first w1) (first w2)) (longest-common-prefix-helper (cdr w1) (cdr w2) (append acc (list (first w1)))))
      (else (list acc w1 w2))))

  (longest-common-prefix-helper w1 w2 null))

; TODO 3
; Implementați recursiv o funcție care primește o listă nevidă 
; de cuvinte care încep cu același caracter și calculează cel 
; mai lung prefix comun al acestora.
; Opriți căutarea (parcurgerea) în momentul în care aveți garanția 
; că prefixul comun curent este prefixul comun final.
(define (longest-common-prefix-of-list words)
  (longest-common-prefix-of-list-helper (cdr words) (car words)))
(define (longest-common-prefix-of-list-helper words acc)
  (cond ((null? words) acc)
        (else (longest-common-prefix-of-list-helper (cdr words) (car (longest-common-prefix acc (car words)))))))


;; Următoarele două funcții sunt utile căutării unui șablon
;; (pattern) într-un text cu ajutorul arborelui de sufixe.
;; Ideea de căutare este următoarea:
;; - dacă șablonul există în text, atunci există un sufix care
;;   începe cu acest șablon, deci există o cale care începe din
;;   rădăcina arborelui care se potrivește cu șablonul
;; - vom căuta ramura a cărei etichetă începe cu prima literă
;;   din șablon
;; - dacă nu găsim această ramură, șablonul nu apare în text
;; - dacă șablonul este conținut integral în eticheta ramurii,
;;   atunci el apare în text
;; - dacă șablonul se potrivește cu eticheta dar nu este conținut
;;   în ea (de exemplu șablonul "nana$" se potrivește cu eticheta
;;   "na"), atunci continuăm căutarea în subarborele ramurii
;; - dacă șablonul nu se potrivește cu eticheta (de exemplu
;;   șablonul "numai" nu se potrivește cu eticheta "na"), atunci
;;   el nu apare în text (altfel, eticheta ar fi fost "n", nu
;;   "na", pentru că eticheta este cel mai lung prefix comun al
;;   sufixelor din subarborele său)

; TODO 4
; Implementați funcția match-pattern-with-label care primește un
; arbore de sufixe și un șablon nevid și realizează un singur pas 
; din procesul prezentat mai sus - identifică ramura arborelui a
; cărei etichetă începe cu prima literă din șablon, apoi
; determină cât de bine se potrivește șablonul cu eticheta,
; întorcând ca rezultat:
; - true, dacă șablonul este conținut integral în etichetă
; - lista (etichetă, nou pattern, subarbore), dacă șablonul se
;   potrivește cu eticheta dar nu este conținut în ea
;   (ex: ("na", "na$", subarborele de sub eticheta "na")
;   pentru șablonul inițial "nana$" și eticheta "na")
; - lista (false, cel mai lung prefix comun între etichetă și
;   șablon), dacă șablonul nu s-a potrivit cu eticheta sau nu
;   s-a găsit din start o etichetă care începe cu litera dorită
;   (ex1: (false, "n") pentru șablonul "numai" și eticheta "na")
;   (ex2: (false, "") pentru etichetă negăsită)
; Obs: deși exemplele folosesc stringuri pentru claritate, vă
; reamintim că în realitate lucrăm cu liste de caractere.

(define (check-if-identical p l)
  (if (and (null? l) (not (null? p)))
      #f
      (if (null? p)
          #t
          (if (eq? (car p) (car l))
              (check-if-identical (cdr p) (cdr l))
              #f))))

(define (match-pattern-with-label st pattern)
  (let ((branch (get-ch-branch st (car pattern))))
    (if branch
        (let* ((label (get-branch-label branch))
               (subtree (get-branch-subtree branch)))
          (if (check-if-identical pattern label) ; Pattern = label
              #t
              (let ((match-length (length (car (longest-common-prefix pattern label)))))
                (if (equal? match-length (length label)) ; Label este full match
                    (list label (list-tail pattern match-length) subtree) ; Partiala
                    (list #f (take pattern match-length))))))
        (list #f '())))) ; Ncio legatura

(define (take lst n)
  (if (or (zero? n) (null? lst))
      '()
      (cons (car lst) (take (cdr lst) (- n 1)))))


; TODO 5
; Implementați funcția st-has-pattern? care primește un
; arbore de sufixe și un șablon și întoarce true dacă șablonul
; apare în arbore, respectiv false în caz contrar.
(define (st-has-pattern? st pattern)
 
  (define match (match-pattern-with-label st pattern))
 
  (cond ((equal? match #t) #t)
        ((equal? (first match) #f) #f)
        (else (st-has-pattern? (third match) (second match)))))
