#lang racket
(require "suffix-tree-stream.rkt")
(require "collection.rkt")

(provide (all-defined-out))

;; Vom prelua toate funcțiile din etapele 1-3 (exceptând
;; longest-common-substring, care nu beneficiază de 
;; reprezentarea ca flux întrucât parcurge tot arborele)
;; și le vom adapta la noua reprezentare a unui ST.
;;
;; Pentru că un ST este construit pornind de la o colecție
;; de sufixe și pentru că ne dorim să nu calculăm toate
;; sufixele decât dacă este nevoie, vom modifica toate
;; funcțiile care prelucrau liste de sufixe pentru a
;; prelucra fluxuri de sufixe.
;;
;; Obs: fără această modificare a listelor de sufixe în
;; fluxuri de sufixe, și presupunând că am manipulat
;; arborii de sufixe doar prin interfața definită în
;; fișierul suffix-tree (respectând astfel bariera de 
;; abstractizare), ar trebui să alterăm doar funcția 
;; suffixes->st care este practic un constructor pentru
;; tipul ST.
;; Din cauza transformării listelor de sufixe în fluxuri,
;; avem mult mai multe implementări de modificat.
;; Puteam evita acest lucru? Da, utilizând conceptul de
;; colecție de sufixe de la început (în loc să presupunem
;; că ele vor fi prelucrate ca liste). În loc de cons,
;; car, cdr, map, filter, etc. am fi folosit de fiecare
;; dată collection-cons, collection-first, ... etc. -
;; aceste funcții fiind definite într-o bibliotecă
;; inițială ca fiind echivalentele lor pe liste, și
;; redefinite ulterior în stream-cons, stream-first,
;; ... etc. Operatorii pe colecții de sufixe ar fi 
;; folosit, desigur, doar funcții de tip collection-.
;;
;; Am ales să nu procedăm astfel pentru că ar fi provocat
;; confuzie la momentul respectiv (când chiar operatorii
;; pe liste erau o noutate) și pentru a vă da ocazia să
;; faceți singuri acest "re-design".


; TODO
; Copiați din etapele anterioare implementările funcțiilor
; de mai jos și modificați-le astfel:
; - Toate funcțiile care lucrează cu liste de sufixe vor
;   lucra cu un nou tip de date Collection, ai cărui
;   constructori și operatori vor fi definiți de voi
;   în fișierul collection.rkt.
; - Pentru toate funcțiile, trebuie să vă asigurați că
;   este respectată bariera de abstractizare (atât în 
;   cazul tipului ST cât și în cazul tipului Collection).
; Obs: cu cât mai multe funcții rămân nemodificate, cu atât
; este mai bine (înseamnă că design-ul inițial a fost bun).

(define (longest-common-prefix w1 w2)
  
  (define (longest-common-prefix-helper w1 w2 acc)
    (cond
      ((or (null? w1) (null? w2)) (list acc w1 w2))
      ((equal? (first w1) (first w2)) (longest-common-prefix-helper (cdr w1) (cdr w2) (append acc (list (first w1)))))
      (else (list acc w1 w2))))

  (longest-common-prefix-helper w1 w2 null))


; am schimbat, în numele funcției, cuvântul list în
; cuvântul collection
(define (longest-common-prefix-of-collection words)
  (longest-common-prefix-of-list-helper (collection-rest words) (collection-first words)))
(define (longest-common-prefix-of-list-helper words acc)
  (cond ((collection-empty? words) acc)
        (else (longest-common-prefix-of-list-helper (collection-rest words) (collection-first (longest-common-prefix acc (collection-first words)))))))

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
        (list #f '())))) ; Nicio legatura

(define (take lst n)
  (if (or (zero? n) (null? lst))
      '()
      (cons (car lst) (take (cdr lst) (- n 1)))))


(define (st-has-pattern? st pattern)
 
  (define match (match-pattern-with-label st pattern))
 
  (cond ((equal? match #t) #t)
        ((equal? (first match) #f) #f)
        (else (st-has-pattern? (third match) (second match)))))


(define (get-suffixes text)
  (define (suffixes-helper text)
    (if (collection-empty? text)
        (collection-empty) 
        (collection-cons text (suffixes-helper (collection-rest text)))))
  (suffixes-helper text))



(define (get-ch-words words ch)
  (collection-filter (lambda (x) (equal? (collection-first x) ch)) words))


(define (ast-func suffixes)
  (let ((ast-label (collection-first (collection-first suffixes))))  ; Extrage eticheta AST
    (cons(list ast-label)                                ; Creează perechea cu eticheta AST
                     (collection-map (lambda (suffix)(collection-rest suffix))suffixes))))


(define (remove-prefix prefix word)
  (define prefix-length (length prefix))
  (if (list-prefix? prefix word)
      (drop word prefix-length)  ; Elimină prefixul din cuvânt
      word))

(define helper
  (lambda (prefix)
    (lambda (word) (remove-prefix prefix word))))

(define (cst-func suffixes)
  (let* ((prefix-info  (longest-common-prefix-of-collection suffixes)))
    (cons prefix-info (collection-map (helper prefix-info) suffixes))))


; considerați că și parametrul alphabet este un flux
; (desigur, și suffixes este un flux, fiind o colecție
; de sufixe)
(define (suffixes->st labeling-func suffixes alphabet)
  'your-code-here)


; nu uitați să convertiți alfabetul într-un flux
(define text->st
  'your-code-here)


(define text->ast
  'your-code-here)


(define text->cst
  'your-code-here)

; dacă ați respectat bariera de abstractizare,
; această funcție va rămâne nemodificată.
(define (substring? text pattern)
  'your-code-here)


; dacă ați respectat bariera de abstractizare,
; această funcție va rămâne nemodificată.
(define (repeated-substring-of-given-length text len)
  'your-code-here)