;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Sorting) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
; insert-sort: [List-of Number] -> [List-of Number]
; Sorts a list of numbers in increasing order

(define (insert-sort lon)
  (cond
    [(empty? lon) '()]
    [(cons? lon)
     (insert (first lon) (insert-sort (rest lon)))]))

; insert: Number [List-of Number] -> [List-of Number]
; inserts the given number into the given list so that the list is sorted

(define (insert num lon)
  (cond
    [(empty? lon) (list num)]
    [(cons? lon)
     (if (< num (first lon))
         (cons num lon)
         (cons (first lon) (insert num (rest lon))))]))

(define (smarter-sort lon)
  (cond
    [(empty? lon) '()]
    [(cons? lon)
     (local ((define n (first lon))
             ; Number -> Boolean
             ; Is the given number smaller than n?
             ; suppose n = 42
             ; (smaller-than-n 28) -> true
             ; (smaller-than-n 54) -> false
             (define (smaller-than-n num) (< num n))
             (define smallish (filter smaller-than-n (rest lon)))
             (define (same-as-n num) (= num n))
             (define middlish (filter same-as-n (rest lon)))
             (define (bigger-than-n num) (> num n))
             (define biggish (filter bigger-than-n (rest lon))))
       (append (smarter-sort smallish) (cons n middlish) (smarter-sort biggish)))]))