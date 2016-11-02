#lang dssl

#|Cem Ozer && Can Aygen
HW3: Binary Heaps
EECS 214, Fall 2016

Due: Thursday, November 3, at 11:59 PM, on Canvas

*** You may work on your own or with one (1) partner. ***
|#

; An [Ord X] is a function [X X -> Boolean]
; Interpretation: a total order on X

; A [Heap-of X] is (make-heap Natural [Ord X] [Vector-of X])
(define-struct heap [size lt? data])
;
; Interpretation: Given a heap h,
; - (heap-size h) is the number of elements in the heap
; - (heap-lt? h) is the ordering used by the heap
; - (heap-data h) is a vector containing the heap's elements, where the
;   first (heap-size h) elements are an implicit complete binary tree (i.e.,
;   they contain the level-order traversal of the represented tree as we saw
;   in class.
;
; Invariant: The implicit tree satisfies the min-heap property; that is,
; if c is the value of some element and p is the value of its parent then
;    (not ((heap-lt? h) c p))
; must be true. That is, children cannot be less than parents.

; create : N [Ord X] -> [Heap-of X]
; Creates a new heap with capacity `capacity` and order `lt?`.
(define (create capacity lt?)
  (make-heap 0 lt? (make-vector capacity #false)))

; insert! : [Heap-of X] X -> Void
; Adds an element to a heap.
; Error if the heap has reached capacity and cannot grow further.
(define (insert! heap new-element)
  (begin
    (set-heap-data! heap (heap-data (ensure-size! heap)))
    (hset! heap (heap-size heap) new-element)
    (bubble-up! heap (heap-size heap))
    (set-heap-size! heap (+ (heap-size heap) 1))
    ))
;;;; my function is 7 lines (but see helpers below) ;;;;

; find-min : [Heap-of X] -> X
; Returns the least element in the heap.
; Error if the heap is empty.
(define (find-min heap)
  (if (= (heap-size heap) 0)
      (error "Heap empty")
      (href heap 0)))
;;;; my function is 4 lines ;;;;

; remove-min! : [Heap-of X] -> Void
; Removes the least element in the heap.
; Error if the heap is empty.
(define (remove-min! heap)
  (cond
    ((= (heap-size heap) 0) (error "Heap empty" ))
    (else (begin
           (define ret (href heap 0))
           (hset! heap 0 (href heap (- (heap-size heap) 2)))
           (percolate-down! heap 0)
           (hset! heap (- (heap-size heap) 2) #false)
           ))))


;;;; my function is 9 lines (but see helpers below) ;;;;


;;;;
;;;; PRIVATE HELPERS
;;;;

;;;;
;;;; Below are the helpers that I used to implement my solution. You
;;;; needn’t use the same design that I did, but they may help.
;;;;

; A [Maybe X] is one of:
; -- #false
; -- X
; Interpretation: maybe an X, or maybe not

; N is the set of natural numbers
; N+ is the set of positive integers

; heap:ensure-size! : [Heap-of X] N -> Void
; Ensures that the heap has room for `size` elements by throwing an error
; if it doesn't.
;(define (ensure-size! h)
;  (if (> (vector-length (heap-data h)) (heap-size h)) #t (error "Capacity full")))

(define (ensure-size! h)
  (if (> (vector-length (heap-data h)) (heap-size h))
      h
      (begin
          (define new (create (* 2 (heap-size h)) (heap-lt? h)))
          (set-heap-size! new (heap-size h))
          (let copy ([i 0])
            (if (>= i (heap-size h)) new (begin
                                           (hset! new i (href h i))
                                           (copy (+ 1 i))))))))
;;;; my function is 3 lines ;;;;

; heap:percolate-down! : [Heap-of X] N -> Void
; Restores the heap invariant by percolating down, starting with the element
; at `index`.
(define (percolate-down! h i)
  (define schildind (find-smaller-child h i))
  (if (not  (equal? schildind #f))
      (if (> (href h i) (href h schildind))
          (begin
            (bubble-up! h schildind)
            (percolate-down! h schildind))
          (void))
      (void)))
;;;; my function is 8 lines ;;;;

; heap:find-smaller-child : [Heap-of X] N -> [Maybe N]
; Finds the index of the smaller child of node `index`, or `#false` if
; it has no children.
(define (find-smaller-child h i)
  (if (>= (left i)(heap-size h))
      (equal? 1 0)
      (if (and (< (right i)(heap-size h)) (<= (href h (right i))(href h (left i))))
          (right i)
          (left i))))
;;;; my function is 9 lines ;;;;

;; DONE
; heap:bubble-up! : [Heap-of X] N -> Void
; Restores the heap invariant by bubbling up the element at `index`.
(define (bubble-up! h i)
  (cond
    ((= i 0) (void))
    ((hlt? h (parent i) i) (void))
    (else
     (begin
       (swap! h i (parent i))
       (bubble-up! h (parent i))))))
;;;; my function is 6 lines ;;;;

;; DONE
; heap:ref : [Heap-of X] N -> X
; Gets the heap element at `index`.
(define (href h i)
  (vector-ref (heap-data h) i))
;;;; my function is 2 lines ;;;;

;; DONE
; heap:set! : [Heap-of X] N X -> Void
; Sets the heap element at `index`.
(define (hset! h i element)
  (vector-set! (heap-data h) i element))
;;;; my function is 2 lines ;;;;

;; DONE
; heap:swap! : [Heap-of X] N N -> Void
; Swaps the heap elements at indices `i` and `j`.
(define (swap! h i j)
  (define temp 0)
  (begin
    (set! temp (href h i))
    (hset! h i (href h j))
    (hset! h j temp)))

;; DONE
; heap:lt? : [Heap-of X] N N -> Boolean
; Returns whether the element at `i` is less than the element at `j`
; using the heap's order.
(define (hlt? h i j)
  ((heap-lt? h) (href h i) (href h j)))
;;;; my function is 2 lines ;;;;

;; DONE
; heap:left : N -> N+
; Computes the index of the left child of the given index.
(define (left i)
  (+ (* i 2) 1))
;;;; my function is 2 lines ;;;;

;; DONE
; heap:right : N -> N+
; Computes the index of the left child of the given index.
(define (right i)
  (+ (* i 2) 2))
;;;; my function is 2 lines ;;;;

;; DONE
; heap:parent : N+ -> N
; Computes the index of the parent of the given index.
(define (parent i)
  (floor (/ (- i 1) 2)))
;;;; my function is 2 lines ;;;;

;;example heap
(define hex (create 12 <))
(insert! hex 0)
(insert! hex 2)
(insert! hex 4)
(insert! hex 6)
(insert! hex 8)
(insert! hex 10)
(insert! hex 12)
(insert! hex 14)
(insert! hex 16)
(insert! hex 18)
(insert! hex 19)

(define hev (create 10 <))
(insert! hev 0)
(insert! hev 2)
(insert! hev 4)
(insert! hev 6)
(insert! hev 8)
(insert! hev 10)


(define hez (heap 10 <  (vector 0 2 6 8 18 12 14 16 10 #f)))
;;;;;;;;Check-Expects;;;;;;;;
;;check create capacity ==== PASS
(define h1 (create 5 <))
(check-expect (heap-data h1)
              (vector #f #f #f #f #f))
(check-expect (heap-size h1)
              0)
;;check insert!         ==== PASS
(check-expect (begin
                (insert! h1 20)
                ;; 20
                ;;{20}
                (insert! h1 30)
                ;;  20
                ;; /
                ;;30
                ;;{20,30}
                (insert! h1 40)
                ;;  20
                ;; /  \
                ;;30   40
                ;;{20,30,40}
                (insert! h1 5)
                ;;    20
                ;;   /  \
                ;;  30  40
                ;; /
                ;;5
                ;;{20,30,40,5}
                ;;becomes
                ;;    5
                ;;   / \
                ;;  20 40
                ;; /
                ;;30
                ;;{5,20,40,30}
                (insert! h1 3)
                ;;just like above it becomes
                ;;    3
                ;;   / \
                ;;  5   40
                ;; / \
                ;;30  20
                ;;{3,5,40,30,20}
                (heap-data h1))
              (vector 3 5 40 30 20))
;;check find-min        ==== PASS
(check-expect (begin
                (insert! h1 20)
                (insert! h1 1)
                (insert! h1 2)
                (insert! h1 54)
                (find-min h1))
              1)
;;check remove-min!      ==== FAIL
(check-expect (begin
                (define h2 (create 5 <))
                (insert! h2 20)
                (insert! h2 1)
                (insert! h2 2)
                (insert! h2 54)
                (remove-min! h2)
                (heap-data h2))
              (vector 2 20 54 #f #f))
;;check percolate-down! ==== X
#|
(check-expect (begin
...)
|#

;;check extra credit    ==== PASS
(check-expect (begin (define h3 (create 2 <))
                     (insert! h3 3)
                     (insert! h3 5)
                     (insert! h3 0)
                     (heap-data h3))
              (vector 0 5 3 #f))