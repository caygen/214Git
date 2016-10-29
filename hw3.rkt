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
  (if (ensure-size! heap) (begin
        (hset! heap (heap-size heap) new-element)
        (bubble-up! heap (heap-size heap))
        (set-heap-size! heap (+ (heap-size heap) 1)))(void)))
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
  (...))
   
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
(define (ensure-size! h)
  (if (> (vector-length (heap-data h)) (heap-size h)) #t (error "Capacity full"))) 
;;;; my function is 3 lines ;;;;

; heap:percolate-down! : [Heap-of X] N -> Void
; Restores the heap invariant by percolating down, starting with the element
; at `index`.
;;;; my function is 8 lines ;;;;

; heap:find-smaller-child : [Heap-of X] N -> [Maybe N]
; Finds the index of the smaller child of node `index`, or `#false` if
; it has no children.
(define (find-smaller-child h i)
  (if (>= (left i)(heap-size h))
      (#false)
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
(define hex (create 11 <))
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
(insert! hex 17)