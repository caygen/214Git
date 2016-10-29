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
  ...)
;;;; my function is 7 lines (but see helpers below) ;;;;

; find-min : [Heap-of X] -> X
; Returns the least element in the heap.
; Error if the heap is empty.
(define (find-min heap)
  ...)
;;;; my function is 4 lines ;;;;

; remove-min! : [Heap-of X] -> Void
; Removes the least element in the heap.
; Error if the heap is empty.
(define (remove-min! heap)
  ...)
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
;;;; my function is 3 lines ;;;;

; heap:percolate-down! : [Heap-of X] N -> Void
; Restores the heap invariant by percolating down, starting with the element
; at `index`.
;;;; my function is 8 lines ;;;;

; heap:find-smaller-child : [Heap-of X] N -> [Maybe N]
; Finds the index of the smaller child of node `index`, or `#false` if
; it has no children.
;;;; my function is 9 lines ;;;;

; heap:bubble-up! : [Heap-of X] N -> Void
; Restores the heap invariant by bubbling up the element at `index`.
;;;; my function is 6 lines ;;;;

; heap:ref : [Heap-of X] N -> X
; Gets the heap element at `index`.
;;;; my function is 2 lines ;;;;

; heap:set! : [Heap-of X] N X -> Void
; Sets the heap element at `index`.
;;;; my function is 2 lines ;;;;

; heap:swap! : [Heap-of X] N N -> Void
; Swaps the heap elements at indices `i` and `j`.
;;;; my function is 5 lines ;;;;

; heap:lt? : [Heap-of X] N N -> Boolean
; Returns whether the element at `i` is less than the element at `j`
; using the heap's order.
;;;; my function is 2 lines ;;;;

; heap:left : N -> N+
; Computes the index of the left child of the given index.
;;;; my function is 2 lines ;;;;

; heap:right : N -> N+
; Computes the index of the left child of the given index.
;;;; my function is 2 lines ;;;;

; heap:parent : N+ -> N
; Computes the index of the parent of the given index.
;;;; my function is 2 lines ;;;;