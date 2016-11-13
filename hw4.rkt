#lang dssl

#|Cem Ozer && Can Aygen
HW4: Union-Find
Due: Thursday, Nov. 17 at 11:59 PM, via Canvas

** You may work on your own or with one (1) partner. **
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; PART I: UNION-FIND ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; A UnionFind is [YOUR DEFINITION HERE]
(define-struct UnionFindEntry (id size))


; create : N -> UnionFind
; Creates a new union-find structure having `size` initially-disjoint
; sets numbered 0 through `(- size 1)`.
(define (create size)
  (build-vector size  (lambda (x) (UnionFindEntry x 1))))
;;;; My function is 5 lines using ASL’s `build-vector` ;;;;

; size : UnionFind -> N
; Returns the number of objects in `uf`.
(define (size uf)
  (return (vector-length uf)))

;;;; My function is 2 lines ;;;;

(check-expect (size (create 12)) 12)

; same-set? : UnionFind N N -> Boolean
; Returns whether objects `obj1` and `obj2` are in the same set.
(define (same-set? uf obj1 obj2)
  (if (equal? (find uf obj1)(find uf obj2))
      (return #t)
      (return #f)))
;;;; My function is 2 lines ;;;;

; find : UnionFind N -> N
; Finds the representative (root) object for `obj`.
(define (find uf obj)
  (let loop ((i obj))
    (define parent (UnionFindEntry-id (uf:get-entry uf i)))
    (if (equal? i parent)
        (return i)
        (begin
          (set-UnionFindEntry-id! (uf:get-entry uf i) (UnionFindEntry-id (uf:get-entry uf parent)))
          (loop parent)))))
;;;; My function is 10 lines (using one helper) ;;;;

; union : UnionFind N N -> Void
; Unions the set containing `obj1` with the set containing `obj2`.
(define (union! uf obj1 obj2)
  (define root1 (find uf obj1))
  (define root2 (find uf obj2))
  (if (not (equal? root1 root2))
      (if (< (UnionFindEntry-size (uf:get-entry uf root1))(UnionFindEntry-size (uf:get-entry uf root2)))
          (begin
            (set-UnionFindEntry-id! (uf:get-entry uf root2) root1)
            (set-UnionFindEntry-size!  (uf:get-entry uf root1) (+ (UnionFindEntry-size (uf:get-entry uf root1)) (UnionFindEntry-size (uf:get-entry uf root2)))))
          (begin
            (set-UnionFindEntry-id! (uf:get-entry uf root1) root2)
            (set-UnionFindEntry-size!  (uf:get-entry uf root2) (+ (UnionFindEntry-size (uf:get-entry uf root1)) (UnionFindEntry-size (uf:get-entry uf root2))))))
      (void)))
          
;;;; My function is 12 lines (using two helpers) ;;;;

;;;
;;; SUGGESTED HELPERS
;;;

;; The suggested helpers below assume a type UnionFindEntry
;; that contains both the parent id and the weight for one
;; object.

; uf:reparent! : UnionFindEntry UnionFindEntry -> Void
; Sets the parent of `child` to be `parent` and adjusts `parent`’s
; weight accordingly.
 (define (uf:reparent! child parent)...)
   
;;; My function is 5 lines ;;;;

; uf:get-entry : UnionFind N -> UnionFindEntry
; Gets the entry for object `ix`.
 (define (uf:get-entry uf ix)
   (vector-ref uf ix))

;;;; My function is 2 lines ;;;;


;;;
;;; UNION-FIND TESTING
;;;

; The code below gives a clean way to test your union-find code. The
; idea is that you write a “script” consisting of “union” commands and
; “same” queries, and then running the script returns a list of the
; results of the 'same queries.

; A UnionFindCommand is one of:
; - (list 'union N N)
; - (list 'same N N)
; Interp.:
; - (list 'union m n) means to union the sets containing `m` and `n`
; - (list 'same m n) means to check whether `m` and `n` are in the same
;   set, producing a boolean in the script output

; A UnionFindScript is [List-of UnionFindCommand]

; run-script : N UnionFindScript -> [List-of Boolean]
; Runs the given script on a new UnionFind universe of size `n`
; and returns the list of query results.
(define (run-script n script)
  (interpret-script! (create n) script))

; interpret-script! : UnionFind UnionFindScript -> [List-of Boolean]
; Runs the given script on a the given UnionFind universe and returns the
; list of query results.
(define (interpret-script! uf script)
  (local
    [(define (interpret-command command)
       (if (symbol=? (first command) 'union)
           (begin
             (union! uf (second command) (third command))
             (interpret-script! uf (rest script)))
           (local
             [(define b (same-set? uf (second command) (third command)))]
             (cons b (interpret-script! uf (rest script))))))]
    (if (null? script) '()
        (interpret-command (first script)))))

; Now some example tests:
;
;(check-expect
; (run-script 10 '())
; '())
;
;(check-expect
; (run-script 10
;   '((same 0 1)
;     (same 0 2)
;     (same 0 3)))
; '(#false #false #false))
;
;(check-expect
; (run-script 10
;   '((same 0 1)
;     (union 0 1)
;     (same 0 1)
;     (union 1 2)
;     (union 2 3)
;     (same 0 3)
;     (same 0 4)))
; '(#false #true #true #false))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; PART II: KRUSKAL’S MST ALGORITHM ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; kruskal-mst : WUGraph -> WUGraph
;; Returns the minimum spanning forest for a given graph, represented as
;; another graph.
(define (kruskal-mst g)
  ...)
;;;; my function is 14 lines, using several helpers (below) ;;;;


;;;
;;; KRUSKAL HELPERS YOU MAY FIND USEFUL
;;;

;; get-all-edges/increasing : WUGraph -> [List-of (list Vertex Vertex)]
;; Gets a list of all the edges in the graph sorted by increasing weight;
;; includes only one (arbitrary) direction for each edge.
(define (get-all-edges/increasing g)
  ...)
;;;; my function is 4 lines ;;;;

;; get-all-edges : WUGraph -> [List-of (list Vertex Vertex)]
;; Gets all the edges in a graph as a list of 2-element lists; includes
;; only one (arbitrary) direction for each edge.
(define (get-all-edges g)
  ...)
;;;; my function is 13 lines ;;;;

;; heap-sort : [Ord X] [List-of X] -> [List-of X]
;; Sorts a list based on the given less-than function.
(define (heap-sort lt? xs)
  ...)
;;;; my function is 10 lines ;;;;
;; [Impl. note: a heap sort works by inserting every element into a heap
;; and then removing every element, which yields the elements in sorted
;; order. You may of course use a different sort if you wish.]

;; pq-extract-min! : [Heap X] -> X
;; Removes and returns the minimum heap element.
(define (pq-extract-min! heap)
  ...)
;;;; my function is 4 lines ;;;;
;;;;;


;;;
;;; MST TESTING
;;;

;; The following function may be convenient for creating graphs for
;; tests. It uses the graph API from HW2, so if you have defined
;; make-graph and set-edge! correctly then it will work.

;; build-graph : N [List-of (list Vertex Vertex Weight)] -> WUGraph
;; Returns a new graph of n vertices containing the given edges.
;(define (build-graph n edges)
;  (local [(define new-graph (make-graph n))]
;    (begin
;      (map (lambda (edge)
;             (set-edge! new-graph (first edge) (second edge) (third edge)))
;           edges)
;      new-graph)))
;
;(define EXAMPLE-GRAPH-0
;  (build-graph 6
;               '((0 1 5)
;                 (0 2 7)
;                 (0 3 2)
;                 (1 4 9)
;                 (1 5 6)
;                 (3 5 0)
;                 (3 4 1))))
;
;(check-expect (graph-size EXAMPLE-GRAPH-0) 6)
;(check-expect (get-edge EXAMPLE-GRAPH-0 0 1) 5)
;(check-expect (get-edge EXAMPLE-GRAPH-0 1 0) 5)
;(check-expect (get-edge EXAMPLE-GRAPH-0 0 2) 7)
;(check-expect (get-edge EXAMPLE-GRAPH-0 2 0) 7)
;(check-expect (get-edge EXAMPLE-GRAPH-0 3 5) 0)
;(check-expect (get-edge EXAMPLE-GRAPH-0 5 3) 0)
;(check-expect (get-edge EXAMPLE-GRAPH-0 0 4) #false)
;(check-expect (get-edge EXAMPLE-GRAPH-0 4 0) #false)
;
;;; Note that my get-adjacent returns a sorted list, but yours doesn’t
;;; need to---and if it doesn’t then you will have to modify these tests.
;(check-expect (get-adjacent EXAMPLE-GRAPH-0 0) '(1 2 3))
;(check-expect (get-adjacent EXAMPLE-GRAPH-0 1) '(0 4 5))
;(check-expect (get-adjacent EXAMPLE-GRAPH-0 5) '(1 3))
;
;;; This graph looks like a "wagon wheel" with six spokes emanating from
;;; vertex 6 in the center. The weights of the spokes are mostly less
;;; than the weights along the perimeter, except that 3 is closer to 2
;;; than it is to 6. Thus, the resulting MST is all spokes except that it
;;; connects 3 to 2 rather than to 6.
;(define EXAMPLE-GRAPH-1
;  (build-graph 7
;               '((0 1 3)
;                 (1 2 3)
;                 (2 3 1)
;                 (3 4 3)
;                 (4 5 3)
;                 (6 0 2)
;                 (6 1 2)
;                 (6 2 2)
;                 (6 3 3)
;                 (6 4 2)
;                 (6 5 2))))
;
;(define EXAMPLE-MST-1 (kruskal-mst EXAMPLE-GRAPH-1))
;
;(check-expect (get-adjacent EXAMPLE-MST-1 0) '(6))
;(check-expect (get-adjacent EXAMPLE-MST-1 1) '(6))
;(check-expect (get-adjacent EXAMPLE-MST-1 2) '(3 6))
;(check-expect (get-adjacent EXAMPLE-MST-1 3) '(2))
;(check-expect (get-adjacent EXAMPLE-MST-1 4) '(6))
;(check-expect (get-adjacent EXAMPLE-MST-1 5) '(6))
;(check-expect (get-adjacent EXAMPLE-MST-1 6) '(0 1 2 4 5))

;; You probably need more tests than these.