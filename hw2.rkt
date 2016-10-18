#lang dssl

#| Cem Ozer && Can Aygen
HW2: Graphs
Due: Thursday, Oct. 27 at 11:59 PM, via Canvas

** You may work on your own or with one (1) partner. **
|#

;;;
;;; REPRESENTATION
;;;

;; Vertex is Natural
;; Weight is Number

;; MaybeWeight is one of:
;; -- Weight
;; -- #false

;; A WUGraph is [FILL IN YOUR REPRESENTATION HERE]
(define-struct WUGraph [vertex-list])



;;;
;;; GRAPH EXAMPLES
;;;

;; FILL THESE IN:
;; either adjacency list or adjecency matrix
(define GRAPH1 (WUGraph (vector (vector #false 2 #false 5)
                                (vector 2 #false 3 #false)
                                (vector #false 3 #false 4)
                                (vector 5 #false 4 #false))))
                         
(define GRAPH2 (WUGraph (vector (vector #false 5 #false #false #false #false)
                                (vector 5 #false 1 3 #false #false)
                                (vector #false 1 #false #false 2 7)
                                (vector #false 3 #false #false 4 6)
                                (vector #false #false 2 4 #false #false)
                                (vector #false #false 7 6 #false #false))))
;;;
;;; GRAPH OPERATIONS
;;;

;; make-graph : Natural -> WUGraph
;; Creates a new graph with the specified number of vertices.
(define (make-graph size)
  (WUGraph (build-vector size (lambda (x) (make-vector size #false)))))
;; DONE!!!

;; set-edge! : WUGraph Vertex Vertex MaybeWeight -> Void
;; Sets the edge (i, j) to have weight `weight`, where `#false` means
;;; there is no edge between `i` and `j`.
(define (set-edge! graph i j weight)
  (begin
    (vector-set! (vector-ref (WUGraph-vertex-list graph) i) j weight)
    (vector-set! (vector-ref (WUGraph-vertex-list graph) j) i weight)))
;  
;    
      
;; DONE!!!

;; graph-size : WUGraph -> Natural
;; Returns the number of vertices in the graph. This should always be
;; the same as the parameter passed to `make-graph`.
(define (graph-size graph)
  (vector-length (WUGraph-vertex-list graph)))
;; DONE!!!


;; get-edge : WUGraph Vertex Vertex -> MaybeWeight
;; Gets the weight of the edge between vertices `i` or `j`; returns
;; #false if there is no such edge.
(define (get-edge graph i j)
  (vector-ref (vector-ref (WUGraph-vertex-list graph) i) j))
;; DONE!!!

;; The following function uses this data definition for its result:


;; A [List-of Vertex] is one of:
;; -- '()
;; -- (cons Vertex [List-of Vertex])

;; get-adjacent : WUGraph Vertex -> [List-of Vertex]
;; Returns a list of all vertices adjacent to vertex `i`. The order of (cons curr adjacent-list)


;; the list is arbitrary.
(define (get-adjacent graph i)
  (define adjacent-list '())
  (define index -1)
  (for  ([curr (vector-ref (WUGraph-vertex-list graph) i)])
    (begin
      (set! index (+ index 1))
      (if (not (false? curr)) (set! adjacent-list (append adjacent-list (list index))) (void))))
  adjacent-list)
;; DONE!!!

;;;
;;; DFS
;;;

;; WUGraph Vertex -> [List-of Vertex]
;; Performs a depth-first search starting at `start` and returns a
;; list of all reachable vertices.
(define (dfs graph start)
  (define visited-list '())
  (set! visited-list (append visited-list (list start)))
  (for  ([curr (get-adjacent graph start)])
    (if (not(member curr visited-list))(dfs graph curr)(void)))
  visited-list)
      
  
  
;  
;;;;
;;; TESTING
;;;

;; You should test your code thoroughly. Here are some tests to get you
;; started, which you should uncomment when ready:


(check-expect (graph-size GRAPH1) 4)
(check-expect (graph-size GRAPH2) 6)

(check-expect (get-edge GRAPH1 0 1) 2)
(check-expect (get-edge GRAPH1 0 2) #false)

(check-expect (get-adjacent GRAPH1 0)
              (list 1 3))
(check-expect (get-adjacent GRAPH2 3)
              (list 1 4 5))

(check-expect
  (begin
    (define graph (make-graph 5))
    (set-edge! graph 1 3 10)
    (list (get-edge graph 1 3)
          (get-edge graph 1 3)
          (get-edge graph 1 4)))
  (list 10 10 #false))

;; DFS tests---see below for explanation of `sort`.

(check-expect
  (sort (dfs GRAPH1 0))
  '(0 1 2 3))

(check-expect
  (sort (dfs GRAPH2 0))
  '(0 1 2 3 4 5))

(check-expect
  (begin
    (define graph (make-graph 3))
    (set-edge! graph 0 2 10)
    (sort (dfs graph 0)))
  '(0 2))


;;;
;;; TESTING HELPERS
;;;

;; The following function may be convenient for creating graphs for
;; tests. It uses the graph API that you are defining above, so if you
;; define make-graph and set-edge! correctly then it will work.

;; build-graph : N [List-of (list Vertex Vertex Weight)] -> WUGraph
;; Returns a new graph of n vertices containing the given edges.
;(define (build-graph n edges)
;  (local [(define new-graph (make-graph n))]
;    (begin
;      (map (lambda (edge)
;             (set-edge! new-graph (first edge) (second edge) (third edge)))
;           edges)
;      new-graph)))

;; Here's an example using build-graph to create a graph:
;(define GRAPH3
;  (build-graph 6
;               '((0 1 5)
;                 (0 2 7)
;                 (0 3 2)
;                 (1 4 9)
;                 (1 5 6)
;                 (3 5 0)
;                 (3 4 1))))

;; DFS returns the list of nodes visited, but not in any particular
;; order, which makes it difficult to test. One way to test it is to
;; sort the resulting list and compare to the result we expect from
;; that. For example, suppose we have a graph where the DFS reaches
;; vertices 1, 2, and 3. Then the result of `dfs` could be (list 1 2 3)
;; or (list 2 1 3) or (list 3 1 2) etc. However, if we sort that then
;; the result of *that* should always be (list 1 2 3).

;; [List-of Number] -> [List-of Number]
;; Sorts a list of numbers.
(define (sort lst)
  (cond
    [(or (empty? lst) (empty? (rest lst)))
     lst]
    [else
      (define pivot (first lst))
      (define non-pivot (rest lst))
      (define before (filter (lambda (x) (< x pivot)) non-pivot))
      (define after (filter (lambda (x) (>= x pivot)) non-pivot))
      (append (sort before) (cons pivot (sort after)))]))

(check-expect (sort '()) '())
(check-expect (sort '(4)) '(4))
(check-expect (sort '(5 3)) '(3 5))
(check-expect (sort '(3 5)) '(3 5))
(check-expect (sort '(4 4 4)) '(4 4 4))
(check-expect (sort '(2 8 5 3 0 2 1)) '(0 1 2 2 3 5 8))

(run-all-tests)