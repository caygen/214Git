#lang dssl
(define-struct WUGraph (V(vector)))
(define GRAPH1 (WUGraph ((vector #f 2 #f 5)
                         (vector 2 #f 3 #f)
                         (vector #f 3 #f 4)
                         (vector 5 #f 4 #f))))
(define GRAPH2 (WUGraph ((vector 