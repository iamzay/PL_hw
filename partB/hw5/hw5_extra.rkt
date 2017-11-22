; Racket structs
(struct btree-leaf () #:transparent)
(struct btree-node (value left right) #:transparent)

(define (tree-height t)
  (if (btree-leaf? t)
      0
      (+ 1 (max (tree-height (btree-node-left t))
                (tree-height (btree-node-right t))))))

(define (sum-tree t)
  (if (btree-leaf? t)
      0
      (+ (btree-node-value t)
         (sum-tree (btree-node-left t))
         (sum-tree (btree-node-right t)))))

(define (prune-at-v t v)
  (cond [(btree-leaf? t) (btree-leaf)]
        [(= (btree-node-value t) v) (btree-leaf)]
        [#t (btree-node (btree-node-value t)
                        (prune-at-v (btree-node-left t))
                        (prune-at-v (btree-node-right t)))]))

(define (fold-tree f acc t)
  (if (btree-leaf? t)
      acc
      ()))
