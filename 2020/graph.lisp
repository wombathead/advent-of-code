;;;; A collection of graph functions

(ql:quickload :alexandria)

(defpackage :graph
  (:use :cl :cl-user :alexandria)
  (:export :print-graph
           :reverse-edges
           :get-sources
           :breadth-first-search
           :depth-first-search
           :topological-ordering
           :count-paths
           :edmonds-karp
           ))

(in-package :graph)

;; TODO: convert to use graph structure
(defun print-graph (G)
  (maphash #'(lambda (k v) (format T "~A: ~{~A ~}~%" k v))
           G))

(defun reverse-edges (G)
  " reverse all edges in digraph G "
  (let ((reversed (make-hash-table :test 'equal)))
    (maphash #'(lambda (u adjacent)
                 (loop for v in adjacent do
                       (push u (gethash v reversed))))
             G)
    reversed))

(defun get-sources (G)
  " return a list of all sources in graph G "
  (let ((in-degrees (make-hash-table)))

    ;; initialise in-degrees
    (alexandria:maphash-keys #'(lambda (u) (setf (gethash u in-degrees) 0)) G)

    (loop for u being each hash-key of G do
          (mapcar #'(lambda (v)
                      (if (numberp (gethash v in-degrees))
                          (incf (gethash v in-degrees))
                          (setf (gethash v in-degrees) 1)))
                  (gethash u G)))
    (remove-if-not #'(lambda (u) (= 0 (gethash u in-degrees)))
                   (loop for k being each hash-key of G collect k))))

(defun breadth-first-search (s G)
  " perform BFS traversal on G starting from S "
  (let ((tree (make-hash-table :test 'equal))
        (discovered (make-hash-table :test 'equal))
        queue)
    (setf (gethash s discovered) T)  ; mark s as discoverd
    (push s queue)
    (loop while queue do
          (let ((u (pop queue)))
            ;; for each node v adjacent to u
            (loop for v in (gethash u G) do
                  (unless (gethash v discovered)
                    (setf (gethash v discovered) T)         ; mark v discovered
                    (setf queue (append queue (list v)))    ; enqueue v
                    (push v (gethash u tree))))))           ; add edge in tree

    ;; return the tree and the size of the graph
    (values tree (hash-table-count discovered))))

(defun depth-first-search (s G)
  " perform DFS traversal on G starting from s "
  (let ((tree (make-hash-table :test 'equal))
        (discovered (make-hash-table :test 'equal))
        stack)
    (setf (gethash s discovered) T)  ; mark s as discoverd
    (push s stack)
    (loop while stack do
          (let ((u (pop stack)))
            ;; for each node v adjacent to u
            (loop for v in (gethash u G) do
                  (unless (gethash v discovered)
                    (setf (gethash v discovered) T) ; mark v discovered
                    (push v stack)                  ; push v onto stack
                    (push v (gethash u tree))))))   ; add edge in tree

    ;; return the tree and the size of the graph
    (values tree (hash-table-count discovered))))

(defun topological-ordering (G)
  " return a list of vertices that is a topological ordering on G using Kahn's algorithm "
  (let ((sources (get-sources G))
        (H (alexandria:copy-hash-table G))
        ordering)
    (loop for adjacent in (alexandria:hash-table-values H) do
          (loop for v in adjacent do
                (if (not (gethash v H))
                    (setf (gethash v H) NIL))))
    
    (loop while sources do
          (let ((u (pop sources)))
            (push u ordering)
            (loop while (gethash u H) do
                  (let ((v (pop (gethash u H))))
                    ;; if v has no incoming edges
                    (if (member v (get-sources H))
                        (push v sources))))))

    (unless (member NIL (mapcar #'(lambda (u) (not (gethash u H)))
                                (alexandria:hash-table-keys H)))
      ordering)))

(defun count-paths (G u v)
  " count the number of paths from source U to (some) sink V in DAG G "
  (let ((topo (topological-ordering G))
        (paths (make-hash-table))
        (w v))
    (setf (gethash v paths) 1)
    (setf w (pop topo))
    (loop while (not (equal w u)) do
          ; go to the next node in the ordering 
          (setf w (pop topo))

          ;; count number of paths for every neigbour of w in G
          (let ((neighbour-paths (mapcar #'(lambda (n)
                                             (gethash n paths))
                                         (gethash w G))))

            (unless (eq neighbour-paths NIL)
              (setf (gethash w paths) (reduce #'+ neighbour-paths)))))
    (gethash u paths)))

;;; TODO: matching algorithms: ford-fulkerson (edmonds-karp), hopcroft-karp, more???
