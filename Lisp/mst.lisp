;;;; -*- Mode: Lisp -*- 
;;;; Componenti del gruppo:
;;;; -Balzarotti Niccolo 852003
;;;; -Covelli Matteo 861277
;;;; -Ghanimi Alaa eddine 856573

(defparameter *vertices* (make-hash-table :test #'equal))
(defparameter *arcs* (make-hash-table :test #'equal))
(defparameter *graphs* (make-hash-table :test #'equal))
(defparameter *visited* (make-hash-table :test #'equal))
(defparameter *vertex-keys* (make-hash-table :test #'equal))
(defparameter *previous* (make-hash-table :test #'equal))
(defparameter *heaps* (make-hash-table :test #'equal))
(defparameter *positions* (make-hash-table :test #'equal))
(defparameter heap-id 'heap)


(defun is-graph (graph-id)
  (gethash graph-id *graphs*))

(defun new-graph (graph-id)
  (if (null graph-id)
      nil
    (or (gethash graph-id *graphs*)
        (setf (gethash graph-id *graphs*) graph-id))))    

(defun delete-graph (graph-id)
  (progn
    (maphash #'(lambda (k v) 
                 (cond ((equal (second k) graph-id)
                        (remhash k *arcs*))) v) *arcs*)
    (maphash #'(lambda (k v) 
                 (cond ((equal (second k) graph-id) 
                        (remhash k *vertices*))) v) *vertices*)
    (remhash graph-id *graphs*)
    nil))

(defun new-vertex (graph-id vertex-id)
  (cond ((null vertex-id) nil)
        ((not (atom vertex-id)) nil)
        ((stringp vertex-id) nil)
        ((null (is-graph graph-id)) nil)
        ((or (symbolp vertex-id) (numberp vertex-id))
         (setf (gethash (list 'vertex graph-id vertex-id) *vertices*)
               (list 'vertex graph-id vertex-id)))))

(defun graph-vertices (graph-id)
  (if (null (is-graph graph-id)) 
      ()
    (progn
      (let ((vertices ()))
        (maphash #'(lambda (k v)
                     (cond 
                      ((equal (second k) graph-id) 
                       (push v vertices)))) *vertices*) vertices))))

(defun new-arc (graph-id vertex-id-1 vertex-id-2 &optional weight)
  (progn
    (new-vertex graph-id vertex-id-1)
    (new-vertex graph-id vertex-id-2)
    (let ((reverse (gethash 
                    (list 'arc graph-id vertex-id-2 vertex-id-1
                          weight) *arcs*)))
      (cond
       ((null (is-graph graph-id)) nil)
       ((null (numberp weight)) nil)
       ((null (gethash (list 'vertex graph-id vertex-id-1) *vertices*))
        nil)
       ((null (gethash (list 'vertex graph-id vertex-id-2) *vertices*))
        nil)
       ((and (not (null reverse)) ;reverse esiste
             (equal (third reverse) vertex-id-2) 
             (equal (fourth reverse)  vertex-id-1))
        (list 'arc graph-id vertex-id-1 vertex-id-2 weight))

       ((not
         (null (gethash 
                (list 'arc graph-id vertex-id-1 vertex-id-2 weight) 
                *arcs*)))
        (list 'arc graph-id vertex-id-1 vertex-id-2 weight))
       ((update-arc graph-id vertex-id-1 vertex-id-2 weight)
        (list 'arc graph-id vertex-id-1 vertex-id-2 weight))
       (t (setf (gethash  
                 (list 'arc graph-id vertex-id-1 vertex-id-2 weight) 
                 *arcs*) 
                (list 'arc graph-id vertex-id-1 vertex-id-2 weight)))))))
  
                     

(defun update-arc (graph-id vertex-id-1 vertex-id-2 weight)
  (maphash 
   #'(lambda (k v)
       (cond
        ((and (equal (second k) graph-id)
              (equal (third v) vertex-id-1) 
              (equal (fourth k)  vertex-id-2)
              (not (equal (fifth k) weight)))
         (progn
           (remhash k *arcs*)
           (setf 
            (gethash 
             (list 'arc graph-id vertex-id-1 vertex-id-2 weight) 
             *arcs*)
            (list 'arc graph-id vertex-id-1 vertex-id-2 weight))) 
         T)

        ((and (equal (second k) graph-id)
              (equal (third k) vertex-id-2) 
              (equal (fourth k)  vertex-id-1)
              (not (equal (fifth k) weight)))
         (progn
           (remhash k *arcs*)
           (setf 
            (gethash 
             (list 'arc graph-id vertex-id-1 vertex-id-2 weight) 
             *arcs*)
            (list 'arc graph-id vertex-id-1 vertex-id-2
                  weight)) 
           T)))) *arcs*))     



(defun graph-arcs (graph-id)
  (if (null (is-graph graph-id)) 
      ()
    (progn
      (let ((arcs ()))
          (maphash #'(lambda (k v)
                       (cond ((equal (second k) graph-id)  
                              (push k arcs))) v) *arcs*) arcs))))
       
(defun graph-vertex-neighbors (graph-id vertex-id)
  (cond ((null (is-graph graph-id)) nil)
        ((null vertex-id) nil)
        ((null (atom vertex-id)) nil)
        (t (progn
             (let ((neighbors ()))
               (maphash 
                #'(lambda (k v)
                    (cond 
                     ((and 
                       (equal (second k) graph-id) 
                       (equal (third k) vertex-id)) 
                      (push v neighbors))
                     ((and 
                       (equal (second k) graph-id)
                       (equal (fourth k) vertex-id)) 
                      (push (list 'arc graph-id vertex-id 
                                  (third k) (fifth k)) neighbors)))) 
                *arcs*) neighbors)))))      

(defun graph-vertex-adjacent (graph-id vertex-id)
  (cond ((null (is-graph graph-id)) nil)
        ((null vertex-id) nil)
        ((null (atom vertex-id)) nil)
        (t (progn
             (let ((adj ()))
               (maphash #'(lambda (k v)
                            (cond ((and (equal (second k) graph-id)
                                        (equal (third k) vertex-id))
                                   (push (list 'vertex graph-id
                                               (fourth v)) adj))
                                  ((and (equal (second k) graph-id)
                                        (equal (fourth k) vertex-id)) 
                                   (push (list 'vertex graph-id
                                               (third v)) adj))))
                        *arcs*) adj)))))


(defun graph-print (graph-id)
  (if (null (is-graph graph-id))
      nil
    (progn
      (print (graph-vertices graph-id))
      (print (graph-arcs graph-id))
      T)))
      
 
     
(defun new-heap (heap-id &optional capacity) 
  (or (gethash heap-id *heaps*) 
      (setf (gethash heap-id *heaps*)
            (list 'heap heap-id 0 (make-array  capacity)))))

(defun heap-delete (heap-id)
  (progn
    (remhash heap-id *heaps*)
    T))

(defun heap-size (heap-id)
  (let ((entry (gethash heap-id *heaps*)))
    (cond
     ((null entry) -1) ;heap non esiste
     (T (third entry)))))


(defun heap-actual-heap (heap-id)
  (fourth (gethash heap-id *heaps*)))

(defun heap-empty (heap-id)
  (cond
   ((null (gethash heap-id *heaps*)) nil)
   ((= (heap-size heap-id) 0) T)
   ((> (heap-size heap-id) 0) nil)))
 
(defun heap-id (heap-id)
  (cond
   ((null (gethash heap-id *heaps*)) nil)
   (t t)))

(defun heap-not-empty (heap-id)
  (cond
   ((null (gethash heap-id *heaps*)) nil)
   ((= (heap-size heap-id) 0) nil)
   ((> (heap-size heap-id) 0) T)))


(defun heap-print (heap-id)
  (if (null (gethash heap-id *heaps*))
      nil
    (progn
      (format t "HEAP:~%Nome: ~S~%" (second (gethash heap-id *heaps*)))
      (format t "Heap-size: ~D~%" (heap-size heap-id))
      (format t "Heap: ~A" (heap-actual-heap heap-id))
     T)))

(defun heap-head (heap-id)
  (if (null (gethash heap-id *heaps*))
      ()
    (progn
      (let ((head (aref (heap-actual-heap heap-id) 0)))
        (cond
         ((heap-not-empty heap-id) head))))))


(defun parent (i)
  (if (evenp i)
      (1- (floor i 2))
    (floor i 2)))

(defun left (i)
  (+ (* 2 i) 1))

(defun right (i)
  (+ (* 2 i) 2))

(defun get-val (heap-id pos)
  (aref (heap-actual-heap heap-id) pos))



(defun heapify (heap-id i)
  (if (and (not (null (gethash heap-id *heaps*))) (< i (heap-size heap-id)))
      (if (or (< (left i) (heap-size heap-id))
              (< (right i) (heap-size heap-id)))

          (let
              ((index-l (left i)) ;indice left
               (index-r (right i)) ;indice right
               (hs (heap-size heap-id))
               (left (first (get-val heap-id (left i))))
               (right (first (get-val heap-id (right i))))
               (val-i (first (get-val heap-id i)))
               (index-max i))
            (progn
              (if (and (not (null left)) (<= index-l hs) (< left val-i))
                  (setf index-max index-l))
             
              (if (and (not (null right)) (<= index-r hs) 
                       (< right (first (get-val heap-id index-max))))
                  (setf index-max index-r))
             
              (if (/= index-max i)
                  (progn
                    (swap heap-id i index-max)
                    (setf (gethash (get-val heap-id index-max)
                                   *positions*) index-max)
                    (setf (gethash (get-val heap-id i)
                                   *positions*) i)
                    (heapify heap-id index-max)
                    
                    T))
              T))
        T)
    nil))
        
        
(defun swap (heap-id i j)
  (if (null (gethash heap-id *heaps*))
      nil
    (progn
      (let ((aus (aref (heap-actual-heap heap-id) i)))
        (setf (aref (heap-actual-heap heap-id) i)
              (aref (heap-actual-heap heap-id) j))
        (setf (aref (heap-actual-heap heap-id) j)
              aus))
      T)))


(defun heap-extract (heap-id)
  (if (null (gethash heap-id *heaps*))
      ()
    (if (< (heap-size heap-id) 1)
        ()
      (let ((max (get-val heap-id 0))
            (hs (heap-size heap-id)))
        (progn
         
          (setf (aref (heap-actual-heap heap-id) 0)
                (get-val heap-id (1- hs)))

          (setf (gethash (get-val heap-id (1- hs)) *positions*) 0)

          (setf (third (gethash heap-id *heaps*))
                (1- hs))
          (setf (aref (heap-actual-heap heap-id) (1- hs))
                nil)
          (remhash max *positions*)
          (remhash (get-val heap-id (1- hs)) *positions*)

          (heapify heap-id 0)
          max)))))


   
(defun heap-insert (heap-id k v) 
  (if (null (gethash heap-id *heaps*))
      ()
    (progn
      (let ((pos -1))
        (maphash #'(lambda (key value)
                     (if (and (equal (second key) v) (/= (first key) k))
                         (progn
                           (setf pos (gethash key *positions*))
                           value))) *positions*)
     
        (if (>= pos 0)
            (heap-modify-key heap-id k (first (get-val heap-id pos)) v)

          (progn
            (setf (third (gethash heap-id *heaps*))
                  (1+ (heap-size heap-id)))
            (setf (aref (heap-actual-heap heap-id) 
                        (1- (heap-size heap-id))) 
                  (list most-positive-double-float -1))

            (heap-increase-key heap-id (heap-size heap-id) k v))))
      T)))
       
        

(defun heap-increase-key (heap-id i k v)
  (progn
    (setf (aref (heap-actual-heap heap-id) (1- i)) 
          (list k v))
    (check-parent heap-id (1- i))))
  
  
(defun check-parent (heap-id i)
  (if (> i 0)
      (let 
          ((parent (first (get-val heap-id (parent i))))
           (son (first (get-val heap-id i))))
        
        (if (and (> i 0) (< son parent))                                   
            (progn 
              (swap heap-id i (parent i)) 
              (setf (gethash (get-val heap-id (parent i)) *positions*)
                    (parent i))
              (setf (gethash (get-val heap-id i) *positions*)
                    i)
              (check-parent heap-id (parent i)))

          (setf (gethash (get-val heap-id i) *positions*) i)))

    (setf (gethash (get-val heap-id i) *positions*) i)))
  


(defun heap-modify-key (heap-id new-key old-key v)
  (if (not (null (gethash heap-id *heaps*)))
      (progn 
        (let ((pos-v (gethash (list old-key v) *positions*)))
 
          (if (> new-key old-key)
              (progn
                (setf (aref (heap-actual-heap heap-id) pos-v)
                      (list new-key v))
                (remhash (list old-key v) *positions*)
                (setf (gethash (list new-key v) *positions*) pos-v)
                (heapify heap-id pos-v))
                 
            (if (< new-key old-key)
                (progn ;else new-key < old-key
                  (setf (aref (heap-actual-heap heap-id) pos-v)
                        (list new-key v))
                  (remhash (list old-key v) *positions*)
                  (setf (gethash (list new-key v) *positions*) pos-v)
                  (check-parent heap-id pos-v))))))))
        
               
(defun initialization (graph-id source)
  (if (null (gethash graph-id *graphs*))
      nil
    (progn   
      (clrhash *previous*)
      (clrhash *vertex-keys*)
      (clrhash *positions*)
      (let ((v (graph-vertices graph-id)))
        (if (not (null (gethash (list 'vertex graph-id source)
                                *vertices*)))
            (progn
              (new-heap heap-id (length v))
              (initialization-vertices graph-id heap-id source v))))))) 

(defun initialization-vertices (graph-id heap-id source vertices)
  (progn
    (let ((val (car vertices)))
      (cond
       ((null val) T)
       ((equal (third val) source)
        (progn 
          (setf (gethash (list graph-id source) *vertex-keys*) 0)
          (setf (gethash (list graph-id source) *previous*) nil)
          (heap-insert heap-id 0 (third val))
          (initialization-vertices graph-id heap-id source
                                   (rest vertices))))
       ((not (equal (third val) source))
        (progn
          (setf (gethash (rest val) *vertex-keys*)
                most-positive-double-float)
          (setf (gethash (rest val) *previous*) nil)
          (heap-insert heap-id (gethash (rest val) *vertex-keys*)
                       (third val))
          (initialization-vertices graph-id heap-id source 
                                   (rest vertices))))))))

(defun update (graph-id heap-id v neighbors)
  (progn
    (if (not (null neighbors))
        (progn
          (let ((pos ())
                (weight-n (fifth (car neighbors)))
                (neighbor (fourth (car neighbors))))

            (maphash #'(lambda (key value)
                         (cond
                          ((equal (second key) neighbor)
                           (push value pos)))) *positions*) 
       
            (if (and (not (null pos)) (>= (car pos) 0))
                (progn
                  (let ((weight-v (first (get-val heap-id (car pos)))))
                    (cond
                     ((< weight-n weight-v)
                      (progn
                        (heap-insert heap-id weight-n neighbor)
                        (setf (gethash (list graph-id neighbor)
                                       *vertex-keys*) weight-n)
                        (setf (gethash (list graph-id neighbor)
                                       *previous*) v)))))))
            (update graph-id heap-id v (rest neighbors))))
      T)))
                            
             
              
(defun mst-vertex-key (graph-id vertex-id)
  (if (null (gethash graph-id *graphs*))
      nil
    (gethash (list graph-id vertex-id) *vertex-keys*)))

(defun mst-previous (graph-id v)
  (if (null (gethash graph-id *graphs*))
      nil
    (gethash (list graph-id v) *previous*)))

(defun mst-prim (graph-id source)
  (if (and (not (null (gethash graph-id *graphs*)))
           (not (null (gethash (list 'vertex graph-id source)
                               *vertices*))))
      (progn
        (if (null (gethash heap-id *heaps*))
            (initialization graph-id source))
        (progn
          (if (heap-not-empty heap-id)
              (progn
                (let ((v (heap-extract heap-id)))
                  (let ((n (graph-vertex-neighbors graph-id 
                                                   (second v)))) 
                    (update graph-id heap-id (second v) n)))
                (mst-prim graph-id (second (heap-head heap-id)))))
          (if (= (heap-size heap-id) 0)
              (heap-delete heap-id))
          nil))))
        
                  
(defun mst-get (graph-id source)
  (progn
    (cond 
     ((and (is-graph graph-id)
           (not (null (gethash (list 'vertex graph-id source)
                               *vertices*))))
      (progn
        (remhash (list graph-id source) *previous*)
        (remhash (list graph-id source) *vertex-keys*)
        (let ((neighbor ()))
          (maphash #'(lambda (k v)
                       (cond
                        ((equal v source)
                         (progn
                           (push (list 'arc graph-id source (second k)
                                       (mst-vertex-key graph-id
                                                       (second k))) 
                                 neighbor)
                           (remhash k *previous*)
                           (remhash k *vertex-keys*))))) *previous*)
           
          (if (> (length neighbor) 0)
              (progn
                (sort neighbor (sort-arcs 'weight< 'adj<))
                (append
                 (list (first neighbor))
                 (mst-get graph-id (fourth (first neighbor)))
                 (call-mst-get graph-id source (rest neighbor)))))))))))
     


    
(defun call-mst-get (graph-id source list)
  (cond
   ((null list) nil)
   (t (progn
        (let ((list1 (mst-get graph-id (fourth (first list))))
              (list2 (call-mst-get graph-id source (rest list))))
          (append (list (first list)) list1 list2))))))
      
                              
(defun sort-arcs (original-predicate next-predicate)
  (lambda (x y)
    (cond
     ((funcall original-predicate x y) t)
     ((funcall original-predicate y x) nil)
     (t (funcall next-predicate x y)))))

(defun weight< (arc1 arc2)
  (< (fifth arc1)
     (fifth arc2)))

(defun adj< (arc1 arc2)
  (let ((id1 (fourth arc1))
        (id2 (fourth arc2)))
    (cond
     ((and (numberp id1) (symbolp id2)) T)
     ((and (numberp id2) (symbolp id1)) nil)
     ((and (numberp id1) (numberp id2)) (< id1 id2))               
     ((and (symbolp id1) (symbolp id2)) 
      (string< (write-to-string id1)
               (write-to-string id2))))))
    
      

