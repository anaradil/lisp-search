;Anar Adilova
(defvar nodes)
(defvar h)
(defvar INF 1000000)

(defun geti(l i)
  (if (= i 0)
      (car l)
      (geti (cdr l) (- i 1))))


(defun findpos(l x)
  (let ((i nil))    
     (loop for i from 0 to (- (list-length l) 1) by 1 do
       ;(print x)
       ;(print (geti l i))
       (if (eq x (geti l i))
           (return-from findpos i))))
  (return-from findpos nil)
)

(defvar k 4.0)
(defvar matrix (make-array '(k k) :initial-contents '((0 1 1 0)
                                                    (1 0 1 0)
                                                    (0 0 0 1)
                                                    (1 1 0 0))
                         ))

;(defvar heu (make-array '(k) :initial-contents '(5 5 5 5))) ;inconsistent
;(defvar heu (make-array '(k) :initial-contents '(2 3 1 0))) ;consistent

;consistent works faster

;used O(n) additional memory
(defun astar(l matrix start goal heu)
  (tagbody
   (setf start (findpos l start))
   (setf goal (findpos l goal))
   

   (setf nodes l)
   (setf h heu)
   (setf n (list-length l))
 
   (setf dist (make-array '(4)))
   (setf state (make-array '(4))) ; 0 - if closed, 1 - if opened, nil if neither
   (setf parent (make-array '(4)))
   (loop for i from 0 to (- n 1) by 1 do
     (setf (aref dist i) INF))
   (setf (aref dist start) 0)
   (setf (aref state start) 1)
   (setf (aref parent start) -1)
   
   (loop for iter from 1 to n by 1 do
     (setf v -1)
     (loop for i from 0 to (- n 1) by 1 do
       (if (and (eq (aref state i) 1) (or (= v -1) (< (+ (aref dist v) (aref h v)) (+ (aref dist i) (aref h i)))))
           (setf v i)))
    
     (when (eq v goal) (go finish))
     (setf (aref state v) 0)
     (loop for i from 0 to (- n 1) by 1 do
       (if (and (= 1 (aref matrix v i)) (< (+ (aref dist v) 1) (aref dist i)))
           (progn 
           
             (setf (aref parent i) v)
             (setf (aref state i) 1)
             (setf (aref dist i) (+ (aref dist v) 1))))))
   finish
   (setf v goal)
   (setf path '())
  (loop
    (when (eq v -1) (return-from astar path))
    (setf path (cons (geti nodes v) path))
    (setf v (aref parent v)))
  )
  )

(print matrix)

(print (astar '(A B C D) matrix 'A 'D heu))