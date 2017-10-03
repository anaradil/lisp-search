;Anar Adilova
(defvar d)
(defvar nodes)

(defun geti(l i)
  (if (= i 0)
      (car l)
      (geti (cdr l) (- i 1))))

(defun counti(l x)
  (if (null l)
      0
      (if (= (car l) x)
          (+ 1 (counti (cdr l) x))
          (counti (cdr l) x))))
;root index is 0
;if vertex has an index v, then its children will be v * d + 1, v * d + 1, ... , v * d + d

(defun parent(p)
  (if (= 0 (rem p d))
      (setf p (- p 1)))
  (truncate p d))

;this function generates a list of adjacent vertices of v, children + parent
(defun adj(v)
  (setf l '())
  (setf child (* v d))
  (loop
    (setf child (+ child 1))
    (setf l (cons child l))
    (when (= child (+ (* v d) d)) (return l))))

(defun pushi(l x)
  (append l (list x))


(defun dfs(cur goal l dep path depth)
  (print (geti l cur))
  (if (= cur goal)
      (return-from dfs (cons (geti nodes cur) path)))
  (if (= depth 0)
      (return-from dfs nil))
  (let ((i -1)(to (adj cur)))
  (loop for i from 0 to (- (list-length to) 1) by 1 do
    (if (< (geti to i) (list-length l))
        (let ((ret (dfs (geti to i) goal l dep (cons (geti nodes cur) path) (- depth 1))))
          (if (neq nil ret)
              (return-from dfs ret))))))
  (return-from dfs nil)    
)

(defun iterative-deepening(start goal l dep)
  (loop for depth from 0 to 1000000 by 1 do
    (let ((ret (dfs start goal l dep '() depth)))
      (if (neq nil ret)
          (return-from iterative-deepening ret))))          
)

(defun getpath(cur)
  (if (= cur 0)
      (list (geti nodes cur))
      (cons (geti nodes cur) (getpath (parent cur))))
)

(defun bfs(start goal l dep)
  
  (let ((q nil) (cur -1) (to) (i -1))
    (setf q (pushi q start))
    (loop 
      (when (eq nil q) (return-from bfs nil))
      (setf cur (car q))
      (print (geti l cur))
      (setf q (cdr q))
      (setf to (adj cur))
      (if (= cur goal)
          (return-from bfs (getpath cur)))
      (loop for i from 0 to (- (list-length to) 1) by 1 do
        (if ( < (geti to i) (list-length l))
            (setf q (pushi q (geti to i)))))))
)

(defun findpos(l x)
  (let ((i nil))    
     (loop for i from 0 to (- (list-length l) 1) by 1 do
       (if (eq x (geti l i))
           (return-from findpos i))))
  (return-from findpos nil)
)

(defun searchi(start goal l dep switch)
  (setf nodes l)
  (setf start (findpos l start))
  (setf goal (findpos l goal))
  (setf d (counti dep 2))
  (if (= switch 1)
      (dfs start goal l dep '() 1000000) 
      (if (= switch 2)
          (iterative-deepening start goal l dep)
          (if (= switch 0)
              (bfs start goal l dep))))  
)


(print (reverse (searchi 'A 'E '(A B C D E F G H I J K L M) '(1 2 2 2 3 3 3 3 3 3 3 3 3) 0)))



