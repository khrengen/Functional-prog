(defun matrix-t1-t2(n)
  (let ((i 0)                                            ;индексы для передвижения
        (j 0)                                            ;по массиву
        (count 1)
        (a (make-array (list n n) :initial-element 0)))  ;создаем матрицу n*n
                                                         ;с нулевыми элементами
    (setf (aref a i j) count)
    (loop 
     (when (= count (* n n)) (return a))                 ;условие окончания цикла
     
     (when (and (= i 0) (< j (- n 1)))                   ;упираемся сверху и справа есть место
       (incf j)                                          ;тогда делаем шаг вправо и
       (incf count)                                      
       (setf (aref a i j) count)
       (do () ((= j 0))                                  ;спускаемся влево по диагонали до упора
         (incf i)
         (decf j)
         (incf count)
         (setf (aref a i j) count)))

     (when (and (= j 0) (< i (- n 1)))                   ;упираемся слева и снизу есть место    
       (incf i)                                          ;тогда делаем шаг вниз и 
       (incf count)
       (setf (aref a i j) count)
       (do () ((= i 0))                                  ;поднимаемся вправо по диагонали до упора
         (decf i)
         (incf j)
         (incf count)
         (setf (aref a i j) count)))

     (when (and (= j (- n 1)) (< i (- n 1)))             ;упираемся справа и внизу есть место
       (incf i)                                          ;тогда делаем шаг вниз и 
       (incf count)
       (setf (aref a i j) count)
       (do () ((= i (- n 1)))                            ;спускаемся влево по диагонали до упора
         (incf i)
         (decf j)
         (incf count)
         (setf (aref a i j) count)))

     (when (and (= i (- n 1)) (< j (- n 1)))             ;упираемся снизу и справа есть место
       (incf j)                                          ;тогда делаем шаг вправо и
       (incf count)
       (setf (aref a i j) count)
       (do () ((= j (- n 1)))                            ;поднимаемся вправо по диагонали до упора
         (decf i)
         (incf j)
         (incf count)
         (setf (aref a i j) count))))))

(defun print-matrix (matrix &optional (chars 3) stream)  ;печать матрицы
  (let ((*print-right-margin* (+ 6  (* (1+ chars)
                                       (array-dimension matrix 1)))))
    (pprint matrix stream)
    (values)))
