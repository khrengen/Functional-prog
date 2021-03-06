(defun matrix-t1-t2(n)
  (let ((i 0)                                            ;������� ��� ������������
        (j 0)                                            ;�� �������
        (count 1)
        (a (make-array (list n n) :initial-element 0)))  ;������� ������� n*n
                                                         ;� �������� ����������
    (setf (aref a i j) count)
    (loop 
     (when (= count (* n n)) (return a))                 ;������� ��������� �����
     
     (when (and (= i 0) (< j (- n 1)))                   ;��������� ������ � ������ ���� �����
       (incf j)                                          ;����� ������ ��� ������ �
       (incf count)                                      
       (setf (aref a i j) count)
       (do () ((= j 0))                                  ;���������� ����� �� ��������� �� �����
         (incf i)
         (decf j)
         (incf count)
         (setf (aref a i j) count)))

     (when (and (= j 0) (< i (- n 1)))                   ;��������� ����� � ����� ���� �����    
       (incf i)                                          ;����� ������ ��� ���� � 
       (incf count)
       (setf (aref a i j) count)
       (do () ((= i 0))                                  ;����������� ������ �� ��������� �� �����
         (decf i)
         (incf j)
         (incf count)
         (setf (aref a i j) count)))

     (when (and (= j (- n 1)) (< i (- n 1)))             ;��������� ������ � ����� ���� �����
       (incf i)                                          ;����� ������ ��� ���� � 
       (incf count)
       (setf (aref a i j) count)
       (do () ((= i (- n 1)))                            ;���������� ����� �� ��������� �� �����
         (incf i)
         (decf j)
         (incf count)
         (setf (aref a i j) count)))

     (when (and (= i (- n 1)) (< j (- n 1)))             ;��������� ����� � ������ ���� �����
       (incf j)                                          ;����� ������ ��� ������ �
       (incf count)
       (setf (aref a i j) count)
       (do () ((= j (- n 1)))                            ;����������� ������ �� ��������� �� �����
         (decf i)
         (incf j)
         (incf count)
         (setf (aref a i j) count))))))

(defun print-matrix (matrix &optional (chars 3) stream)  ;������ �������
  (let ((*print-right-margin* (+ 6  (* (1+ chars)
                                       (array-dimension matrix 1)))))
    (pprint matrix stream)
    (values)))
