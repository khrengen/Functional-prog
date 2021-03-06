(defun make-term (&key order coeff)
  (list order coeff))

(defun order (term) (first term))   ; �������
(defun coeff (term) (second term))  ; �����������

(defclass polynom ()
 ((var-symbol :initarg :var :reader var)
  ;; ����������� ������ ������ � ������� �������� �������
  (term-list :initarg :terms :reader terms)))

(defgeneric zerop1 (arg)
 (:method ((n number))   ; (= n 0)
  (zerop n)))

(defgeneric minusp1 (arg)
 (:method ((n number))   ; (< n 0)
  (minusp n)))

(defmethod print-object ((p polynom) stream)
  (format stream "[�� (~s) ~:{~:[~:[+~;-~]~d~[~2*~;~s~*~:;~s^~d~]~;~]~}]"
          (var p)
          (mapcar (lambda (term)
                    (list (zerop1 (coeff term))
                          (minusp1 (coeff term))
                          (if (minusp1 (coeff term))
                              (abs (coeff term))
                              (coeff term))
                          (order term)
                          (var p)
                          (order term)))
                  (terms p))))

(defun pow (x n)            ;���������� � �������
  (if (zerop n)
      1
    (* x (pow x (- n 1)))))

(defun funcall-polynom (p1 x) ;������� ��������
  (let ((res 0))
    (dolist (ter (terms p1))
      (setf res (+ res 
                   (* (coeff ter)
                      (pow x (order ter))))))
    res))
