;;; ������ �������� ���������� �8�-307�

;;; ������������ ������ �1
;;;     ������� 1.30


;; �������� ������� ����������� ����������� ���������
  
(defun new_value (a b c)
  (* a                                                             ;a - �������� f(n-1)
     (+ b 2)                                                       ;b - �������� f(n-2)
     (+ c 3)))                                                     ;c - �������� f(n-3)

(defun func-iter (a b c count)
  (if (< count 3)
      a
      (func-iter (new_value a b c) a b (- count 1))))


(defun func (n)                                                     ;�������� �������
  (func-iter 1 1 1 n)) 
