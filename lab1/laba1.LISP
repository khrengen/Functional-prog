;;; Хренов Геннадий Николаевич М8О-307Б

;;; Лабораторная работа №1
;;;     Вариант 1.30


;; значение функции вычисляется итеративным процессом
  
(defun new_value (a b c)
  (* a                                                             ;a - значение f(n-1)
     (+ b 2)                                                       ;b - значение f(n-2)
     (+ c 3)))                                                     ;c - значение f(n-3)

(defun func-iter (a b c count)
  (if (< count 3)
      a
      (func-iter (new_value a b c) a b (- count 1))))


(defun func (n)                                                     ;итоговая функция
  (func-iter 1 1 1 n)) 
