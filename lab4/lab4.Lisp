(defun check+ (text)                    
  (dolist (sent text)                ;делим текст на предложения
    (if (find #\+ sent)              ; есть ли плюс?
        (return sent))))

(defun digit-to-minus (inText)
  (let ((text inText)) 
    (dolist (sent text)
      (dotimes (i (length sent))                ;делим предложение на символы
        (when (char= (char sent i) #\+)         ;после плюса ничего менять не надо
          (return-from digit-to-minus  text))   ;завершаем работу всей функции
        (when (digit-char-p (char sent i))      ;цифры меняем на минус
          (setf (char sent i) #\-))))))

(defun lab4 (text)
  (if (check+ text)                  ;если плюс есть 
      (digit-to-minus text)          ;заменяем цифры на минус до него
    text))                           ;иначе возращаем исходный текст
