(defun check+ (text)                    
  (dolist (sent text)                ;����� ����� �� �����������
    (if (find #\+ sent)              ; ���� �� ����?
        (return sent))))

(defun digit-to-minus (inText)
  (let ((text inText)) 
    (dolist (sent text)
      (dotimes (i (length sent))                ;����� ����������� �� �������
        (when (char= (char sent i) #\+)         ;����� ����� ������ ������ �� ����
          (return-from digit-to-minus  text))   ;��������� ������ ���� �������
        (when (digit-char-p (char sent i))      ;����� ������ �� �����
          (setf (char sent i) #\-))))))

(defun lab4 (text)
  (if (check+ text)                  ;���� ���� ���� 
      (digit-to-minus text)          ;�������� ����� �� ����� �� ����
    text))                           ;����� ��������� �������� �����
