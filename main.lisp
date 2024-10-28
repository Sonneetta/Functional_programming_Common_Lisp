(defun bubble-imper (list)
  "Імперативний варіант алгоритму сортування обміном №2 (із використанням прапорця) за незменшенням."
  (let ((a (copy-list list)))                 ; Копіюємо список, щоб не змінювати оригінал
    (let* ((n (length a)) 
           (flag t) 
           (r (1- n)))                        ; Ініціалізація змінних, якими будемо працювати із списком
      (do () ((not flag))                     ; Закінчуємо цикл, якщо flag є NIL
        (setf flag nil)                       ; Скидаємо прапорець
        (dotimes (i r)                        ; Проходимо через всі елементи до R
          (when (> (nth i a) (nth (1+ i) a))
            (rotatef (nth i a) (nth (1+ i) a)); Змінюємо місцями елементи
            (setf flag t)))                   ; Встановлюємо flag в t
      (decf r)))                              ; Зменшуємо R
  a))                                         ; Повертаємо відсортований список

(defun check-my-bubble-imper (name input expected)
  "Функція, яка виконує перевірку фактичного результату з очікуваним і виводить повідомлення про те, чи пройшла перевірка."
  (format t "~:[Failed....~;Passed!!!!~] ~a~%" 
          (equal (bubble-imper input) expected) 
          name))

(defun test-bubble-imper ()
  "Тестові набори для першої  функції."
  (format t "Function bubble-imper ~%")
  (check-my-bubble-imper "test-1" '(1 2 3 4) '(1 2 3 4))       
  (check-my-bubble-imper "test-2" nil nil)                           
  (check-my-bubble-imper "test-3" '(4 3 2 1 0) '(0 1 2 3 4))
  (check-my-bubble-imper "test-4" '(1 1 2 2) '(1 1 2 2 ))
  (check-my-bubble-imper "test-5" '(1) '(1))
  (check-my-bubble-imper"test-6" '(2 2 1 1 0) '(0 1 1 2 2))) 

(defun bubble-func (list)                                                  ; Функція виконує один прохід бульбашкового сортування з прапором
  (cond
    ((or (null list) (null (cdr list)))                                    ; Якщо список порожній або має один елемент, повертаємо його
     (values list nil))                                                    ; Повертаємо список і прапор nil, оскільки змін не було
     
    ((> (car list) (cadr list))                                            ; Якщо перший елемент більше другого
     (multiple-value-bind (remaining sorted-flag)                          ; Виконуємо рекурсивний виклик з оновленим списком
         (bubble-func (cons (car list) (cddr list)))                       ; Створюємо новий список з першим елементом у кінці
       (values (cons (cadr list) remaining) t)))                           ; Повертаємо оновлений список і прапор t, щоб позначити зміну

    (t                                                                     ; Інакше (якщо перший елемент менше або дорівнює другому)
     (multiple-value-bind (remaining sorted-flag) (bubble-func (cdr list)) ; Рекурсивно опрацьовуємо решту списку
       (values (cons (car list) remaining) sorted-flag)))))                ; Повертаємо список з початковим елементом і прапором

(defun bubble-sort-func (list)                                             ; Основна функція бульбашкового сортування
  (multiple-value-bind (sorted-lst flag) (bubble-func list)                ; Виконуємо прохід функції bubble-func
    (if flag                                                               ; Якщо прапор true (тобто були зміни)
        (bubble-sort-func sorted-lst)                                      ; Рекурсивно викликаємо bubble-sort-func для наступного проходу
        sorted-lst)))                                                      ; Інакше (якщо змін не було) повертаємо відсортований список


(defun check-my-bubble-func (name input expected)
  "Функція, яка виконує перевірку фактичного результату з очікуваним і виводить повідомлення про те, чи пройшла перевірка."
  (format t "~:[Failed....~;Passed!!!!~] ~a~%" 
          (equal (bubble-sort-func input) expected) 
          name))

(defun test-bubble-func ()
  "Тестові набори для другої  функції."
  (format t "Function bubble-imper ~%")
  (check-my-bubble-func "test-1" '(1 2 3 4) '(1 2 3 4))       
  (check-my-bubble-func "test-2" nil nil)                           
  (check-my-bubble-func "test-3" '(4 3 2 1 0) '(0 1 2 3 4))
  (check-my-bubble-func "test-4" '(1 1 2 2) '(1 1 2 2))
  (check-my-bubble-func "test-5" '(1) '(1))
  (check-my-bubble-func"test-6" '(2 2 1 1 0) '(0 1 1 2 2))) 