(defun bubble-func (list &key (key #'identity) (test #'>))                      ; Функція виконує один прохід бульбашкового сортування з прапором
  (cond
    ((or (null list) (null (cdr list)))                                         ; Якщо список порожній або має один елемент, повертаємо його
     (values list nil))                                                         ; Повертаємо список і прапор nil, оскільки змін не було 
    ((funcall test (funcall key (car list)) (funcall key (cadr list)))          ; Якщо перший елемент більше другого
     (multiple-value-bind (remaining sorted-flag)                               ; Виконуємо рекурсивний виклик з оновленим списком
       (bubble-func (cons (car list) (cddr list)) :key key :test test)          ; Створюємо новий список з першим елементом у кінці
       (values (cons (cadr list) remaining) t)))                                                                                                                                                                                                                         ; Повертаємо оновлений список і прапор t, щоб позначити зміну
    (t                                                                          ; Інакше (якщо перший елемент менше або дорівнює другому)
     (multiple-value-bind (remaining sorted-flag) 
       (bubble-func (cdr list) :key key :test test)                             ; Рекурсивно опрацьовуємо решту списку
       (values (cons (car list) remaining) sorted-flag)))))                     ; Повертаємо список з початковим елементом і прапором

(defun bubble-sort-func (list &key (key #'identity) (test #'>))                 ; Основна функція бульбашкового сортування
  (multiple-value-bind (sorted-lst flag) (bubble-func list :key key :test test) ; Виконуємо прохід функції bubble-func
    (if flag                                                                    ; Якщо прапор true (тобто були зміни)
        (bubble-sort-func sorted-lst :key key :test test)                       ; Рекурсивно викликаємо bubble-sort-func для наступного проходу
        sorted-lst)))  

(defun check-my-bubble-func (name input expected &key (key #'identity) (test #'>))
  "Функція, яка виконує перевірку фактичного результату з очікуваним і виводить повідомлення про те, чи пройшла перевірка."
  (let ((result (bubble-sort-func input :key key :test test)))
    (format t "~:[Failed....~;Passed!!!!~] ~a~%"
            (equal result expected)
            name)
    (when (not (equal result expected))
      (format t "  Expected: ~a~%  Got: ~a~%~%" expected result))))

(defun test-bubble-func ()
  "Тестові набори для першої функції."
  (format t "Function bubble-imper ~%")

  ;;Тести за замовчуванням
  (check-my-bubble-func "test-1" '(1 2 3 4) '(1 2 3 4))       
  (check-my-bubble-func "test-2" nil nil)                           
  (check-my-bubble-func "test-3" '(4 3 2 1 0) '(0 1 2 3 4))
  (check-my-bubble-func "test-4" '(1 1 2 2) '(1 1 2 2))
  (check-my-bubble-func "test-5" '(1) '(1))
  (check-my-bubble-func "test-6" '(2 2 1 1 0) '(0 1 1 2 2))

  ;;Тести з вибраними ключовими параметрами
  (check-my-bubble-func "test-7" '(4 2 5 3 1) '(5 4 3 2 1) :test #'<)
  (check-my-bubble-func "test-8" '((2 . 3) (1 . 2) (4 . 5) (3 . 4)) 
                                 '((1 . 2) (2 . 3) (3 . 4) (4 . 5)) 
                                   :key #'car :test #'>)  
  (check-my-bubble-func "test-9" '((2 . 3) (1 . 2) (4 . 5) (3 . 4)) 
                                 '((4 . 5) (3 . 4) (2 . 3) (1 . 2)) 
                                 :key #'cdr :test #'<)
  (check-my-bubble-func "test-10" '("t" "tttt" "tt" "ttt")
                                  '("tttt" "ttt" "tt" "t")
                                  :key #'length :test #'<)
  (check-my-bubble-func "test-11" '(-4 -2 -5 3 1) '(1 -2 3 -4 -5) :key #'abs :test #'>))