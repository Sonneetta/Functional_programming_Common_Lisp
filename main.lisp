(defun remove-even-triples (lst &optional (index 1))
  "Рекурсивна функція, яка видаляє трійки послідовних елементів, які стоять на парних позиціях в списку."
  (cond
    ((< (length lst) 3) lst)                                    ; Якщо список порожній або має менше трьох елементів
    ((evenp index)                                              ; Якщо індекс парний, пропускаємо трійку і збільшуємо індекс
     (remove-even-triples (cdddr lst) (1+ index)))
    (t                                                          ; Якщо індекс непарний, зберігаємо трійку і продовжуємо
     (append (list (car lst) (cadr lst) (caddr lst))
             (remove-even-triples (cdddr lst) (1+ index))))))

(defun check-my-function-1 (name input expected)
  "Функція, яка виконує перевірку фактичного результату з очікуваним і виводить повідомлення про те, чи пройшла перевірка."
    (format t "~:[Failed....~;Passed!!!!~] ~a~%"
            (equal (remove-even-triples input) expected)
            name))

(defun test-function-1 ()
  "Тестові набори для першої рекурсивної функції."
  (format t "Function 1 ~%")
  (check-my-function-1 "test-1" '(1 2 3 5 6 7 8) '(1 2 3 8))         
  (check-my-function-1 "test-2" nil nil)                           
  (check-my-function-1 "test-3" '(a b c) '(a b c))
  (check-my-function-1 "test-4" '(a b c d a a) '(a b c))
  (check-my-function-1 "test-5" '(a b c a b c a) '(a b c a))
  (check-my-function-1 "test-6" '(a 1) '(a 1)))                        

(defun decompress-list (lst)
  "Рекурсивна функція, яка розпаковує зі списку пар виду (кількість повторень та елемент) послідовність елементів визначеної кількості."
  (cond
    ((null lst) nil)                                                                     ; Якщо список пустий, повертаємо nil
    (t (let ((count (caar lst))                                                          ; Отримуємо кількість повторень
              (elem (cadar lst)))                                                        ; Отримуємо сам елемент
         (if (or (null count) (<= count 0))                                                                ; Якщо кількість повторень менша або дорівнює нулю
             (decompress-list (cdr lst))                                                 ; Продовжуємо з рештою списку
             (cons elem (decompress-list (cons (list (1- count) elem) (cdr lst)))))))))  ; Інакше, додаємо елемент до результату рекурсії
 


(defun check-my-function-2 (name input expected)
  "Функція, яка виконує перевірку фактичного результату з очікуваним і виводить повідомлення про те, чи пройшла перевірка."
  (format t "~:[Failed....~;Passed!!!!~] ~a~%"
          (equal (decompress-list input) expected)
          name))

(defun test-function-2 ()
  "Тестові набори для першої рекурсивної функції."
  (format t "Function 2 ~%")
  (check-my-function-2 "test-1" '(() () ()) '())         
  (check-my-function-2 "test-2" nil nil)                           
  (check-my-function-2 "test-3" '((1 2) (2 1)) '(2 1 1))
  (check-my-function-2 "test-4" '((1 a) (2 b) (0 c)) '(a b b))
  (check-my-function-2 "test-5" '((0 2)) '())
  (check-my-function-2 "test-6" '((1 2)) '(2)))
