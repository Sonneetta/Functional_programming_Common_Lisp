<p align="center"><b>МОНУ НТУУ КПІ ім. Ігоря Сікорського ФПМ СПіСКС</b></p>
<p align="center">
<b>Звіт з лабораторної роботи 4</b><br/>
"Функції вищого порядку та замикання"<br/>
дисципліни "Вступ до функціонального програмування"
</p>
<p align="right">Студентка: Михайліченко Софія Віталіївна КВ-11<p>
<p align="right">Рік: 2004<p>

## Загальне завдання
Завдання складається з двох частин:
1. Переписати функціональну реалізацію алгоритму сортування з лабораторної
роботи 3 з такими змінами:
 - використати функції вищого порядку для роботи з послідовностями (де це
доречно);
 - додати до інтерфейсу функції (та використання в реалізації) два ключових
параметра: key та test , що працюють аналогічно до того, як працюють
параметри з такими назвами в функціях, що працюють з послідовностями. При
цьому key має виконатись мінімальну кількість разів.
2. Реалізувати функцію, що створює замикання, яке працює згідно із завданням за варіантом . Використання псевдо-функцій не забороняється, але, за можливості, має бути мінімізоване.
## Варіант першої частини 13(5) варіант
Алгоритм сортування обміном No2 (із використанням прапорця) за незменшенням.
## Лістинг реалізації першої частини завдання
```lisp
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
```
### Тестові набори та утиліти першої частини
```lisp
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
```
### Тестування першої частини
```lisp
* (test-bubble-func )
Function bubble-imper 
Passed!!!! test-1
Passed!!!! test-2
Passed!!!! test-3
Passed!!!! test-4
Passed!!!! test-5
Passed!!!! test-6
Passed!!!! test-7
Passed!!!! test-8
Passed!!!! test-9
Passed!!!! test-10
Passed!!!! test-11
NIL
```
## Варіант другої частини 13(1) варіант
Написати функцію add-prev-fn , яка має один ключовий параметр — функцію
transform . add-prev-fn має повернути функцію, яка при застосуванні в якості першого аргументу mapcar разом з одним списком-аргументом робить наступне: кожен елемент списку перетворюється на точкову пару, де в комірці CAR знаходиться значення поточного елемента, а в комірці CDR знаходиться значення попереднього елемента списку. Якщо функція transform передана, тоді значення поточного і попереднього елементів, що потраплять у результат, мають бути змінені згідно transform . transform має виконатись мінімальну кількість разів.
```lisp
CL-USER> (mapcar (add-prev-fn) '(1 2 3))
((1 . NIL) (2 . 1) (3 . 2))
CL-USER> (mapcar (add-prev-fn :transform #'1+) '(1 2 3))
((2 . NIL) (3 . 2) (4 . 3))
```
## Лістинг функції з використанням деструктивного підходу
### Тестові набори та утиліти
### Тестування