<p align="center"><b>МОНУ НТУУ КПІ ім. Ігоря Сікорського ФПМ СПіСКС</b></p>
<p align="center">
<b>Звіт з лабораторної роботи 2</b><br/>
"Рекурсія"<br/>
дисципліни "Вступ до функціонального програмування"
</p>
<p align="right">Студентка: Михайліченко Софія Віталіївна КВ-11<p>
<p align="right">Рік: 2004<p>

## Загальне завдання:
Реалізуйте дві рекурсивні функції, що виконують деякі дії з вхідним(и) списком(-ами), за
можливості/необхідності використовуючи різні види рекурсії. Функції, які необхідно
реалізувати, задаються варіантом. Вимоги до функцій:

1. Зміна списку згідно із завданням має відбуватись за рахунок конструювання нового
списку, а не зміни наявного (вхідного).

2. Не допускається використання функцій вищого порядку чи стандартних функцій
для роботи зі списками, що не наведені в четвертому розділі навчального
посібника.

3. Реалізована функція не має бути функцією вищого порядку, тобто приймати функції
в якості аргументів.

4. Не допускається використання псевдофункцій (деструктивного підходу).

5. Не допускається використання циклів.

Кожна реалізована функція має бути протестована для різних тестових наборів. Тести
мають бути оформленні у вигляді модульних тестів.

Додатковий бал за лабораторну роботу можна отримати в разі виконання всіх наступних
умов:
- робота виконана до дедлайну (включно з датою дедлайну);
- крім основних реалізацій функцій за варіантом, також реалізовано додатковий
варіант однієї чи обох функцій, який працюватиме швидше за основну реалізацію,
не порушуючи при цьому перші три вимоги до основної реалізації (вимоги 4 і 5
можуть бути порушені), за виключенням того, що в разі необхідності можна також
використати стандартну функцію copy-list.

## Варіант 13:
1. Написати функцію <b>remove-even-triples </b>, яка видалить парні трійки послідовних
елементів зі списку:
```
CL-USER> (remove-even-triples '(a b c d e f g h))
(A B C G H)
```
2. Написати функцію <b>decompress-list</b> , яка "розпакує" зі списку пар виду (кількість-
повторень елемент) послідовність елементів визначеної кількості:
```
CL-USER> (decompress-list '((1 1) (2 a) (3 3) (1 4)))
(1 A A 3 3 3 4)
```
## Лістинг функції remove-even-triples:
```lisp
(defun remove-even-triples (lst &optional (index 1))
  "Рекурсивна функція, яка видаляє трійки послідовних елементів, які стоять на парних позиціях в списку."
  (cond
    ((or (null lst) (null (cdr lst)) (null (cddr lst))) lst)    ; Якщо список порожній або має менше трьох елементів
    ((evenp index)                                              ; Якщо індекс парний, пропускаємо трійку і збільшуємо індекс
     (remove-even-triples (cdddr lst) (1+ index)))
    (t                                                          ; Якщо індекс непарний, зберігаємо трійку і продовжуємо
     (list* (car lst) (cadr lst) (caddr lst)
            (remove-even-triples (cdddr lst) (1+ index))))))  

```
### Тестові набори
```lisp
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
```
### Тестування
```
(test-function-1) 
Function 1 
Passed!!!! test-1
Passed!!!! test-2
Passed!!!! test-3
Passed!!!! test-4
Passed!!!! test-5
Passed!!!! test-6
NIL

```
## Лістинг функції decompress-list
```lisp
(defun decompress-list (lst)
  "Основна функція, що обробляє список пар (кількість повторень та елемент)."
  (if (null lst)                                              ; Якщо список порожній
      nil                                                     ; Повертаємо nil
      (let ((count (caar lst))                                ; Отримуємо кількість повторень з першої пари
            (elem (cadar lst)))                               ; Отримуємо сам елемент з першої пари
        (decompress-list-helper count elem (cdr lst)))))      ; Викликаємо допоміжну функцію

(defun decompress-list-helper (count elem rest)
  "Допоміжна функція, що додає елемент `elem` `count` разів і продовжує розпакування."
  (if (<= count 0)                                            ; Якщо кількість повторень менша або дорівнює нулю
      (decompress-list rest)                                  ; Продовжуємо з рештою списку
      (cons elem                                              ; Якщо більше 0, додаємо елемент до результату
            (decompress-list-helper (1- count) elem rest))))  ; Рекурсивно викликаємо, зменшуючи `count`

 

```
### Тестові набори
```lisp
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
```
### Тестування
```
(test-function-2) 
Function 2 
Passed!!!! test-1
Passed!!!! test-2
Passed!!!! test-3
Passed!!!! test-4
Passed!!!! test-5
Passed!!!! test-6
NIL
```
### Додаткове завдання 
```lisp
(defun decompress-list-1 (lst &optional (result nil))
  "Рекурсивна функція для розпакування зі списку, зберігаючи результати в зворотному порядку."
  (if (null lst)                                                      ; Якщо список порожній
      (nreverse result)                                               ; Повертаємо результат у правильному порядку
      (let ((count (caar lst))                                        ; Отримуємо кількість повторень
            (elem (cadar lst)))                                       ; Отримуємо сам елемент
        (if (or (null count) (<= count 0))                            ; Якщо кількість повторень менша або дорівнює нулю
            (decompress-list-1 (cdr lst) result)                      ; Продовжуємо з рештою списку
            (decompress-list-1                                        ; Інакше, викликаємо з оновленим списком
                             (cons (list (1- count) elem) (cdr lst))  ; Зменшуємо count на 1
                             (cons elem result))))))                  ; Додаємо елемент до результату
```
### Тестові набори
```lisp
(defun check-my-function-3 (name input expected)
  "Функція, яка виконує перевірку фактичного результату з очікуваним і виводить повідомлення про те, чи пройшла перевірка."
  (format t "~:[Failed....~;Passed!!!!~] ~a~%" 
          (equal (decompress-list-1 input) expected) 
          name))

(defun test-function-3 ()
  "Тестові набори для другої рекурсивної функції."
  (format t "Function 3 ~%")
  (check-my-function-3 "test-1" '(() () ()) '())         
  (check-my-function-3 "test-2" nil nil)                           
  (check-my-function-3 "test-3" '((1 2) (2 1)) '(2 1 1))
  (check-my-function-3 "test-4" '((1 a) (2 b) (0 c)) '(a b b))
  (check-my-function-3 "test-5" '((0 2)) '())
  (check-my-function-3 "test-6" '((1 2)) '(2)))


```
### Тестування
```
(load "main.lisp" )
Function 3 
Passed!!!! test-1
Passed!!!! test-2
Passed!!!! test-3
Passed!!!! test-4
Passed!!!! test-5
Passed!!!! test-6
T
```