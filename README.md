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
<Лістинг реалізації першої функції>
```
### Тестові набори
```lisp
<Лістинг реалізації тестових наборів першої функції>
```
### Тестування
```lisp
<Виклик і результат виконання тестів першої функції>

```
## Лістинг функції <назва другої функції>
```lisp
<Лістинг реалізації другої функції>
```
### Тестові набори
```lisp
<Лістинг реалізації тестових наборів другої функції>
```
### Тестування
```lisp
<Виклик і результат виконання тестів другої функції>