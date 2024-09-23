<p align="center"><b>МОНУ НТУУ КПІ ім. Ігоря Сікорського ФПМ СПіСКС</b></p>
<p align="center">
<b>Звіт з лабораторної роботи 1</b><br/>
"Обробка списків з використанням базових функцій"<br/>
дисципліни "Вступ до функціонального програмування"
</p>
<p align="right">Студентка: Михайліченко Софія Віталіївна КВ-11<p>
<p align="right">Рік: 2004<p>

### Загальне завдання

#### Пункт 1
Створіть список з <b>п'яти елементів</b>, використовуючи функції LIST і CONS . Форма створення списку має бути одна — використання SET чи SETQ (або інших допоміжних форм) для збереження проміжних значень не допускається. Загальна кількість елементів (включно з підсписками та їх елементами) не має перевищувати 10-12 шт. (дуже великий список робити не потрібно). Збережіть створений список у якусь змінну з SET або SETQ . Список має містити (напряму або у підсписках): 

- хоча б один символ
- хоча б одне число
- хоча б один не пустий підсписок
- хоча б один пустий підсписок
```
(setq my-list (list 'a
                    4 
                    (cons 'b (list 1 2)) 
                    ()
                    (cons 'd nil))) 
```
Результат виконання:
```
(A 4 (B 1 2) NIL (D))
```
#### Пункт 2

Отримати голову списку :
```
(car my-list) 
```
Результат виконання:
```
A
```
#### Пункт 3
Отримати хвіст списку:
```
(cdr my-list) 
```
Результат виконання:
```
(4 (B 1 2) NIL (D))
```
#### Пункт 4
Отримайте третій елемент списку:
```
(car (cdr(cdr my-list)))
```
Результат виконання:
```
(B 1 2)
```
#### Пункт 5

Отримайте останній елемент списку:

1.
```
(car (cdr(cdr(cdr(cdr(cdr my-list))))))
```
Результат виконання:
```
(D)
```
2.
```
(car(last my-list))
```
Результат виконання:
```
(D)
```
#### Пункт 6
Використайте предикати ATOM та LISTP на різних елементах списку (по 2-3
приклади для кожної функції):

1. Предикати ATOM:
Спочатку перевіримо як поводитиме себе предикат ATOM, коли агрументом буде атомом — тобто числом або символом:

(A):
```
(atom (car my-list) )
```
Результат виконанння:
```
T
```
Тепер розглянемо, коли аргумент є списком (B 1 2):
```
(ATOM (car(cdr(cdr my-list))))
```
Результат виконанння:
```
NIL
```
Також перевіримо як поводитиме себе при аргументі nil:
```
(atom(cadddr my-list))    
```
Результат виконанння:
```
T
```
2. Предикат LISTP(визначає, чи є аргумент списком):
Спочатку перевіримо як поводитиме себе предикат LISTP, коли агрументом буде атомом — тобто числом або символом (4):
```
(LISTP(cadr my-list)) 
```
Результат виконанння:
```
NIL
```
Тепер розглянемо, коли аргумент є списком (B 1 2):
```
(listp(caddr my-list))
```
Результат виконанння:
```
T
```
Також перевіримо як поводитиме себе при аргументі nil:
```
(listp(cadddr my-list)) 
```
Результат виконанння:
```
T
```
#### Пункт 7
Використайте на елементах списку 2-3 інших предикати з розглянутих у розділі 4
навчального посібника:
- NUMBERP(перевіряє, чи є аргумент числом):
  - Для числа (4):
  ```
  (numberp (cadr my-list)) 
  ```
  Результтат виконання:
  ```
  T
  ```
  - Для символа (A):
  ```
  (numberp(car my-list)) 
  ```
  Результат виконанння:
  ```
  NIL
  ```
- /=(перевірка чисел на нерівність):
   - Перевіряємо чи число 4 не дорівнює саме собі, тобто 4:
   ```
   (/= (cadr my-list)(cadr my-list)) 
   ```
   Результат виконанння:
   ```
   NIL
   ```
   - Перевірямо чи число 1 та 2 з підсписка є нерівними:
   ```
   (/=(car(cdaddr my-list))(cadr (cdaddr my-list))) 
   ```
   Результат виконанння:
   ```
   T
   ```
- EQUAL(можна порівнювати списки та рядки):

   - Порівняємо один і той же список (D):
   ```
   (equal (last my-list)(last my-list)) 
   ```
   Результат виконання:
   ```
   T
   ```
   - Порівняємо два списки (D) та (B 1 2): 
   ```
   (equal(caddr my-list)(last my-list)) 
   ``` 
   Результат виконання:
   ```
   NIL
   ```

#### Пункт 8
Об'єднайте створений список з одним із його непустих підсписків. Для цього
використайте функцію APPEND:
```
(append my-list (caddr my-list)) 
```
  Результат виконання:
   ```
   (A 4 (B 1 2) NIL (D) B 1 2)
   ```
### Варіант 5(13):
