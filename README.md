# HW06

1. Реализуйте парсер для предложенного вами конкретного синтаксиса языка L (функция `parseL`).
   1. Обязательно написать тесты
   2. Если в процессе написания парсера окажется, что ваш конкретный синтаксис неудобен, можно его изменить, но напишите об этом явно.
2. Поддержите в `uberExpr` унарные операции.
   1. Я обновила сигнатуру функции, теперь в списке операций могут встречаться унарные операции.
   2. Обновите `parseExpr`, чтобы он поддерживал унарные операции `-` и `!` со следующим приоритетом. В качестве базового используйте парсер положительного числа (не `parseNegNum`).

         | Приоритет | Оператор             | Арность  | Ассоциативность   |
         | :-------- | :------------------- | :------- | :---------------- |
         | Высший    | ^                    | Бинарный | Правоассоциативна |
         |           | -                    | Унарный  |                   |
         |           | *, /                 | Бинарный | Левоассоциативна  |
         |           | +, -                 | Бинарный | Левоассоциативна  |
         |           | ==, /=, <=, <, >=, > | Бинарный | Неассоциативна    |
         |           | !                    | Унарный  |                   |
         |           | &&                   | Бинарный | Правоассоциативна |
         | Низший    | \|\|                 | Бинарный | Правоассоциативна |

# Замечания

* Написанные юнит-тесты должны проходить.
* Можно добавлять свои юнит-тесты.
* Можно изменять любой код, имеющийся в репозитории, при условии сохранения сигнатур имеющихся функций и юнит-тестов.
* Можно писать задание на любом другом языке, при условии максимального сохранения сигнатур функций, юнит-тестов и предоставление инструкций по сборке и запуску вашего проекта.

# Сборка и запуск

* `stack build` для сборки
* `stack test` для запуска тестов
* `stack exec expr` исполнит соответствующий исполняемый файл

# Жесткий дедлайн: 23:59 02.04
