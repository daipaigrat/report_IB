# Анализ встроенного пакета dplyr

## Цель работы

Развить практические навыки использования функций обработки данных пакета dplyr – функции select(), filter(), mutate(), arrange(), group_by()

## Исходные данные

1.  Программное обеспечение MacOS 14.4.1 Sonoma
2.  Rstudio Desktop
3.  Интерпретатор языка R 4.4.1
4.  Программный пакет dplyr

## План

1.  Установить программный пакет dplyr
2.  Проанализировать набор данных
3.  Ответить на вопросы

## Шаги

1.  Для начала прохождения курса необходимо установить программный пакет dplyr.

```{r}
install.packages("dplyr")
```
2. Загружаем библиотеку.

```{r}
library(dplyr)
```
3. Выполняем задания.

  а) Сколько строк в датафрейме?
```{r}
starwars %>% nrow()
```
  
  b) Сколько столбцов в датафрейме?
```{r}
starwars %>% ncol()
```
  
  c) Как просмотреть примерный вид датафрейма?
```{r}
starwars %>% glimpse()
```
  
  d) Сколько уникальных рас персонажей (species) представлено в данных?
```{r}
starwars %>% filter(!is.na(species)) %>% reframe(unique(species))
```
  
  e) Найти самого высокого персонажа.
```{r}
starwars %>% filter(!is.na(height)) %>% arrange(desc(height)) %>% slice(1) %>% select(name,height)
```
  
  f) Найти всех персонажей ниже 170
```{r}
starwars %>% filter(!is.na(height) & height < 170) %>% select(name,height)

```
  
  g) Подсчитать ИМТ (индекс массы тела) для всех персонажей. ИМТ подсчитать по
  формуле I = m / h^2 где m - масса, h - рост
```{r}
starwars %>% filter(!is.na(mass) & !is.na(height)) %>% mutate(bmi = mass / (height/100)^2) %>% select(name,bmi)

```
  
  h) Найти 10 самых “вытянутых” персонажей. “Вытянутость” оценить по отношению
массы (mass) к росту (height) персонажей.
```{r}
starwars %>% filter(!is.na(mass) & !is.na(height)) %>% mutate(stretch = mass / height) %>% arrange(desc(stretch)) %>% slice(1:10) %>% select(name,stretch)
```

  i) Найти средний возраст персонажей каждой расы вселенной Звездных войн.
```{r}
starwars %>% filter(!is.na(species) & !is.na(birth_year)) %>% group_by(species) %>% summarise(average_age = mean(birth_year, na.rm = TRUE))
```
  
  j) Найти самый распространенный цвет глаз персонажей вселенной Звездных
войн.
```{r}
starwars %>% filter(!is.na(eye_color)) %>% group_by(eye_color) %>% summarise(count = n()) %>% arrange(desc(count)) %>% slice(1)
```

  k) Подсчитать среднюю длину имени в каждой расе вселенной Звездных войн.
```{r}
starwars %>% filter(!is.na(species) & !is.na(name)) %>% mutate(name_length = nchar(name)) %>% group_by(species) %>% summarise(len = mean(name_length, na.rm = TRUE))
```

## Оценка результата

В результате работы была скачана библиотека dplyr и были выполнены задания с использованием набора данных starwars.

## Вывод

Были изучены функции библиотеки dplyr. Были выполнены поставленные практикой задачи.
