---
title: "Продолжение работы с пакетом dplyr"
author: "lieless@yandex.ru"
format: 
  md:
    output-file: README.md
---

# Анализ встроенного пакета dplyr

## Цель работы

Развить практические навыки использования функций обработки данных пакета dplyr – функции select(), filter(), mutate(), arrange(), group_by()

## Исходные данные

1.  Программное обеспечение MacOS 14.4.1 Sonoma
2.  Rstudio Desktop
3.  Интерпретатор языка R 4.4.1
4.  Программный пакет dplyr
5.  Программный пакет nyclights13

## План

1.  Установить программный пакет nyclights13
2.  Проанализировать набор данных
3.  Ответить на вопросы

## Шаги

1.  Для начала прохождения курса необходимо установить пакет nyclights13.

```{r}
install.packages("nycflights13")
```
2. Загружаем библиотеки.

```{r}
library(nycflights13)
library(dplyr)
```

3. Переходим непосредственно к анализу набора данных nycflights13 и выполнению заданий:
  
  a) Сколько встроенных в пакет nycflights13 датафреймов?
```{r}
length(data(package = "nycflights13")$results[, "Item"])
```
  
  b) Сколько строк в каждом датафрейме?
```{r}
list(
  flights = nrow(flights),
  airlines = nrow(airlines),
  airports = nrow(airports),
  planes = nrow(planes),
  weather = nrow(weather)
)
```
  
  c) Сколько столбцов в каждом датафрейме?
```{r}
list(
  flights = ncol(flights),
  airlines = ncol(airlines),
  airports = ncol(airports),
  planes = ncol(planes),
  weather = ncol(weather)
)
```
  
  d) Как просмотреть примерный вид датафрейма?
```{r}
flights %>% glimpse()
```
  
  e) Сколько компаний-перевозчиков (carrier) учитывают эти наборы данных
  (представлено в наборах данных)?
```{r}
flights %>% filter(!is.na(carrier)) %>% distinct(carrier) %>% nrow()
```
  
  f) Сколько рейсов принял аэропорт John F Kennedy Intl в мае?
```{r}
flights %>% filter(origin == "JFK", month == 5) %>% nrow()
```
  
  g) Какой самый северный аэропорт?
```{r}
airports %>% arrange(desc(lat)) %>% slice(1)
```
  
  h) Какой аэропорт самый высокогорный (находится выше всех над уровнем моря)?
```{r}
airports %>% arrange(desc(alt)) %>% slice(1)
```
  
  i) Какие бортовые номера у самых старых самолетов?
```{r}
planes %>% arrange(desc(year)) %>% select(tailnum)
```
  
  j) Какая средняя температура воздуха была в сентябре в аэропорту John F
  Kennedy Intl (в градусах Цельсия).
```{r}
weather %>% filter(origin == "JFK", month == 9) %>% summarise(avg_temp = mean((temp - 32) * 5 / 9, na.rm = TRUE))
```
  
  k) Самолеты какой авиакомпании совершили больше всего вылетов в июне?
```{r}
flights %>% filter(month == 6) %>% count(carrier, sort = TRUE)
```
  
  l) Самолеты какой авиакомпании задерживались чаще других в 2013 году?
```{r}
flights %>% filter(dep_delay > 0 & year == 2013) %>% count(carrier, sort = TRUE)
```
  
## Оценка результата

В результате работы была скачан пакет nycflights13 и были выполнены задания с использованием наборов данных.

## Вывод

Были проанализованы наборы данных пакета nyclights13. Были выполнены поставленные практикой задачи.
