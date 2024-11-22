# Продолжение работы с пакетом dplyr
lieless@yandex.ru

# Анализ встроенного пакета dplyr

## Цель работы

Развить практические навыки использования функций обработки данных
пакета dplyr – функции select(), filter(), mutate(), arrange(),
group_by()

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

1.  Для начала прохождения курса необходимо установить пакет
    nyclights13.

``` r
#install.packages("nycflights13")
```

1.  Загружаем библиотеки.

``` r
library(nycflights13)
library(dplyr)
```


    Attaching package: 'dplyr'

    The following objects are masked from 'package:stats':

        filter, lag

    The following objects are masked from 'package:base':

        intersect, setdiff, setequal, union

1.  Переходим непосредственно к анализу набора данных nycflights13 и
    выполнению заданий:

<!-- -->

1.  Сколько встроенных в пакет nycflights13 датафреймов?

``` r
length(data(package = "nycflights13")$results[, "Item"])
```

    [1] 5

1.  Сколько строк в каждом датафрейме?

``` r
list(
  flights = nrow(flights),
  airlines = nrow(airlines),
  airports = nrow(airports),
  planes = nrow(planes),
  weather = nrow(weather)
)
```

    $flights
    [1] 336776

    $airlines
    [1] 16

    $airports
    [1] 1458

    $planes
    [1] 3322

    $weather
    [1] 26115

1.  Сколько столбцов в каждом датафрейме?

``` r
list(
  flights = ncol(flights),
  airlines = ncol(airlines),
  airports = ncol(airports),
  planes = ncol(planes),
  weather = ncol(weather)
)
```

    $flights
    [1] 19

    $airlines
    [1] 2

    $airports
    [1] 8

    $planes
    [1] 9

    $weather
    [1] 15

1.  Как просмотреть примерный вид датафрейма?

``` r
flights %>% glimpse()
```

    Rows: 336,776
    Columns: 19
    $ year           <int> 2013, 2013, 2013, 2013, 2013, 2013, 2013, 2013, 2013, 2…
    $ month          <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1…
    $ day            <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1…
    $ dep_time       <int> 517, 533, 542, 544, 554, 554, 555, 557, 557, 558, 558, …
    $ sched_dep_time <int> 515, 529, 540, 545, 600, 558, 600, 600, 600, 600, 600, …
    $ dep_delay      <dbl> 2, 4, 2, -1, -6, -4, -5, -3, -3, -2, -2, -2, -2, -2, -1…
    $ arr_time       <int> 830, 850, 923, 1004, 812, 740, 913, 709, 838, 753, 849,…
    $ sched_arr_time <int> 819, 830, 850, 1022, 837, 728, 854, 723, 846, 745, 851,…
    $ arr_delay      <dbl> 11, 20, 33, -18, -25, 12, 19, -14, -8, 8, -2, -3, 7, -1…
    $ carrier        <chr> "UA", "UA", "AA", "B6", "DL", "UA", "B6", "EV", "B6", "…
    $ flight         <int> 1545, 1714, 1141, 725, 461, 1696, 507, 5708, 79, 301, 4…
    $ tailnum        <chr> "N14228", "N24211", "N619AA", "N804JB", "N668DN", "N394…
    $ origin         <chr> "EWR", "LGA", "JFK", "JFK", "LGA", "EWR", "EWR", "LGA",…
    $ dest           <chr> "IAH", "IAH", "MIA", "BQN", "ATL", "ORD", "FLL", "IAD",…
    $ air_time       <dbl> 227, 227, 160, 183, 116, 150, 158, 53, 140, 138, 149, 1…
    $ distance       <dbl> 1400, 1416, 1089, 1576, 762, 719, 1065, 229, 944, 733, …
    $ hour           <dbl> 5, 5, 5, 5, 6, 5, 6, 6, 6, 6, 6, 6, 6, 6, 6, 5, 6, 6, 6…
    $ minute         <dbl> 15, 29, 40, 45, 0, 58, 0, 0, 0, 0, 0, 0, 0, 0, 0, 59, 0…
    $ time_hour      <dttm> 2013-01-01 05:00:00, 2013-01-01 05:00:00, 2013-01-01 0…

1.  Сколько компаний-перевозчиков (carrier) учитывают эти наборы данных
    (представлено в наборах данных)?

``` r
flights %>% filter(!is.na(carrier)) %>% distinct(carrier) %>% nrow()
```

    [1] 16

1.  Сколько рейсов принял аэропорт John F Kennedy Intl в мае?

``` r
flights %>% filter(origin == "JFK", month == 5) %>% nrow()
```

    [1] 9397

1.  Какой самый северный аэропорт?

``` r
airports %>% arrange(desc(lat)) %>% slice(1) %>% knitr::kable()
```

<table>
<thead>
<tr class="header">
<th style="text-align: left;">faa</th>
<th style="text-align: left;">name</th>
<th style="text-align: right;">lat</th>
<th style="text-align: right;">lon</th>
<th style="text-align: right;">alt</th>
<th style="text-align: right;">tz</th>
<th style="text-align: left;">dst</th>
<th style="text-align: left;">tzone</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">EEN</td>
<td style="text-align: left;">Dillant Hopkins Airport</td>
<td style="text-align: right;">72.27083</td>
<td style="text-align: right;">42.89833</td>
<td style="text-align: right;">149</td>
<td style="text-align: right;">-5</td>
<td style="text-align: left;">A</td>
<td style="text-align: left;">NA</td>
</tr>
</tbody>
</table>

1.  Какой аэропорт самый высокогорный (находится выше всех над уровнем
    моря)?

``` r
airports %>% arrange(desc(alt)) %>% slice(1) %>% knitr::kable()
```

<table>
<thead>
<tr class="header">
<th style="text-align: left;">faa</th>
<th style="text-align: left;">name</th>
<th style="text-align: right;">lat</th>
<th style="text-align: right;">lon</th>
<th style="text-align: right;">alt</th>
<th style="text-align: right;">tz</th>
<th style="text-align: left;">dst</th>
<th style="text-align: left;">tzone</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">TEX</td>
<td style="text-align: left;">Telluride</td>
<td style="text-align: right;">37.95376</td>
<td style="text-align: right;">-107.9085</td>
<td style="text-align: right;">9078</td>
<td style="text-align: right;">-7</td>
<td style="text-align: left;">A</td>
<td style="text-align: left;">America/Denver</td>
</tr>
</tbody>
</table>

1.  Какие бортовые номера у самых старых самолетов?

``` r
planes %>% arrange(year) %>% head(1) %>% select(tailnum) %>% knitr::kable()
```

<table>
<thead>
<tr class="header">
<th style="text-align: left;">tailnum</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">N381AA</td>
</tr>
</tbody>
</table>

1.  Какая средняя температура воздуха была в сентябре в аэропорту John F
    Kennedy Intl (в градусах Цельсия).

``` r
weather %>% filter(origin == "JFK", month == 9) %>% summarise(avg_temp = mean((temp - 32) * 5 / 9, na.rm = TRUE)) %>% knitr::kable()
```

<table>
<thead>
<tr class="header">
<th style="text-align: right;">avg_temp</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: right;">19.38764</td>
</tr>
</tbody>
</table>

1.  Самолеты какой авиакомпании совершили больше всего вылетов в июне?

``` r
flights %>% filter(month == 6) %>% count(carrier, sort = TRUE) %>% knitr::kable()
```

<table>
<thead>
<tr class="header">
<th style="text-align: left;">carrier</th>
<th style="text-align: right;">n</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">UA</td>
<td style="text-align: right;">4975</td>
</tr>
<tr class="even">
<td style="text-align: left;">B6</td>
<td style="text-align: right;">4622</td>
</tr>
<tr class="odd">
<td style="text-align: left;">EV</td>
<td style="text-align: right;">4456</td>
</tr>
<tr class="even">
<td style="text-align: left;">DL</td>
<td style="text-align: right;">4126</td>
</tr>
<tr class="odd">
<td style="text-align: left;">AA</td>
<td style="text-align: right;">2757</td>
</tr>
<tr class="even">
<td style="text-align: left;">MQ</td>
<td style="text-align: right;">2178</td>
</tr>
<tr class="odd">
<td style="text-align: left;">US</td>
<td style="text-align: right;">1736</td>
</tr>
<tr class="even">
<td style="text-align: left;">9E</td>
<td style="text-align: right;">1437</td>
</tr>
<tr class="odd">
<td style="text-align: left;">WN</td>
<td style="text-align: right;">1028</td>
</tr>
<tr class="even">
<td style="text-align: left;">VX</td>
<td style="text-align: right;">480</td>
</tr>
<tr class="odd">
<td style="text-align: left;">FL</td>
<td style="text-align: right;">252</td>
</tr>
<tr class="even">
<td style="text-align: left;">AS</td>
<td style="text-align: right;">60</td>
</tr>
<tr class="odd">
<td style="text-align: left;">F9</td>
<td style="text-align: right;">55</td>
</tr>
<tr class="even">
<td style="text-align: left;">YV</td>
<td style="text-align: right;">49</td>
</tr>
<tr class="odd">
<td style="text-align: left;">HA</td>
<td style="text-align: right;">30</td>
</tr>
<tr class="even">
<td style="text-align: left;">OO</td>
<td style="text-align: right;">2</td>
</tr>
</tbody>
</table>

1.  Самолеты какой авиакомпании задерживались чаще других в 2013 году?

``` r
flights %>% filter(dep_delay > 0 & year == 2013) %>% count(carrier, sort = TRUE) %>% knitr::kable()
```

<table>
<thead>
<tr class="header">
<th style="text-align: left;">carrier</th>
<th style="text-align: right;">n</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">UA</td>
<td style="text-align: right;">27261</td>
</tr>
<tr class="even">
<td style="text-align: left;">EV</td>
<td style="text-align: right;">23139</td>
</tr>
<tr class="odd">
<td style="text-align: left;">B6</td>
<td style="text-align: right;">21445</td>
</tr>
<tr class="even">
<td style="text-align: left;">DL</td>
<td style="text-align: right;">15241</td>
</tr>
<tr class="odd">
<td style="text-align: left;">AA</td>
<td style="text-align: right;">10162</td>
</tr>
<tr class="even">
<td style="text-align: left;">MQ</td>
<td style="text-align: right;">8031</td>
</tr>
<tr class="odd">
<td style="text-align: left;">9E</td>
<td style="text-align: right;">7063</td>
</tr>
<tr class="even">
<td style="text-align: left;">WN</td>
<td style="text-align: right;">6558</td>
</tr>
<tr class="odd">
<td style="text-align: left;">US</td>
<td style="text-align: right;">4775</td>
</tr>
<tr class="even">
<td style="text-align: left;">VX</td>
<td style="text-align: right;">2225</td>
</tr>
<tr class="odd">
<td style="text-align: left;">FL</td>
<td style="text-align: right;">1654</td>
</tr>
<tr class="even">
<td style="text-align: left;">F9</td>
<td style="text-align: right;">341</td>
</tr>
<tr class="odd">
<td style="text-align: left;">YV</td>
<td style="text-align: right;">233</td>
</tr>
<tr class="even">
<td style="text-align: left;">AS</td>
<td style="text-align: right;">226</td>
</tr>
<tr class="odd">
<td style="text-align: left;">HA</td>
<td style="text-align: right;">69</td>
</tr>
<tr class="even">
<td style="text-align: left;">OO</td>
<td style="text-align: right;">9</td>
</tr>
</tbody>
</table>

## Оценка результата

В результате работы была скачан пакет nycflights13 и были выполнены
задания с использованием наборов данных.

## Вывод

Были проанализованы наборы данных пакета nyclights13. Были выполнены
поставленные практикой задачи.
