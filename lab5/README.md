# Исследование информации о состоянии беспроводных сетей

## Цель работы

1. Получить знания о методах исследования радиоэлектронной обстановки.
2. Составить представление о механизмах работы Wi-Fi сетей на канальном и
сетевом уровне модели OSI.
3. Закрепить практические навыки использования языка программирования R для
обработки данных
4. Закрепить знания основных функций обработки данных экосистемы tidyverse языка R

## Исходные данные

1.  Программное обеспечение MacOS 14.4.1 Sonoma
2.  Rstudio Desktop
3.  Интерпретатор языка R 4.4.1
3.  csv данные p2_wifi_data

## План

1.  Импортировать csv файл
2.  Используя пакет dplyr решить задания

## Шаги

1. Импортируем csv файл, учитывая два датасета.

```{r}
library(tidyverse)
library(dplyr)
library(readr)
dataset1 <- read.csv("P2_wifi_data.csv", nrows = 167)
dataset2 <- read.csv("P2_wifi_data.csv", skip = 169)
```

2. Приводим данные к соответствующим типам.

```{r}

dataset1 <- dataset1 %>% 
  mutate_at(vars(BSSID, Privacy, Cipher, Authentication, LAN.IP, ESSID), trimws) %>%
  mutate_at(vars(BSSID, Privacy, Cipher, Authentication, LAN.IP, ESSID), na_if, "")

dataset1$First.time.seen <- as.POSIXct(dataset1$First.time.seen, format = "%Y-%m-%d %H:%M:%S")
dataset1$Last.time.seen <- as.POSIXct(dataset1$Last.time.seen, format = "%Y-%m-%d %H:%M:%S")

dataset2 <- dataset2 %>% 
  mutate_at(vars(Station.MAC, BSSID, Probed.ESSIDs), trimws) %>%
  mutate_at(vars(Station.MAC, BSSID, Probed.ESSIDs), na_if, "")

dataset2$First.time.seen <- as.POSIXct(dataset2$First.time.seen, format = "%Y-%m-%d %H:%M:%S")
dataset2$Last.time.seen <- as.POSIXct(dataset2$Last.time.seen, format = "%Y-%m-%d %H:%M:%S")

glimpse(dataset1)
glimpse(dataset2)

```
3. Точки доступа
  a. Определить небезопасные точки доступа (без шифрования – OPN)
  
```{r}
unsafe_access_points <- dataset1 %>% filter(grepl("OPN", Privacy))

unsafe_access_points %>% select(BSSID, ESSID, Privacy)
```
  
  b. Определить производителя для каждого обнаруженного устройства
  
  E8:28:C1 - Eltex Enterprise Ltd
  00:25:00 - Apple Inc
  E0:D9:E3 - Eltex Enterprise Ltd
  00:26:99 - Cisco Systems
  00:03:7A - Taiyo Yuden Co
  00:3E:1A - Xerox
  00:03:7F6 - Atheros Communications, Inc.
  
  c. Выявить устройства, использующие последнюю версию протокола шифрования WPA3, и названия точек доступа, реализованных на этих устройствах
  
```{r}
wpa3_devices <- dataset1 %>% filter(grepl("WPA3", Privacy))

wpa3_devices %>% select(BSSID, ESSID, Privacy)
```
  
  d. Отсортировать точки доступа по интервалу времени, в течение которого они находились на связи, по убыванию.

```{r}
wireNetData <- dataset1 %>% mutate(Time = difftime(Last.time.seen, First.time.seen, units = "mins")) %>% arrange(desc(Time)) %>% select(BSSID, Time)


# Просмотр отсортированных данных
wireNetData %>% select(BSSID, Time)
```

  e.Обнаружить топ-10 самых быстрых точек доступа.
  
```{r}
top_10_fastest <- dataset1 %>% arrange(desc(Speed)) %>% slice(1:10)

top_10_fastest %>% select(BSSID, ESSID, Speed)
```
  
  f.Отсортировать точки доступа по частоте отправки запросов (beacons) в единицу времени по их убыванию.
  
```{r}
beacon_rate <- dataset1 %>% mutate(beacon_rate = beacons /as.numeric(difftime(Last.time.seen,First.time.seen,units="mins"))) %>%filter(!is.infinite(beacon_rate)) %>%arrange(desc(beacon_rate))


beacon_rate %>% select(BSSID, ESSID, beacon_rate)
```
4. Данные клиентов
  a) Определить производителя для каждого обнаруженного устройства
  
```{r}
manufa <- dataset2 %>% filter(BSSID != '(not associated)') %>% mutate(Manufacturer = substr(BSSID, 1, 8)) %>% select(Manufacturer)

unique(manufa)
```
    DC:09:4C Huawei Technologies Co.,Ltd

    00:25:00 Apple, Inc.

    00:03:7F Atheros Communications, Inc.

    00:23:EB Cisco Systems, Inc

    00:0D:97 Hitachi Energy USA Inc.

    08:3A:2F Guangzhou Juan Intelligent Tech Joint Stock Co.,Ltd

    E0:D9:E3 Eltex Enterprise Ltd.

    E8:28:C1 Eltex Enterprise Ltd.

    0C:80:63 Tp-Link Technologies Co.,Ltd.
  
  b) Обнаружить устройства, которые НЕ рандомизируют свой MAC адрес
  
```{r}
non_randomized_mac <- dataset2 %>% filter(!grepl("^02|^06|^0A|^0E", BSSID)) %>% filter(BSSID != '(not associated)')

non_randomized_mac %>% select(BSSID)
```
  
  
  c) Кластеризовать запросы от устройств к точкам доступа по их именам. Определить время появления устройства в зоне радиовидимости и время выхода его из нее.
  
```{r}
clustered_requests <- dataset2 %>% group_by(Probed.ESSIDs) %>% summarise(
first_seen = min(First.time.seen, na.rm = TRUE),
last_seen = max(Last.time.seen, na.rm = TRUE))

clustered_requests
```
  
  
  d) Оценить стабильность уровня сигнала внури кластера во времени. Выявить наиболее стабильный кластер.
  
```{r}
signal_stability <- dataset2 %>% group_by(Probed.ESSIDs) %>% summarise(sd_signal = sd(Power, na.rm = TRUE)) %>% arrange(sd_signal)

signal_stability 
```

## Оценка результата

В результате работы были импортированы данные из csv-файла и сделаны задания.

## Вывод

Были проанализированы датасеты с использованием tidyverse. Были улучшены навыки пользования библиотеками dplyr, read.