# Беспроводные сети
lieless@yandex.ru

# Исследование информации о состоянии беспроводных сетей

## Цель работы

1.  Получить знания о методах исследования радиоэлектронной обстановки.
2.  Составить представление о механизмах работы Wi-Fi сетей на канальном
    и сетевом уровне модели OSI.
3.  Закрепить практические навыки использования языка программирования R
    для обработки данных
4.  Закрепить знания основных функций обработки данных экосистемы
    tidyverse языка R

## Исходные данные

1.  Программное обеспечение MacOS 14.4.1 Sonoma
2.  Rstudio Desktop
3.  Интерпретатор языка R 4.4.1
4.  csv данные p2_wifi_data

## План

1.  Импортировать csv файл
2.  Используя пакет dplyr решить задания

## Шаги

1.  Импортируем csv файл, учитывая два датасета.

``` r
library(tidyverse)
```

    ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
    ✔ lubridate 1.9.3     ✔ tidyr     1.3.1
    ✔ purrr     1.0.2     
    ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ✖ dplyr::filter() masks stats::filter()
    ✖ dplyr::lag()    masks stats::lag()
    ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(dplyr)
library(readr)
dataset1 <- read.csv("P2_wifi_data.csv", nrows = 167)
dataset2 <- read.csv("P2_wifi_data.csv", skip = 169)
```

1.  Приводим данные к соответствующим типам.

``` r
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
```

    Rows: 167
    Columns: 15
    $ BSSID           <chr> "BE:F1:71:D5:17:8B", "6E:C7:EC:16:DA:1A", "9A:75:A8:B9…
    $ First.time.seen <dttm> 2023-07-28 09:13:03, 2023-07-28 09:13:03, 2023-07-28 …
    $ Last.time.seen  <dttm> 2023-07-28 11:50:50, 2023-07-28 11:55:12, 2023-07-28 …
    $ channel         <int> 1, 1, 1, 7, 6, 6, 11, 11, 11, 1, 6, 14, 11, 11, 6, 6, …
    $ Speed           <int> 195, 130, 360, 360, 130, 130, 195, 130, 130, 195, 180,…
    $ Privacy         <chr> "WPA2", "WPA2", "WPA2", "WPA2", "WPA2", "OPN", "WPA2",…
    $ Cipher          <chr> "CCMP", "CCMP", "CCMP", "CCMP", "CCMP", NA, "CCMP", "C…
    $ Authentication  <chr> "PSK", "PSK", "PSK", "PSK", "PSK", NA, "PSK", "PSK", "…
    $ Power           <int> -30, -30, -68, -37, -57, -63, -27, -38, -38, -66, -42,…
    $ beacons         <int> 846, 750, 694, 510, 647, 251, 1647, 1251, 704, 617, 13…
    $ IV              <int> 504, 116, 26, 21, 6, 3430, 80, 11, 0, 0, 86, 0, 0, 0, …
    $ LAN.IP          <chr> "0.  0.  0.  0", "0.  0.  0.  0", "0.  0.  0.  0", "0.…
    $ ID.length       <int> 12, 4, 2, 14, 25, 13, 12, 13, 24, 12, 10, 0, 24, 24, 1…
    $ ESSID           <chr> "C322U13 3965", "Cnet", "KC", "POCO X5 Pro 5G", NA, "M…
    $ Key             <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…

``` r
glimpse(dataset2)
```

    Rows: 12,269
    Columns: 7
    $ Station.MAC     <chr> "CA:66:3B:8F:56:DD", "96:35:2D:3D:85:E6", "5C:3A:45:9E…
    $ First.time.seen <dttm> 2023-07-28 09:13:03, 2023-07-28 09:13:03, 2023-07-28 …
    $ Last.time.seen  <dttm> 2023-07-28 10:59:44, 2023-07-28 09:13:03, 2023-07-28 …
    $ Power           <chr> " -33", " -65", " -39", " -61", " -53", " -43", " -31"…
    $ packets         <chr> "      858", "        4", "      432", "      958", " …
    $ BSSID           <chr> "BE:F1:71:D5:17:8B", "(not associated)", "BE:F1:71:D6:…
    $ Probed.ESSIDs   <chr> "C322U13 3965", "IT2 Wireless", "C322U21 0566", "C322U…

1.  Точки доступа

<!-- -->

1.  Определить небезопасные точки доступа (без шифрования – OPN)

``` r
unsafe_access_points <- dataset1 %>% filter(grepl("OPN", Privacy))

unsafe_access_points %>% select(BSSID, ESSID, Privacy) %>% knitr::kable()
```

<table>
<thead>
<tr class="header">
<th style="text-align: left;">BSSID</th>
<th style="text-align: left;">ESSID</th>
<th style="text-align: left;">Privacy</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">E8:28:C1:DC:B2:52</td>
<td style="text-align: left;">MIREA_HOTSPOT</td>
<td style="text-align: left;">OPN</td>
</tr>
<tr class="even">
<td style="text-align: left;">E8:28:C1:DC:B2:50</td>
<td style="text-align: left;">MIREA_GUESTS</td>
<td style="text-align: left;">OPN</td>
</tr>
<tr class="odd">
<td style="text-align: left;">E8:28:C1:DC:B2:51</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">OPN</td>
</tr>
<tr class="even">
<td style="text-align: left;">E8:28:C1:DC:FF:F2</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">OPN</td>
</tr>
<tr class="odd">
<td style="text-align: left;">00:25:00:FF:94:73</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">OPN</td>
</tr>
<tr class="even">
<td style="text-align: left;">E8:28:C1:DD:04:52</td>
<td style="text-align: left;">MIREA_HOTSPOT</td>
<td style="text-align: left;">OPN</td>
</tr>
<tr class="odd">
<td style="text-align: left;">E8:28:C1:DE:74:31</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">OPN</td>
</tr>
<tr class="even">
<td style="text-align: left;">E8:28:C1:DE:74:32</td>
<td style="text-align: left;">MIREA_HOTSPOT</td>
<td style="text-align: left;">OPN</td>
</tr>
<tr class="odd">
<td style="text-align: left;">E8:28:C1:DC:C8:32</td>
<td style="text-align: left;">MIREA_HOTSPOT</td>
<td style="text-align: left;">OPN</td>
</tr>
<tr class="even">
<td style="text-align: left;">E8:28:C1:DD:04:50</td>
<td style="text-align: left;">MIREA_GUESTS</td>
<td style="text-align: left;">OPN</td>
</tr>
<tr class="odd">
<td style="text-align: left;">E8:28:C1:DD:04:51</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">OPN</td>
</tr>
<tr class="even">
<td style="text-align: left;">E8:28:C1:DC:C8:30</td>
<td style="text-align: left;">MIREA_GUESTS</td>
<td style="text-align: left;">OPN</td>
</tr>
<tr class="odd">
<td style="text-align: left;">E8:28:C1:DE:74:30</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">OPN</td>
</tr>
<tr class="even">
<td style="text-align: left;">E0:D9:E3:48:FF:D2</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">OPN</td>
</tr>
<tr class="odd">
<td style="text-align: left;">E8:28:C1:DC:B2:41</td>
<td style="text-align: left;">MIREA_GUESTS</td>
<td style="text-align: left;">OPN</td>
</tr>
<tr class="even">
<td style="text-align: left;">E8:28:C1:DC:B2:40</td>
<td style="text-align: left;">MIREA_HOTSPOT</td>
<td style="text-align: left;">OPN</td>
</tr>
<tr class="odd">
<td style="text-align: left;">00:26:99:F2:7A:E0</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">OPN</td>
</tr>
<tr class="even">
<td style="text-align: left;">E8:28:C1:DC:B2:42</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">OPN</td>
</tr>
<tr class="odd">
<td style="text-align: left;">E8:28:C1:DD:04:40</td>
<td style="text-align: left;">MIREA_HOTSPOT</td>
<td style="text-align: left;">OPN</td>
</tr>
<tr class="even">
<td style="text-align: left;">E8:28:C1:DD:04:41</td>
<td style="text-align: left;">MIREA_GUESTS</td>
<td style="text-align: left;">OPN</td>
</tr>
<tr class="odd">
<td style="text-align: left;">E8:28:C1:DE:47:D2</td>
<td style="text-align: left;">MIREA_HOTSPOT</td>
<td style="text-align: left;">OPN</td>
</tr>
<tr class="even">
<td style="text-align: left;">02:BC:15:7E:D5:DC</td>
<td style="text-align: left;">MT_FREE</td>
<td style="text-align: left;">OPN</td>
</tr>
<tr class="odd">
<td style="text-align: left;">E8:28:C1:DC:C6:B1</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">OPN</td>
</tr>
<tr class="even">
<td style="text-align: left;">E8:28:C1:DD:04:42</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">OPN</td>
</tr>
<tr class="odd">
<td style="text-align: left;">E8:28:C1:DC:C8:31</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">OPN</td>
</tr>
<tr class="even">
<td style="text-align: left;">E8:28:C1:DE:47:D1</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">OPN</td>
</tr>
<tr class="odd">
<td style="text-align: left;">00:AB:0A:00:10:10</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">OPN</td>
</tr>
<tr class="even">
<td style="text-align: left;">E8:28:C1:DC:C6:B0</td>
<td style="text-align: left;">MIREA_GUESTS</td>
<td style="text-align: left;">OPN</td>
</tr>
<tr class="odd">
<td style="text-align: left;">E8:28:C1:DC:C6:B2</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">OPN</td>
</tr>
<tr class="even">
<td style="text-align: left;">E8:28:C1:DC:BD:50</td>
<td style="text-align: left;">MIREA_GUESTS</td>
<td style="text-align: left;">OPN</td>
</tr>
<tr class="odd">
<td style="text-align: left;">E8:28:C1:DC:0B:B2</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">OPN</td>
</tr>
<tr class="even">
<td style="text-align: left;">E8:28:C1:DC:33:12</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">OPN</td>
</tr>
<tr class="odd">
<td style="text-align: left;">00:03:7A:1A:03:56</td>
<td style="text-align: left;">MT_FREE</td>
<td style="text-align: left;">OPN</td>
</tr>
<tr class="even">
<td style="text-align: left;">00:03:7F:12:34:56</td>
<td style="text-align: left;">MT_FREE</td>
<td style="text-align: left;">OPN</td>
</tr>
<tr class="odd">
<td style="text-align: left;">00:3E:1A:5D:14:45</td>
<td style="text-align: left;">MT_FREE</td>
<td style="text-align: left;">OPN</td>
</tr>
<tr class="even">
<td style="text-align: left;">E0:D9:E3:49:00:B1</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">OPN</td>
</tr>
<tr class="odd">
<td style="text-align: left;">E8:28:C1:DC:BD:52</td>
<td style="text-align: left;">MIREA_HOTSPOT</td>
<td style="text-align: left;">OPN</td>
</tr>
<tr class="even">
<td style="text-align: left;">00:26:99:F2:7A:EF</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">OPN</td>
</tr>
<tr class="odd">
<td style="text-align: left;">02:67:F1:B0:6C:98</td>
<td style="text-align: left;">MT_FREE</td>
<td style="text-align: left;">OPN</td>
</tr>
<tr class="even">
<td style="text-align: left;">02:CF:8B:87:B4:F9</td>
<td style="text-align: left;">MT_FREE</td>
<td style="text-align: left;">OPN</td>
</tr>
<tr class="odd">
<td style="text-align: left;">00:53:7A:99:98:56</td>
<td style="text-align: left;">MT_FREE</td>
<td style="text-align: left;">OPN</td>
</tr>
<tr class="even">
<td style="text-align: left;">E8:28:C1:DE:47:D0</td>
<td style="text-align: left;">MIREA_GUESTS</td>
<td style="text-align: left;">OPN</td>
</tr>
</tbody>
</table>

1.  Определить производителя для каждого обнаруженного устройства

E8:28:C1 - Eltex Enterprise Ltd 00:25:00 - Apple Inc E0:D9:E3 - Eltex
Enterprise Ltd 00:26:99 - Cisco Systems 00:03:7A - Taiyo Yuden Co
00:3E:1A - Xerox 00:03:7F6 - Atheros Communications, Inc.

1.  Выявить устройства, использующие последнюю версию протокола
    шифрования WPA3, и названия точек доступа, реализованных на этих
    устройствах

``` r
wpa3_devices <- dataset1 %>% filter(grepl("WPA3", Privacy))

wpa3_devices %>% select(BSSID, ESSID, Privacy) %>% knitr::kable()
```

<table>
<thead>
<tr class="header">
<th style="text-align: left;">BSSID</th>
<th style="text-align: left;">ESSID</th>
<th style="text-align: left;">Privacy</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">26:20:53:0C:98:E8</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">WPA3 WPA2</td>
</tr>
<tr class="even">
<td style="text-align: left;">A2:FE:FF:B8:9B:C9</td>
<td style="text-align: left;">Christie’s</td>
<td style="text-align: left;">WPA3 WPA2</td>
</tr>
<tr class="odd">
<td style="text-align: left;">96:FF:FC:91:EF:64</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">WPA3 WPA2</td>
</tr>
<tr class="even">
<td style="text-align: left;">CE:48:E7:86:4E:33</td>
<td style="text-align: left;">iPhone (Анастасия)</td>
<td style="text-align: left;">WPA3 WPA2</td>
</tr>
<tr class="odd">
<td style="text-align: left;">8E:1F:94:96:DA:FD</td>
<td style="text-align: left;">iPhone (Анастасия)</td>
<td style="text-align: left;">WPA3 WPA2</td>
</tr>
<tr class="even">
<td style="text-align: left;">BE:FD:EF:18:92:44</td>
<td style="text-align: left;">Димасик</td>
<td style="text-align: left;">WPA3 WPA2</td>
</tr>
<tr class="odd">
<td style="text-align: left;">3A:DA:00:F9:0C:02</td>
<td style="text-align: left;">iPhone XS Max 🦊🐱🦊</td>
<td style="text-align: left;">WPA3 WPA2</td>
</tr>
<tr class="even">
<td style="text-align: left;">76:C5:A0:70:08:96</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">WPA3 WPA2</td>
</tr>
</tbody>
</table>

1.  Отсортировать точки доступа по интервалу времени, в течение которого
    они находились на связи, по убыванию.

``` r
wireNetData <- dataset1 %>% mutate(Time = difftime(Last.time.seen, First.time.seen, units = "mins")) %>% arrange(desc(Time)) %>% select(BSSID, Time)


# Просмотр отсортированных данных
wireNetData %>% select(BSSID, Time) %>% knitr::kable()
```

<table>
<thead>
<tr class="header">
<th style="text-align: left;">BSSID</th>
<th style="text-align: left;">Time</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">00:25:00:FF:94:73</td>
<td style="text-align: left;">163.2500000 mins</td>
</tr>
<tr class="even">
<td style="text-align: left;">E8:28:C1:DD:04:52</td>
<td style="text-align: left;">162.9333333 mins</td>
</tr>
<tr class="odd">
<td style="text-align: left;">E8:28:C1:DC:B2:52</td>
<td style="text-align: left;">162.5833333 mins</td>
</tr>
<tr class="even">
<td style="text-align: left;">08:3A:2F:56:35:FE</td>
<td style="text-align: left;">162.4333333 mins</td>
</tr>
<tr class="odd">
<td style="text-align: left;">6E:C7:EC:16:DA:1A</td>
<td style="text-align: left;">162.1500000 mins</td>
</tr>
<tr class="even">
<td style="text-align: left;">E8:28:C1:DC:B2:50</td>
<td style="text-align: left;">162.1000000 mins</td>
</tr>
<tr class="odd">
<td style="text-align: left;">E8:28:C1:DC:B2:51</td>
<td style="text-align: left;">162.0833333 mins</td>
</tr>
<tr class="even">
<td style="text-align: left;">48:5B:39:F9:7A:48</td>
<td style="text-align: left;">162.0833333 mins</td>
</tr>
<tr class="odd">
<td style="text-align: left;">E8:28:C1:DC:FF:F2</td>
<td style="text-align: left;">162.0666667 mins</td>
</tr>
<tr class="even">
<td style="text-align: left;">8E:55:4A:85:5B:01</td>
<td style="text-align: left;">162.0500000 mins</td>
</tr>
<tr class="odd">
<td style="text-align: left;">00:26:99:BA:75:80</td>
<td style="text-align: left;">161.8333333 mins</td>
</tr>
<tr class="even">
<td style="text-align: left;">00:26:99:F2:7A:E2</td>
<td style="text-align: left;">161.7833333 mins</td>
</tr>
<tr class="odd">
<td style="text-align: left;">1E:93:E3:1B:3C:F4</td>
<td style="text-align: left;">160.5500000 mins</td>
</tr>
<tr class="even">
<td style="text-align: left;">9A:75:A8:B9:04:1E</td>
<td style="text-align: left;">160.4666667 mins</td>
</tr>
<tr class="odd">
<td style="text-align: left;">0C:80:63:A9:6E:EE</td>
<td style="text-align: left;">160.4666667 mins</td>
</tr>
<tr class="even">
<td style="text-align: left;">00:23:EB:E3:81:F2</td>
<td style="text-align: left;">159.9166667 mins</td>
</tr>
<tr class="odd">
<td style="text-align: left;">9E:A3:A9:DB:7E:01</td>
<td style="text-align: left;">159.2500000 mins</td>
</tr>
<tr class="even">
<td style="text-align: left;">E8:28:C1:DC:C8:32</td>
<td style="text-align: left;">159.2500000 mins</td>
</tr>
<tr class="odd">
<td style="text-align: left;">1C:7E:E5:8E:B7:DE</td>
<td style="text-align: left;">158.7333333 mins</td>
</tr>
<tr class="even">
<td style="text-align: left;">00:26:99:F2:7A:E1</td>
<td style="text-align: left;">158.2000000 mins</td>
</tr>
<tr class="odd">
<td style="text-align: left;">BE:F1:71:D5:17:8B</td>
<td style="text-align: left;">157.7833333 mins</td>
</tr>
<tr class="even">
<td style="text-align: left;">BE:F1:71:D6:10:D7</td>
<td style="text-align: left;">157.6833333 mins</td>
</tr>
<tr class="odd">
<td style="text-align: left;">9E:A3:A9:D6:28:3C</td>
<td style="text-align: left;">157.5166667 mins</td>
</tr>
<tr class="even">
<td style="text-align: left;">E8:28:C1:DD:04:40</td>
<td style="text-align: left;">156.6666667 mins</td>
</tr>
<tr class="odd">
<td style="text-align: left;">E8:28:C1:DD:04:41</td>
<td style="text-align: left;">156.6666667 mins</td>
</tr>
<tr class="even">
<td style="text-align: left;">00:23:EB:E3:81:F1</td>
<td style="text-align: left;">155.8000000 mins</td>
</tr>
<tr class="odd">
<td style="text-align: left;">00:23:EB:E3:81:FE</td>
<td style="text-align: left;">155.0833333 mins</td>
</tr>
<tr class="even">
<td style="text-align: left;">00:23:EB:E3:81:FD</td>
<td style="text-align: left;">155.0833333 mins</td>
</tr>
<tr class="odd">
<td style="text-align: left;">9E:A3:A9:BF:12:C0</td>
<td style="text-align: left;">154.5000000 mins</td>
</tr>
<tr class="even">
<td style="text-align: left;">E8:28:C1:DC:B2:40</td>
<td style="text-align: left;">153.5333333 mins</td>
</tr>
<tr class="odd">
<td style="text-align: left;">AA:F4:3F:EE:49:0B</td>
<td style="text-align: left;">150.7500000 mins</td>
</tr>
<tr class="even">
<td style="text-align: left;">E8:28:C1:DE:47:D2</td>
<td style="text-align: left;">150.6833333 mins</td>
</tr>
<tr class="odd">
<td style="text-align: left;">E8:28:C1:DD:04:50</td>
<td style="text-align: left;">149.8166667 mins</td>
</tr>
<tr class="even">
<td style="text-align: left;">14:EB:B6:6A:76:37</td>
<td style="text-align: left;">148.5833333 mins</td>
</tr>
<tr class="odd">
<td style="text-align: left;">56:99:98:EE:5A:4E</td>
<td style="text-align: left;">146.8500000 mins</td>
</tr>
<tr class="even">
<td style="text-align: left;">E8:28:C1:DC:B2:42</td>
<td style="text-align: left;">144.8833333 mins</td>
</tr>
<tr class="odd">
<td style="text-align: left;">38:1A:52:0D:90:A1</td>
<td style="text-align: left;">144.3500000 mins</td>
</tr>
<tr class="even">
<td style="text-align: left;">0A:C5:E1:DB:17:7B</td>
<td style="text-align: left;">143.4666667 mins</td>
</tr>
<tr class="odd">
<td style="text-align: left;">E8:28:C1:DC:C8:30</td>
<td style="text-align: left;">140.7500000 mins</td>
</tr>
<tr class="even">
<td style="text-align: left;">E8:28:C1:DC:C6:B1</td>
<td style="text-align: left;">139.8333333 mins</td>
</tr>
<tr class="odd">
<td style="text-align: left;">E8:28:C1:DD:04:42</td>
<td style="text-align: left;">138.6333333 mins</td>
</tr>
<tr class="even">
<td style="text-align: left;">E8:28:C1:DC:B2:41</td>
<td style="text-align: left;">138.4500000 mins</td>
</tr>
<tr class="odd">
<td style="text-align: left;">12:51:07:FF:29:D6</td>
<td style="text-align: left;">124.7166667 mins</td>
</tr>
<tr class="even">
<td style="text-align: left;">CE:B3:FF:84:45:FC</td>
<td style="text-align: left;">121.1833333 mins</td>
</tr>
<tr class="odd">
<td style="text-align: left;">E8:28:C1:DC:C8:31</td>
<td style="text-align: left;">119.9833333 mins</td>
</tr>
<tr class="even">
<td style="text-align: left;">E8:28:C1:DC:C6:B2</td>
<td style="text-align: left;">113.6500000 mins</td>
</tr>
<tr class="odd">
<td style="text-align: left;">4A:EC:1E:DB:BF:95</td>
<td style="text-align: left;">110.9666667 mins</td>
</tr>
<tr class="even">
<td style="text-align: left;">00:26:99:F2:7A:E0</td>
<td style="text-align: left;">103.6333333 mins</td>
</tr>
<tr class="odd">
<td style="text-align: left;">E8:28:C1:DD:04:51</td>
<td style="text-align: left;">94.0500000 mins</td>
</tr>
<tr class="even">
<td style="text-align: left;">E0:D9:E3:48:FF:D2</td>
<td style="text-align: left;">93.7333333 mins</td>
</tr>
<tr class="odd">
<td style="text-align: left;">00:AB:0A:00:10:10</td>
<td style="text-align: left;">89.2666667 mins</td>
</tr>
<tr class="even">
<td style="text-align: left;">E8:28:C1:DE:74:32</td>
<td style="text-align: left;">86.5000000 mins</td>
</tr>
<tr class="odd">
<td style="text-align: left;">10:50:72:00:11:08</td>
<td style="text-align: left;">83.2833333 mins</td>
</tr>
<tr class="even">
<td style="text-align: left;">EA:D8:D1:77:C8:08</td>
<td style="text-align: left;">83.2500000 mins</td>
</tr>
<tr class="odd">
<td style="text-align: left;">D2:6D:52:61:51:5D</td>
<td style="text-align: left;">77.2666667 mins</td>
</tr>
<tr class="even">
<td style="text-align: left;">E0:D9:E3:49:04:52</td>
<td style="text-align: left;">76.9000000 mins</td>
</tr>
<tr class="odd">
<td style="text-align: left;">7E:3A:10:A7:59:4E</td>
<td style="text-align: left;">76.8500000 mins</td>
</tr>
<tr class="even">
<td style="text-align: left;">BE:F1:71:D5:0E:53</td>
<td style="text-align: left;">76.3000000 mins</td>
</tr>
<tr class="odd">
<td style="text-align: left;">A6:02:B9:73:83:18</td>
<td style="text-align: left;">76.2833333 mins</td>
</tr>
<tr class="even">
<td style="text-align: left;">9A:9F:06:44:24:5B</td>
<td style="text-align: left;">76.2000000 mins</td>
</tr>
<tr class="odd">
<td style="text-align: left;">E8:28:C1:DE:74:31</td>
<td style="text-align: left;">73.8833333 mins</td>
</tr>
<tr class="even">
<td style="text-align: left;">92:F5:7B:43:0B:69</td>
<td style="text-align: left;">73.2000000 mins</td>
</tr>
<tr class="odd">
<td style="text-align: left;">E8:28:C1:DC:3C:92</td>
<td style="text-align: left;">72.1833333 mins</td>
</tr>
<tr class="even">
<td style="text-align: left;">38:1A:52:0D:84:D7</td>
<td style="text-align: left;">71.9833333 mins</td>
</tr>
<tr class="odd">
<td style="text-align: left;">38:1A:52:0D:90:5D</td>
<td style="text-align: left;">70.9166667 mins</td>
</tr>
<tr class="even">
<td style="text-align: left;">A2:64:E8:97:58:EE</td>
<td style="text-align: left;">70.8666667 mins</td>
</tr>
<tr class="odd">
<td style="text-align: left;">A6:02:B9:73:81:47</td>
<td style="text-align: left;">70.4000000 mins</td>
</tr>
<tr class="even">
<td style="text-align: left;">56:C5:2B:9F:84:90</td>
<td style="text-align: left;">69.5500000 mins</td>
</tr>
<tr class="odd">
<td style="text-align: left;">A6:02:B9:73:2F:76</td>
<td style="text-align: left;">69.0666667 mins</td>
</tr>
<tr class="even">
<td style="text-align: left;">38:1A:52:0D:97:60</td>
<td style="text-align: left;">68.1000000 mins</td>
</tr>
<tr class="odd">
<td style="text-align: left;">0A:24:D8:D9:24:70</td>
<td style="text-align: left;">67.8500000 mins</td>
</tr>
<tr class="even">
<td style="text-align: left;">E8:28:C1:DC:C6:B0</td>
<td style="text-align: left;">64.6500000 mins</td>
</tr>
<tr class="odd">
<td style="text-align: left;">8A:A3:03:73:52:08</td>
<td style="text-align: left;">57.5166667 mins</td>
</tr>
<tr class="even">
<td style="text-align: left;">5E:C7:C0:E4:D7:D4</td>
<td style="text-align: left;">54.4166667 mins</td>
</tr>
<tr class="odd">
<td style="text-align: left;">E8:28:C1:DC:54:72</td>
<td style="text-align: left;">51.2333333 mins</td>
</tr>
<tr class="even">
<td style="text-align: left;">4A:86:77:04:B7:28</td>
<td style="text-align: left;">50.1333333 mins</td>
</tr>
<tr class="odd">
<td style="text-align: left;">B6:C4:55:B5:53:24</td>
<td style="text-align: left;">49.7833333 mins</td>
</tr>
<tr class="even">
<td style="text-align: left;">E8:28:C1:DC:BD:50</td>
<td style="text-align: left;">45.7166667 mins</td>
</tr>
<tr class="odd">
<td style="text-align: left;">76:70:AF:A4:D2:AF</td>
<td style="text-align: left;">45.5500000 mins</td>
</tr>
<tr class="even">
<td style="text-align: left;">86:DF:BF:E4:2F:23</td>
<td style="text-align: left;">44.8000000 mins</td>
</tr>
<tr class="odd">
<td style="text-align: left;">38:1A:52:0D:8F:EC</td>
<td style="text-align: left;">43.9166667 mins</td>
</tr>
<tr class="even">
<td style="text-align: left;">EA:7B:9B:D8:56:34</td>
<td style="text-align: left;">37.3500000 mins</td>
</tr>
<tr class="odd">
<td style="text-align: left;">38:1A:52:0D:85:1D</td>
<td style="text-align: left;">34.7000000 mins</td>
</tr>
<tr class="even">
<td style="text-align: left;">00:26:CB:AA:62:71</td>
<td style="text-align: left;">32.8166667 mins</td>
</tr>
<tr class="odd">
<td style="text-align: left;">96:FF:FC:91:EF:64</td>
<td style="text-align: left;">32.1333333 mins</td>
</tr>
<tr class="even">
<td style="text-align: left;">E8:28:C1:DC:33:12</td>
<td style="text-align: left;">22.9833333 mins</td>
</tr>
<tr class="odd">
<td style="text-align: left;">E8:28:C1:DC:F0:90</td>
<td style="text-align: left;">21.8666667 mins</td>
</tr>
<tr class="even">
<td style="text-align: left;">3A:70:96:C6:30:2C</td>
<td style="text-align: left;">21.6666667 mins</td>
</tr>
<tr class="odd">
<td style="text-align: left;">36:46:53:81:12:A0</td>
<td style="text-align: left;">20.8000000 mins</td>
</tr>
<tr class="even">
<td style="text-align: left;">CE:C3:F7:A4:7E:B3</td>
<td style="text-align: left;">20.4000000 mins</td>
</tr>
<tr class="odd">
<td style="text-align: left;">26:20:53:0C:98:E8</td>
<td style="text-align: left;">17.4166667 mins</td>
</tr>
<tr class="even">
<td style="text-align: left;">92:12:38:E5:7E:1E</td>
<td style="text-align: left;">14.4666667 mins</td>
</tr>
<tr class="odd">
<td style="text-align: left;">E8:28:C1:DC:33:10</td>
<td style="text-align: left;">14.1000000 mins</td>
</tr>
<tr class="even">
<td style="text-align: left;">E8:28:C1:DB:F5:F0</td>
<td style="text-align: left;">14.0333333 mins</td>
</tr>
<tr class="odd">
<td style="text-align: left;">E8:28:C1:DC:0B:B0</td>
<td style="text-align: left;">13.8666667 mins</td>
</tr>
<tr class="even">
<td style="text-align: left;">E8:28:C1:DB:F5:F2</td>
<td style="text-align: left;">13.0333333 mins</td>
</tr>
<tr class="odd">
<td style="text-align: left;">02:67:F1:B0:6C:98</td>
<td style="text-align: left;">10.8500000 mins</td>
</tr>
<tr class="even">
<td style="text-align: left;">E8:28:C1:DE:74:30</td>
<td style="text-align: left;">8.4666667 mins</td>
</tr>
<tr class="odd">
<td style="text-align: left;">1E:C2:8E:D8:30:91</td>
<td style="text-align: left;">8.3000000 mins</td>
</tr>
<tr class="even">
<td style="text-align: left;">8E:1F:94:96:DA:FD</td>
<td style="text-align: left;">6.9166667 mins</td>
</tr>
<tr class="odd">
<td style="text-align: left;">E0:D9:E3:49:04:50</td>
<td style="text-align: left;">6.6833333 mins</td>
</tr>
<tr class="even">
<td style="text-align: left;">CE:48:E7:86:4E:33</td>
<td style="text-align: left;">4.9166667 mins</td>
</tr>
<tr class="odd">
<td style="text-align: left;">00:26:99:BA:75:8F</td>
<td style="text-align: left;">4.8000000 mins</td>
</tr>
<tr class="even">
<td style="text-align: left;">2A:E8:A2:02:01:73</td>
<td style="text-align: left;">3.6666667 mins</td>
</tr>
<tr class="odd">
<td style="text-align: left;">2E:FE:13:D0:96:51</td>
<td style="text-align: left;">0.9666667 mins</td>
</tr>
<tr class="even">
<td style="text-align: left;">9C:A5:13:28:D5:89</td>
<td style="text-align: left;">0.7166667 mins</td>
</tr>
<tr class="odd">
<td style="text-align: left;">22:C9:7F:A9:BA:9C</td>
<td style="text-align: left;">0.6833333 mins</td>
</tr>
<tr class="even">
<td style="text-align: left;">E8:28:C1:DC:54:B0</td>
<td style="text-align: left;">0.6000000 mins</td>
</tr>
<tr class="odd">
<td style="text-align: left;">D2:25:91:F6:6C:D8</td>
<td style="text-align: left;">0.2166667 mins</td>
</tr>
<tr class="even">
<td style="text-align: left;">3A:DA:00:F9:0C:02</td>
<td style="text-align: left;">0.1500000 mins</td>
</tr>
<tr class="odd">
<td style="text-align: left;">E8:28:C1:DB:FC:F2</td>
<td style="text-align: left;">0.1500000 mins</td>
</tr>
<tr class="even">
<td style="text-align: left;">DC:09:4C:32:34:9B</td>
<td style="text-align: left;">0.1333333 mins</td>
</tr>
<tr class="odd">
<td style="text-align: left;">F2:30:AB:E9:03:ED</td>
<td style="text-align: left;">0.1166667 mins</td>
</tr>
<tr class="even">
<td style="text-align: left;">E0:D9:E3:49:04:40</td>
<td style="text-align: left;">0.1166667 mins</td>
</tr>
<tr class="odd">
<td style="text-align: left;">00:03:7A:1A:03:56</td>
<td style="text-align: left;">0.1000000 mins</td>
</tr>
<tr class="even">
<td style="text-align: left;">B2:CF:C0:00:4A:60</td>
<td style="text-align: left;">0.0833333 mins</td>
</tr>
<tr class="odd">
<td style="text-align: left;">BE:FD:EF:18:92:44</td>
<td style="text-align: left;">0.0666667 mins</td>
</tr>
<tr class="even">
<td style="text-align: left;">02:BC:15:7E:D5:DC</td>
<td style="text-align: left;">0.0333333 mins</td>
</tr>
<tr class="odd">
<td style="text-align: left;">00:23:EB:E3:49:31</td>
<td style="text-align: left;">0.0333333 mins</td>
</tr>
<tr class="even">
<td style="text-align: left;">00:3E:1A:5D:14:45</td>
<td style="text-align: left;">0.0333333 mins</td>
</tr>
<tr class="odd">
<td style="text-align: left;">76:C5:A0:70:08:96</td>
<td style="text-align: left;">0.0333333 mins</td>
</tr>
<tr class="even">
<td style="text-align: left;">82:CD:7D:04:17:3B</td>
<td style="text-align: left;">0.0333333 mins</td>
</tr>
<tr class="odd">
<td style="text-align: left;">E0:D9:E3:49:00:B0</td>
<td style="text-align: left;">0.0166667 mins</td>
</tr>
<tr class="even">
<td style="text-align: left;">E8:28:C1:DC:54:B2</td>
<td style="text-align: left;">0.0166667 mins</td>
</tr>
<tr class="odd">
<td style="text-align: left;">C6:BC:37:7A:67:0D</td>
<td style="text-align: left;">0.0000000 mins</td>
</tr>
<tr class="even">
<td style="text-align: left;">12:48:F9:CF:58:8E</td>
<td style="text-align: left;">0.0000000 mins</td>
</tr>
<tr class="odd">
<td style="text-align: left;">76:E4:ED:B0:5C:9A</td>
<td style="text-align: left;">0.0000000 mins</td>
</tr>
<tr class="even">
<td style="text-align: left;">E0:D9:E3:48:FF:D0</td>
<td style="text-align: left;">0.0000000 mins</td>
</tr>
<tr class="odd">
<td style="text-align: left;">E2:37:BF:8F:6A:7B</td>
<td style="text-align: left;">0.0000000 mins</td>
</tr>
<tr class="even">
<td style="text-align: left;">C2:B5:D7:7F:07:A8</td>
<td style="text-align: left;">0.0000000 mins</td>
</tr>
<tr class="odd">
<td style="text-align: left;">8A:4E:75:44:5A:F6</td>
<td style="text-align: left;">0.0000000 mins</td>
</tr>
<tr class="even">
<td style="text-align: left;">00:03:7A:1A:18:56</td>
<td style="text-align: left;">0.0000000 mins</td>
</tr>
<tr class="odd">
<td style="text-align: left;">E8:28:C1:DE:47:D1</td>
<td style="text-align: left;">0.0000000 mins</td>
</tr>
<tr class="even">
<td style="text-align: left;">A2:FE:FF:B8:9B:C9</td>
<td style="text-align: left;">0.0000000 mins</td>
</tr>
<tr class="odd">
<td style="text-align: left;">00:09:9A:12:55:04</td>
<td style="text-align: left;">0.0000000 mins</td>
</tr>
<tr class="even">
<td style="text-align: left;">E8:28:C1:DC:3A:B0</td>
<td style="text-align: left;">0.0000000 mins</td>
</tr>
<tr class="odd">
<td style="text-align: left;">E8:28:C1:DC:0B:B2</td>
<td style="text-align: left;">0.0000000 mins</td>
</tr>
<tr class="even">
<td style="text-align: left;">E8:28:C1:DC:3C:80</td>
<td style="text-align: left;">0.0000000 mins</td>
</tr>
<tr class="odd">
<td style="text-align: left;">00:23:EB:E3:44:31</td>
<td style="text-align: left;">0.0000000 mins</td>
</tr>
<tr class="even">
<td style="text-align: left;">A6:F7:05:31:E8:EE</td>
<td style="text-align: left;">0.0000000 mins</td>
</tr>
<tr class="odd">
<td style="text-align: left;">BA:2A:7A:DD:38:3E</td>
<td style="text-align: left;">0.0000000 mins</td>
</tr>
<tr class="even">
<td style="text-align: left;">12:54:1A:C6:FF:71</td>
<td style="text-align: left;">0.0000000 mins</td>
</tr>
<tr class="odd">
<td style="text-align: left;">76:5E:F3:F9:A5:1C</td>
<td style="text-align: left;">0.0000000 mins</td>
</tr>
<tr class="even">
<td style="text-align: left;">00:03:7F:12:34:56</td>
<td style="text-align: left;">0.0000000 mins</td>
</tr>
<tr class="odd">
<td style="text-align: left;">E8:28:C1:DC:03:30</td>
<td style="text-align: left;">0.0000000 mins</td>
</tr>
<tr class="even">
<td style="text-align: left;">B2:1B:0C:67:0A:BD</td>
<td style="text-align: left;">0.0000000 mins</td>
</tr>
<tr class="odd">
<td style="text-align: left;">E0:D9:E3:49:00:B1</td>
<td style="text-align: left;">0.0000000 mins</td>
</tr>
<tr class="even">
<td style="text-align: left;">E8:28:C1:DC:BD:52</td>
<td style="text-align: left;">0.0000000 mins</td>
</tr>
<tr class="odd">
<td style="text-align: left;">E8:28:C1:DE:72:D0</td>
<td style="text-align: left;">0.0000000 mins</td>
</tr>
<tr class="even">
<td style="text-align: left;">E0:D9:E3:49:04:41</td>
<td style="text-align: left;">0.0000000 mins</td>
</tr>
<tr class="odd">
<td style="text-align: left;">00:26:99:F1:1A:E1</td>
<td style="text-align: left;">0.0000000 mins</td>
</tr>
<tr class="even">
<td style="text-align: left;">00:23:EB:E3:44:32</td>
<td style="text-align: left;">0.0000000 mins</td>
</tr>
<tr class="odd">
<td style="text-align: left;">00:26:CB:AA:62:72</td>
<td style="text-align: left;">0.0000000 mins</td>
</tr>
<tr class="even">
<td style="text-align: left;">E0:D9:E3:48:B4:D2</td>
<td style="text-align: left;">0.0000000 mins</td>
</tr>
<tr class="odd">
<td style="text-align: left;">AE:3E:7F:C8:BC:8E</td>
<td style="text-align: left;">0.0000000 mins</td>
</tr>
<tr class="even">
<td style="text-align: left;">02:B3:45:5A:05:93</td>
<td style="text-align: left;">0.0000000 mins</td>
</tr>
<tr class="odd">
<td style="text-align: left;">00:00:00:00:00:00</td>
<td style="text-align: left;">0.0000000 mins</td>
</tr>
<tr class="even">
<td style="text-align: left;">6A:B0:1A:C2:DF:49</td>
<td style="text-align: left;">0.0000000 mins</td>
</tr>
<tr class="odd">
<td style="text-align: left;">E8:28:C1:DC:3C:90</td>
<td style="text-align: left;">0.0000000 mins</td>
</tr>
<tr class="even">
<td style="text-align: left;">30:B4:B8:11:C0:90</td>
<td style="text-align: left;">0.0000000 mins</td>
</tr>
<tr class="odd">
<td style="text-align: left;">00:26:99:F2:7A:EF</td>
<td style="text-align: left;">0.0000000 mins</td>
</tr>
<tr class="even">
<td style="text-align: left;">02:CF:8B:87:B4:F9</td>
<td style="text-align: left;">0.0000000 mins</td>
</tr>
<tr class="odd">
<td style="text-align: left;">E8:28:C1:DC:03:32</td>
<td style="text-align: left;">0.0000000 mins</td>
</tr>
<tr class="even">
<td style="text-align: left;">00:53:7A:99:98:56</td>
<td style="text-align: left;">0.0000000 mins</td>
</tr>
<tr class="odd">
<td style="text-align: left;">00:03:7F:10:17:56</td>
<td style="text-align: left;">0.0000000 mins</td>
</tr>
<tr class="even">
<td style="text-align: left;">00:0D:97:6B:93:DF</td>
<td style="text-align: left;">0.0000000 mins</td>
</tr>
<tr class="odd">
<td style="text-align: left;">E8:28:C1:DE:47:D0</td>
<td style="text-align: left;">0.0000000 mins</td>
</tr>
</tbody>
</table>

e.Обнаружить топ-10 самых быстрых точек доступа.

``` r
top_10_fastest <- dataset1 %>% arrange(desc(Speed)) %>% slice(1:10)

top_10_fastest %>% select(BSSID, ESSID, Speed) %>% knitr::kable()
```

<table>
<thead>
<tr class="header">
<th style="text-align: left;">BSSID</th>
<th style="text-align: left;">ESSID</th>
<th style="text-align: right;">Speed</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">26:20:53:0C:98:E8</td>
<td style="text-align: left;">NA</td>
<td style="text-align: right;">866</td>
</tr>
<tr class="even">
<td style="text-align: left;">96:FF:FC:91:EF:64</td>
<td style="text-align: left;">NA</td>
<td style="text-align: right;">866</td>
</tr>
<tr class="odd">
<td style="text-align: left;">CE:48:E7:86:4E:33</td>
<td style="text-align: left;">iPhone (Анастасия)</td>
<td style="text-align: right;">866</td>
</tr>
<tr class="even">
<td style="text-align: left;">8E:1F:94:96:DA:FD</td>
<td style="text-align: left;">iPhone (Анастасия)</td>
<td style="text-align: right;">866</td>
</tr>
<tr class="odd">
<td style="text-align: left;">9A:75:A8:B9:04:1E</td>
<td style="text-align: left;">KC</td>
<td style="text-align: right;">360</td>
</tr>
<tr class="even">
<td style="text-align: left;">4A:EC:1E:DB:BF:95</td>
<td style="text-align: left;">POCO X5 Pro 5G</td>
<td style="text-align: right;">360</td>
</tr>
<tr class="odd">
<td style="text-align: left;">56:C5:2B:9F:84:90</td>
<td style="text-align: left;">OnePlus 6T</td>
<td style="text-align: right;">360</td>
</tr>
<tr class="even">
<td style="text-align: left;">E8:28:C1:DC:B2:41</td>
<td style="text-align: left;">MIREA_GUESTS</td>
<td style="text-align: right;">360</td>
</tr>
<tr class="odd">
<td style="text-align: left;">E8:28:C1:DC:B2:40</td>
<td style="text-align: left;">MIREA_HOTSPOT</td>
<td style="text-align: right;">360</td>
</tr>
<tr class="even">
<td style="text-align: left;">E8:28:C1:DC:B2:42</td>
<td style="text-align: left;">NA</td>
<td style="text-align: right;">360</td>
</tr>
</tbody>
</table>

f.Отсортировать точки доступа по частоте отправки запросов (beacons) в
единицу времени по их убыванию.

``` r
beacon_rate <- dataset1 %>% mutate(beacon_rate = beacons /as.numeric(difftime(Last.time.seen,First.time.seen,units="mins"))) %>%filter(!is.infinite(beacon_rate)) %>%arrange(desc(beacon_rate))


beacon_rate %>% select(BSSID, ESSID, beacon_rate) %>% knitr::kable()
```

<table>
<thead>
<tr class="header">
<th style="text-align: left;">BSSID</th>
<th style="text-align: left;">ESSID</th>
<th style="text-align: right;">beacon_rate</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">F2:30:AB:E9:03:ED</td>
<td style="text-align: left;">iPhone (Uliana)</td>
<td style="text-align: right;">51.4285714</td>
</tr>
<tr class="even">
<td style="text-align: left;">B2:CF:C0:00:4A:60</td>
<td style="text-align: left;">Михаил’s Galaxy M32</td>
<td style="text-align: right;">48.0000000</td>
</tr>
<tr class="odd">
<td style="text-align: left;">3A:DA:00:F9:0C:02</td>
<td style="text-align: left;">iPhone XS Max 🦊🐱🦊</td>
<td style="text-align: right;">33.3333333</td>
</tr>
<tr class="even">
<td style="text-align: left;">02:BC:15:7E:D5:DC</td>
<td style="text-align: left;">MT_FREE</td>
<td style="text-align: right;">30.0000000</td>
</tr>
<tr class="odd">
<td style="text-align: left;">00:3E:1A:5D:14:45</td>
<td style="text-align: left;">MT_FREE</td>
<td style="text-align: right;">30.0000000</td>
</tr>
<tr class="even">
<td style="text-align: left;">76:C5:A0:70:08:96</td>
<td style="text-align: left;">NA</td>
<td style="text-align: right;">30.0000000</td>
</tr>
<tr class="odd">
<td style="text-align: left;">D2:25:91:F6:6C:D8</td>
<td style="text-align: left;">Саня</td>
<td style="text-align: right;">23.0769231</td>
</tr>
<tr class="even">
<td style="text-align: left;">BE:F1:71:D6:10:D7</td>
<td style="text-align: left;">C322U21 0566</td>
<td style="text-align: right;">10.4449847</td>
</tr>
<tr class="odd">
<td style="text-align: left;">00:03:7A:1A:03:56</td>
<td style="text-align: left;">MT_FREE</td>
<td style="text-align: right;">10.0000000</td>
</tr>
<tr class="even">
<td style="text-align: left;">38:1A:52:0D:84:D7</td>
<td style="text-align: left;">EBFCD57F-EE81fI_DL_1AO2T</td>
<td style="text-align: right;">9.7800417</td>
</tr>
<tr class="odd">
<td style="text-align: left;">0A:C5:E1:DB:17:7B</td>
<td style="text-align: left;">AndroidAP177B</td>
<td style="text-align: right;">8.7197955</td>
</tr>
<tr class="even">
<td style="text-align: left;">1E:93:E3:1B:3C:F4</td>
<td style="text-align: left;">Galaxy A71</td>
<td style="text-align: right;">8.6577390</td>
</tr>
<tr class="odd">
<td style="text-align: left;">D2:6D:52:61:51:5D</td>
<td style="text-align: left;">NA</td>
<td style="text-align: right;">8.3735979</td>
</tr>
<tr class="even">
<td style="text-align: left;">BE:F1:71:D5:0E:53</td>
<td style="text-align: left;">C322U06 9080</td>
<td style="text-align: right;">8.0865007</td>
</tr>
<tr class="odd">
<td style="text-align: left;">4A:86:77:04:B7:28</td>
<td style="text-align: left;">iPhone (Искандер)</td>
<td style="text-align: right;">7.2007979</td>
</tr>
<tr class="even">
<td style="text-align: left;">3A:70:96:C6:30:2C</td>
<td style="text-align: left;">iPhone (Искандер)</td>
<td style="text-align: right;">6.6923077</td>
</tr>
<tr class="odd">
<td style="text-align: left;">76:70:AF:A4:D2:AF</td>
<td style="text-align: left;">витя</td>
<td style="text-align: right;">5.5543359</td>
</tr>
<tr class="even">
<td style="text-align: left;">BE:F1:71:D5:17:8B</td>
<td style="text-align: left;">C322U13 3965</td>
<td style="text-align: right;">5.3617830</td>
</tr>
<tr class="odd">
<td style="text-align: left;">AA:F4:3F:EE:49:0B</td>
<td style="text-align: left;">Redmi Note 8 Pro</td>
<td style="text-align: right;">4.8955224</td>
</tr>
<tr class="even">
<td style="text-align: left;">6E:C7:EC:16:DA:1A</td>
<td style="text-align: left;">Cnet</td>
<td style="text-align: right;">4.6253469</td>
</tr>
<tr class="odd">
<td style="text-align: left;">4A:EC:1E:DB:BF:95</td>
<td style="text-align: left;">POCO X5 Pro 5G</td>
<td style="text-align: right;">4.5959748</td>
</tr>
<tr class="even">
<td style="text-align: left;">56:C5:2B:9F:84:90</td>
<td style="text-align: left;">OnePlus 6T</td>
<td style="text-align: right;">4.5578720</td>
</tr>
<tr class="odd">
<td style="text-align: left;">9A:75:A8:B9:04:1E</td>
<td style="text-align: left;">KC</td>
<td style="text-align: right;">4.3248857</td>
</tr>
<tr class="even">
<td style="text-align: left;">9C:A5:13:28:D5:89</td>
<td style="text-align: left;">Galaxy M012063</td>
<td style="text-align: right;">4.1860465</td>
</tr>
<tr class="odd">
<td style="text-align: left;">36:46:53:81:12:A0</td>
<td style="text-align: left;">GGWPRedmi Note 10S</td>
<td style="text-align: right;">3.9423077</td>
</tr>
<tr class="even">
<td style="text-align: left;">38:1A:52:0D:85:1D</td>
<td style="text-align: left;">EBFCD5C1-EE81fI_DN11AOcY</td>
<td style="text-align: right;">3.7463977</td>
</tr>
<tr class="odd">
<td style="text-align: left;">38:1A:52:0D:8F:EC</td>
<td style="text-align: left;">EBFCD6C2-EE81fI_DR21AOa0</td>
<td style="text-align: right;">2.4364326</td>
</tr>
<tr class="even">
<td style="text-align: left;">2E:FE:13:D0:96:51</td>
<td style="text-align: left;">Redmi Note 8 Pro</td>
<td style="text-align: right;">2.0689655</td>
</tr>
<tr class="odd">
<td style="text-align: left;">CE:48:E7:86:4E:33</td>
<td style="text-align: left;">iPhone (Анастасия)</td>
<td style="text-align: right;">1.8305085</td>
</tr>
<tr class="even">
<td style="text-align: left;">8E:1F:94:96:DA:FD</td>
<td style="text-align: left;">iPhone (Анастасия)</td>
<td style="text-align: right;">1.7349398</td>
</tr>
<tr class="odd">
<td style="text-align: left;">E8:28:C1:DC:B2:51</td>
<td style="text-align: left;">NA</td>
<td style="text-align: right;">1.7213368</td>
</tr>
<tr class="even">
<td style="text-align: left;">E8:28:C1:DC:B2:50</td>
<td style="text-align: left;">MIREA_GUESTS</td>
<td style="text-align: right;">1.6039482</td>
</tr>
<tr class="odd">
<td style="text-align: left;">5E:C7:C0:E4:D7:D4</td>
<td style="text-align: left;">realme 10</td>
<td style="text-align: right;">1.5620214</td>
</tr>
<tr class="even">
<td style="text-align: left;">E8:28:C1:DC:B2:52</td>
<td style="text-align: left;">MIREA_HOTSPOT</td>
<td style="text-align: right;">1.5438237</td>
</tr>
<tr class="odd">
<td style="text-align: left;">8E:55:4A:85:5B:01</td>
<td style="text-align: left;">Vladimir</td>
<td style="text-align: right;">1.5303919</td>
</tr>
<tr class="even">
<td style="text-align: left;">38:1A:52:0D:90:5D</td>
<td style="text-align: left;">EBFCD615-EE81fI_DOL1AO8w</td>
<td style="text-align: right;">1.2690952</td>
</tr>
<tr class="odd">
<td style="text-align: left;">1C:7E:E5:8E:B7:DE</td>
<td style="text-align: left;">NA</td>
<td style="text-align: right;">0.8945821</td>
</tr>
<tr class="even">
<td style="text-align: left;">38:1A:52:0D:90:A1</td>
<td style="text-align: left;">EBFCD597-EE81fI_DMN1AOe1</td>
<td style="text-align: right;">0.7758919</td>
</tr>
<tr class="odd">
<td style="text-align: left;">A2:64:E8:97:58:EE</td>
<td style="text-align: left;">Mi Phone</td>
<td style="text-align: right;">0.7337723</td>
</tr>
<tr class="even">
<td style="text-align: left;">1E:C2:8E:D8:30:91</td>
<td style="text-align: left;">Damy</td>
<td style="text-align: right;">0.7228916</td>
</tr>
<tr class="odd">
<td style="text-align: left;">48:5B:39:F9:7A:48</td>
<td style="text-align: left;">NA</td>
<td style="text-align: right;">0.6724936</td>
</tr>
<tr class="even">
<td style="text-align: left;">00:26:99:F2:7A:E2</td>
<td style="text-align: left;">GIVC</td>
<td style="text-align: right;">0.5192129</td>
</tr>
<tr class="odd">
<td style="text-align: left;">38:1A:52:0D:97:60</td>
<td style="text-align: left;">EBFCD593-EE81fI_DMJ1AOI4</td>
<td style="text-align: right;">0.4111601</td>
</tr>
<tr class="even">
<td style="text-align: left;">00:26:99:F2:7A:E1</td>
<td style="text-align: left;">IKB</td>
<td style="text-align: right;">0.4108723</td>
</tr>
<tr class="odd">
<td style="text-align: left;">00:26:99:BA:75:80</td>
<td style="text-align: left;">GIVC</td>
<td style="text-align: right;">0.3769310</td>
</tr>
<tr class="even">
<td style="text-align: left;">A6:02:B9:73:2F:76</td>
<td style="text-align: left;">C239U51 0701</td>
<td style="text-align: right;">0.3764479</td>
</tr>
<tr class="odd">
<td style="text-align: left;">9E:A3:A9:D6:28:3C</td>
<td style="text-align: left;">NA</td>
<td style="text-align: right;">0.3237753</td>
</tr>
<tr class="even">
<td style="text-align: left;">00:23:EB:E3:81:FE</td>
<td style="text-align: left;">IKB</td>
<td style="text-align: right;">0.3030629</td>
</tr>
<tr class="odd">
<td style="text-align: left;">00:23:EB:E3:81:FD</td>
<td style="text-align: left;">GIVC</td>
<td style="text-align: right;">0.2966147</td>
</tr>
<tr class="even">
<td style="text-align: left;">9A:9F:06:44:24:5B</td>
<td style="text-align: left;">Long Huong Galaxy M12</td>
<td style="text-align: right;">0.2887139</td>
</tr>
<tr class="odd">
<td style="text-align: left;">96:FF:FC:91:EF:64</td>
<td style="text-align: left;">NA</td>
<td style="text-align: right;">0.2800830</td>
</tr>
<tr class="even">
<td style="text-align: left;">A6:02:B9:73:81:47</td>
<td style="text-align: left;">C239U53 6056</td>
<td style="text-align: right;">0.2698864</td>
</tr>
<tr class="odd">
<td style="text-align: left;">0C:80:63:A9:6E:EE</td>
<td style="text-align: left;">NA</td>
<td style="text-align: right;">0.2617366</td>
</tr>
<tr class="even">
<td style="text-align: left;">12:51:07:FF:29:D6</td>
<td style="text-align: left;">DESKTOP-KITJO8R 5262</td>
<td style="text-align: right;">0.2565816</td>
</tr>
<tr class="odd">
<td style="text-align: left;">9E:A3:A9:DB:7E:01</td>
<td style="text-align: left;">NA</td>
<td style="text-align: right;">0.2511774</td>
</tr>
<tr class="even">
<td style="text-align: left;">92:F5:7B:43:0B:69</td>
<td style="text-align: left;">Redmi 12</td>
<td style="text-align: right;">0.2459016</td>
</tr>
<tr class="odd">
<td style="text-align: left;">86:DF:BF:E4:2F:23</td>
<td style="text-align: left;">DESKTOP-Q7R0KRV 2433</td>
<td style="text-align: right;">0.2455357</td>
</tr>
<tr class="even">
<td style="text-align: left;">A6:02:B9:73:83:18</td>
<td style="text-align: left;">C239U52 6706</td>
<td style="text-align: right;">0.2228534</td>
</tr>
<tr class="odd">
<td style="text-align: left;">E8:28:C1:DD:04:40</td>
<td style="text-align: left;">MIREA_HOTSPOT</td>
<td style="text-align: right;">0.1914894</td>
</tr>
<tr class="even">
<td style="text-align: left;">26:20:53:0C:98:E8</td>
<td style="text-align: left;">NA</td>
<td style="text-align: right;">0.1722488</td>
</tr>
<tr class="odd">
<td style="text-align: left;">E8:28:C1:DD:04:42</td>
<td style="text-align: left;">NA</td>
<td style="text-align: right;">0.1659053</td>
</tr>
<tr class="even">
<td style="text-align: left;">E8:28:C1:DD:04:41</td>
<td style="text-align: left;">MIREA_GUESTS</td>
<td style="text-align: right;">0.1595745</td>
</tr>
<tr class="odd">
<td style="text-align: left;">B6:C4:55:B5:53:24</td>
<td style="text-align: left;">Redmi 9</td>
<td style="text-align: right;">0.1406093</td>
</tr>
<tr class="even">
<td style="text-align: left;">E8:28:C1:DD:04:50</td>
<td style="text-align: left;">MIREA_GUESTS</td>
<td style="text-align: right;">0.1334965</td>
</tr>
<tr class="odd">
<td style="text-align: left;">00:23:EB:E3:81:F1</td>
<td style="text-align: left;">IKB</td>
<td style="text-align: right;">0.1219512</td>
</tr>
<tr class="even">
<td style="text-align: left;">E8:28:C1:DC:BD:50</td>
<td style="text-align: left;">MIREA_GUESTS</td>
<td style="text-align: right;">0.1093693</td>
</tr>
<tr class="odd">
<td style="text-align: left;">E8:28:C1:DD:04:51</td>
<td style="text-align: left;">NA</td>
<td style="text-align: right;">0.0956938</td>
</tr>
<tr class="even">
<td style="text-align: left;">02:67:F1:B0:6C:98</td>
<td style="text-align: left;">MT_FREE</td>
<td style="text-align: right;">0.0921659</td>
</tr>
<tr class="odd">
<td style="text-align: left;">E8:28:C1:DC:C8:32</td>
<td style="text-align: left;">MIREA_HOTSPOT</td>
<td style="text-align: right;">0.0753532</td>
</tr>
<tr class="even">
<td style="text-align: left;">E8:28:C1:DC:C8:31</td>
<td style="text-align: left;">NA</td>
<td style="text-align: right;">0.0666759</td>
</tr>
<tr class="odd">
<td style="text-align: left;">E8:28:C1:DC:C6:B0</td>
<td style="text-align: left;">MIREA_GUESTS</td>
<td style="text-align: right;">0.0618716</td>
</tr>
<tr class="even">
<td style="text-align: left;">00:26:CB:AA:62:71</td>
<td style="text-align: left;">IKB</td>
<td style="text-align: right;">0.0609446</td>
</tr>
<tr class="odd">
<td style="text-align: left;">9E:A3:A9:BF:12:C0</td>
<td style="text-align: left;">NA</td>
<td style="text-align: right;">0.0582524</td>
</tr>
<tr class="even">
<td style="text-align: left;">E8:28:C1:DC:C8:30</td>
<td style="text-align: left;">MIREA_GUESTS</td>
<td style="text-align: right;">0.0497336</td>
</tr>
<tr class="odd">
<td style="text-align: left;">00:23:EB:E3:81:F2</td>
<td style="text-align: left;">GIVC</td>
<td style="text-align: right;">0.0437728</td>
</tr>
<tr class="even">
<td style="text-align: left;">7E:3A:10:A7:59:4E</td>
<td style="text-align: left;">vivo Y21</td>
<td style="text-align: right;">0.0390371</td>
</tr>
<tr class="odd">
<td style="text-align: left;">E8:28:C1:DC:B2:41</td>
<td style="text-align: left;">MIREA_GUESTS</td>
<td style="text-align: right;">0.0361141</td>
</tr>
<tr class="even">
<td style="text-align: left;">E8:28:C1:DC:C6:B1</td>
<td style="text-align: left;">NA</td>
<td style="text-align: right;">0.0357569</td>
</tr>
<tr class="odd">
<td style="text-align: left;">E8:28:C1:DC:B2:42</td>
<td style="text-align: left;">NA</td>
<td style="text-align: right;">0.0345105</td>
</tr>
<tr class="even">
<td style="text-align: left;">E8:28:C1:DC:B2:40</td>
<td style="text-align: left;">MIREA_HOTSPOT</td>
<td style="text-align: right;">0.0325662</td>
</tr>
<tr class="odd">
<td style="text-align: left;">0A:24:D8:D9:24:70</td>
<td style="text-align: left;">IgorKotya</td>
<td style="text-align: right;">0.0294768</td>
</tr>
<tr class="even">
<td style="text-align: left;">E8:28:C1:DE:74:31</td>
<td style="text-align: left;">NA</td>
<td style="text-align: right;">0.0270697</td>
</tr>
<tr class="odd">
<td style="text-align: left;">EA:7B:9B:D8:56:34</td>
<td style="text-align: left;">POCO C40</td>
<td style="text-align: right;">0.0267738</td>
</tr>
<tr class="even">
<td style="text-align: left;">E8:28:C1:DD:04:52</td>
<td style="text-align: left;">MIREA_HOTSPOT</td>
<td style="text-align: right;">0.0245499</td>
</tr>
<tr class="odd">
<td style="text-align: left;">10:50:72:00:11:08</td>
<td style="text-align: left;">MGTS_GPON_B563</td>
<td style="text-align: right;">0.0240144</td>
</tr>
<tr class="even">
<td style="text-align: left;">E8:28:C1:DE:47:D2</td>
<td style="text-align: left;">MIREA_HOTSPOT</td>
<td style="text-align: right;">0.0199093</td>
</tr>
<tr class="odd">
<td style="text-align: left;">EA:D8:D1:77:C8:08</td>
<td style="text-align: left;">DIRECT-08-HP M428fdw LJ</td>
<td style="text-align: right;">0.0120120</td>
</tr>
<tr class="even">
<td style="text-align: left;">E8:28:C1:DE:74:32</td>
<td style="text-align: left;">MIREA_HOTSPOT</td>
<td style="text-align: right;">0.0115607</td>
</tr>
<tr class="odd">
<td style="text-align: left;">56:99:98:EE:5A:4E</td>
<td style="text-align: left;">tementy-phone</td>
<td style="text-align: right;">0.0068097</td>
</tr>
<tr class="even">
<td style="text-align: left;">E8:28:C1:DC:FF:F2</td>
<td style="text-align: left;">NA</td>
<td style="text-align: right;">0.0000000</td>
</tr>
<tr class="odd">
<td style="text-align: left;">00:25:00:FF:94:73</td>
<td style="text-align: left;">NA</td>
<td style="text-align: right;">0.0000000</td>
</tr>
<tr class="even">
<td style="text-align: left;">08:3A:2F:56:35:FE</td>
<td style="text-align: left;">NA</td>
<td style="text-align: right;">0.0000000</td>
</tr>
<tr class="odd">
<td style="text-align: left;">E8:28:C1:DE:74:30</td>
<td style="text-align: left;">NA</td>
<td style="text-align: right;">0.0000000</td>
</tr>
<tr class="even">
<td style="text-align: left;">E0:D9:E3:48:FF:D2</td>
<td style="text-align: left;">NA</td>
<td style="text-align: right;">0.0000000</td>
</tr>
<tr class="odd">
<td style="text-align: left;">00:26:99:F2:7A:E0</td>
<td style="text-align: left;">NA</td>
<td style="text-align: right;">0.0000000</td>
</tr>
<tr class="even">
<td style="text-align: left;">2A:E8:A2:02:01:73</td>
<td style="text-align: left;">Redmi Note 9S</td>
<td style="text-align: right;">0.0000000</td>
</tr>
<tr class="odd">
<td style="text-align: left;">E8:28:C1:DC:3C:92</td>
<td style="text-align: left;">NA</td>
<td style="text-align: right;">0.0000000</td>
</tr>
<tr class="even">
<td style="text-align: left;">14:EB:B6:6A:76:37</td>
<td style="text-align: left;">Gnezdo_lounge 2</td>
<td style="text-align: right;">0.0000000</td>
</tr>
<tr class="odd">
<td style="text-align: left;">CE:B3:FF:84:45:FC</td>
<td style="text-align: left;">NA</td>
<td style="text-align: right;">0.0000000</td>
</tr>
<tr class="even">
<td style="text-align: left;">E8:28:C1:DC:54:72</td>
<td style="text-align: left;">NA</td>
<td style="text-align: right;">0.0000000</td>
</tr>
<tr class="odd">
<td style="text-align: left;">00:AB:0A:00:10:10</td>
<td style="text-align: left;">NA</td>
<td style="text-align: right;">0.0000000</td>
</tr>
<tr class="even">
<td style="text-align: left;">E8:28:C1:DC:C6:B2</td>
<td style="text-align: left;">NA</td>
<td style="text-align: right;">0.0000000</td>
</tr>
<tr class="odd">
<td style="text-align: left;">E8:28:C1:DB:F5:F2</td>
<td style="text-align: left;">NA</td>
<td style="text-align: right;">0.0000000</td>
</tr>
<tr class="even">
<td style="text-align: left;">BE:FD:EF:18:92:44</td>
<td style="text-align: left;">Димасик</td>
<td style="text-align: right;">0.0000000</td>
</tr>
<tr class="odd">
<td style="text-align: left;">00:23:EB:E3:49:31</td>
<td style="text-align: left;">NA</td>
<td style="text-align: right;">0.0000000</td>
</tr>
<tr class="even">
<td style="text-align: left;">CE:C3:F7:A4:7E:B3</td>
<td style="text-align: left;">DIRECT-A6-HP</td>
<td style="text-align: right;">0.0000000</td>
</tr>
<tr class="odd">
<td style="text-align: left;">E8:28:C1:DC:33:12</td>
<td style="text-align: left;">NA</td>
<td style="text-align: right;">0.0000000</td>
</tr>
<tr class="even">
<td style="text-align: left;">E8:28:C1:DB:FC:F2</td>
<td style="text-align: left;">NA</td>
<td style="text-align: right;">0.0000000</td>
</tr>
<tr class="odd">
<td style="text-align: left;">00:26:99:BA:75:8F</td>
<td style="text-align: left;">NA</td>
<td style="text-align: right;">0.0000000</td>
</tr>
<tr class="even">
<td style="text-align: left;">DC:09:4C:32:34:9B</td>
<td style="text-align: left;">kak_vashi_dela</td>
<td style="text-align: right;">0.0000000</td>
</tr>
<tr class="odd">
<td style="text-align: left;">E8:28:C1:DC:F0:90</td>
<td style="text-align: left;">NA</td>
<td style="text-align: right;">0.0000000</td>
</tr>
<tr class="even">
<td style="text-align: left;">E0:D9:E3:49:04:52</td>
<td style="text-align: left;">NA</td>
<td style="text-align: right;">0.0000000</td>
</tr>
<tr class="odd">
<td style="text-align: left;">E0:D9:E3:49:04:50</td>
<td style="text-align: left;">NA</td>
<td style="text-align: right;">0.0000000</td>
</tr>
<tr class="even">
<td style="text-align: left;">E0:D9:E3:49:04:40</td>
<td style="text-align: left;">NA</td>
<td style="text-align: right;">0.0000000</td>
</tr>
<tr class="odd">
<td style="text-align: left;">E8:28:C1:DC:54:B0</td>
<td style="text-align: left;">NA</td>
<td style="text-align: right;">0.0000000</td>
</tr>
<tr class="even">
<td style="text-align: left;">E0:D9:E3:49:00:B0</td>
<td style="text-align: left;">NA</td>
<td style="text-align: right;">0.0000000</td>
</tr>
<tr class="odd">
<td style="text-align: left;">E8:28:C1:DC:33:10</td>
<td style="text-align: left;">NA</td>
<td style="text-align: right;">0.0000000</td>
</tr>
<tr class="even">
<td style="text-align: left;">E8:28:C1:DB:F5:F0</td>
<td style="text-align: left;">MIREA_GUESTS</td>
<td style="text-align: right;">0.0000000</td>
</tr>
<tr class="odd">
<td style="text-align: left;">8A:A3:03:73:52:08</td>
<td style="text-align: left;">Galaxy A30s5208</td>
<td style="text-align: right;">0.0000000</td>
</tr>
<tr class="even">
<td style="text-align: left;">22:C9:7F:A9:BA:9C</td>
<td style="text-align: left;">NA</td>
<td style="text-align: right;">0.0000000</td>
</tr>
<tr class="odd">
<td style="text-align: left;">92:12:38:E5:7E:1E</td>
<td style="text-align: left;">NA</td>
<td style="text-align: right;">0.0000000</td>
</tr>
<tr class="even">
<td style="text-align: left;">E8:28:C1:DC:0B:B0</td>
<td style="text-align: left;">NA</td>
<td style="text-align: right;">0.0000000</td>
</tr>
<tr class="odd">
<td style="text-align: left;">82:CD:7D:04:17:3B</td>
<td style="text-align: left;">NA</td>
<td style="text-align: right;">0.0000000</td>
</tr>
<tr class="even">
<td style="text-align: left;">E8:28:C1:DC:54:B2</td>
<td style="text-align: left;">NA</td>
<td style="text-align: right;">0.0000000</td>
</tr>
<tr class="odd">
<td style="text-align: left;">C6:BC:37:7A:67:0D</td>
<td style="text-align: left;">Mura’s Galaxy A52</td>
<td style="text-align: right;">NaN</td>
</tr>
<tr class="even">
<td style="text-align: left;">12:48:F9:CF:58:8E</td>
<td style="text-align: left;">NA</td>
<td style="text-align: right;">NaN</td>
</tr>
<tr class="odd">
<td style="text-align: left;">E0:D9:E3:48:FF:D0</td>
<td style="text-align: left;">NA</td>
<td style="text-align: right;">NaN</td>
</tr>
<tr class="even">
<td style="text-align: left;">E2:37:BF:8F:6A:7B</td>
<td style="text-align: left;">NA</td>
<td style="text-align: right;">NaN</td>
</tr>
<tr class="odd">
<td style="text-align: left;">8A:4E:75:44:5A:F6</td>
<td style="text-align: left;">quiksmobile</td>
<td style="text-align: right;">NaN</td>
</tr>
<tr class="even">
<td style="text-align: left;">00:03:7A:1A:18:56</td>
<td style="text-align: left;">NA</td>
<td style="text-align: right;">NaN</td>
</tr>
<tr class="odd">
<td style="text-align: left;">00:09:9A:12:55:04</td>
<td style="text-align: left;">NA</td>
<td style="text-align: right;">NaN</td>
</tr>
<tr class="even">
<td style="text-align: left;">E8:28:C1:DC:3A:B0</td>
<td style="text-align: left;">NA</td>
<td style="text-align: right;">NaN</td>
</tr>
<tr class="odd">
<td style="text-align: left;">E8:28:C1:DC:0B:B2</td>
<td style="text-align: left;">NA</td>
<td style="text-align: right;">NaN</td>
</tr>
<tr class="even">
<td style="text-align: left;">E8:28:C1:DC:3C:80</td>
<td style="text-align: left;">NA</td>
<td style="text-align: right;">NaN</td>
</tr>
<tr class="odd">
<td style="text-align: left;">00:23:EB:E3:44:31</td>
<td style="text-align: left;">NA</td>
<td style="text-align: right;">NaN</td>
</tr>
<tr class="even">
<td style="text-align: left;">A6:F7:05:31:E8:EE</td>
<td style="text-align: left;">NA</td>
<td style="text-align: right;">NaN</td>
</tr>
<tr class="odd">
<td style="text-align: left;">12:54:1A:C6:FF:71</td>
<td style="text-align: left;">Amuler</td>
<td style="text-align: right;">NaN</td>
</tr>
<tr class="even">
<td style="text-align: left;">E8:28:C1:DC:03:30</td>
<td style="text-align: left;">NA</td>
<td style="text-align: right;">NaN</td>
</tr>
<tr class="odd">
<td style="text-align: left;">B2:1B:0C:67:0A:BD</td>
<td style="text-align: left;">NA</td>
<td style="text-align: right;">NaN</td>
</tr>
<tr class="even">
<td style="text-align: left;">E8:28:C1:DE:72:D0</td>
<td style="text-align: left;">NA</td>
<td style="text-align: right;">NaN</td>
</tr>
<tr class="odd">
<td style="text-align: left;">E0:D9:E3:49:04:41</td>
<td style="text-align: left;">NA</td>
<td style="text-align: right;">NaN</td>
</tr>
<tr class="even">
<td style="text-align: left;">00:26:99:F1:1A:E1</td>
<td style="text-align: left;">IKB</td>
<td style="text-align: right;">NaN</td>
</tr>
<tr class="odd">
<td style="text-align: left;">00:23:EB:E3:44:32</td>
<td style="text-align: left;">NA</td>
<td style="text-align: right;">NaN</td>
</tr>
<tr class="even">
<td style="text-align: left;">E0:D9:E3:48:B4:D2</td>
<td style="text-align: left;">NA</td>
<td style="text-align: right;">NaN</td>
</tr>
<tr class="odd">
<td style="text-align: left;">AE:3E:7F:C8:BC:8E</td>
<td style="text-align: left;">Igorek✨</td>
<td style="text-align: right;">NaN</td>
</tr>
<tr class="even">
<td style="text-align: left;">02:B3:45:5A:05:93</td>
<td style="text-align: left;">HONOR 30</td>
<td style="text-align: right;">NaN</td>
</tr>
<tr class="odd">
<td style="text-align: left;">00:00:00:00:00:00</td>
<td style="text-align: left;">NA</td>
<td style="text-align: right;">NaN</td>
</tr>
<tr class="even">
<td style="text-align: left;">E8:28:C1:DC:3C:90</td>
<td style="text-align: left;">NA</td>
<td style="text-align: right;">NaN</td>
</tr>
<tr class="odd">
<td style="text-align: left;">30:B4:B8:11:C0:90</td>
<td style="text-align: left;">NA</td>
<td style="text-align: right;">NaN</td>
</tr>
<tr class="even">
<td style="text-align: left;">00:26:99:F2:7A:EF</td>
<td style="text-align: left;">NA</td>
<td style="text-align: right;">NaN</td>
</tr>
<tr class="odd">
<td style="text-align: left;">E8:28:C1:DC:03:32</td>
<td style="text-align: left;">NA</td>
<td style="text-align: right;">NaN</td>
</tr>
<tr class="even">
<td style="text-align: left;">00:53:7A:99:98:56</td>
<td style="text-align: left;">MT_FREE</td>
<td style="text-align: right;">NaN</td>
</tr>
<tr class="odd">
<td style="text-align: left;">00:03:7F:10:17:56</td>
<td style="text-align: left;">NA</td>
<td style="text-align: right;">NaN</td>
</tr>
<tr class="even">
<td style="text-align: left;">00:0D:97:6B:93:DF</td>
<td style="text-align: left;">NA</td>
<td style="text-align: right;">NaN</td>
</tr>
</tbody>
</table>

1.  Данные клиентов

<!-- -->

1.  Определить производителя для каждого обнаруженного устройства

``` r
manufa <- dataset2 %>% filter(BSSID != '(not associated)') %>% mutate(Manufacturer = substr(BSSID, 1, 8)) %>% select(Manufacturer)

unique(manufa) %>% knitr::kable()
```

<table>
<thead>
<tr class="header">
<th style="text-align: left;"></th>
<th style="text-align: left;">Manufacturer</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">1</td>
<td style="text-align: left;">BE:F1:71</td>
</tr>
<tr class="even">
<td style="text-align: left;">4</td>
<td style="text-align: left;">1E:93:E3</td>
</tr>
<tr class="odd">
<td style="text-align: left;">5</td>
<td style="text-align: left;">E8:28:C1</td>
</tr>
<tr class="even">
<td style="text-align: left;">6</td>
<td style="text-align: left;">00:25:00</td>
</tr>
<tr class="odd">
<td style="text-align: left;">7</td>
<td style="text-align: left;">00:26:99</td>
</tr>
<tr class="even">
<td style="text-align: left;">8</td>
<td style="text-align: left;">0C:80:63</td>
</tr>
<tr class="odd">
<td style="text-align: left;">10</td>
<td style="text-align: left;">0A:C5:E1</td>
</tr>
<tr class="even">
<td style="text-align: left;">12</td>
<td style="text-align: left;">9A:75:A8</td>
</tr>
<tr class="odd">
<td style="text-align: left;">13</td>
<td style="text-align: left;">8A:A3:03</td>
</tr>
<tr class="even">
<td style="text-align: left;">14</td>
<td style="text-align: left;">4A:EC:1E</td>
</tr>
<tr class="odd">
<td style="text-align: left;">16</td>
<td style="text-align: left;">08:3A:2F</td>
</tr>
<tr class="even">
<td style="text-align: left;">17</td>
<td style="text-align: left;">6E:C7:EC</td>
</tr>
<tr class="odd">
<td style="text-align: left;">21</td>
<td style="text-align: left;">2A:E8:A2</td>
</tr>
<tr class="even">
<td style="text-align: left;">28</td>
<td style="text-align: left;">56:C5:2B</td>
</tr>
<tr class="odd">
<td style="text-align: left;">30</td>
<td style="text-align: left;">9A:9F:06</td>
</tr>
<tr class="even">
<td style="text-align: left;">31</td>
<td style="text-align: left;">12:48:F9</td>
</tr>
<tr class="odd">
<td style="text-align: left;">35</td>
<td style="text-align: left;">AA:F4:3F</td>
</tr>
<tr class="even">
<td style="text-align: left;">37</td>
<td style="text-align: left;">3A:70:96</td>
</tr>
<tr class="odd">
<td style="text-align: left;">42</td>
<td style="text-align: left;">8E:55:4A</td>
</tr>
<tr class="even">
<td style="text-align: left;">43</td>
<td style="text-align: left;">5E:C7:C0</td>
</tr>
<tr class="odd">
<td style="text-align: left;">44</td>
<td style="text-align: left;">E2:37:BF</td>
</tr>
<tr class="even">
<td style="text-align: left;">48</td>
<td style="text-align: left;">96:FF:FC</td>
</tr>
<tr class="odd">
<td style="text-align: left;">50</td>
<td style="text-align: left;">CE:B3:FF</td>
</tr>
<tr class="even">
<td style="text-align: left;">58</td>
<td style="text-align: left;">76:70:AF</td>
</tr>
<tr class="odd">
<td style="text-align: left;">60</td>
<td style="text-align: left;">00:AB:0A</td>
</tr>
<tr class="even">
<td style="text-align: left;">64</td>
<td style="text-align: left;">MIREA_HO</td>
</tr>
<tr class="odd">
<td style="text-align: left;">66</td>
<td style="text-align: left;">8E:1F:94</td>
</tr>
<tr class="even">
<td style="text-align: left;">78</td>
<td style="text-align: left;">EA:7B:9B</td>
</tr>
<tr class="odd">
<td style="text-align: left;">79</td>
<td style="text-align: left;">BE:FD:EF</td>
</tr>
<tr class="even">
<td style="text-align: left;">81</td>
<td style="text-align: left;">7E:3A:10</td>
</tr>
<tr class="odd">
<td style="text-align: left;">83</td>
<td style="text-align: left;">00:23:EB</td>
</tr>
<tr class="even">
<td style="text-align: left;">87</td>
<td style="text-align: left;">E0:D9:E3</td>
</tr>
<tr class="odd">
<td style="text-align: left;">88</td>
<td style="text-align: left;">3A:DA:00</td>
</tr>
<tr class="even">
<td style="text-align: left;">100</td>
<td style="text-align: left;">92:F5:7B</td>
</tr>
<tr class="odd">
<td style="text-align: left;">103</td>
<td style="text-align: left;">AndroidS</td>
</tr>
<tr class="even">
<td style="text-align: left;">104</td>
<td style="text-align: left;">DC:09:4C</td>
</tr>
<tr class="odd">
<td style="text-align: left;">110</td>
<td style="text-align: left;">22:C9:7F</td>
</tr>
<tr class="even">
<td style="text-align: left;">112</td>
<td style="text-align: left;">TP-Link_</td>
</tr>
<tr class="odd">
<td style="text-align: left;">117</td>
<td style="text-align: left;">92:12:38</td>
</tr>
<tr class="even">
<td style="text-align: left;">120</td>
<td style="text-align: left;">B2:1B:0C</td>
</tr>
<tr class="odd">
<td style="text-align: left;">134</td>
<td style="text-align: left;">1E:C2:8E</td>
</tr>
<tr class="even">
<td style="text-align: left;">136</td>
<td style="text-align: left;">A2:64:E8</td>
</tr>
<tr class="odd">
<td style="text-align: left;">138</td>
<td style="text-align: left;">A6:02:B9</td>
</tr>
<tr class="even">
<td style="text-align: left;">150</td>
<td style="text-align: left;">AE:3E:7F</td>
</tr>
<tr class="odd">
<td style="text-align: left;">158</td>
<td style="text-align: left;">B6:C4:55</td>
</tr>
<tr class="even">
<td style="text-align: left;">161</td>
<td style="text-align: left;">86:DF:BF</td>
</tr>
<tr class="odd">
<td style="text-align: left;">163</td>
<td style="text-align: left;">02:67:F1</td>
</tr>
<tr class="even">
<td style="text-align: left;">169</td>
<td style="text-align: left;">36:46:53</td>
</tr>
<tr class="odd">
<td style="text-align: left;">176</td>
<td style="text-align: left;">82:CD:7D</td>
</tr>
<tr class="even">
<td style="text-align: left;">182</td>
<td style="text-align: left;">00:03:7F</td>
</tr>
<tr class="odd">
<td style="text-align: left;">183</td>
<td style="text-align: left;">00:0D:97</td>
</tr>
</tbody>
</table>

DC:09:4C Huawei Technologies Co.,Ltd

00:25:00 Apple, Inc.

00:03:7F Atheros Communications, Inc.

00:23:EB Cisco Systems, Inc

00:0D:97 Hitachi Energy USA Inc.

08:3A:2F Guangzhou Juan Intelligent Tech Joint Stock Co.,Ltd

E0:D9:E3 Eltex Enterprise Ltd.

E8:28:C1 Eltex Enterprise Ltd.

0C:80:63 Tp-Link Technologies Co.,Ltd.

1.  Обнаружить устройства, которые НЕ рандомизируют свой MAC адрес

``` r
non_randomized_mac <- dataset2 %>% filter(!grepl("^02|^06|^0A|^0E", BSSID)) %>% filter(BSSID != '(not associated)')

non_randomized_mac %>% select(BSSID) %>% knitr::kable()
```

<table>
<thead>
<tr class="header">
<th style="text-align: left;">BSSID</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">BE:F1:71:D5:17:8B</td>
</tr>
<tr class="even">
<td style="text-align: left;">BE:F1:71:D6:10:D7</td>
</tr>
<tr class="odd">
<td style="text-align: left;">BE:F1:71:D5:17:8B</td>
</tr>
<tr class="even">
<td style="text-align: left;">1E:93:E3:1B:3C:F4</td>
</tr>
<tr class="odd">
<td style="text-align: left;">E8:28:C1:DC:FF:F2</td>
</tr>
<tr class="even">
<td style="text-align: left;">00:25:00:FF:94:73</td>
</tr>
<tr class="odd">
<td style="text-align: left;">00:26:99:F2:7A:E2</td>
</tr>
<tr class="even">
<td style="text-align: left;">0C:80:63:A9:6E:EE</td>
</tr>
<tr class="odd">
<td style="text-align: left;">E8:28:C1:DD:04:52</td>
</tr>
<tr class="even">
<td style="text-align: left;">1E:93:E3:1B:3C:F4</td>
</tr>
<tr class="odd">
<td style="text-align: left;">9A:75:A8:B9:04:1E</td>
</tr>
<tr class="even">
<td style="text-align: left;">8A:A3:03:73:52:08</td>
</tr>
<tr class="odd">
<td style="text-align: left;">4A:EC:1E:DB:BF:95</td>
</tr>
<tr class="even">
<td style="text-align: left;">BE:F1:71:D5:0E:53</td>
</tr>
<tr class="odd">
<td style="text-align: left;">08:3A:2F:56:35:FE</td>
</tr>
<tr class="even">
<td style="text-align: left;">6E:C7:EC:16:DA:1A</td>
</tr>
<tr class="odd">
<td style="text-align: left;">6E:C7:EC:16:DA:1A</td>
</tr>
<tr class="even">
<td style="text-align: left;">08:3A:2F:56:35:FE</td>
</tr>
<tr class="odd">
<td style="text-align: left;">00:26:99:F2:7A:E2</td>
</tr>
<tr class="even">
<td style="text-align: left;">2A:E8:A2:02:01:73</td>
</tr>
<tr class="odd">
<td style="text-align: left;">E8:28:C1:DC:B2:52</td>
</tr>
<tr class="even">
<td style="text-align: left;">E8:28:C1:DC:C6:B2</td>
</tr>
<tr class="odd">
<td style="text-align: left;">00:26:99:F2:7A:E2</td>
</tr>
<tr class="even">
<td style="text-align: left;">08:3A:2F:56:35:FE</td>
</tr>
<tr class="odd">
<td style="text-align: left;">E8:28:C1:DC:B2:52</td>
</tr>
<tr class="even">
<td style="text-align: left;">E8:28:C1:DC:C8:32</td>
</tr>
<tr class="odd">
<td style="text-align: left;">56:C5:2B:9F:84:90</td>
</tr>
<tr class="even">
<td style="text-align: left;">E8:28:C1:DC:B2:52</td>
</tr>
<tr class="odd">
<td style="text-align: left;">9A:9F:06:44:24:5B</td>
</tr>
<tr class="even">
<td style="text-align: left;">12:48:F9:CF:58:8E</td>
</tr>
<tr class="odd">
<td style="text-align: left;">E8:28:C1:DC:B2:52</td>
</tr>
<tr class="even">
<td style="text-align: left;">E8:28:C1:DD:04:50</td>
</tr>
<tr class="odd">
<td style="text-align: left;">E8:28:C1:DC:B2:52</td>
</tr>
<tr class="even">
<td style="text-align: left;">AA:F4:3F:EE:49:0B</td>
</tr>
<tr class="odd">
<td style="text-align: left;">E8:28:C1:DC:B2:52</td>
</tr>
<tr class="even">
<td style="text-align: left;">3A:70:96:C6:30:2C</td>
</tr>
<tr class="odd">
<td style="text-align: left;">E8:28:C1:DC:FF:F2</td>
</tr>
<tr class="even">
<td style="text-align: left;">E8:28:C1:DC:3C:92</td>
</tr>
<tr class="odd">
<td style="text-align: left;">00:26:99:F2:7A:E2</td>
</tr>
<tr class="even">
<td style="text-align: left;">00:26:99:F2:7A:E2</td>
</tr>
<tr class="odd">
<td style="text-align: left;">8E:55:4A:85:5B:01</td>
</tr>
<tr class="even">
<td style="text-align: left;">5E:C7:C0:E4:D7:D4</td>
</tr>
<tr class="odd">
<td style="text-align: left;">E2:37:BF:8F:6A:7B</td>
</tr>
<tr class="even">
<td style="text-align: left;">E8:28:C1:DC:B2:52</td>
</tr>
<tr class="odd">
<td style="text-align: left;">E8:28:C1:DC:B2:52</td>
</tr>
<tr class="even">
<td style="text-align: left;">8E:55:4A:85:5B:01</td>
</tr>
<tr class="odd">
<td style="text-align: left;">96:FF:FC:91:EF:64</td>
</tr>
<tr class="even">
<td style="text-align: left;">E8:28:C1:DD:04:52</td>
</tr>
<tr class="odd">
<td style="text-align: left;">CE:B3:FF:84:45:FC</td>
</tr>
<tr class="even">
<td style="text-align: left;">E8:28:C1:DC:B2:52</td>
</tr>
<tr class="odd">
<td style="text-align: left;">8E:55:4A:85:5B:01</td>
</tr>
<tr class="even">
<td style="text-align: left;">0C:80:63:A9:6E:EE</td>
</tr>
<tr class="odd">
<td style="text-align: left;">8E:55:4A:85:5B:01</td>
</tr>
<tr class="even">
<td style="text-align: left;">00:26:99:BA:75:80</td>
</tr>
<tr class="odd">
<td style="text-align: left;">08:3A:2F:56:35:FE</td>
</tr>
<tr class="even">
<td style="text-align: left;">00:26:99:BA:75:80</td>
</tr>
<tr class="odd">
<td style="text-align: left;">76:70:AF:A4:D2:AF</td>
</tr>
<tr class="even">
<td style="text-align: left;">E8:28:C1:DC:B2:50</td>
</tr>
<tr class="odd">
<td style="text-align: left;">00:AB:0A:00:10:10</td>
</tr>
<tr class="even">
<td style="text-align: left;">E8:28:C1:DC:C8:30</td>
</tr>
<tr class="odd">
<td style="text-align: left;">00:25:00:FF:94:73</td>
</tr>
<tr class="even">
<td style="text-align: left;">00:25:00:FF:94:73</td>
</tr>
<tr class="odd">
<td style="text-align: left;">MIREA_HOTSPOT</td>
</tr>
<tr class="even">
<td style="text-align: left;">00:25:00:FF:94:73</td>
</tr>
<tr class="odd">
<td style="text-align: left;">8E:1F:94:96:DA:FD</td>
</tr>
<tr class="even">
<td style="text-align: left;">00:25:00:FF:94:73</td>
</tr>
<tr class="odd">
<td style="text-align: left;">00:25:00:FF:94:73</td>
</tr>
<tr class="even">
<td style="text-align: left;">00:25:00:FF:94:73</td>
</tr>
<tr class="odd">
<td style="text-align: left;">E8:28:C1:DB:F5:F2</td>
</tr>
<tr class="even">
<td style="text-align: left;">00:25:00:FF:94:73</td>
</tr>
<tr class="odd">
<td style="text-align: left;">E8:28:C1:DC:FF:F2</td>
</tr>
<tr class="even">
<td style="text-align: left;">00:25:00:FF:94:73</td>
</tr>
<tr class="odd">
<td style="text-align: left;">00:25:00:FF:94:73</td>
</tr>
<tr class="even">
<td style="text-align: left;">E8:28:C1:DC:B2:52</td>
</tr>
<tr class="odd">
<td style="text-align: left;">E8:28:C1:DD:04:40</td>
</tr>
<tr class="even">
<td style="text-align: left;">E8:28:C1:DC:C6:B2</td>
</tr>
<tr class="odd">
<td style="text-align: left;">EA:7B:9B:D8:56:34</td>
</tr>
<tr class="even">
<td style="text-align: left;">BE:FD:EF:18:92:44</td>
</tr>
<tr class="odd">
<td style="text-align: left;">8E:55:4A:85:5B:01</td>
</tr>
<tr class="even">
<td style="text-align: left;">7E:3A:10:A7:59:4E</td>
</tr>
<tr class="odd">
<td style="text-align: left;">00:26:99:F2:7A:E1</td>
</tr>
<tr class="even">
<td style="text-align: left;">00:23:EB:E3:49:31</td>
</tr>
<tr class="odd">
<td style="text-align: left;">E8:28:C1:DC:C6:B2</td>
</tr>
<tr class="even">
<td style="text-align: left;">E8:28:C1:DC:C6:B2</td>
</tr>
<tr class="odd">
<td style="text-align: left;">E8:28:C1:DC:B2:40</td>
</tr>
<tr class="even">
<td style="text-align: left;">E0:D9:E3:49:04:40</td>
</tr>
<tr class="odd">
<td style="text-align: left;">3A:DA:00:F9:0C:02</td>
</tr>
<tr class="even">
<td style="text-align: left;">00:25:00:FF:94:73</td>
</tr>
<tr class="odd">
<td style="text-align: left;">00:25:00:FF:94:73</td>
</tr>
<tr class="even">
<td style="text-align: left;">E8:28:C1:DC:B2:41</td>
</tr>
<tr class="odd">
<td style="text-align: left;">E8:28:C1:DE:74:32</td>
</tr>
<tr class="even">
<td style="text-align: left;">E8:28:C1:DD:04:40</td>
</tr>
<tr class="odd">
<td style="text-align: left;">E8:28:C1:DD:04:52</td>
</tr>
<tr class="even">
<td style="text-align: left;">E8:28:C1:DE:74:32</td>
</tr>
<tr class="odd">
<td style="text-align: left;">00:26:99:BA:75:80</td>
</tr>
<tr class="even">
<td style="text-align: left;">E8:28:C1:DC:33:12</td>
</tr>
<tr class="odd">
<td style="text-align: left;">00:26:99:BA:75:80</td>
</tr>
<tr class="even">
<td style="text-align: left;">00:26:99:BA:75:80</td>
</tr>
<tr class="odd">
<td style="text-align: left;">92:F5:7B:43:0B:69</td>
</tr>
<tr class="even">
<td style="text-align: left;">AA:F4:3F:EE:49:0B</td>
</tr>
<tr class="odd">
<td style="text-align: left;">E8:28:C1:DC:C6:B2</td>
</tr>
<tr class="even">
<td style="text-align: left;">AndroidShare_6329</td>
</tr>
<tr class="odd">
<td style="text-align: left;">DC:09:4C:32:34:9B</td>
</tr>
<tr class="even">
<td style="text-align: left;">E8:28:C1:DC:F0:90</td>
</tr>
<tr class="odd">
<td style="text-align: left;">00:25:00:FF:94:73</td>
</tr>
<tr class="even">
<td style="text-align: left;">E0:D9:E3:49:04:52</td>
</tr>
<tr class="odd">
<td style="text-align: left;">E8:28:C1:DC:F0:90</td>
</tr>
<tr class="even">
<td style="text-align: left;">00:25:00:FF:94:73</td>
</tr>
<tr class="odd">
<td style="text-align: left;">22:C9:7F:A9:BA:9C</td>
</tr>
<tr class="even">
<td style="text-align: left;">E8:28:C1:DD:04:41</td>
</tr>
<tr class="odd">
<td style="text-align: left;">TP-Link_9872_5G</td>
</tr>
<tr class="even">
<td style="text-align: left;">00:25:00:FF:94:73</td>
</tr>
<tr class="odd">
<td style="text-align: left;">E8:28:C1:DD:04:52</td>
</tr>
<tr class="even">
<td style="text-align: left;">E8:28:C1:DC:B2:52</td>
</tr>
<tr class="odd">
<td style="text-align: left;">E8:28:C1:DE:74:32</td>
</tr>
<tr class="even">
<td style="text-align: left;">92:12:38:E5:7E:1E</td>
</tr>
<tr class="odd">
<td style="text-align: left;">E8:28:C1:DC:C6:B2</td>
</tr>
<tr class="even">
<td style="text-align: left;">E8:28:C1:DC:C6:B2</td>
</tr>
<tr class="odd">
<td style="text-align: left;">B2:1B:0C:67:0A:BD</td>
</tr>
<tr class="even">
<td style="text-align: left;">E8:28:C1:DE:74:32</td>
</tr>
<tr class="odd">
<td style="text-align: left;">00:25:00:FF:94:73</td>
</tr>
<tr class="even">
<td style="text-align: left;">E8:28:C1:DC:B2:50</td>
</tr>
<tr class="odd">
<td style="text-align: left;">00:26:99:BA:75:80</td>
</tr>
<tr class="even">
<td style="text-align: left;">E8:28:C1:DC:B2:50</td>
</tr>
<tr class="odd">
<td style="text-align: left;">E8:28:C1:DC:33:10</td>
</tr>
<tr class="even">
<td style="text-align: left;">00:25:00:FF:94:73</td>
</tr>
<tr class="odd">
<td style="text-align: left;">AA:F4:3F:EE:49:0B</td>
</tr>
<tr class="even">
<td style="text-align: left;">E0:D9:E3:49:04:41</td>
</tr>
<tr class="odd">
<td style="text-align: left;">00:25:00:FF:94:73</td>
</tr>
<tr class="even">
<td style="text-align: left;">E8:28:C1:DC:C6:B2</td>
</tr>
<tr class="odd">
<td style="text-align: left;">00:25:00:FF:94:73</td>
</tr>
<tr class="even">
<td style="text-align: left;">00:25:00:FF:94:73</td>
</tr>
<tr class="odd">
<td style="text-align: left;">1E:C2:8E:D8:30:91</td>
</tr>
<tr class="even">
<td style="text-align: left;">E8:28:C1:DC:B2:52</td>
</tr>
<tr class="odd">
<td style="text-align: left;">A2:64:E8:97:58:EE</td>
</tr>
<tr class="even">
<td style="text-align: left;">00:25:00:FF:94:73</td>
</tr>
<tr class="odd">
<td style="text-align: left;">A6:02:B9:73:2F:76</td>
</tr>
<tr class="even">
<td style="text-align: left;">E8:28:C1:DD:04:50</td>
</tr>
<tr class="odd">
<td style="text-align: left;">A6:02:B9:73:81:47</td>
</tr>
<tr class="even">
<td style="text-align: left;">92:F5:7B:43:0B:69</td>
</tr>
<tr class="odd">
<td style="text-align: left;">00:25:00:FF:94:73</td>
</tr>
<tr class="even">
<td style="text-align: left;">00:25:00:FF:94:73</td>
</tr>
<tr class="odd">
<td style="text-align: left;">00:25:00:FF:94:73</td>
</tr>
<tr class="even">
<td style="text-align: left;">E8:28:C1:DC:F0:90</td>
</tr>
<tr class="odd">
<td style="text-align: left;">1E:93:E3:1B:3C:F4</td>
</tr>
<tr class="even">
<td style="text-align: left;">00:25:00:FF:94:73</td>
</tr>
<tr class="odd">
<td style="text-align: left;">00:25:00:FF:94:73</td>
</tr>
<tr class="even">
<td style="text-align: left;">00:26:99:BA:75:80</td>
</tr>
<tr class="odd">
<td style="text-align: left;">AE:3E:7F:C8:BC:8E</td>
</tr>
<tr class="even">
<td style="text-align: left;">00:25:00:FF:94:73</td>
</tr>
<tr class="odd">
<td style="text-align: left;">00:25:00:FF:94:73</td>
</tr>
<tr class="even">
<td style="text-align: left;">00:25:00:FF:94:73</td>
</tr>
<tr class="odd">
<td style="text-align: left;">00:25:00:FF:94:73</td>
</tr>
<tr class="even">
<td style="text-align: left;">00:26:99:F2:7A:E2</td>
</tr>
<tr class="odd">
<td style="text-align: left;">00:25:00:FF:94:73</td>
</tr>
<tr class="even">
<td style="text-align: left;">00:25:00:FF:94:73</td>
</tr>
<tr class="odd">
<td style="text-align: left;">B6:C4:55:B5:53:24</td>
</tr>
<tr class="even">
<td style="text-align: left;">00:25:00:FF:94:73</td>
</tr>
<tr class="odd">
<td style="text-align: left;">E8:28:C1:DC:B2:50</td>
</tr>
<tr class="even">
<td style="text-align: left;">86:DF:BF:E4:2F:23</td>
</tr>
<tr class="odd">
<td style="text-align: left;">E8:28:C1:DC:C8:32</td>
</tr>
<tr class="even">
<td style="text-align: left;">E8:28:C1:DC:B2:50</td>
</tr>
<tr class="odd">
<td style="text-align: left;">00:26:99:BA:75:80</td>
</tr>
<tr class="even">
<td style="text-align: left;">00:25:00:FF:94:73</td>
</tr>
<tr class="odd">
<td style="text-align: left;">CE:B3:FF:84:45:FC</td>
</tr>
<tr class="even">
<td style="text-align: left;">00:25:00:FF:94:73</td>
</tr>
<tr class="odd">
<td style="text-align: left;">36:46:53:81:12:A0</td>
</tr>
<tr class="even">
<td style="text-align: left;">00:25:00:FF:94:73</td>
</tr>
<tr class="odd">
<td style="text-align: left;">8E:55:4A:85:5B:01</td>
</tr>
<tr class="even">
<td style="text-align: left;">86:DF:BF:E4:2F:23</td>
</tr>
<tr class="odd">
<td style="text-align: left;">00:26:99:BA:75:80</td>
</tr>
<tr class="even">
<td style="text-align: left;">00:26:99:F2:7A:E2</td>
</tr>
<tr class="odd">
<td style="text-align: left;">E8:28:C1:DC:0B:B0</td>
</tr>
<tr class="even">
<td style="text-align: left;">82:CD:7D:04:17:3B</td>
</tr>
<tr class="odd">
<td style="text-align: left;">00:25:00:FF:94:73</td>
</tr>
<tr class="even">
<td style="text-align: left;">E8:28:C1:DC:54:B2</td>
</tr>
<tr class="odd">
<td style="text-align: left;">00:25:00:FF:94:73</td>
</tr>
<tr class="even">
<td style="text-align: left;">00:25:00:FF:94:73</td>
</tr>
<tr class="odd">
<td style="text-align: left;">00:25:00:FF:94:73</td>
</tr>
<tr class="even">
<td style="text-align: left;">00:03:7F:10:17:56</td>
</tr>
<tr class="odd">
<td style="text-align: left;">00:0D:97:6B:93:DF</td>
</tr>
<tr class="even">
<td style="text-align: left;">00:25:00:FF:94:73</td>
</tr>
<tr class="odd">
<td style="text-align: left;">00:25:00:FF:94:73</td>
</tr>
<tr class="even">
<td style="text-align: left;">00:26:99:F2:7A:E2</td>
</tr>
<tr class="odd">
<td style="text-align: left;">00:25:00:FF:94:73</td>
</tr>
<tr class="even">
<td style="text-align: left;">00:25:00:FF:94:73</td>
</tr>
<tr class="odd">
<td style="text-align: left;">00:25:00:FF:94:73</td>
</tr>
</tbody>
</table>

1.  Кластеризовать запросы от устройств к точкам доступа по их именам.
    Определить время появления устройства в зоне радиовидимости и время
    выхода его из нее.

``` r
clustered_requests <- dataset2 %>% group_by(Probed.ESSIDs) %>% summarise(
first_seen = min(First.time.seen, na.rm = TRUE),
last_seen = max(Last.time.seen, na.rm = TRUE))

clustered_requests %>% knitr::kable()
```

<table>
<colgroup>
<col style="width: 45%" />
<col style="width: 27%" />
<col style="width: 27%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;">Probed.ESSIDs</th>
<th style="text-align: left;">first_seen</th>
<th style="text-align: left;">last_seen</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">-D-13-</td>
<td style="text-align: left;">2023-07-28 09:14:42</td>
<td style="text-align: left;">2023-07-28 10:26:42</td>
</tr>
<tr class="even">
<td style="text-align: left;">1</td>
<td style="text-align: left;">2023-07-28 10:36:12</td>
<td style="text-align: left;">2023-07-28 11:56:13</td>
</tr>
<tr class="odd">
<td style="text-align: left;">107</td>
<td style="text-align: left;">2023-07-28 10:29:43</td>
<td style="text-align: left;">2023-07-28 10:29:43</td>
</tr>
<tr class="even">
<td style="text-align: left;">531</td>
<td style="text-align: left;">2023-07-28 10:57:04</td>
<td style="text-align: left;">2023-07-28 10:57:04</td>
</tr>
<tr class="odd">
<td style="text-align: left;">AAAAAOB/CC0ADwGkRedmi 3S</td>
<td style="text-align: left;">2023-07-28 09:34:20</td>
<td style="text-align: left;">2023-07-28 11:44:40</td>
</tr>
<tr class="even">
<td style="text-align: left;">AKADO-D967</td>
<td style="text-align: left;">2023-07-28 10:31:55</td>
<td style="text-align: left;">2023-07-28 10:31:55</td>
</tr>
<tr class="odd">
<td style="text-align: left;">AQAAAB6zaIoATwEURedmi Note 5</td>
<td style="text-align: left;">2023-07-28 10:25:19</td>
<td style="text-align: left;">2023-07-28 11:51:48</td>
</tr>
<tr class="even">
<td style="text-align: left;">ASUS</td>
<td style="text-align: left;">2023-07-28 10:31:13</td>
<td style="text-align: left;">2023-07-28 10:31:13</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Alex-net2</td>
<td style="text-align: left;">2023-07-28 10:01:06</td>
<td style="text-align: left;">2023-07-28 10:01:06</td>
</tr>
<tr class="even">
<td style="text-align: left;">AndroidAP177B</td>
<td style="text-align: left;">2023-07-28 09:13:09</td>
<td style="text-align: left;">2023-07-28 11:34:42</td>
</tr>
<tr class="odd">
<td style="text-align: left;">AndroidShare_1576</td>
<td style="text-align: left;">2023-07-28 10:33:17</td>
<td style="text-align: left;">2023-07-28 11:55:35</td>
</tr>
<tr class="even">
<td style="text-align: left;">AndroidShare_1901</td>
<td style="text-align: left;">2023-07-28 10:35:30</td>
<td style="text-align: left;">2023-07-28 11:47:33</td>
</tr>
<tr class="odd">
<td style="text-align: left;">AndroidShare_2061</td>
<td style="text-align: left;">2023-07-28 10:29:24</td>
<td style="text-align: left;">2023-07-28 10:29:24</td>
</tr>
<tr class="even">
<td style="text-align: left;">AndroidShare_2280</td>
<td style="text-align: left;">2023-07-28 10:43:22</td>
<td style="text-align: left;">2023-07-28 11:45:00</td>
</tr>
<tr class="odd">
<td style="text-align: left;">AndroidShare_3542</td>
<td style="text-align: left;">2023-07-28 10:25:11</td>
<td style="text-align: left;">2023-07-28 10:25:11</td>
</tr>
<tr class="even">
<td style="text-align: left;">AndroidShare_8927</td>
<td style="text-align: left;">2023-07-28 10:29:02</td>
<td style="text-align: left;">2023-07-28 10:29:02</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Antonchik</td>
<td style="text-align: left;">2023-07-28 09:40:00</td>
<td style="text-align: left;">2023-07-28 09:40:00</td>
</tr>
<tr class="even">
<td style="text-align: left;">Avenue611</td>
<td style="text-align: left;">2023-07-28 09:41:01</td>
<td style="text-align: left;">2023-07-28 10:00:55</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Beeline121</td>
<td style="text-align: left;">2023-07-28 10:55:57</td>
<td style="text-align: left;">2023-07-28 11:49:15</td>
</tr>
<tr class="even">
<td style="text-align: left;">Beeline_5G_F2F425</td>
<td style="text-align: left;">2023-07-28 10:41:15</td>
<td style="text-align: left;">2023-07-28 11:56:14</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Belorusneft Free</td>
<td style="text-align: left;">2023-07-28 09:17:43</td>
<td style="text-align: left;">2023-07-28 11:24:55</td>
</tr>
<tr class="even">
<td style="text-align: left;">C322U06 5179</td>
<td style="text-align: left;">2023-07-28 09:13:06</td>
<td style="text-align: left;">2023-07-28 11:50:50</td>
</tr>
<tr class="odd">
<td style="text-align: left;">C322U13 3965</td>
<td style="text-align: left;">2023-07-28 09:13:03</td>
<td style="text-align: left;">2023-07-28 11:53:16</td>
</tr>
<tr class="even">
<td style="text-align: left;">C322U21 0566</td>
<td style="text-align: left;">2023-07-28 09:13:03</td>
<td style="text-align: left;">2023-07-28 11:51:54</td>
</tr>
<tr class="odd">
<td style="text-align: left;">C349U26 3598</td>
<td style="text-align: left;">2023-07-28 09:33:11</td>
<td style="text-align: left;">2023-07-28 09:33:11</td>
</tr>
<tr class="even">
<td style="text-align: left;">CPPK_FREE</td>
<td style="text-align: left;">2023-07-28 10:29:07</td>
<td style="text-align: left;">2023-07-28 10:29:07</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Cnet</td>
<td style="text-align: left;">2023-07-28 09:13:29</td>
<td style="text-align: left;">2023-07-28 11:51:50</td>
</tr>
<tr class="even">
<td style="text-align: left;">DIRECT-03-BMW44597</td>
<td style="text-align: left;">2023-07-28 09:29:55</td>
<td style="text-align: left;">2023-07-28 09:29:55</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Damy</td>
<td style="text-align: left;">2023-07-28 10:28:41</td>
<td style="text-align: left;">2023-07-28 11:50:44</td>
</tr>
<tr class="even">
<td style="text-align: left;">Dex_Pro</td>
<td style="text-align: left;">2023-07-28 11:09:12</td>
<td style="text-align: left;">2023-07-28 11:09:12</td>
</tr>
<tr class="odd">
<td style="text-align: left;">GGWPRedmi Note 10S</td>
<td style="text-align: left;">2023-07-28 11:34:44</td>
<td style="text-align: left;">2023-07-28 11:51:48</td>
</tr>
<tr class="even">
<td style="text-align: left;">GIVC</td>
<td style="text-align: left;">2023-07-28 09:13:06</td>
<td style="text-align: left;">2023-07-28 11:56:10</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Galaxy A31CAED</td>
<td style="text-align: left;">2023-07-28 09:43:08</td>
<td style="text-align: left;">2023-07-28 10:00:55</td>
</tr>
<tr class="even">
<td style="text-align: left;">Galaxy A71</td>
<td style="text-align: left;">2023-07-28 09:13:13</td>
<td style="text-align: left;">2023-07-28 11:50:22</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Gardenpasanauri</td>
<td style="text-align: left;">2023-07-28 09:13:14</td>
<td style="text-align: left;">2023-07-28 09:53:34</td>
</tr>
<tr class="even">
<td style="text-align: left;">Guest Concept</td>
<td style="text-align: left;">2023-07-28 10:32:29</td>
<td style="text-align: left;">2023-07-28 10:32:29</td>
</tr>
<tr class="odd">
<td style="text-align: left;">HONOR 30</td>
<td style="text-align: left;">2023-07-28 09:41:01</td>
<td style="text-align: left;">2023-07-28 10:00:55</td>
</tr>
<tr class="even">
<td style="text-align: left;">IKB</td>
<td style="text-align: left;">2023-07-28 09:28:54</td>
<td style="text-align: left;">2023-07-28 10:34:05</td>
</tr>
<tr class="odd">
<td style="text-align: left;">IT2 Wireless</td>
<td style="text-align: left;">2023-07-28 09:13:03</td>
<td style="text-align: left;">2023-07-28 11:52:57</td>
</tr>
<tr class="even">
<td style="text-align: left;">Iceberg Village</td>
<td style="text-align: left;">2023-07-28 10:26:01</td>
<td style="text-align: left;">2023-07-28 10:26:01</td>
</tr>
<tr class="odd">
<td style="text-align: left;">KB-12</td>
<td style="text-align: left;">2023-07-28 10:08:05</td>
<td style="text-align: left;">2023-07-28 11:48:13</td>
</tr>
<tr class="even">
<td style="text-align: left;">KB-13</td>
<td style="text-align: left;">2023-07-28 10:15:49</td>
<td style="text-align: left;">2023-07-28 11:55:44</td>
</tr>
<tr class="odd">
<td style="text-align: left;">KC</td>
<td style="text-align: left;">2023-07-28 09:13:14</td>
<td style="text-align: left;">2023-07-28 11:51:50</td>
</tr>
<tr class="even">
<td style="text-align: left;">KOTIKI_XXX</td>
<td style="text-align: left;">2023-07-28 09:13:08</td>
<td style="text-align: left;">2023-07-28 11:53:36</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Keenetic-8899</td>
<td style="text-align: left;">2023-07-28 09:29:43</td>
<td style="text-align: left;">2023-07-28 10:27:57</td>
</tr>
<tr class="even">
<td style="text-align: left;">Kesha</td>
<td style="text-align: left;">2023-07-28 10:21:39</td>
<td style="text-align: left;">2023-07-28 10:22:00</td>
</tr>
<tr class="odd">
<td style="text-align: left;">M26</td>
<td style="text-align: left;">2023-07-28 10:03:19</td>
<td style="text-align: left;">2023-07-28 10:16:25</td>
</tr>
<tr class="even">
<td style="text-align: left;">MGTC_GRON5_1333</td>
<td style="text-align: left;">2023-07-28 11:04:30</td>
<td style="text-align: left;">2023-07-28 11:04:30</td>
</tr>
<tr class="odd">
<td style="text-align: left;">MGTS_GPON5_06B2</td>
<td style="text-align: left;">2023-07-28 10:18:06</td>
<td style="text-align: left;">2023-07-28 10:18:06</td>
</tr>
<tr class="even">
<td style="text-align: left;">MGTS_GPON5_C59A</td>
<td style="text-align: left;">2023-07-28 10:26:05</td>
<td style="text-align: left;">2023-07-28 11:45:31</td>
</tr>
<tr class="odd">
<td style="text-align: left;">MGTS_makmak</td>
<td style="text-align: left;">2023-07-28 10:17:35</td>
<td style="text-align: left;">2023-07-28 10:17:35</td>
</tr>
<tr class="even">
<td style="text-align: left;">MIREA_GUESTS</td>
<td style="text-align: left;">2023-07-28 09:15:26</td>
<td style="text-align: left;">2023-07-28 11:52:28</td>
</tr>
<tr class="odd">
<td style="text-align: left;">MIREA_HOTSPOT</td>
<td style="text-align: left;">2023-07-28 09:13:17</td>
<td style="text-align: left;">2023-07-28 11:56:21</td>
</tr>
<tr class="even">
<td style="text-align: left;">MTS_1676884</td>
<td style="text-align: left;">2023-07-28 10:24:39</td>
<td style="text-align: left;">2023-07-28 10:24:49</td>
</tr>
<tr class="odd">
<td style="text-align: left;">MTS_GPON5_ac0968</td>
<td style="text-align: left;">2023-07-28 09:42:00</td>
<td style="text-align: left;">2023-07-28 10:59:08</td>
</tr>
<tr class="even">
<td style="text-align: left;">MTS_GPON_8cedb0</td>
<td style="text-align: left;">2023-07-28 10:24:59</td>
<td style="text-align: left;">2023-07-28 10:25:04</td>
</tr>
<tr class="odd">
<td style="text-align: left;">MT_FREE</td>
<td style="text-align: left;">2023-07-28 09:20:37</td>
<td style="text-align: left;">2023-07-28 11:55:55</td>
</tr>
<tr class="even">
<td style="text-align: left;">Nizhegorodets</td>
<td style="text-align: left;">2023-07-28 09:15:54</td>
<td style="text-align: left;">2023-07-28 11:24:29</td>
</tr>
<tr class="odd">
<td style="text-align: left;">NoW</td>
<td style="text-align: left;">2023-07-28 10:35:07</td>
<td style="text-align: left;">2023-07-28 10:35:07</td>
</tr>
<tr class="even">
<td style="text-align: left;">OKB</td>
<td style="text-align: left;">2023-07-28 09:40:41</td>
<td style="text-align: left;">2023-07-28 11:55:41</td>
</tr>
<tr class="odd">
<td style="text-align: left;">OnePlus 6T</td>
<td style="text-align: left;">2023-07-28 09:17:17</td>
<td style="text-align: left;">2023-07-28 10:14:37</td>
</tr>
<tr class="even">
<td style="text-align: left;">Optus_B818_87A5</td>
<td style="text-align: left;">2023-07-28 09:35:04</td>
<td style="text-align: left;">2023-07-28 10:35:58</td>
</tr>
<tr class="odd">
<td style="text-align: left;">POCO X5 Pro 5G</td>
<td style="text-align: left;">2023-07-28 09:13:19</td>
<td style="text-align: left;">2023-07-28 11:51:24</td>
</tr>
<tr class="even">
<td style="text-align: left;">RT-WiFi-8A6C</td>
<td style="text-align: left;">2023-07-28 09:24:47</td>
<td style="text-align: left;">2023-07-28 10:27:04</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Ramnet_A56263</td>
<td style="text-align: left;">2023-07-28 11:19:24</td>
<td style="text-align: left;">2023-07-28 11:55:04</td>
</tr>
<tr class="even">
<td style="text-align: left;">Rayskaya_banya</td>
<td style="text-align: left;">2023-07-28 10:39:23</td>
<td style="text-align: left;">2023-07-28 11:41:08</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Reconn-Guest</td>
<td style="text-align: left;">2023-07-28 10:37:00</td>
<td style="text-align: left;">2023-07-28 10:37:15</td>
</tr>
<tr class="even">
<td style="text-align: left;">Redmi</td>
<td style="text-align: left;">2023-07-28 09:16:59</td>
<td style="text-align: left;">2023-07-28 10:21:15</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Redmi 12</td>
<td style="text-align: left;">2023-07-28 10:45:04</td>
<td style="text-align: left;">2023-07-28 11:43:26</td>
</tr>
<tr class="even">
<td style="text-align: left;">Redmi Note 8 Pro</td>
<td style="text-align: left;">2023-07-28 09:19:44</td>
<td style="text-align: left;">2023-07-28 11:53:03</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Redmi Note 9S</td>
<td style="text-align: left;">2023-07-28 09:13:51</td>
<td style="text-align: left;">2023-07-28 11:33:50</td>
</tr>
<tr class="even">
<td style="text-align: left;">SERVICE.live</td>
<td style="text-align: left;">2023-07-28 10:31:12</td>
<td style="text-align: left;">2023-07-28 11:55:34</td>
</tr>
<tr class="odd">
<td style="text-align: left;">SSU_BR5</td>
<td style="text-align: left;">2023-07-28 10:49:52</td>
<td style="text-align: left;">2023-07-28 11:16:27</td>
</tr>
<tr class="even">
<td style="text-align: left;">Sber-Guest</td>
<td style="text-align: left;">2023-07-28 10:01:05</td>
<td style="text-align: left;">2023-07-28 10:01:06</td>
</tr>
<tr class="odd">
<td style="text-align: left;">SevenSky2.4G</td>
<td style="text-align: left;">2023-07-28 10:51:08</td>
<td style="text-align: left;">2023-07-28 10:51:08</td>
</tr>
<tr class="even">
<td style="text-align: left;">Snickers_ASSA</td>
<td style="text-align: left;">2023-07-28 11:22:27</td>
<td style="text-align: left;">2023-07-28 11:22:27</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Spartak</td>
<td style="text-align: left;">2023-07-28 10:28:55</td>
<td style="text-align: left;">2023-07-28 10:30:05</td>
</tr>
<tr class="even">
<td style="text-align: left;">Tatiana</td>
<td style="text-align: left;">2023-07-28 10:22:35</td>
<td style="text-align: left;">2023-07-28 11:45:19</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Timo Resort</td>
<td style="text-align: left;">2023-07-28 10:19:20</td>
<td style="text-align: left;">2023-07-28 11:00:17</td>
</tr>
<tr class="even">
<td style="text-align: left;">Tupik</td>
<td style="text-align: left;">2023-07-28 10:01:05</td>
<td style="text-align: left;">2023-07-28 10:01:06</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Vladimir</td>
<td style="text-align: left;">2023-07-28 09:31:57</td>
<td style="text-align: left;">2023-07-28 11:54:07</td>
</tr>
<tr class="even">
<td style="text-align: left;">Wi-Fi</td>
<td style="text-align: left;">2023-07-28 11:05:11</td>
<td style="text-align: left;">2023-07-28 11:05:12</td>
</tr>
<tr class="odd">
<td style="text-align: left;">WiFi2</td>
<td style="text-align: left;">2023-07-28 11:05:16</td>
<td style="text-align: left;">2023-07-28 11:50:28</td>
</tr>
<tr class="even">
<td style="text-align: left;"></td>
<td style="text-align: left;">2023-07-28 09:16:55</td>
<td style="text-align: left;">2023-07-28 11:54:02</td>
</tr>
<tr class="odd">
<td style="text-align: left;">cpe-26C1C3</td>
<td style="text-align: left;">2023-07-28 10:33:38</td>
<td style="text-align: left;">2023-07-28 10:33:39</td>
</tr>
<tr class="even">
<td style="text-align: left;">edeeeèe</td>
<td style="text-align: left;">2023-07-28 10:38:27</td>
<td style="text-align: left;">2023-07-28 11:44:17</td>
</tr>
<tr class="odd">
<td style="text-align: left;">helvetia-free</td>
<td style="text-align: left;">2023-07-28 10:43:22</td>
<td style="text-align: left;">2023-07-28 10:43:22</td>
</tr>
<tr class="even">
<td style="text-align: left;">home 466</td>
<td style="text-align: left;">2023-07-28 10:25:47</td>
<td style="text-align: left;">2023-07-28 10:26:20</td>
</tr>
<tr class="odd">
<td style="text-align: left;">iPhone (2)</td>
<td style="text-align: left;">2023-07-28 09:51:31</td>
<td style="text-align: left;">2023-07-28 09:51:32</td>
</tr>
<tr class="even">
<td style="text-align: left;">iPhone (Дима )</td>
<td style="text-align: left;">2023-07-28 09:51:00</td>
<td style="text-align: left;">2023-07-28 10:00:55</td>
</tr>
<tr class="odd">
<td style="text-align: left;">iPhone (Искандер)</td>
<td style="text-align: left;">2023-07-28 09:21:02</td>
<td style="text-align: left;">2023-07-28 11:43:18</td>
</tr>
<tr class="even">
<td style="text-align: left;">iPhone (Максим)</td>
<td style="text-align: left;">2023-07-28 09:50:51</td>
<td style="text-align: left;">2023-07-28 10:00:55</td>
</tr>
<tr class="odd">
<td style="text-align: left;">iPhone (тамара)</td>
<td style="text-align: left;">2023-07-28 10:28:34</td>
<td style="text-align: left;">2023-07-28 10:28:34</td>
</tr>
<tr class="even">
<td style="text-align: left;">it</td>
<td style="text-align: left;">2023-07-28 10:39:50</td>
<td style="text-align: left;">2023-07-28 11:55:58</td>
</tr>
<tr class="odd">
<td style="text-align: left;">linksys</td>
<td style="text-align: left;">2023-07-28 10:44:29</td>
<td style="text-align: left;">2023-07-28 11:49:05</td>
</tr>
<tr class="even">
<td style="text-align: left;">lounge</td>
<td style="text-align: left;">2023-07-28 10:30:17</td>
<td style="text-align: left;">2023-07-28 10:30:17</td>
</tr>
<tr class="odd">
<td style="text-align: left;">mirea</td>
<td style="text-align: left;">2023-07-28 10:15:26</td>
<td style="text-align: left;">2023-07-28 10:15:43</td>
</tr>
<tr class="even">
<td style="text-align: left;">nvripcsuite</td>
<td style="text-align: left;">2023-07-28 09:13:11</td>
<td style="text-align: left;">2023-07-28 11:56:17</td>
</tr>
<tr class="odd">
<td style="text-align: left;">podval</td>
<td style="text-align: left;">2023-07-28 09:13:16</td>
<td style="text-align: left;">2023-07-28 11:49:29</td>
</tr>
<tr class="even">
<td style="text-align: left;">realme 10</td>
<td style="text-align: left;">2023-07-28 09:29:51</td>
<td style="text-align: left;">2023-07-28 10:25:55</td>
</tr>
<tr class="odd">
<td style="text-align: left;">rediska</td>
<td style="text-align: left;">2023-07-28 10:21:35</td>
<td style="text-align: left;">2023-07-28 10:21:35</td>
</tr>
<tr class="even">
<td style="text-align: left;">region</td>
<td style="text-align: left;">2023-07-28 09:45:44</td>
<td style="text-align: left;">2023-07-28 10:18:07</td>
</tr>
<tr class="odd">
<td style="text-align: left;">support22</td>
<td style="text-align: left;">2023-07-28 10:28:24</td>
<td style="text-align: left;">2023-07-28 10:28:24</td>
</tr>
<tr class="even">
<td style="text-align: left;">vestis.local</td>
<td style="text-align: left;">2023-07-28 10:22:14</td>
<td style="text-align: left;">2023-07-28 11:55:25</td>
</tr>
<tr class="odd">
<td style="text-align: left;">xt3 w64dtgv5cfrxhttps://vk.com/v</td>
<td style="text-align: left;">2023-07-28 09:54:40</td>
<td style="text-align: left;">2023-07-28 11:55:36</td>
</tr>
<tr class="even">
<td style="text-align: left;">Максимка</td>
<td style="text-align: left;">2023-07-28 11:53:29</td>
<td style="text-align: left;">2023-07-28 11:53:29</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Шк</td>
<td style="text-align: left;">2023-07-28 11:00:54</td>
<td style="text-align: left;">2023-07-28 11:47:43</td>
</tr>
<tr class="even">
<td style="text-align: left;">NA</td>
<td style="text-align: left;">2023-07-28 09:13:04</td>
<td style="text-align: left;">2023-07-28 11:56:24</td>
</tr>
</tbody>
</table>

1.  Оценить стабильность уровня сигнала внури кластера во времени.
    Выявить наиболее стабильный кластер.

``` r
signal_stability <- dataset2 %>% group_by(Probed.ESSIDs) %>% summarise(sd_signal = sd(Power, na.rm = TRUE)) %>% arrange(sd_signal)
```

    Warning: There was 1 warning in `summarise()`.
    ℹ In argument: `sd_signal = sd(Power, na.rm = TRUE)`.
    ℹ In group 108: `Probed.ESSIDs = NA`.
    Caused by warning in `var()`:
    ! NAs introduced by coercion

``` r
signal_stability %>% knitr::kable()
```

<table>
<thead>
<tr class="header">
<th style="text-align: left;">Probed.ESSIDs</th>
<th style="text-align: right;">sd_signal</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Galaxy A71</td>
<td style="text-align: right;">0.7071068</td>
</tr>
<tr class="even">
<td style="text-align: left;">Kesha</td>
<td style="text-align: right;">1.4142136</td>
</tr>
<tr class="odd">
<td style="text-align: left;">podval</td>
<td style="text-align: right;">1.6329932</td>
</tr>
<tr class="even">
<td style="text-align: left;">Tupik</td>
<td style="text-align: right;">2.3094011</td>
</tr>
<tr class="odd">
<td style="text-align: left;">KB-12</td>
<td style="text-align: right;">2.8284271</td>
</tr>
<tr class="even">
<td style="text-align: left;">Reconn-Guest</td>
<td style="text-align: right;">2.8284271</td>
</tr>
<tr class="odd">
<td style="text-align: left;">MT_FREE</td>
<td style="text-align: right;">2.9664794</td>
</tr>
<tr class="even">
<td style="text-align: left;">IKB</td>
<td style="text-align: right;">3.0550505</td>
</tr>
<tr class="odd">
<td style="text-align: left;">OKB</td>
<td style="text-align: right;">3.0550505</td>
</tr>
<tr class="even">
<td style="text-align: left;">Rayskaya_banya</td>
<td style="text-align: right;">3.0550505</td>
</tr>
<tr class="odd">
<td style="text-align: left;">vestis.local</td>
<td style="text-align: right;">3.4156503</td>
</tr>
<tr class="even">
<td style="text-align: left;">AAAAAOB/CC0ADwGkRedmi 3S</td>
<td style="text-align: right;">4.0000000</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Vladimir</td>
<td style="text-align: right;">4.1231056</td>
</tr>
<tr class="even">
<td style="text-align: left;">Alex-net2</td>
<td style="text-align: right;">4.2426407</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Beeline_5G_F2F425</td>
<td style="text-align: right;">4.2426407</td>
</tr>
<tr class="even">
<td style="text-align: left;">Sber-Guest</td>
<td style="text-align: right;">4.2426407</td>
</tr>
<tr class="odd">
<td style="text-align: left;">1</td>
<td style="text-align: right;">4.3065342</td>
</tr>
<tr class="even">
<td style="text-align: left;">-D-13-</td>
<td style="text-align: right;">5.0066622</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Galaxy A31CAED</td>
<td style="text-align: right;">5.0103608</td>
</tr>
<tr class="even">
<td style="text-align: left;">Spartak</td>
<td style="text-align: right;">5.0332230</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Avenue611</td>
<td style="text-align: right;">5.0413005</td>
</tr>
<tr class="even">
<td style="text-align: left;">GIVC</td>
<td style="text-align: right;">5.0839173</td>
</tr>
<tr class="odd">
<td style="text-align: left;">edeeeèe</td>
<td style="text-align: right;">5.2190129</td>
</tr>
<tr class="even">
<td style="text-align: left;">Шк</td>
<td style="text-align: right;">5.2915026</td>
</tr>
<tr class="odd">
<td style="text-align: left;">iPhone (Максим)</td>
<td style="text-align: right;">5.3397848</td>
</tr>
<tr class="even">
<td style="text-align: left;">iPhone (Дима )</td>
<td style="text-align: right;">5.3816584</td>
</tr>
<tr class="odd">
<td style="text-align: left;">region</td>
<td style="text-align: right;">5.4078385</td>
</tr>
<tr class="even">
<td style="text-align: left;">AndroidShare_1901</td>
<td style="text-align: right;">5.4986629</td>
</tr>
<tr class="odd">
<td style="text-align: left;">iPhone (2)</td>
<td style="text-align: right;">5.6568542</td>
</tr>
<tr class="even">
<td style="text-align: left;">IT2 Wireless</td>
<td style="text-align: right;">5.7294528</td>
</tr>
<tr class="odd">
<td style="text-align: left;">HONOR 30</td>
<td style="text-align: right;">5.7608964</td>
</tr>
<tr class="even">
<td style="text-align: left;">Gardenpasanauri</td>
<td style="text-align: right;">5.8344828</td>
</tr>
<tr class="odd">
<td style="text-align: left;">M26</td>
<td style="text-align: right;">6.0000000</td>
</tr>
<tr class="even">
<td style="text-align: left;">MGTS_GPON5_C59A</td>
<td style="text-align: right;">6.1101009</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Keenetic-8899</td>
<td style="text-align: right;">7.5689424</td>
</tr>
<tr class="even">
<td style="text-align: left;">mirea</td>
<td style="text-align: right;">7.5718778</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Beeline121</td>
<td style="text-align: right;">7.7974355</td>
</tr>
<tr class="even">
<td style="text-align: left;">MIREA_GUESTS</td>
<td style="text-align: right;">8.2318732</td>
</tr>
<tr class="odd">
<td style="text-align: left;">C322U06 5179</td>
<td style="text-align: right;">8.4852814</td>
</tr>
<tr class="even">
<td style="text-align: left;">Redmi Note 8 Pro</td>
<td style="text-align: right;">8.4852814</td>
</tr>
<tr class="odd">
<td style="text-align: left;">home 466</td>
<td style="text-align: right;">8.4852814</td>
</tr>
<tr class="even">
<td style="text-align: left;">SSU_BR5</td>
<td style="text-align: right;">8.7177979</td>
</tr>
<tr class="odd">
<td style="text-align: left;">it</td>
<td style="text-align: right;">9.0000000</td>
</tr>
<tr class="even">
<td style="text-align: left;">AndroidShare_1576</td>
<td style="text-align: right;">9.1225671</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Timo Resort</td>
<td style="text-align: right;">9.3897107</td>
</tr>
<tr class="even">
<td style="text-align: left;">NA</td>
<td style="text-align: right;">9.7125092</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Optus_B818_87A5</td>
<td style="text-align: right;">9.8994949</td>
</tr>
<tr class="even">
<td style="text-align: left;">MTS_GPON5_ac0968</td>
<td style="text-align: right;">10.1324561</td>
</tr>
<tr class="odd">
<td style="text-align: left;">MIREA_HOTSPOT</td>
<td style="text-align: right;">10.3365586</td>
</tr>
<tr class="even">
<td style="text-align: left;">RT-WiFi-8A6C</td>
<td style="text-align: right;">10.9348721</td>
</tr>
<tr class="odd">
<td style="text-align: left;">KOTIKI_XXX</td>
<td style="text-align: right;">11.0151411</td>
</tr>
<tr class="even">
<td style="text-align: left;">KB-13</td>
<td style="text-align: right;">11.1892806</td>
</tr>
<tr class="odd">
<td style="text-align: left;">nvripcsuite</td>
<td style="text-align: right;">18.1475435</td>
</tr>
<tr class="even">
<td style="text-align: left;">C322U13 3965</td>
<td style="text-align: right;">19.7989899</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Cnet</td>
<td style="text-align: right;">25.0333111</td>
</tr>
<tr class="even">
<td style="text-align: left;">107</td>
<td style="text-align: right;">NA</td>
</tr>
<tr class="odd">
<td style="text-align: left;">531</td>
<td style="text-align: right;">NA</td>
</tr>
<tr class="even">
<td style="text-align: left;">AKADO-D967</td>
<td style="text-align: right;">NA</td>
</tr>
<tr class="odd">
<td style="text-align: left;">AQAAAB6zaIoATwEURedmi Note 5</td>
<td style="text-align: right;">NA</td>
</tr>
<tr class="even">
<td style="text-align: left;">ASUS</td>
<td style="text-align: right;">NA</td>
</tr>
<tr class="odd">
<td style="text-align: left;">AndroidAP177B</td>
<td style="text-align: right;">NA</td>
</tr>
<tr class="even">
<td style="text-align: left;">AndroidShare_2061</td>
<td style="text-align: right;">NA</td>
</tr>
<tr class="odd">
<td style="text-align: left;">AndroidShare_2280</td>
<td style="text-align: right;">NA</td>
</tr>
<tr class="even">
<td style="text-align: left;">AndroidShare_3542</td>
<td style="text-align: right;">NA</td>
</tr>
<tr class="odd">
<td style="text-align: left;">AndroidShare_8927</td>
<td style="text-align: right;">NA</td>
</tr>
<tr class="even">
<td style="text-align: left;">Antonchik</td>
<td style="text-align: right;">NA</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Belorusneft Free</td>
<td style="text-align: right;">NA</td>
</tr>
<tr class="even">
<td style="text-align: left;">C322U21 0566</td>
<td style="text-align: right;">NA</td>
</tr>
<tr class="odd">
<td style="text-align: left;">C349U26 3598</td>
<td style="text-align: right;">NA</td>
</tr>
<tr class="even">
<td style="text-align: left;">CPPK_FREE</td>
<td style="text-align: right;">NA</td>
</tr>
<tr class="odd">
<td style="text-align: left;">DIRECT-03-BMW44597</td>
<td style="text-align: right;">NA</td>
</tr>
<tr class="even">
<td style="text-align: left;">Damy</td>
<td style="text-align: right;">NA</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Dex_Pro</td>
<td style="text-align: right;">NA</td>
</tr>
<tr class="even">
<td style="text-align: left;">GGWPRedmi Note 10S</td>
<td style="text-align: right;">NA</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Guest Concept</td>
<td style="text-align: right;">NA</td>
</tr>
<tr class="even">
<td style="text-align: left;">Iceberg Village</td>
<td style="text-align: right;">NA</td>
</tr>
<tr class="odd">
<td style="text-align: left;">KC</td>
<td style="text-align: right;">NA</td>
</tr>
<tr class="even">
<td style="text-align: left;">MGTC_GRON5_1333</td>
<td style="text-align: right;">NA</td>
</tr>
<tr class="odd">
<td style="text-align: left;">MGTS_GPON5_06B2</td>
<td style="text-align: right;">NA</td>
</tr>
<tr class="even">
<td style="text-align: left;">MGTS_makmak</td>
<td style="text-align: right;">NA</td>
</tr>
<tr class="odd">
<td style="text-align: left;">MTS_1676884</td>
<td style="text-align: right;">NA</td>
</tr>
<tr class="even">
<td style="text-align: left;">MTS_GPON_8cedb0</td>
<td style="text-align: right;">NA</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Nizhegorodets</td>
<td style="text-align: right;">NA</td>
</tr>
<tr class="even">
<td style="text-align: left;">NoW</td>
<td style="text-align: right;">NA</td>
</tr>
<tr class="odd">
<td style="text-align: left;">OnePlus 6T</td>
<td style="text-align: right;">NA</td>
</tr>
<tr class="even">
<td style="text-align: left;">POCO X5 Pro 5G</td>
<td style="text-align: right;">NA</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Ramnet_A56263</td>
<td style="text-align: right;">NA</td>
</tr>
<tr class="even">
<td style="text-align: left;">Redmi</td>
<td style="text-align: right;">NA</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Redmi 12</td>
<td style="text-align: right;">NA</td>
</tr>
<tr class="even">
<td style="text-align: left;">Redmi Note 9S</td>
<td style="text-align: right;">NA</td>
</tr>
<tr class="odd">
<td style="text-align: left;">SERVICE.live</td>
<td style="text-align: right;">NA</td>
</tr>
<tr class="even">
<td style="text-align: left;">SevenSky2.4G</td>
<td style="text-align: right;">NA</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Snickers_ASSA</td>
<td style="text-align: right;">NA</td>
</tr>
<tr class="even">
<td style="text-align: left;">Tatiana</td>
<td style="text-align: right;">NA</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Wi-Fi</td>
<td style="text-align: right;">NA</td>
</tr>
<tr class="even">
<td style="text-align: left;">WiFi2</td>
<td style="text-align: right;">NA</td>
</tr>
<tr class="odd">
<td style="text-align: left;"></td>
<td style="text-align: right;">NA</td>
</tr>
<tr class="even">
<td style="text-align: left;">cpe-26C1C3</td>
<td style="text-align: right;">NA</td>
</tr>
<tr class="odd">
<td style="text-align: left;">helvetia-free</td>
<td style="text-align: right;">NA</td>
</tr>
<tr class="even">
<td style="text-align: left;">iPhone (Искандер)</td>
<td style="text-align: right;">NA</td>
</tr>
<tr class="odd">
<td style="text-align: left;">iPhone (тамара)</td>
<td style="text-align: right;">NA</td>
</tr>
<tr class="even">
<td style="text-align: left;">linksys</td>
<td style="text-align: right;">NA</td>
</tr>
<tr class="odd">
<td style="text-align: left;">lounge</td>
<td style="text-align: right;">NA</td>
</tr>
<tr class="even">
<td style="text-align: left;">realme 10</td>
<td style="text-align: right;">NA</td>
</tr>
<tr class="odd">
<td style="text-align: left;">rediska</td>
<td style="text-align: right;">NA</td>
</tr>
<tr class="even">
<td style="text-align: left;">support22</td>
<td style="text-align: right;">NA</td>
</tr>
<tr class="odd">
<td style="text-align: left;">xt3 w64dtgv5cfrxhttps://vk.com/v</td>
<td style="text-align: right;">NA</td>
</tr>
<tr class="even">
<td style="text-align: left;">Максимка</td>
<td style="text-align: right;">NA</td>
</tr>
</tbody>
</table>

## Оценка результата

В результате работы были импортированы данные из csv-файла и сделаны
задания.

## Вывод

Были проанализированы датасеты с использованием tidyverse. Были улучшены
навыки пользования библиотеками dplyr, read.
