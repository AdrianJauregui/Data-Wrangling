Lubridate Lab
================
Tepi
10/6/2024

## Laboratorio de la libreria Lubridate

``` r
##cargar librerias
library(nycflights13)
library(lubridate)
library(dplyr)
```

## Resuelva las siguientes preguntas:

### Ejercicio 1: Convertir columnas de hora en fecha-hora

Problema: Convierte las columnas dep_time (hora de salida) y arr_time
(hora de llegada) en objetos de tipo datetime usando make_datetime() de
lubridate. Recuerda que estas columnas están en formato militar (HHMM).

Ayuda: Investiga la funcion matematica de modulo de r.

``` r
flights <- flights %>%
  mutate(
    dep_hour = dep_time %/% 100,     
    dep_min = dep_time %% 100,        
    dep_datetime = make_datetime(year, month, day, dep_hour, dep_min)
  )

flights <- flights %>%
  mutate(
    arr_hour = arr_time %/% 100,      
    arr_min = arr_time %% 100,        
    arr_datetime = make_datetime(year, month, day, arr_hour, arr_min)
  )

select(flights, year, month, day, dep_time, dep_datetime, arr_time, arr_datetime)
```

    ## # A tibble: 336,776 × 7
    ##     year month   day dep_time dep_datetime        arr_time arr_datetime       
    ##    <int> <int> <int>    <int> <dttm>                 <int> <dttm>             
    ##  1  2013     1     1      517 2013-01-01 05:17:00      830 2013-01-01 08:30:00
    ##  2  2013     1     1      533 2013-01-01 05:33:00      850 2013-01-01 08:50:00
    ##  3  2013     1     1      542 2013-01-01 05:42:00      923 2013-01-01 09:23:00
    ##  4  2013     1     1      544 2013-01-01 05:44:00     1004 2013-01-01 10:04:00
    ##  5  2013     1     1      554 2013-01-01 05:54:00      812 2013-01-01 08:12:00
    ##  6  2013     1     1      554 2013-01-01 05:54:00      740 2013-01-01 07:40:00
    ##  7  2013     1     1      555 2013-01-01 05:55:00      913 2013-01-01 09:13:00
    ##  8  2013     1     1      557 2013-01-01 05:57:00      709 2013-01-01 07:09:00
    ##  9  2013     1     1      557 2013-01-01 05:57:00      838 2013-01-01 08:38:00
    ## 10  2013     1     1      558 2013-01-01 05:58:00      753 2013-01-01 07:53:00
    ## # ℹ 336,766 more rows

## Ejercicio 2: Duracion del vuelo

Calcula el tiempo de vuelo total en minutos entre las columnas dep_time
y arr_time que calculaste en el primer Ejercicio.

``` r
flights %>%
  mutate (
    flight_duration = difftime(arr_datetime,dep_datetime, units = "mins")
)%>%
select(flight, dep_datetime, arr_datetime, flight_duration) %>%
head()
```

    ## # A tibble: 6 × 4
    ##   flight dep_datetime        arr_datetime        flight_duration
    ##    <int> <dttm>              <dttm>              <drtn>         
    ## 1   1545 2013-01-01 05:17:00 2013-01-01 08:30:00 193 mins       
    ## 2   1714 2013-01-01 05:33:00 2013-01-01 08:50:00 197 mins       
    ## 3   1141 2013-01-01 05:42:00 2013-01-01 09:23:00 221 mins       
    ## 4    725 2013-01-01 05:44:00 2013-01-01 10:04:00 260 mins       
    ## 5    461 2013-01-01 05:54:00 2013-01-01 08:12:00 138 mins       
    ## 6   1696 2013-01-01 05:54:00 2013-01-01 07:40:00 106 mins

## Ejercicio 3: Extraer componentes de fechas

Extrae el dia de la semana y la hora en que salieron los aviones y
guardalos en las variables `dep_day_of_week` y `dep_hour`.

``` r
flights <- flights %>%
  mutate(
    dep_day_of_week = wday(dep_datetime, label = TRUE, abbr = TRUE),
    dep_hour = hour(dep_datetime) 
  )

flights %>% 
  select(dep_datetime, dep_day_of_week, dep_hour) %>% 
  head()
```

    ## # A tibble: 6 × 3
    ##   dep_datetime        dep_day_of_week dep_hour
    ##   <dttm>              <ord>              <int>
    ## 1 2013-01-01 05:17:00 "mar\\."               5
    ## 2 2013-01-01 05:33:00 "mar\\."               5
    ## 3 2013-01-01 05:42:00 "mar\\."               5
    ## 4 2013-01-01 05:44:00 "mar\\."               5
    ## 5 2013-01-01 05:54:00 "mar\\."               5
    ## 6 2013-01-01 05:54:00 "mar\\."               5

## Ejercicio 4: Crear nuevas columnas con el día de la semana y la semana del año

Problema: Usando la columna `time_hour`, crea una nueva columna que
indique el día de la semana y otra que indique la semana del año en la
que ocurrió el vuelo.

Ayuda: Invesitga la funcion wday de lubridate.

``` r
flights <- flights %>%
  mutate(day_of_week = wday(time_hour,label = TRUE, abb = TRUE),
         week_of_year = week(time_hour))

flights%>%
  select(time_hour,day_of_week,week_of_year)%>%
  head()
```

    ## # A tibble: 6 × 3
    ##   time_hour           day_of_week week_of_year
    ##   <dttm>              <ord>              <dbl>
    ## 1 2013-01-01 05:00:00 "mar\\."               1
    ## 2 2013-01-01 05:00:00 "mar\\."               1
    ## 3 2013-01-01 05:00:00 "mar\\."               1
    ## 4 2013-01-01 05:00:00 "mar\\."               1
    ## 5 2013-01-01 06:00:00 "mar\\."               1
    ## 6 2013-01-01 05:00:00 "mar\\."               1

## Ejercicio 5: Encontrar los vuelos que salieron los fines de semana

Problema: Filtra los vuelos que despegaron un sábado o domingo y
devuelve el total de vuelos en fines de semana.

``` r
filter_data <- flights %>%
  filter (dep_day_of_week %in% c("Sat","Sun")) %>%
  summarise (total_flights_weekend = n())

filter_data
```

    ## # A tibble: 1 × 1
    ##   total_flights_weekend
    ##                   <int>
    ## 1                     0
