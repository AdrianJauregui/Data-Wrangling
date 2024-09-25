dw-2024-parcial-1
================
Tepi
25/09/2024

# Examen parcial

Indicaciones generales:

- Usted tiene el período de la clase para resolver el examen parcial.

- La entrega del parcial, al igual que las tareas, es por medio de su
  cuenta de github, pegando el link en el portal de MiU.

- Pueden hacer uso del material del curso e internet (stackoverflow,
  etc.). Sin embargo, si encontramos algún indicio de copia, se anulará
  el exámen para los estudiantes involucrados. Por lo tanto, aconsejamos
  no compartir las agregaciones que generen.

## Sección 0: Preguntas de temas vistos en clase (20pts)

- Responda las siguientes preguntas de temas que fueron tocados en
  clase.

1.  ¿Qué es una ufunc y por qué debemos de utilizarlas cuando
    programamos trabajando datos? Es una función universal la cual opera
    de manera vectorizada, es decir que trabaja con cada uno de los
    elementos del array al hacer una operación, al trabajar en R se
    deben utilizar este tipo de funciones ya que este utiliza un
    lenguaje vectorizado y es más facíl manjear grandes cantidades de
    datos

2.  Es una técnica en programación numérica que amplía los objetos que
    son de menor dimensión para que sean compatibles con los de mayor
    dimensión. Describa cuál es la técnica y de un breve ejemplo en R.

Broadcasting es un metodo el cual permite expandir un array de menor
dimensión para que sea compatible con uno de mayor dimension. Por
ejemplo al trabajar en R con un vector de 4 elementos y una maztrix de
4x4, R amplia el vector para que coincidan las dimensiones y se pueda
operar una suma, resta, multiplicación, etc.

3.  ¿Qué es el axioma de elegibilidad y por qué es útil al momento de
    hacer análisis de datos?

Es un axioma el cual establece que cualquiera de las opciones a elegir
deben de ser coherentes con los fines a alcanzar. Por ejemplo al
analizar datos es importante elegir variables las cuales si sean
relevantes y esten conectadas al fin del analisis, en vez de tener
muchas variables las cuales dificultan el analisis y no brindan
informaicón importante.

4.  Cuál es la relación entre la granularidad y la agregación de datos?
    Mencione un breve ejemplo. Luego, exploque cuál es la granularidad o
    agregación requerida para poder generar un reporte como el
    siguiente:

| Pais | Usuarios |
|------|----------|
| US   | 1,934    |
| UK   | 2,133    |
| DE   | 1,234    |
| FR   | 4,332    |
| ROW  | 943      |

La relación está al en que la granularidad define el nivel de detalle de
los datos y la agregación es el juntar los datos detallados para brindar
una imgen o punto de vista más general sobre ellos y extraer información
importante para tomar un decisión.

La granularidad original de los datos probablemente es del nombre de un
usuario, su ID y su país de residencia, y para generar esta tabla se
debio reducir la granularidad agrupar a todos los usuarios segun el país
en el que residen y contar cuantos usuarios hubieron por país.

## Sección I: Preguntas teóricas. (50pts)

- Existen 10 preguntas directas en este Rmarkdown, de las cuales usted
  deberá responder 5. Las 5 a responder estarán determinadas por un
  muestreo aleatorio basado en su número de carné.

- Ingrese su número de carné en `set.seed()` y corra el chunk de R para
  determinar cuáles preguntas debe responder.

``` r
set.seed("20220295") 
v<- 1:10
preguntas <-sort(sample(v, size = 5, replace = FALSE ))

paste0("Mis preguntas a resolver son: ",paste0(preguntas,collapse = ", "))
```

    ## [1] "Mis preguntas a resolver son: 2, 3, 7, 8, 10"

\#Preguntas a resolver carnet 20220295 son: 2,3,7,8,10

### Listado de preguntas teóricas

2.  Al momento de filtrar en SQL, ¿cuál keyword cumple las mismas
    funciones que el keyword `OR` para filtrar uno o más elementos una
    misma columna?

La Keyword que puede cumplir la misma función sería “IN” al usar esta
palabra damos unas condiciónes en la cual los datos deben de cumplir al
menos una para ser incluidos, al igual que “OR” pero esta función es más
practica cuando se utilizan varias condiciones ya que solo se escribe
“IN” una sola vez en vez de “OR” por cada nueva condición puesta.

3.  ¿Por qué en R utilizamos funciones de la familia apply
    (lapply,vapply) en lugar de utilizar ciclos?

Apply es una función que se utiliza en R en vez del bucle cotidiano en
otros lenguajes de programación. Al utilizar las funciones de apply se
utilizan porque son mucho más eficientes para trabajar con vectores y R
al utilizar un lenguaje vectorizado hace los procesos más eficientes y a
mejor rendimiento, con codigos más simples y faciles de leer.

7.  ¿Qué pasa si quiero agregar una nueva categoría a un factor que no
    se encuentra en los niveles existentes? Si quieres hacer eso en R
    entonces el valor que devolvería R sería un NA, como el valor no
    esta incluido en las categorías originales no se reconoce como una
    parte del factor.

8.  Si en un dataframe, a una variable de tipo `factor` le agrego un
    nuevo elemento que *no se encuentra en los niveles existentes*,
    ¿cuál sería el resultado esperado y por qué?

    - El nuevo elemento
    - `NA` En ese caso el valor que devolvería sería un “NA”, ya que
      para que un factor reconozca un nuevo elemento este debe de
      pertenecer a los niveles existentes del factor ya definido.

9.  Si quiero obtener como resultado las filas de la tabla A que no se
    encuentran en la tabla B, ¿cómo debería de completar la siguiente
    sentencia de SQL?

    - SELECT \* FROM A *LEFT JOIN* B ON A.KEY = B.KEY WHERE *B.KEY* =
      *NULL*

Extra: ¿Cuántos posibles exámenes de 5 preguntas se pueden realizar
utilizando como banco las diez acá presentadas? (responder con código de
R.)

## Sección II Preguntas prácticas. (30pts)

- Conteste las siguientes preguntas utilizando sus conocimientos de R.
  Adjunte el código que utilizó para llegar a sus conclusiones en un
  chunk del markdown.

A. De los clientes que están en más de un país,¿cuál cree que es el más
rentable y por qué?

B. Estrategia de negocio ha decidido que ya no operará en aquellos
territorios cuyas pérdidas sean “considerables”. Bajo su criterio,
¿cuáles son estos territorios y por qué ya no debemos operar ahí?

### I. Preguntas practicas

## A

``` r
###resuelva acá

dfA <- readRDS("parcial_anonimo.rds")
library(dplyr)
```

    ## 
    ## Adjuntando el paquete: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
clientes_paises <- dfA %>% 
  group_by(Cliente,Pais,`Canal Venta`) %>%
  summarize(TotalVenta = sum(Venta)) %>%
  ungroup()
```

    ## `summarise()` has grouped output by 'Cliente', 'Pais'. You can override using
    ## the `.groups` argument.

``` r
clientes_variosp <- clientes_paises %>%
  group_by(Cliente) %>%
  summarize(
    PaisesDistintos = n_distinct(Pais),
    Rentabilidad = sum(TotalVenta),
    Canales = toString(unique(`Canal Venta`))  
  ) %>%
  filter(PaisesDistintos > 1)


head(clientes_variosp)
```

    ## # A tibble: 6 × 4
    ##   Cliente  PaisesDistintos Rentabilidad Canales                     
    ##   <chr>              <int>        <dbl> <chr>                       
    ## 1 044118d4               2        9436. 7b48292e                    
    ## 2 a17a7558               2       19818. 7b48292e                    
    ## 3 bf1e94e9               2           0  7b48292e, 99ec8140, dca4ccd7
    ## 4 c53868a0               2       13813. 7b48292e                    
    ## 5 f2aab44e               2         400. 7b48292e                    
    ## 6 f676043b               2        3635. 7b48292e

``` r
## Se podria analizar la rentabilidad de cada cliente segun el canal que utilizan para sus ventas 
```

## B

``` r
###resuelva acá

dfB <- readRDS("parcial_anonimo.rds")
library(dplyr)

ventas_por_territorio <- dfA %>%
  group_by(Territorio,Marca,`Canal Venta`) %>%
  summarize(TotalVenta = sum(Venta)) %>%
  ungroup()
```

    ## `summarise()` has grouped output by 'Territorio', 'Marca'. You can override
    ## using the `.groups` argument.

``` r
umbral_bajo <- 50

territorios_perdida <- ventas_por_territorio %>%
  filter(TotalVenta < 0 | TotalVenta < umbral_bajo) %>%
  arrange(TotalVenta)

head(territorios_perdida)
```

    ## # A tibble: 6 × 4
    ##   Territorio Marca    `Canal Venta` TotalVenta
    ##   <chr>      <chr>    <chr>              <dbl>
    ## 1 3153c73e   c1a7fe7f b105050f          -155. 
    ## 2 c31adb2f   c1a7fe7f 7b48292e          -110. 
    ## 3 67696f68   f1a48f8b 7b48292e          -109. 
    ## 4 a9e783db   f1a48f8b 7b48292e           -62.3
    ## 5 9fdcc550   f1a48f8b 7b48292e           -39.0
    ## 6 0320288f   290f7be8 7b48292e           -33.6

``` r
#Podemos analizar en que terriotiro dejar de operar segun el numero de perdida que tiene y que canal utiliza para este 
```
