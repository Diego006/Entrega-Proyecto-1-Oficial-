Proyecto 1
================

# Alumno:

## Diego Herrera

# Librerias

``` r
library(psych)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(stringr)
library(datasets)
library(tidyverse)
```

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.0 --

    ## v ggplot2 3.3.3     v readr   1.3.1
    ## v tibble  3.1.1     v purrr   0.3.4
    ## v tidyr   1.1.3     v forcats 0.5.0

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x ggplot2::%+%()   masks psych::%+%()
    ## x ggplot2::alpha() masks psych::alpha()
    ## x dplyr::filter()  masks stats::filter()
    ## x dplyr::lag()     masks stats::lag()

``` r
library(tm)
```

    ## Loading required package: NLP

    ## 
    ## Attaching package: 'NLP'

    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     annotate

``` r
library(tidytext)
library(quanteda)
```

    ## Package version: 2.1.2

    ## Parallel computing: 2 of 4 threads used.

    ## See https://quanteda.io for tutorials and examples.

    ## 
    ## Attaching package: 'quanteda'

    ## The following objects are masked from 'package:tm':
    ## 
    ##     as.DocumentTermMatrix, stopwords

    ## The following objects are masked from 'package:NLP':
    ## 
    ##     meta, meta<-

    ## The following object is masked from 'package:utils':
    ## 
    ##     View

``` r
library(ggplot2)
library(wordcloud)
```

    ## Loading required package: RColorBrewer

# Cargando datoss

``` r
#setwd("C:/Users/Dieca/OneDrive/Escritorio/entrega proyecto 1")
setwd("D:/Users/Italo/Documents/Italo Felipe/UAI/Semestre 11/Ayudantia Mineria de Datos/ayudaalumnos")

sanguchez <- read.csv("sanguchez.csv", sep = ";")
```

# Ordenamos los datos

Ocupamos las siguentes funciones para limpiar la base de datos y
posteriormente ordenamos la notas de mayor a menor para trabajar la base
de datos facilmente.

``` r
sanguchez <- sanguchez[,!(colnames(sanguchez) %in% c("url", "Direccion", "texto"))]
ordenar_NOTAS <- sanguchez[order(sanguchez$nota,decreasing = TRUE),]
```

# Empezaramos a buscar funciones para crear el sandwich ideal

La idea fue indentificar un patron de ingredientes que se repiten mas en
las notas de mejor calificacion (4 y 5) ocupamos la primera funcion para
pasar todos los ingredientes a mayuscula y encontrar los ingredientes de
manera mas facil Posteriormente transformamos la base de datos de
ingredientes a caracteres pora posteriomente ocupar la funcion dfm de
quanteda Finalmente guardamos las mejores calificaciones en "nota\_4y5

``` r
sanguchez$Ingredientes = str_to_upper(sanguchez$Ingredientes)
sanguchez$Ingredientes <- as.character(sanguchez$Ingredientes)
nota_5y4 <- filter(sanguchez, nota > 3 )
```

# Buscando los ingredientes mas usados en los sandwiches mejor calificados

Ocupamos la funcion dfm para poder separa todos los ingredientes en
columnas distintas y tener a todos estos separados por si solos y
trabajarlos con mayor facilidad.

Posteriomente Borramos de la base de dato de ingredientes las palabras
que no sirven de nada y que realmente no son ingredientes. Finalmente
utilizamos las ultimas funciones para saber cuales son los ingredientes
que se usan con mayor frecuencia en los sandwiches mejores calificados,
la penultima fincion nos muestra un Ranking de lo ingredientes y la
ultima nos muestra una version grafica de los ingredientes, siendo las
palabras mas grande las que se repiten com mayor frecuencia.

``` r
tabla_ingredientes_nota5y4 <- dfm(nota_5y4$Ingredientes)
tabla_ingredientes_nota5y4 <- tabla_ingredientes_nota5y4[,!(colnames(tabla_ingredientes_nota5y4) %in% c(",", "y", "de", "en","al", ".", "con", "la", ")", "(", "a", "un"))] #elimina cadenas string indicadas
textstat_frequency(tabla_ingredientes_nota5y4, n = 35)
```

    ##         feature frequency rank docfreq group
    ## 1      <U+FFFD>       137    1      90   all
    ## 2         queso        88    2      81   all
    ## 3       cebolla        55    3      53   all
    ## 4      mayonesa        52    4      52   all
    ## 5        tomate        50    5      49   all
    ## 6         salsa        42    6      36   all
    ## 7             n        36    7      32   all
    ## 8       lechuga        34    8      34   all
    ## 9           pan        32    9      32   all
    ## 10        palta        30   10      30   all
    ## 11  hamburguesa        30   10      30   all
    ## 12       tocino        23   12      22   all
    ## 13            r        19   13      18   all
    ## 14       casera        19   13      19   all
    ## 15 caramelizada        18   15      18   all
    ## 16         cula        17   16      17   all
    ## 17      cheddar        17   16      16   all
    ## 18           aj        17   16      17   all
    ## 19       champi        16   19      16   all
    ## 20        frito        15   20      15   all
    ## 21   mozzarella        14   21      13   all
    ## 22         ones        13   22      13   all
    ## 23       morada        13   22      13   all
    ## 24        verde        13   22      13   all
    ## 25        cerdo        13   22      13   all
    ## 26        papas        12   26      12   all
    ## 27       verdes        11   27      11   all
    ## 28        huevo        11   27      11   all
    ## 29        carne        11   27      11   all
    ## 30         hilo        10   30      10   all
    ## 31        crema        10   30      10   all
    ## 32         azul        10   30      10   all
    ## 33          ajo        10   30      10   all
    ## 34   pepinillos        10   30      10   all
    ## 35      mostaza         9   35       9   all

``` r
textplot_wordcloud(tabla_ingredientes_nota5y4, max_words = 30)   #grafico de palabras que se repiten mas
```

![](ENTREGA-PROYECTO-1_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

# Otra forma de ver los ingredientes que mas se repiten en las mejores notas (en cuanto a carnes)

Esto con el fin de ver cuales son Las carnes que se usan mas en los
sandwiches mas exitosos, pero de forma especifica.

``` r
lomo <- nota_5y4[str_detect(nota_5y4$Ingredientes, "LOMO"), ] 
CHURRASCO <- nota_5y4[str_detect(nota_5y4$Ingredientes, "CHURRASCO"), ] #Solo uno de estos. 
MECHADA <- nota_5y4[str_detect(nota_5y4$Ingredientes, "MECHADA"), ] #PODEMOS VER SOLO 4 PRODUCTOS RELACIONADOS CON Mechada CON NOTA 5
POLLO <- nota_5y4[str_detect(nota_5y4$Ingredientes, "POLLO"), ] #PODEMOS VER SOLO 4 PRODUCTOS RELACIONADOS CON Pollo CON NOTA 5
SALCHICHA <- nota_5y4[str_detect(nota_5y4$Ingredientes, "SALCHICHA"), ] #DESCARTADO NINGUN PRODUCTO 
ALBONDIGAS <- nota_5y4[str_detect(nota_5y4$Ingredientes, "ALBONDIGAS"), ] #QUEDA DESCARTADO, NINGUN PRODUCTO
HAMBURGUESA <- nota_5y4[str_detect(nota_5y4$Ingredientes, "HAMBURGUESA"), ] ##PODEMOS VER SOLO 11 PRODUCTOS RELACIONADOS CON Hamburguesa CON NOTA 5
TOCINO <- nota_5y4[str_detect(nota_5y4$Ingredientes, "TOCINO"), ] ##PODEMOS VER SOLO 12 PRODUCTOS RELACIONADOS CON TOCINO CON NOTA 5
PLATEADA <- nota_5y4[str_detect(nota_5y4$Ingredientes, "PLATEADA"), ] #SOLO 1
JAMON <- nota_5y4[str_detect(nota_5y4$Ingredientes, "JAMON"), ] ## DESCARTADO 
CERDO <- nota_5y4[str_detect(nota_5y4$Ingredientes, "CERDO"), ] ## PODEMOS VER SOLO 4 PRODUCTOS RELACIONADOS CON CERDO CON NOTA 5
MILANESA <- nota_5y4[str_detect(nota_5y4$Ingredientes, "MILANESA"), ] # SOLO 1
```

# Ahora con los ingredientes especificos

Esto con el fin de ser mas especificos, es decir sabemos que el queso es
el que mas se repite que cual especificamente? para eso realizaremos lo
siguente , guardar todos los ingredientes que mas se repiten en estas
variables.

``` r
QUESO_MOZZARELLA <- nota_5y4[str_detect(nota_5y4$Ingredientes, "QUESO MOZZARELLA"), ] 
QUESO_azul <- nota_5y4[str_detect(nota_5y4$Ingredientes, "QUESO AZUL"), ]
QUESO_CHEDDAR <- nota_5y4[str_detect(nota_5y4$Ingredientes, "QUESO CHEDDAR"), ] 
QUESO_CREMA <- nota_5y4[str_detect(nota_5y4$Ingredientes, "QUESO CREMA"), ] 
MAYONESA_NORMAL <- nota_5y4[str_detect(nota_5y4$Ingredientes, "MAYONESA"), ] 
MAYONESA_CASERA <- nota_5y4[str_detect(nota_5y4$Ingredientes, "MAYONESA CASERA"), ] 
CEBOLLA_MORADA <- nota_5y4[str_detect(nota_5y4$Ingredientes, "CEBOLLA MORADA"), ] 
CEBOLLA_ACARAMELIZADA <- nota_5y4[str_detect(nota_5y4$Ingredientes, "CEBOLLA CARAMELIZADA"), ] 
MIX_VERDE <- nota_5y4[str_detect(nota_5y4$Ingredientes, " MIX VERDE"), ] 
AJI_VERDE <- nota_5y4[str_detect(nota_5y4$Ingredientes, " AJÍ VERDE"), ] 
SALSA_BBQ<- nota_5y4[str_detect(nota_5y4$Ingredientes, "SALSA BBQ"), ] 
SALSA <- nota_5y4[str_detect(nota_5y4$Ingredientes, "SALSA"), ] 
PAN <- nota_5y4[str_detect(nota_5y4$Ingredientes, "PAN"), ]  
CEBOLLA_BALSAMICA <- nota_5y4[str_detect(nota_5y4$Ingredientes, "CEBOLLA BALSAMICA"), ] 
CEBOLLA_GRILLADAS <- nota_5y4[str_detect(nota_5y4$Ingredientes, "CEBOLLA GRILLADAS"), ] 
PAN_CIABATTA <- nota_5y4[str_detect(nota_5y4$Ingredientes, "PAN CIABATTA"), ] 
PAN_FRICA <- nota_5y4[str_detect(nota_5y4$Ingredientes, "PAN FRICA"), ]
PAN_FOCACCIA <- nota_5y4[str_detect(nota_5y4$Ingredientes, "PAN FOCACCIA"), ] 
PAN_BRIOCHE <- nota_5y4[str_detect(nota_5y4$Ingredientes, "PAN BRIOCHE"), ] 
PAN_POTATO_ROLL <-nota_5y4[str_detect(nota_5y4$Ingredientes, "PAN POTATO ROLL"), ] 
PAN_FRANCÉS <- nota_5y4[str_detect(nota_5y4$Ingredientes, "PAN FRANCÉS"), ] 
PAN_DE_HAMBUERGUESA <- nota_5y4[str_detect(nota_5y4$Ingredientes, "PAN DE HAMBUERGUESA"), ]
HUEVO_FRITO <- nota_5y4[str_detect(nota_5y4$Ingredientes, "HUEVO FRITO"), ]
CARNE <- nota_5y4[str_detect(nota_5y4$Ingredientes, "CARNE"), ]
```

# Sandwiches ideales

Realizamos los siguientes dataframes para tener una mejor vision de los
sandwiches ideales con su respectiva calificacion y el ranking de cada
ingredientes.

``` r
Hambuerguesa_ideal  <- data.frame(ingrediente = c("Hamburguesa","Queso (Cheddar de preferencia)","Cebolla (Caramelizada de preferencia)","Tocino","Mayonesa","Palta"),
                                  ranking=c("9","1",
                                            "2","10",
                                            "3","8"),
                                  calificacion =c("5"))

Hambuerguesa_ideal2 <- data.frame(ingrediente = c("Hamburguesa","Queso (AZUL de preferencia)","Cebolla (MORADA de preferencia)" , "TOMATE","LECHUGA","Palta", "MAYONESA"),
                                  ranking=c("9","1",
                                            "2","4",
                                            "6","8", "3"),
                                  
                                  calificacion =c("5"))
```

## RANKING EN QUESO

  - 1.Queso Cheddar
  - 2.Queso Azul
  - 3.Queso Mozzarella
  - 3.Queso Crema

## RANKIN EN CEBOLLA

  - 1.Cebolla Caramelizada
  - 2.Cebolla Morada

## RANKING EN PAN

  - 1.Pan Ciabatta
  - 2.Pan Brioche
  - 3.Pan Frica
