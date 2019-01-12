##########################################################################
# Jose Cajide - @jrcajide
# Master Data Science: EDA Tarjetas Black (Simple)
##########################################################################

library(tidyverse)
library(scales)
library(ggrepel)

theme_set(theme_bw())


# IMPORTACION DE DATOS ----------------------------------------------------

movimientos <- 


min(movimientos$FECHA); max(movimientos$FECHA)

titulares <-

# EJERCICIO 1 -------------------------------------------------------------

# Número de observacionse (movimientos) por titular de la tarjeta



# EJERCICIO 2 -------------------------------------------------------------
# Cada anotación tiene un código de operación ¿Qué significa cada uno de ellos?



# Calcula la media y la mediana de todos los importes exceptos de los que son abonos.

movimientos %>% 
  filter(COD_OPERACION == 400 )


# EJERCICIO 3 -------------------------------------------------------------


# Quiénes son los titulares "extremos" que destacan por haber comprado y a la vez retirado más dinero en efectivo

# NOMBRE_Y_APELLIDOS                    COMPRA `RETIRADA EFECTIVO`
# <chr>                                  <dbl>               <dbl>
#   1 ILDEFONSO JOSE SANCHEZ BARCOJ        165388.              53680.
# 2 JUAN MANUEL ASTORQUI PORTERA         109650.               4000.
# 3 CARMEN CONTRERAS GOMEZ               107341.               4231.
# 4 CARLOS VELA GARCIA                   102117.               3000.
# 5 RAMON FERRAZ RICARTE                  64204.              45573.
# 6 ENRIQUE DE LA TORRE MARTINEZ          29413.             102286.
# 7 RICARDO MORADO IGLESIAS               24005.             286200.
# 8 RICARDO ROMERO DE TEJADA Y PICATOSTE  13953.              11930.


# Agrupa las operaciones de reintegro en cajeros y disposiciones en efectivo dentro de una misma categoría
movimientos %>% 
  distinct(COD_OPE_DESC) %>% 
  arrange(desc(COD_OPE_DESC))


# Hacer un boxplot de los importes de los movimientos que nos permita comparar las disposicione en efectivo Vs las compras


# Repetir lo mismo pero eliminando los valores extremos siguiendo el criterio de Tukey: https://en.wikipedia.org/wiki/Outlier#Tukey's_fences


# ¿Quiénes son los titulares "extremos" que destacan por haber comprado y a la vez retirado más dinero en efectivo?



# EJERCICIO 4 -------------------------------------------------------------

# ¿En qué han gastado más el dinero?


# Gasto medio por activadad



# EJERCICIO 5 -------------------------------------------------------------

# ¿Cómo eran los gastos en restauración?



# EJERCICIO 6 -------------------------------------------------------------

# Gasto total en hoteles y restaurantes
# Agrupar hoteles y restaurantes para comparar los gastos


# EJERCICIO 7 -------------------------------------------------------------

# ¿Cuántos acusados hay por puesto en Bankia? ¿Y por organización de pertenencia?


# Número de acusados por puesto en Bankia


# Acusados por organización


# ¿Cuántos acusados hay por puesto en Bankia? ¿Y por organización de pertenencia?


# Realiza un boxplot para comparar los importes de las operaciones por organización de pertenencia de cada acusado.


# Realiza un boxplot para comparar los importes de las operaciones por organización de pertenencia de cada acusado.


# ¿Quién es el acusado que más ha gastado dentro de cada organización?


# EJERCICIO 8 -------------------------------------------------------------

# Fecha de la primera y última operación de cada acusado

