##########################################################################
# Jose Cajide - @jrcajide
# Master Data Science: EDA Tarjetas Black (Simple)
##########################################################################

library(tidyverse)
library(scales)
library(ggrepel)

theme_set(theme_bw())


# IMPORTACION DE DATOS ----------------------------------------------------

movimientos <- read_csv("datos/movimientos_tarjetas_black.csv", 
                        col_types = cols(TARJETA = col_character()))

min(movimientos$FECHA); max(movimientos$FECHA)

titulares <- read_delim('datos/titulares_tarjetas_black.csv', delim = "#", col_types = cols(TARJETA = col_character()))


movimientos <- movimientos %>% 
  left_join(titulares) 

# EJERCICIO 1 -------------------------------------------------------------

# Número de observacionse (movimientos) por titular de la tarjeta

par(mar=c(4,24,4,2))

barplot(sort(table(movimientos$NOMBRE_Y_APELLIDOS), decreasing = F), horiz = T,las=2)

table(movimientos$NOMBRE_Y_APELLIDOS) %>% 
  sort(decreasing = F) %>% 
  barplot(main= "Movimientos por titular", horiz = T, las=2)

dev.off()

movimientos %>% 
  group_by(NOMBRE_Y_APELLIDOS) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>% 
  ggplot(aes(x = reorder(NOMBRE_Y_APELLIDOS,n) , y= n)) + 
  geom_bar(stat = 'identity') + 
  labs(title="Movimientos por titular") +
  xlab("") +
  ylab("Número de movimientos") +
  scale_y_continuous(breaks= pretty_breaks()) +
  coord_flip() + 
  theme_minimal()



# EJERCICIO 2 -------------------------------------------------------------
# Cada anotación tiene un código de operación ¿Qué significa cada uno de ellos?

movimientos %>% 
  mutate(TIPO_OP = paste(COD_OPERACION, COD_OPE_DESC)) %>% 
  distinct(TIPO_OP) %>% 
  arrange(TIPO_OP)


# Calcula la media y la mediana de todos los importes exceptos de los que son abonos.

movimientos %>% 
  filter(!COD_OPERACION == 400 )

movimientos %>% 
  filter(!COD_OPERACION == 400 ) %>% 
  summarise(importe_media = mean(IMPORTE),
            importe_mediana  =median(IMPORTE))


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

movimientos <- movimientos %>% 
  mutate(COD_OPE_DESC_AGRUPADO = case_when(COD_OPE_DESC == 'REINTEGRO EN CAJERO PROPIO' ~ "RETIRADA EFECTIVO",
                                           COD_OPE_DESC == 'REINTEGRO EN CAJERO AJENO NACIONAL' ~ "RETIRADA EFECTIVO",
                                           COD_OPE_DESC == 'DISPOSICION EFECTIVO OFICINA' ~ "RETIRADA EFECTIVO",
                                           TRUE ~ as.character(COD_OPE_DESC)))

movimientos %>% 
  distinct(COD_OPE_DESC_AGRUPADO) %>% 
  arrange(desc(COD_OPE_DESC_AGRUPADO))

# Hacer un boxplot de los importes de los movimientos que nos permita comparar las disposicione en efectivo Vs las compras
movimientos %>% 
  filter(COD_OPE_DESC_AGRUPADO %in% c('COMPRA', 'RETIRADA EFECTIVO')) %>%
  ggplot(aes(x=COD_OPE_DESC_AGRUPADO, y=IMPORTE)) + 
  geom_boxplot() +
  coord_flip()

# Repetir lo mismo pero eliminando los valores extremos siguiendo el criterio de Tukey: https://en.wikipedia.org/wiki/Outlier#Tukey's_fences

# Incorrecto
movimientos %>% 
  filter(COD_OPE_DESC_AGRUPADO %in% c('COMPRA', 'RETIRADA EFECTIVO')) %>%
  filter(!(abs(IMPORTE - mean(IMPORTE)) > 1.5*sd(IMPORTE))) %>% 
  ggplot(aes(x=COD_OPE_DESC_AGRUPADO, y=IMPORTE)) + 
  geom_boxplot() +
  coord_flip() 

# Correcto
movimientos %>% 
  filter(COD_OPE_DESC_AGRUPADO %in% c('COMPRA', 'RETIRADA EFECTIVO')) %>%
  group_by(COD_OPE_DESC_AGRUPADO) %>% 
  filter(!(abs(IMPORTE - mean(IMPORTE)) > 2.5*sd(IMPORTE))) %>%
  ggplot(aes(x=COD_OPE_DESC_AGRUPADO, y=IMPORTE)) + 
  geom_boxplot() +
  coord_flip() 

# MEJOR
movimientos %>% 
  filter(COD_OPE_DESC_AGRUPADO %in% c('COMPRA', 'RETIRADA EFECTIVO')) %>%
  group_by(COD_OPE_DESC_AGRUPADO) %>% 
  filter(!(abs(IMPORTE - mean(IMPORTE)) > 2.5*sd(IMPORTE))) %>% 
  ggplot(aes(x='COD_OPE_DESC_AGRUPADO', y=IMPORTE)) + 
  geom_boxplot() +
  facet_wrap( ~ COD_OPE_DESC_AGRUPADO, scales = 'free_y') 


# ¿Quiénes son los titulares "extremos" que destacan por haber comprado y a la vez retirado más dinero en efectivo?

movimientos %>% 
  filter(COD_OPE_DESC_AGRUPADO %in% c('COMPRA', 'RETIRADA EFECTIVO')) %>%
  group_by(COD_OPE_DESC_AGRUPADO) %>% 
  filter((abs(IMPORTE - mean(IMPORTE)) > 2.5*sd(IMPORTE))) %>% 
  ungroup() %>% 
  group_by(NOMBRE_Y_APELLIDOS, COD_OPE_DESC_AGRUPADO) %>% 
  summarise(IMPORTE_TOTAL = sum(IMPORTE)) %>% 
  spread(key = "COD_OPE_DESC_AGRUPADO", value = "IMPORTE_TOTAL") %>% 
  drop_na() %>% 
  arrange(desc(COMPRA))



# EJERCICIO 4 -------------------------------------------------------------

# ¿En qué han gastado más el dinero?

movimientos %>% 
  filter(!COD_OPERACION == 400) %>% 
  group_by(DES_L_SEC_ACTIVI) %>% 
  summarise(TOTAL_POR_ACTIVIDAD = sum(IMPORTE)) %>% 
  arrange(-TOTAL_POR_ACTIVIDAD) %>% 
  top_n(10) %>% 
  ggplot(aes(x=reorder(DES_L_SEC_ACTIVI,TOTAL_POR_ACTIVIDAD), y=TOTAL_POR_ACTIVIDAD)) + 
  geom_bar(stat = 'identity') +
  coord_flip()

# Gasto medio por activadad
movimientos %>% 
  filter(!COD_OPERACION == 400) %>% 
  group_by(DES_L_SEC_ACTIVI) %>% 
  summarise(IMPORTE_MEDIO = mean(IMPORTE, na.rm = T)) %>% 
  arrange(-IMPORTE_MEDIO)


# EJERCICIO 5 -------------------------------------------------------------

# ¿Cómo eran los gastos en restauración?

movimientos %>% 
  filter(!COD_OPERACION == 400, grepl('^RESTAURANTE', DES_L_SEC_ACTIVI)) %>% 
  ggplot(aes(x=IMPORTE)) + 
  geom_histogram(bins = 60) +
  xlim(c(0,750))

movimientos %>% 
  filter(!COD_OPERACION == 400, grepl('^RESTAURANTE', DES_L_SEC_ACTIVI)) %>% 
  ggplot(aes(x=IMPORTE, fill=DES_L_SEC_ACTIVI)) + 
  geom_histogram(bins = 60, alpha=.5) +
  xlim(c(0,750))




# EJERCICIO 6 -------------------------------------------------------------

# Gasto total en hoteles y restaurantes
# Agrupar hoteles y restaurantes para comparar los gastos

movimientos %>% 
  mutate(ACTIVIDAD = case_when(
    grepl('RESTAURANTE', DES_L_SEC_ACTIVI) ~ "Restaurantes",
    grepl('HOT', DES_L_SEC_ACTIVI) ~ "Hoteles",
    TRUE ~ ""
  )) %>% 
  filter(!COD_OPERACION == 400, ACTIVIDAD != "") %>% 
  group_by(ACTIVIDAD) %>% 
  summarise(TOTAL = sum(IMPORTE),
            OPERACIONES = n())


# EJERCICIO 7 -------------------------------------------------------------

# ¿Cuántos acusados hay por puesto en Bankia? ¿Y por organización de pertenencia?

unique(movimientos$FUNCION)
table(movimientos$FUNCION, movimientos$ORGANIZACION)

movimientos %>% 
  group_by(NOMBRE_Y_APELLIDOS, FUNCION) %>% 
  summarise() %>% 
  arrange(FUNCION) 

# Número de acusados por puesto en Bankia
movimientos %>% 
  group_by(FUNCION) %>% 
  summarise(n = n_distinct(NOMBRE_Y_APELLIDOS)) %>% 
  arrange(FUNCION) 

# Acusados por organización

movimientos %>% 
  group_by(ORGANIZACION) %>% 
  summarise(NUM_ACUSADOS = n_distinct(NOMBRE_Y_APELLIDOS)) %>% 
  drop_na() %>% 
  arrange(desc(NUM_ACUSADOS)) 

# ¿Cuántos acusados hay por puesto en Bankia? ¿Y por organización de pertenencia?

movimientos %>% 
  filter(!COD_OPERACION == 400) %>% 
  group_by(FUNCION) %>% 
  summarise(TOTAL = sum(IMPORTE),
            GASTO_MEDIO = mean(IMPORTE),
            OPERACIONES = n())

# Realiza un boxplot para comparar los importes de las operaciones por organización de pertenencia de cada acusado.

movimientos %>% 
  filter(!COD_OPERACION == 400) %>% 
  ggplot(aes(x=ORGANIZACION ,y = IMPORTE)) + 
  geom_boxplot(outlier.colour = 'red')

# Realiza un boxplot para comparar los importes de las operaciones por organización de pertenencia de cada acusado.

movimientos %>% 
  filter(!COD_OPERACION == 400) %>% 
  group_by(ORGANIZACION, NOMBRE_Y_APELLIDOS) %>% 
  summarise(TOTAL = sum(IMPORTE)) %>% 
  ggplot(aes(x=ORGANIZACION ,y = TOTAL)) + 
  geom_boxplot(outlier.colour = 'red')


# ¿Quién es el acusado que más ha gastado dentro de cada organización?

movimientos %>% 
  filter(!COD_OPERACION == 400) %>% 
  group_by(ORGANIZACION, NOMBRE_Y_APELLIDOS) %>% 
  summarise(TOTAL = sum(IMPORTE)) %>% 
  top_n(1) %>% 
  drop_na() %>% 
  arrange(desc(TOTAL)) 


# EJERCICIO 8 -------------------------------------------------------------

# Fecha de la primera y última operación de cada acusado

max(movimientos$FECHA)

movimientos %>% 
  group_by(NOMBRE_Y_APELLIDOS) %>% 
  summarise(PRIMERA_OP = as.Date(min(FECHA)),
            ULTIMA_OP = as.Date(max(FECHA))) %>% 
  ggplot(aes(x= reorder(NOMBRE_Y_APELLIDOS, desc(ULTIMA_OP) ))) + 
  geom_point(aes(y=PRIMERA_OP), size=2) +
  geom_point(aes(y=ULTIMA_OP), size=2) +
  geom_text(aes(x=NOMBRE_Y_APELLIDOS,
                y=ULTIMA_OP,
                label=NOMBRE_Y_APELLIDOS),hjust=-.1,  size=2.6) +
  geom_segment(aes(x=NOMBRE_Y_APELLIDOS, 
                   xend=NOMBRE_Y_APELLIDOS, 
                   y=PRIMERA_OP, 
                   yend=ULTIMA_OP), 
               size=.2) +
  scale_y_date(date_breaks = "1 years", date_labels = "%Y",
               limits = as.Date(c('2003-01-01','2015-01-01'))) +
  coord_flip() +
  theme_minimal() +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())


