##carga de paquetes necesarios

library(easypackages)
packages("readxl")
packages("tidyverse")
packages("plotly")
packages("ggplot2")
packages("janitor")
packages("dplyr")
packages("ggplot2")
packages("sf")
packages("tmap")
packages("plotly")
packages("stringr")

##cargamos la base de datos

RESUMEN_DE_CAMPAÑAS <- read_excel("C:/Users/win10/Desktop/RESUMEN DE CAMPAÑAS.xlsx")

##modificamos los rendimientos de la cebada

unificacion_cebada<- RESUMEN_DE_CAMPAÑAS%>%
  filter(str_detect(deno_producto, "CEBADA"))%>%
  group_by(deno_campo, lote, campania)%>%
  mutate(rto= sum(RENDIMIENTO))%>%
  select(c(-RENDIMIENTO))

agrupados<- unificacion_cebada%>%
  group_by(campania, deno_producto)%>%
  summarise(mean=mean(rto))

##sacamos los valores de cebada erroneos de la base de datos

resumen_sin_cbda<- RESUMEN_DE_CAMPAÑAS%>%
  filter(deno_producto!= "CEBADA FORRAJERA")%>%
  filter(deno_producto!= "CEBADA CERVECERA")%>%
  mutate(rto=RENDIMIENTO)%>%
  select(c(-RENDIMIENTO))

##unimos las tablas

union<- full_join(resumen_sin_cbda, unificacion_cebada)%>%
  filter(deno_producto!= "CEBADA FORRAJERA")%>%
  mutate(cultivo=factor(deno_producto))

agrupados<- union%>%
  group_by(campania, deno_producto)%>%
  summarise(mean=mean(rto))

df<- merge(union, agrupados, by = c("campania", "deno_producto"))%>%
  mutate(rto_norm= rto/mean, departamentos= tolower(DEPARTAMENTOS))



##grafico box plot del rendimiento

grafico1<- ggplot(df, aes(x=DISTRITO, y=rto_norm))+
  geom_boxplot()+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust=1))+
  facet_wrap(df$cultivo)

grafico1

ggplotly(grafico1)

##calculo de desvio estandar

sd<- df%>%
  group_by(deno_producto, cultivo)%>%
  summarise(sd= sd(rto_norm))

##grafico de desvio estandar

ggplot(sd, aes(x= reorder(cultivo, sd), y= sd, fill= cultivo))+
  geom_bar(stat = "identity")
