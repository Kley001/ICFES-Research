-----
#Análisis resultados ICFES - 2018-I
#Kleider Stiven Vásquez Gómez
#Datos Categóricos
-----
#librerías requeridas
library(tidyverse)
library(FactoMineR)
library(kableExtra)
library(factoextra)
library(knitr)
library(lubridate)
library(psych)
library(ggplot2)
library(tidyverse)
library(readr)

----------
Resultados_Saber11 <- read.csv("C:/Users/Usuario/Downloads/Resultados_Saber_11_2018-1_Refinado.csv", header = TRUE, sep = ",", encoding="UTF-8")

levels(Resultados_Saber11$cole_jornada) <- c('COMPLETA', 'MAÑANA', 'NOCHE', 'SABATINA', 'TARDE', 'UNICA')
----------
  
resultado <- cut(Resultados_Saber11$punt_global, breaks=(0:5)*100)
Datos_sel <- Resultados_Saber11[ , c('fami_tieneinternet',
                                     'fami_tieneserviciotv',
                                     'fami_tienecomputador',
                                     'fami_tienelavadora',
                                     'fami_tienehornomicroogas',
                                     'fami_tieneautomovil',
                                     'fami_tienemotocicleta',
                                     'fami_tieneconsolavideojuegos',
                                     'fami_numlibros',
                                     'fami_comelechederivados',
                                     'fami_comecarnepescadohuevo',
                                     'fami_comecerealfrutoslegumbre',
                                     'fami_situacioneconomica',
                                     'estu_dedicacionlecturadiaria',
                                     'estu_dedicacioninternet',
                                     'estu_horassemanatrabaja',
                                     'estu_tiporemuneracion')]

----

  for (i in 1:length(names(Datos_sel))) {
    Datos_sel[ ,i] <- as.factor(Datos_sel[ ,i]) 
  }
Datos_sel <- na.omit(Datos_sel)
# str(Datos_sel)

----

res.mca <- MCA(Datos_sel, graph = FALSE)
--
fviz_screeplot(res.mca, addlabels = TRUE, ylim = c(0, 25), main = "Gráfica de sedimentación", xlab ="Dimensiones", ylab= "Porcentajes de varianza explicados por cada dimensión")
fviz_contrib(res.mca, choice = "var", axes = 1, top = 30, title = "Contribuciones de las variables en la primera dimensión")

-----

Datos_indice <- Datos_sel[ , c('fami_tieneinternet',
                                 'fami_tieneserviciotv',
                                 'fami_tienecomputador',
                                 'fami_tienelavadora',
                                 'fami_tienehornomicroogas',
                                 'fami_tieneautomovil',
                                 'fami_tienemotocicleta',
                                 'fami_tieneconsolavideojuegos',
                                 'fami_numlibros',
                                 'fami_comelechederivados',
                                 'fami_comecarnepescadohuevo',
                                 'fami_comecerealfrutoslegumbre',
                                 'fami_situacioneconomica',
                                 'estu_dedicacionlecturadiaria',
                                 'estu_dedicacioninternet',
                                 'estu_horassemanatrabaja')]

-----

res.mcaI <- MCA(Datos_indice, graph = FALSE)
fviz_screeplot(res.mcaI, addlabels = TRUE, ylim = c(0, 25),  main = "Gráfica de sedimentación", xlab ="Dimensiones", ylab= "Porcentajes de varianza explicados por cada dimensión")

Rico <- res.mcaI$ind$coord[,1]
Rico <- cut(Rico, breaks = 10, labels = 1:10)

plot(Rico, xlab = 'Indicador de Riqueza',
     main = 'Gráfico de barras para la variable Riqueza',
     ylab = '', col = 'blue')
grid()

indices <- row.names(Datos_indice)
DatosFinal <- Resultados_Saber11
DatosFinal$resultado <- resultado
DatosFinal <- DatosFinal[indices, ]
DatosFinal$rico <- Rico

PobrezaColegio <- DatosFinal %>%
  select(cole_cod_dane_sede, rico) %>%
  group_by(cole_cod_dane_sede)  %>%
  summarize(mediaPobreza = mean(as.numeric(rico)))

a_ranks <- rank(PobrezaColegio$mediaPobreza, ties.method = "first")
PobrezaColegio$RiquezaColegio <- with(PobrezaColegio, cut(a_ranks, quantile(a_ranks, probs=seq(0,1, by=0.1), na.rm=TRUE), include.lowest=TRUE))

levels(PobrezaColegio$RiquezaColegio) <- 1:10

-----
  
DatosFinal <- merge(DatosFinal,
                      PobrezaColegio[,c("cole_cod_dane_sede", "RiquezaColegio")],
                      by = "cole_cod_dane_sede",
                      all.x = T)

DatosDepartamentos <- DatosFinal %>%
  filter(estu_depto_reside %in% c('ANTIOQUIA', 'ARAUCA', 'ATLANTICO', 'BOGOTA', 'BOLIVAR', 'BOYACA', 'CALDAS', 'CAQUETA', 'CASANARE', 'CAUCA', 'CESAR', 'CHOCO', 'CORDOBA', 'CUNDINAMARCA', 'GUAINIA', 'GUAVIARE', 'HUILA', 'LA GUAJIRA', 'MAGDALENA', 'META', 'NARIÑO', 'NORTE DE SANTANDER', 'PUTUMAYO', 'QUINDIO', 'RISARALDA', 'SAN ANDRES', 'SANTANDER', 'SUCRE', 'TOLIMA', 'VALLE'), estu_nacionalidad == 'COLOMBIA')
DatosDepartamentos %>%
  group_by(RiquezaColegio, cole_depto_ubicacion, cole_naturaleza) %>% 
  summarize(Media_de_Puntaje = mean(punt_global))
plot(DatosDepartamentos$RiquezaColegio, main = 'Distribución del nivel de riqueza del colegio', col = 'blue', ylab = 'Frecuencia', xlab = 'Riqueza del colegio')
grid()

plot(DatosDepartamentos$resultado, las = 2, main = 'Distribución del resultado del estudiante', col = 'blue', ylab = 'Frecuencia')
grid()

boxplot(punt_global ~ RiquezaColegio, data = DatosDepartamentos, ylab = 'Puntaje del estudiante', xlab = 'Riqueza del colegio', main = 'Puntaje del estudiante en función de la riqueza del colegio', col = 'blue')
grid()

boxplot(punt_global ~ cole_naturaleza, data = DatosDepartamentos, xlab = 'Naturaleza del colegio', ylab = 'Puntaje del estudiante', main = 'Puntaje del estudiante en función de la naturaleza del colegio', col = 'blue')
grid()

boxplot(punt_global ~ cole_jornada, data = DatosDepartamentos, las = 2, xlab = '', sub = 'Jornada', ylab = 'Puntaje del estudiante', main = 'Puntaje del estudiante en función de la jornada del colegio', col = 'blue')
grid()

-----

datosTabla01 <- DatosDepartamentos[DatosDepartamentos$RiquezaColegio == 5 & DatosDepartamentos$cole_jornada == 'MAÑANA', ]

tablas01 <- prop.table(table(datosTabla01$resultado, datosTabla01$cole_naturaleza), 2)

#kable(tablas01, align = 'c', col.names = c('NO OFICIAL', 'OFICIAL')) %>% kable_paper('striped', full_width = F)
---
datosTabla02 <- DatosDepartamentos[DatosDepartamentos$RiquezaColegio == 8 & DatosDepartamentos$cole_jornada == 'MAÑANA', ]

tablas02 <- prop.table(table(datosTabla02$resultado, datosTabla02$cole_naturaleza), 2)

#kable(tablas02, align = 'c', col.names = c('NO OFICIAL', 'OFICIAL')) %>% kable_paper('striped', full_width = F)
---
datosTabla03 <- DatosDepartamentos[DatosDepartamentos$RiquezaColegio == 5 & DatosDepartamentos$cole_jornada == 'NOCHE', ]

tablas03 <- prop.table(table(datosTabla03$resultado, datosTabla03$cole_naturaleza), 2)

#kable(tablas03, align = 'c', col.names = c('NO OFICIAL', 'OFICIAL')) %>% kable_paper('striped', full_width = F)
---
datosTabla04 <- DatosDepartamentos[DatosDepartamentos$RiquezaColegio == 10 & DatosDepartamentos$cole_jornada == 'NOCHE', ]

tablas04 <- prop.table(table(datosTabla04$resultado, datosTabla04$cole_naturaleza), 2)

#kable(tablas04, align = 'c', col.names = c('NO OFICIAL', 'OFICIAL')) %>% kable_paper('striped', full_width = F)
#chisq.test(tablas04[2:4,])
---
datosTabla05 <- DatosDepartamentos[DatosDepartamentos$RiquezaColegio == 5 & DatosDepartamentos$cole_jornada == 'TARDE', ]

tablas05 <- prop.table(table(datosTabla05$resultado, datosTabla05$cole_naturaleza), 2)

#kable(tablas05, align = 'c', col.names = c('NO OFICIAL', 'OFICIAL')) %>% kable_paper('striped', full_width = F)
#chisq.test(tablas05[2:4,])
---
datosTabla06 <- DatosDepartamentos[DatosDepartamentos$RiquezaColegio == 1 & DatosDepartamentos$cole_jornada == 'SABATINA', ]

tablas06 <- prop.table(table(datosTabla06$resultado, datosTabla06$cole_naturaleza), 2)

#kable(tablas06, align = 'c', col.names = c('NO OFICIAL', 'OFICIAL')) %>% kable_paper('striped', full_width = F)
---
datosTabla07 <- DatosDepartamentos[DatosDepartamentos$RiquezaColegio == 4 & DatosDepartamentos$cole_jornada == 'SABATINA', ]

tablas07 <- prop.table(table(datosTabla07$resultado, datosTabla07$cole_naturaleza), 2)

#kable(tablas07, align = 'c', col.names = c('NO OFICIAL', 'OFICIAL')) %>% kable_paper('striped', full_width = F)
---
datosTabla08 <- DatosDepartamentos[DatosDepartamentos$RiquezaColegio == 10 & DatosDepartamentos$cole_jornada == 'SABATINA', ]

tablas08 <- prop.table(table(datosTabla08$resultado, datosTabla08$cole_naturaleza), 2)

#kable(tablas08, align = 'c', col.names = c('NO OFICIAL', 'OFICIAL')) %>% kable_paper('striped', full_width = F)
#chisq.test(tablas08[2:4,])

---
