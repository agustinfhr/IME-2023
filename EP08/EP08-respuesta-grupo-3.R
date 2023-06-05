# Grupo 3 - EP08
# Integrantes: 
# Jaime Carrasco
# Pablo Villarreal Ortiz
# Agustin Henriquez Rojas

if (!require ( dplyr )) {
  install.packages ("dplyr" , dependencies=TRUE)
  require (dplyr)
}

if (!require ( ggpubr )) {
  install.packages ("ggpubr" , dependencies=TRUE)
  require (ggpubr)
}

if (!require ( pwr )) {
  install.packages ("pwr" , dependencies=TRUE)
  require (pwr)
}

if (!require ( tidyverse )) {
  install.packages ("tidyverse" , dependencies=TRUE)
  require (tidyverse)
}

if (!require ( RVAideMemoire )) {
  install.packages ("RVAideMemoire" , dependencies=TRUE)
  require (RVAideMemoire)
}

if (!require ( rcompanion )) {
  install.packages ("rcompanion" , dependencies=TRUE)
  require (rcompanion)
}

if (!require ( car )) {
  install.packages ("car" , dependencies=TRUE)
  require (car)
}

# Explortacion de las librerias 

library(dplyr)
library(ggpubr)
library(pwr)
library(tidyverse)
library(RVAideMemoire)
library(rcompanion)
library(car)

# ------------------------------ PROBLEMA --------------------------------------

# Un equipo de agr√≥nomos est√° estudiando las propiedades de diferentes variedades de manzanas seg√∫n el lugar
# donde fueron cultivadas. La etapa actual de la investigaci√≥n estudia el peso de las manzanas en distintos
# momentos de su etapa de crecimiento. Para ello han registrado el peso de manzanas individuales, cada una de
# un √°rbol diferente, en distintos momentos. As√≠, han construido un conjunto de datos con las siguientes variables:
# ‚ñ™ id: identificador √∫nico de cada manzana observada.
# ‚ñ™ variedad: variedad a la que pertenece la manzana. Variable categ√≥rica con los niveles Fuji, Gala, Golden,
# Granny Smith, Pink Lady, Richard, Starking.
# ‚ñ™ tiempo: semana del periodo de crecimiento de la fruta (semana 5, semana 10, semana 15, semana 20).
# ‚ñ™ peso: peso, en gramos, de la manzana.
# 1. Copiar el enunciado del problema asignados como comentario de un script R.
# 2. Descargar desde UVirtual el archivo EP08 Datos.csv con los datos a emplear.
# 3. Familiarizarse con los datos entregados, y enunciar las hip√≥tesis nula y alternativa para el procedimiento
# ANOVA.
# 4. Analizar si se cumplen las condiciones para usar un procedimiento ANOVA y construir un script R para
# verificarlo.
# 5. Independiente del resultado anterior, aplicar una prueba ANOVA √≥mnibus a los datos y entregar una
# conclusi√≥n usando un nivel de significaci√≥n adecuado.
# 6. Si corresponde, aplicar un an√°lisis post-hoc e interpretar los resultados. En caso contrario, argumentar por
# qu√© no es necesario.
# 7. Redactar la respuesta a la pregunta planteada (comentario) en base a los resultados del an√°lisis realizado.


# ------------------------------- DATOS ----------------------------------------

dir <- "C:/Users/agust/Desktop/REPO IME/IME-2023/EP08"
basename <- "EP08 Datos.csv"
file <- file.path(dir, basename)
poblacion <- read.csv2(file = file)

# -------------------------- PREGUNTAS AL GRUPO --------------------------------

# Grupo 3:
# En este momento, los agr√≥nomos buscan determinar si existen diferencias en el peso a la semana 20 de
# crecimiento entre las manzanas Pink Lady, Starking, Golden y Granny Smith.

# 1. El equipo obtiene y manipula correctamente los datos entregados y propone hipÛtesis pertinentes a
# contrastar para cada problema.

# HipÛtesis nula H0: No hay diferencias significativas en los pesos de las manzanas Pink Lady,
# Starking, Golden y Granny Smith en la semana 20 de crecimiento.
# 
# HipÛtesis alternativa HA: Hay al menos una diferencia significativa en los pesos de las manzanas
# Pink Lady, Starking, Golden y Granny Smith en la semana 20 de crecimiento.
# 
# FormulaciÛn Matem·tica
# 
# H0: ????? = ????? = ????? = ?????
# 
# HA: Al menos una desigualdad en el conjunto {????? ??? ?????, ????? ??? ?????, ????? ??? ?????, ????? ??? ?????, ????? ??? ?????, ????? ??? ?????}

semana20 <- subset(poblacion, tiempo == "semana_20" & variedad %in% c("Pink Lady",
                                                                      "Starking", 
                                                                      "Golden", 
                                                                      "Granny Smith"))

# Lo comento porque no me funcionaba :(
# grupo20 <- poblacion %>% filter (tiempo == "semana_20")
# mz_PL <- grupo20 %>% filter (variedad == "Pink Lady")
# mz_Starking <- grupo20 %>% filter (variedad == "Starking")
# mz_Golden <- grupo20 %>% filter (variedad == "Golden")
# mz_GS <- grupo20 %>% filter (variedad == "Granny Smith")


# 2. El equipo se asegura que cada caso cumple las condiciones para utilizar ANOVA con validez, usando gr·ficos
# o pruebas estadÌsticas auxiliares disponibles en el entorno R.

# Comprobar la 4 Condiciones para ANOVA.

# Primero, el peso de las manzanas (variable dependiente) se mide en gramos, 
# que es una escala de razÛn (porque tiene un cero absoluto y los intervalos son iguales).
# por lo que si se cumple la primera condicion.

# Segundo, se asume que las observaciones son independientes entre si, cumpliendo la condicion de Independencia

# Tercero, comprobar que los datos siguen una distribucion normal

# Prueba de Shapiro-Wilk para la normalidad
shapiro.test(semana20$peso[semana20$variedad == "Pink Lady"])
shapiro.test(semana20$peso[semana20$variedad == "Starking"])
shapiro.test(semana20$peso[semana20$variedad == "Golden"])
shapiro.test(semana20$peso[semana20$variedad == "Granny Smith"])

# Como se puede observar, todos los p-valores son mayores al nivel de significancia (p<0.05)
# No se rechaza la hipotesis nula de la prueba de Shapiro, por lo que el principio de normalidad
# se cumple para estas cuatro variedades.

# Grafico Q-Q, para visualizar la distribucion normal. 
qqnorm(semana20$peso[semana20$variedad == "Pink Lady"])
qqline(semana20$peso[semana20$variedad == "Pink Lady"])

qqnorm(semana20$peso[semana20$variedad == "Starking"])
qqline(semana20$peso[semana20$variedad == "Starking"])

qqnorm(semana20$peso[semana20$variedad == "Golden"])
qqline(semana20$peso[semana20$variedad == "Golden"])

qqnorm(semana20$peso[semana20$variedad == "Granny Smith"])
qqline(semana20$peso[semana20$variedad == "Granny Smith"])

# A partir del grafico Q-Q, tambien se puede obversar que los datos tienden a distribuirse de forma normal.

# Cuarto, verificar la condicion de homocedasticidad(tienen la misma varianza) en los grupos para poder aplicar el ANOVA.

# Para comprobar esta condicion, se puede utilizar la prueba de Levene
leveneTest(peso ~ variedad, data = semana20)

# En este caso, el valor p obtenido por la prueba de Levene es de 0.7712, mayor al nivel de 
# significancia 0.05, por lo que indica que no se puede rechazar la hipotesis nula de que las 
# varianzas son iguales en los grupos, de esta manera, cumpliendo con la condicion de homocedasticidad.

# Finalmente, los datos cumplen las cuatro condiciones para realizar una prueba ANOVA.

# 3. El equipo realiza de forma correcta y completa una prueba ANOVA Ûmnibus para cada problema.

# Ahora procediendo a ejecutar la prueba ANOVA.

# Procedimiento ANOVA con aov().
modelo_anova <- aov(peso ~ variedad, data = semana20)
summary(modelo_anova)

# A partir de los resultados obtenido por la prueba ANOVA
# el p-valor es 0.699, mayor que 0.05. Esto significa que no se puede rechazar
# la hipÛtesis nula de que no hay diferencias en los pesos de las manzanas Pink Lady,
# Starking, Golden y Granny Smith. 

# Por lo tanto, no hay evidencia de que existan diferencias significativas 
# en los pesos de estas cuatro variedades de manzanas en la semana 20 de crecimiento.

# 4. El equipo determina correctamente si corresponde o no hacer un an·lisis post-hoc en cada caso, el que
# aplica de forma correcta y completa (cuando corresponda).

# En este caso, el p-valor de la prueba ANOVA fue 0.699, que es mayor que 0.05. 
# Esto significa que no se rechaza la hipÛtesis nula de que no hay diferencias significativas 
# entre las medias de los grupos. Por lo tanto, no serÌa necesario realizar un an·lisis post-hoc
# porque no se ha encontrado diferencias significativas entre los grupos en el ANOVA.
# 
# A este punto, se est· cumpliendo con el objetivo de la actividad en tÈrminos 
# de determinar si corresponde o no realizar un an·lisis post-hoc. Sin embargo, si el p-valor 
# hubiera sido menor que 0.05, entonces se deberÌa proceder con un an·lisis post-hoc, como la prueba
# de Tukey, para determinar dÛnde est·n exactamente esas diferencias entre los grupos.

# 5. El equipo interpreta adecuadamente los resultados de las pruebas y responde las preguntas planteadas en
# cada caso, bas·ndose en la prueba Ûmnibus y, si corresponde, considerando tambiÈn el an·lisis post-hoc.

# Bas·ndonos en nuestro an·lisis ANOVA, concluimos que no hay diferencias significativas en el peso
# de las manzanas Pink Lady, Starking, Golden y Granny Smith en la semana 20 de crecimiento.
# Esto se debe a que nuestro p-valor de 0.699 es mayor que el nivel de significaciÛn de 0.05,
# lo que significa que no podemos rechazar la hipÛtesis nula de que las medias de los grupos son
# iguales. Por lo tanto, no hay evidencia de que el peso de las manzanas varÌe entre estas cuatro 
# variedades en la semana 20 de crecimiento.







