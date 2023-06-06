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

# Importación de las librerias 

library(dplyr)
library(ggpubr)
library(pwr)
library(tidyverse)
library(RVAideMemoire)
library(rcompanion)
library(car)

# ------------------------------ PROBLEMA --------------------------------------

# Un equipo de agrónomos está estudiando las propiedades de diferentes variedades de manzanas según el lugar
# donde fueron cultivadas. La etapa actual de la investigación estudia el peso de las manzanas en distintos
# momentos de su etapa de crecimiento. Para ello han registrado el peso de manzanas individuales, cada una de
# un árbol diferente, en distintos momentos. Así, han construido un conjunto de datos con las siguientes variables:
# ▪ id: identificador único de cada manzana observada.
# ▪ variedad: variedad a la que pertenece la manzana. Variable categórica con los niveles Fuji, Gala, Golden,
# Granny Smith, Pink Lady, Richard, Starking.
# ▪ tiempo: semana del periodo de crecimiento de la fruta (semana 5, semana 10, semana 15, semana 20).
# ▪ peso: peso, en gramos, de la manzana.
# 1. Copiar el enunciado del problema asignados como comentario de un script R.
# 2. Descargar desde UVirtual el archivo EP08 Datos.csv con los datos a emplear.
# 3. Familiarizarse con los datos entregados, y enunciar las hipótesis nula y alternativa para el procedimiento
# ANOVA.
# 4. Analizar si se cumplen las condiciones para usar un procedimiento ANOVA y construir un script R para
# verificarlo.
# 5. Independiente del resultado anterior, aplicar una prueba ANOVA ómnibus a los datos y entregar una
# conclusión usando un nivel de significación adecuado.
# 6. Si corresponde, aplicar un análisis post-hoc e interpretar los resultados. En caso contrario, argumentar por
# qué no es necesario.
# 7. Redactar la respuesta a la pregunta planteada (comentario) en base a los resultados del análisis realizado.


# ------------------------------- DATOS ----------------------------------------

dir <- "C:/Users/Jaime Carrasco/Documents/Universidad/IME/EPS/IME-2023/EP08"
basename <- "EP08 Datos.csv"
file <- file.path(dir, basename)
poblacion <- read.csv2(file = file)

# -------------------------- PREGUNTAS AL GRUPO --------------------------------

# Grupo 3:
# En este momento, los agrónomos buscan determinar si existen diferencias en el peso a la semana 20 de
# crecimiento entre las manzanas Pink Lady, Starking, Golden y Granny Smith.

# 1. El equipo obtiene y manipula correctamente los datos entregados y propone hip?tesis pertinentes a
# contrastar para cada problema.

# Hipótesis nula H0: No hay diferencias significativas en los pesos de las manzanas Pink Lady,
# Starking, Golden y Granny Smith en la semana 20 de crecimiento.
# 
# Hipótesis alternativa HA: Hay al menos una diferencia significativa en los pesos de las manzanas
# Pink Lady, Starking, Golden y Granny Smith en la semana 20 de crecimiento.
# 
# Formulación Matemática
# 
# H0: mu1 = mu2 = mu3 = mu4
# 
# HA: Al menos una desigualdad en el conjunto {mu1 != mu2, mu1 != mu3, mu1 != mu4, mu2 != mu3, mu2 != mu4, mu3 != mu4}

semana20 <- subset(poblacion, tiempo == "semana_20" & variedad %in% c("Pink Lady",
                                                                      "Starking", 
                                                                      "Golden", 
                                                                      "Granny Smith"))


# 2. El equipo se asegura que cada caso cumpla las condiciones para utilizar ANOVA con validez, usando gráficos
# o pruebas estadísticas auxiliares disponibles en el entorno R.

# Comprobar las 4 Condiciones para ANOVA.

# Primero, el peso de las manzanas (variable dependiente) se mide en gramos, 
# que es una escala de razón (porque tiene un cero absoluto y los intervalos son iguales).
# por lo que si se cumple la primera condicion.

# Segundo, se asume que las observaciones son independientes entre si, cumpliendo la condición de Independencia

# Tercero, comprobar que los datos siguen una distribución normal

# Prueba de Shapiro-Wilk para la normalidad
shapiro.test(semana20$peso[semana20$variedad == "Pink Lady"])
shapiro.test(semana20$peso[semana20$variedad == "Starking"])
shapiro.test(semana20$peso[semana20$variedad == "Golden"])
shapiro.test(semana20$peso[semana20$variedad == "Granny Smith"])

# Como se puede observar, todos los p-valores son mayores al nivel de significancia (p<0.05)
# No se rechaza la hipótesis nula de la prueba de Shapiro, por lo que el principio de normalidad
# se cumple para estas cuatro variedades.

# Grafico Q-Q, para visualizar la distribución normal. 
qqnorm(semana20$peso[semana20$variedad == "Pink Lady"])
qqline(semana20$peso[semana20$variedad == "Pink Lady"])

qqnorm(semana20$peso[semana20$variedad == "Starking"])
qqline(semana20$peso[semana20$variedad == "Starking"])

qqnorm(semana20$peso[semana20$variedad == "Golden"])
qqline(semana20$peso[semana20$variedad == "Golden"])

qqnorm(semana20$peso[semana20$variedad == "Granny Smith"])
qqline(semana20$peso[semana20$variedad == "Granny Smith"])

# A partir del grafico Q-Q, también se puede obversar que los datos tienden a distribuirse de forma normal.

# Cuarto, verificar la condición de homocedasticidad (tienen la misma varianza) en los grupos para poder aplicar el ANOVA.

# Para comprobar esta condición, se puede utilizar la prueba de Levene
leveneTest(peso ~ variedad, data = semana20)

# En este caso, el valor p obtenido por la prueba de Levene es de 0.7712, mayor al nivel de 
# significancia 0.05, por lo que indica que no se puede rechazar la hipótesis nula de que las 
# varianzas son iguales en los grupos, de esta manera, cumpliendo con la condicion de homocedasticidad.

# Finalmente, los datos cumplen las cuatro condiciones para realizar una prueba ANOVA.

# 3. El equipo realiza de forma correcta y completa una prueba ANOVA Ómnibus para cada problema.

# Ahora procediendo a ejecutar la prueba ANOVA.

# Procedimiento ANOVA con aov().
modelo_anova <- aov(peso ~ variedad, data = semana20)
summary(modelo_anova)

# A partir de los resultados obtenido por la prueba ANOVA
# el p-valor es 0.699, mayor que 0.05. Esto significa que no se puede rechazar
# la hipótesis nula de que no hay diferencias en los pesos de las manzanas Pink Lady,
# Starking, Golden y Granny Smith. 

# Por lo tanto, no hay evidencia de que existan diferencias significativas 
# en los pesos de estas cuatro variedades de manzanas en la semana 20 de crecimiento.

# 4. El equipo determina correctamente si corresponde o no hacer un análisis post-hoc en cada caso, el que
# aplica de forma correcta y completa (cuando corresponda).

# En este caso, el p-valor de la prueba ANOVA fue 0.699, que es mayor que 0.05. 
# Esto significa que no se rechaza la hipótesis nula de que no hay diferencias significativas 
# entre las medias de los grupos. Por lo tanto, no será necesario realizar un análisis post-hoc
# porque no se ha encontrado diferencias significativas entre los grupos en el ANOVA.
# 
# A este punto, se está cumpliendo con el objetivo de la actividad en términos 
# de determinar si corresponde o no realizar un análisis post-hoc. Sin embargo, si el p-valor 
# hubiera sido menor que 0.05, entonces se debería proceder con un análisis post-hoc, como la prueba
# de Tukey, para determinar dónde están exactamente esas diferencias entre los grupos.

# 5. El equipo interpreta adecuadamente los resultados de las pruebas y responde las preguntas planteadas en
# cada caso, basándose en la prueba Ómnibus y, si corresponde, considerando también el análisis post-hoc.

# Basándonos en nuestro análisis ANOVA, concluimos que no hay diferencias significativas en el peso
# de las manzanas Pink Lady, Starking, Golden y Granny Smith en la semana 20 de crecimiento.
# Esto se debe a que nuestro p-valor de 0.699 es mayor que el nivel de significación de 0.05,
# lo que significa que no podemos rechazar la hipótesis nula de que las medias de los grupos son
# iguales. Por lo tanto, no hay evidencia de que el peso de las manzanas varíe entre estas cuatro 
# variedades en la semana 20 de crecimiento.







