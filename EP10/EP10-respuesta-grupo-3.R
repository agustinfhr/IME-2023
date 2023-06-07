# Grupo 3
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

# Explortacion de las librerias 

library(dplyr)
library(ggpubr)
library(pwr)
library(tidyverse)
library(RVAideMemoire)
library(rcompanion)

# ------------------------------ PROBLEMA --------------------------------------

# Una empresa se encuentra en las etapas finales del proceso de desarrollo de un nuevo videojuego, por lo que el
# equipo de aseguramiento de la calidad está realizando pruebas exhaustivas con usuarios reales para asegurarse
# de que el producto sea todo un éxito. Una vez completada la primera etapa de pruebas, ha organizado la
# información recolectada en un conjunto de datos con las siguientes columnas:
# ▪ Id: identificador único del usuario.
# ▪ Estetica: percepción de la calidad de la estética, en una escala de 1 (muy mala) a 5 (muy buena).
# ▪ Usabilidad: percepción de la simplicidad de los controles del juego, en una escala de 1 (muy mala) a 5 (muy
# buena).
# ▪ Dificultad: percepción acerca del nivel de dificultad del juego, en una escala de 1 (muy fácil) a 5 (muy difícil).
# ▪ Personaje: tipo de personaje escogido por el jugador, variable categórica con niveles humano, elfo, enano,
# orco, mago, mediano.
# ▪ Nivel_10: tiempo (en horas) que tarda el usuario en completar el nivel 10 del juego.
# ▪ Nivel_20: tiempo (en horas) que tarda el usuario en completar el nivel 20 del juego.
# ▪ Nivel_30: tiempo (en horas) que tarda el usuario en completar el nivel 30 del juego.
# 1. Copiar el enunciado de los problemas asignados como comentarios de un script R.
# 2. Descargar desde UVirtual el archivo EP10 Datos.csv con los datos a emplear.
# 3. Obtener los datos en cada caso y proponer hipótesis que permitan responder las preguntas planteadas.
# 4. Argumentar o escribir código R para verificar que no se cumplen las condiciones para pruebas paramétricas
# con validez para el caso en estudio.
# 5. Escribir código R para realizar las pruebas no paramétricas solicitadas con los datos apropiados.
# 6. Concluir a la luz de los resultados de la prueba.
# 7. Redactar la respuesta a la pregunta planteada (comentario) en base a los resultados del análisis realizado

# ------------------------------- DATOS ----------------------------------------

dir <- "C:/Users/rowin/OneDrive/Escritorio/modelos estadistico/grupo_3.2/EP10"
basename <- "EP10 Datos.csv"
file <- file.path(dir, basename)
poblacion <- read.csv2(file = file)

# -------------------------- PREGUNTAS AL GRUPO --------------------------------

# Grupo 3:
# 1. ¿Existe diferencia en la puntuación dada a las dimensiones de estética y usabilidad entre los usuarios que
# jugaron con un orco como personaje?

# Argumentar o escribir código R para verificar que no se cumplen las condiciones para pruebas paramétricas
# con validez para el caso en estudio.

# H0: No hay diferencia en la puntuacion dada a las dimensiones de estética y usabilidad entre los usuarios que
# jugaron con un orco como personaje
# H1: Hay diferencia en la puntuacion dada a las dimensiones de estética y usabilidad entre los usuarios que
# jugaron con un orco como personaje

orcos <- poblacion %>% filter(Personaje == "Orco")

# Se define un valor de alfa
alfa <- 0.05

# Corresponde a una prueba t de Student con muestras pareadas.

# Se comprueban las condiciones para esta prueba.
# 1. Las observaciones de usuarios pareadas son independientes entre sí.
# 2. Las observaciones provienen de una distribución cercana a la normal.

# Comprobación de normalidad para cada muestra.
shapiro.test(orcos$Estetica)
shapiro.test(orcos$Usabilidad)

# Grafico Q-Q, para visualizar la distribución no normal. 
qqnorm(orcos$Estetica)
qqline(orcos$Estetica)

qqnorm(orcos$Usabilidad)
qqline(orcos$Usabilidad)

# Por el valor p obtenido en los test de Shapiro-Wilk, el problema no cumple las condiciones de prueba paramétrica,
# pues el valor de p en cada muestra es menor al alfa definido (p < alfa). Por esta razón, se usa la prueba de 
# rangos con signo de Wilcoxon, que es el equivalente no paramétrico a la prueba t de student para meustras pareadas.

# Se comprueban las condiciones para esta prueba.
# 1. Las observaciones de usuarios pareadas son independientes entre sí.
# 2. La escala de medición empleada para las observaciones es intrínsecamente continua. Si bien los valores solo son
# considerados del 1 al 5, se cumple que cumple cualquier valor.
# 3. La escala de medición empleada para ambas muestras debe ser a lo menos ordinal. La escala de 1 a 5 se confirma 
# como ordinal.

# Hacer la prueba de rangos con signo de Wilcoxon.
prueba <- wilcox.test(orcos$Estetica, orcos$Usabilidad, alternative = "two.sided", paired = TRUE,
                      conf.level = 1 - alfa)

print(prueba)

# El valor de p = 0.07897 es ligeramente mayor a alfa = 0.05, por lo que se rechaza la hipótesis alternativa
# a favor de la hipótesis nula, lo que significa que no hay diferencia en la puntuacion dada a las dimensiones
# de estética y usabilidad entre los usuarios que jugaron con un orco como personaje con un 95% de confianza.

# 2. ¿Existe diferencia en el tiempo que tardan los usuarios que jugaron con los diferentes tipos de personaje en
# completar el nivel 30? De ser así, ¿entre qué tipos de personaje existen diferencias?