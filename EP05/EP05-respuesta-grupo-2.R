# # Grupo 2 EP05
# Integrantes: 
#   - Angel Vilches
#   - Nicolas Valdes
#   - AgustÃ­n HenrÃ­quez

# Librerias

library(tidyr)
library (ggpubr)
library(ggplot2)
library (dplyr)
library(pwr)
library (tidyverse)

# ////////////////////////// Enunciado //////////////////////////

# El proceso de selección de manzanas para exportación no es sencillo, pues se deben descartar aquellas que
# estén muy maduras, muy verdes o dañadas. Adicionalmente, cada fruta debe cumplir con un determinado
# calibre, establecido por el mercado, para ser considerada apta para la venta. El calibre de una fruta puede ser
# determinado tanto por su peso como por su diámetro máximo ecuatorial. En el caso de las manzanas, el calibre
# mínimo aceptable es de 60 mm si se mide por el diámetro o de 90 g si se mide por el peso.
# 
# Un agricultor dedicado a la producción de manzanas desea determinar, usando una muestra aleatoria de 300
# manzanas, si sus frutas tienen un peso medio de 110 g. Se sabe que el peso de éstas sigue una distribución
# normal con desviación estándar de 15 g

# Desviación Estándar
s <- 15

# Tamaño de la muestra
n <- 300

# Error Estándar
SE = s / sqrt (n)


# ////////////////////////// PREGUNTA 1 //////////////////////////


# El agricultor está seguro de que el verdadero peso medio no puede ser superior a 110 gramos y piensa
# rechazar la hipótesis nula cuando la muestra presente un peso medio menor a 108 gramos. Determine,
# usando herramientas gráficas, la probabilidad de que cometa un error de tipo I.

# Hipotesis
# H0: m = 110
# H1: m != 110

# Calcular la probabilidad de cometer un error de tipo I
# usar función pnorm para calcular la probabilidad acumulada de una variable aleatoria normal estándar en un punto específico de la distribución
cola_inferior <- pnorm(mean = 110, sd = SE, q = 108.5, lower.tail = TRUE)
cola_superior <- pnorm(mean = 110, sd = SE, q = 111.5, lower.tail = FALSE)

# Como se trata de una prueba unilateral, el nivel de significacion es igual la cola superior
alfa = cola_inferior + cola_superior

print(alfa)

# R: El nivel de significación corresponde a 0,01046 aproximadamente, lo
# que equivale a un 1,694% de probabilidad de cometer un error de tipo 1.

# Gráfico área error tipo 1
x <- seq(110 - s*SE,110 + s*SE,0.01)
y <- dnorm(x, mean = 110 , sd = SE)
g <- ggplot(data = data.frame(x,y), aes(x))
g <- g + stat_function(
  fun = dnorm,
  n = 300,
  args = list(mean = 110 , sd = SE),
  colour = "steelblue", size = 1)
g <- g + ylab("Densidad")
g <-g + xlab("Dureza")
g <-g + labs(title = "Área Error tipo 1")
g <- g + theme_pubr()
g <- g + geom_area(data = subset(data.frame(x,y), x < 108.5), 
                   aes(y = y),
                   colour = "red",
                   fill = "red",
                   alpha = 0.5)


g <- g + geom_area(data = subset(data.frame(x,y), x > 111.5), 
                   aes(y = y),
                   colour = "red",
                   fill = "red",
                   alpha = 0.5)

# Agregar una línea vertical para el valor nulo .
g <- g + geom_vline(aes(xintercept = 110) ,
                           color = "steelblue", linetype = 1)

print(g)


# ////////////////////////// PREGUNTA 2 //////////////////////////


# Suponga ahora que el verdadero peso medio de las manzanas es de 109,5 gramos. Determine mediante
# herramientas gráficas cuál sería la probabilidad de que el agricultor, quien obviamente no conoce este dato,
# cometa un error de tipo II.











