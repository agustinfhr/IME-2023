# # Grupo 2 EP05
# Integrantes: 
#   - Angel Vilches
#   - Nicolas Valdes
#   - Agustín Henríquez

# Librerías

library(tidyr)
library (ggpubr)
library(ggplot2)
library (dplyr)
library(pwr)
library (tidyverse)

# ////////////////////////// Enunciado //////////////////////////

# El proceso de selección de manzanas para exportación no es sencillo, pues se
# deben descartar aquellas que están muy maduras, muy verdes o dañadas.
# Adicionalmente, cada fruta debe cumplir con un determinado calibre,
# establecido por el mercado, para ser considerada apta para la venta.
# El calibre de una fruta puede ser determinado tanto por su peso como por su
# diámetro máximo ecuatorial. En el caso de las manzanas, el calibre mínimo
# aceptable es de 60 mm si se mide por el diámetro o de 90 g si se mide por
# el peso.
# 
# Un agricultor dedicado a la producción de manzanas desea determinar, usando
# una muestra aleatoria de 300 manzanas, si sus frutas tienen un peso medio
# de 110 g. Se sabe que el peso de éstas sigue una distribución normal con
# desviación estándar de 15 g.

# Desviación Estándar
s <- 15

# Tamaño de la muestra
n <- 300

# Error Estándar
SE = s / sqrt(n)

# ////////////////////////// PREGUNTA 1 //////////////////////////

# El agricultor piensa rechazar la hipótesis nula cuando la muestra presente
# un peso medio menor a 108,5 gramos o mayor a 111,5 gramos. Determine, usando
# herramientas gráficas, la probabilidad de que cometa un error de tipo I.

# Hipótesis
# H0: Las manzanas tienen un peso medio igual a 110 gramos
# HA: Las manzanas tienen un peso medio diferente de 110 gramos

# Matemáticamente
# H0: mu  = 110
# HA: mu != 110

# Debemos recordar que el error de tipo I refiere a rechazar H0 cuando en 
# realidad es verdadera. 

# Igualmente considerar que se trata de una prueba bilateral al considerar:
# "un peso medio menor a 108.5 gramos o mayor a 111.5"

# Cálcular la probabilidad de cometer un error de tipo I
# Usar función pnorm para cálcular la probabilidad acumulada de una variable
# aleatoria normal estándar en un punto específico de la distribución
cola_inferior <- pnorm(mean = 110, sd = SE, q = 108.5, lower.tail = TRUE)
cola_superior <- pnorm(mean = 110, sd = SE, q = 111.5, lower.tail = FALSE)

# Como se trata de una prueba bilateral, el nivel de significacion es igual la cola inferior más la cola superior
alfa = cola_inferior + cola_superior

print(alfa)

# R: El nivel de significación corresponde a 0,08326 aproximadamente, lo
# que equivale a un 8,326% de probabilidad de cometer un error de tipo 1.

# Gr?fico ?rea error tipo 1
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

# Agregar una l�nea vertical para el valor nulo .
g <- g + geom_vline(aes(xintercept = 110) ,
                           color = "steelblue", linetype = 1)

print(g)


# ////////////////////////// PREGUNTA 2 //////////////////////////

# Suponga ahora que el verdadero peso medio de las manzanas es de 109 gramos.
# Determine mediante herramientas gráficas cuál sería la probabilidad de que
# el agricultor, quien obviamente no conoce este dato, cometa un error de
# tipo II

# Primeramente se establecen las hipótesis nula y alternativa:
# H0: El peso medio de las manzanas es de 110 gramos 
# HA: El peso medio de las manzanas es menor a 110 gramos

# El equivalente matemático de las hipótesis anteriores:
# H0: μ = 110
# HA: μ < 110

# Los datos que se poseen son los siguientes:
# Tamaño de la muestra   : 300 manzanas
# Desviación estándar    : 15  gramos
# Peso medio poblacional : 109 gramos

mediaHipotetica    <- 110
mediaVerdadera     <- 109 
tamanioMuestra     <- 300
gradosLibertad     <- tamanioMuestra - 1
valorNulo          <- 110
desviacionEstandar <- 15

# Se trabajará con una prueba de hipótesis para una muestra (Prueba T de
# Student) ya que se quiere probar una hipótesis sobre la media poblacional
# a partir de una muestra aleatoria, por lo cual se verifican las condiciones:
# 1) Se cumple la independencia de observaciones y
# 2) La muestra sigue una distribución normal (con un tamaño superior a
#    30 muestras)

# Ha de considerarse/asumirse un nivel de significancia del 5%,
# en consecuencia alpha = 0.05. 

# Se realiza una prueba unilateral

alpha <- 0.05 

valorCritico <- qt(alpha, gradosLibertad, lower.tail = TRUE)
poderPrueba <- power.t.test(n = tamanioMuestra,
                            delta = (mediaHipotetica - mediaVerdadera) / desviacionEstandar,
                            sd = desviacionEstandar,
                            sig.level = alpha,
                            type = "one.sample",
                            alternative = "one.sided")$power
# 1-poderPrueba = 0.9415 o 94.15% de que el agricultor no detecte una
# diferencia real en los pesos de las manzanas y acepte incorrectamente
# la hipótesis nula

# Se procede a gráficar
g2 <- ggplot(data.frame(x = c(-4, 4)), aes(x)) + 
  stat_function(fun = dt, args = list(df = gradosLibertad)) +
  geom_vline(xintercept = valorCritico, linetype = "dashed") +
  labs(x = "Valor t", y = "Densidad", title = "Diagrama t de Student") +
  theme_bw()

print(g2)

# De acuerdo a lo calculado y graficado, la prueba tiene un poder bajo, es decir, existe una alta
# probabilidad de comenter un error de tipo II, vale decir, no rechazar H0 cuando HA es verdadera.
# Por lo tanto, el agricultor tiene una baja probabilidad de detectar que el peso medio de sus manzanas
# es menor a 110 gramos.
             

# ////////////////////////// PREGUNTA 3 //////////////////////////


# Teniendo en cuenta que en realidad no se conoce el verdadero peso medio, genere ahora un gr?fico del
# poder teniendo en cuenta que el agricultor piensa rechazar la hip?tesis nula si la muestra presenta un peso
# medio menor a 108,5 gramos o mayor a 111,5 gramos, pero suponiendo ahora que el peso volumen medio
# podr?a variar entre 109,5 y 110,5 gramos.


# Desviaci?n Est?ndar
desv_estandar <- 15

# Tama?o de la muestra
n <- 300


alfa <- 0.05

medias <- seq(109.5, 110.5, 0.01)
media_nula <- 110 # (109.5 + 110.5) / 2


# C?lculo del efecto
efecto <- (medias - media_nula) / desv_estandar

# C?lculo del poder
resultado <- power.t.test(n = n, 
                          delta = efecto, 
                          sd = desv_estandar, 
                          sig.level = alfa, 
                          type = "one.sample", 
                          alternative = "two.sided")$power


# Se define el gr?fico
datos <- data.frame(efecto, resultado)
datos <- datos %>% pivot_longer(!"efecto", 
                                names_to = "fuente", 
                                values_to = "poder")
niveles <- c("resultado")
etiquetas <- c("resultado")
datos[["fuente"]] <- factor(datos[["fuente"]], 
                            levels = niveles, 
                            labels = etiquetas)

g3 <- ggplot (datos, aes(efecto, poder, colour = factor (fuente)))
g3 <- g3 + geom_line()
g3 <- g3 + labs ( colour = "")
g3 <- g3 + ylab ("Poder estad?stico")
g3 <- g3 + xlab ("Tama?o del efecto")

# T?tulo para el gr?fico
g3 <- g3 + theme_pubr ()
g3 <- g3 + ggtitle ("Poder v/s tama?o del efecto pesos")
g3 <- g3 + geom_vline ( xintercept = 0, linetype = "dashed")

print(g3)

# El grafico muestra c?mo el poder estad?stico cambia a medida que el tama?o del efecto cambia. 
# Cuanto mayor es el tama?o del efecto, mayor es el poder estad?stico. 
# Por lo tanto, si el gr?fico muestra que el poder estad?stico es alto para tama?os de efecto en el rango de 109.5 a 110.5, 
# esto quiere decir que el agricultor tiene una alta probabilidad de detectar si el peso medio de las manzanas cae dentro de ese rango, 
# dado su condicion de rechazo de la hip?tesis nula. Si el poder estad?stico es bajo, esto sugiere que el agricultor tiene una baja 
# probabilidad de detectar si el peso medio de las manzanas cae dentro de ese rango.


# ////////////////////////// PREGUNTA 4 //////////////////////////
# 
# Considerando un peso medio verdadero de 109,5 gramos, calcule usando funciones de R (o alguno de sus
# paquetes) cuántas manzanas deberían revisarse para conseguir un poder estadístico de 0,85 y un nivel de
# significación de 0,05.

# Para la resolución de la presente interrogante, ha de considerarse la función pwr.norm.test() dado que
# permitiría el cumplimiento de las condiciones establecidas de acuerdo a parámetros que consideran un
# delta ((media verdadera-media de la muestra)/sd/sqrt(nOriginal)), sd (desviación estándar), sig.level (nivel de significación
# deseado), power (poder estadístico deseado) y n como el tamaño de la muestra, entendiendose que este valor
# es desconocido. En este sentido:

# Tamaño de muestra necesario para alcanzar un poder estadístico de 0.85
# y un nivel de significación de 0.05 para una prueba de hipótesis con
# media poblacional de 109.5 g y desviación estándar poblacional de 15 g.

pwr <- pwr.norm.test(n = NULL, d = (109.5 - 110)/(15/sqrt(300)), sig.level = 0.05, power = 0.85)
cat("Por lo tanto, el número de manzanas quedeberían revisarse para conseguir un poder estadístico de 0,85 y un nivel de significación de 0,05 es de: ")
print(pwr)
cat("donde n es aproximadamente 27, es decir, 27 manzanas son requeridas para obtener los parámetros establecidos")


# ////////////////////////// PREGUNTA 5 //////////////////////////
#
# Repita el ejercicio de la pregunta anterior, suponiendo ahora que el agricultor es muy exigente y desea
# reducir la probabilidad de cometer un error de tipo I a un 1% solamente?

# Para éste ejercicio se tiene que el nivel de significancia sig.level es de 0.01 para que se cumpla con lo solicitado
# se repite el mismo procedimiento teniendo un delta ((media verdadera-media de la muestra)/sd/sqrt(nOriginal)), sig.level
# de 0.01, un poder de 0.85 y n como la variable incógnita

pwr_5 <- pwr.norm.test(n = NULL,
                     d = (109.5 - 110)/(15/sqrt(300)),
                     sig.level = 0.01,
                     power = 0.85)

print(pwr_5)

# Con lo anterior se tiene que la cantidad de manzanas que deberían revisarse para conseguir un 
# poder estadistico de 85% y que solo haya un 1% de probabilidad de cometer un error de tipo I es de
# aproximadamente 39 manzanas 