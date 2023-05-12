# # Grupo 2 EP05
# Integrantes: 
#   - Angel Vilches
#   - Nicolas Valdes
#   - Agustín Henríquez

# Librerias

library(tidyr)
library (ggpubr)
library(ggplot2)
library (dplyr)
library(pwr)
library (tidyverse)

# ////////////////////////// Enunciado //////////////////////////

# El proceso de selecci?n de manzanas para exportaci?n no es sencillo, pues se deben descartar aquellas que
# est?n muy maduras, muy verdes o da?adas. Adicionalmente, cada fruta debe cumplir con un determinado
# calibre, establecido por el mercado, para ser considerada apta para la venta. El calibre de una fruta puede ser
# determinado tanto por su peso como por su di?metro m?ximo ecuatorial. En el caso de las manzanas, el calibre
# m?nimo aceptable es de 60 mm si se mide por el di?metro o de 90 g si se mide por el peso.
# 
# Un agricultor dedicado a la producci?n de manzanas desea determinar, usando una muestra aleatoria de 300
# manzanas, si sus frutas tienen un peso medio de 110 g. Se sabe que el peso de ?stas sigue una distribuci?n
# normal con desviaci?n est?ndar de 15 g

# Desviaci?n Est?ndar
s <- 15

# Tama?o de la muestra
n <- 300

# Error Est?ndar
SE = s / sqrt (n)


# ////////////////////////// PREGUNTA 1 //////////////////////////


# El agricultor piensa rechazar la hip�tesis nula cuando la muestra presente un peso medio menor a 108,5
# gramos o mayor a 111,5 gramos. Determine, usando herramientas gr�ficas, la probabilidad de que cometa
# un error de tipo I.

# Hipotesis
# H0: m = 110
# H1: m != 110

# Calcular la probabilidad de cometer un error de tipo I
# usar funci�n pnorm para calcular la probabilidad acumulada de una variable aleatoria normal est�ndar en un punto espec�fico de la distribuci�n
cola_inferior <- pnorm(mean = 110, sd = SE, q = 108.5, lower.tail = TRUE)
cola_superior <- pnorm(mean = 110, sd = SE, q = 111.5, lower.tail = FALSE)

# Como se trata de una prueba unilateral, el nivel de significacion es igual la cola superior
alfa = cola_inferior + cola_superior

print(alfa)

# R: El nivel de significaci?n corresponde a 0,01046 aproximadamente, lo
# que equivale a un 1,694% de probabilidad de cometer un error de tipo 1.

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


# Suponga ahora que el verdadero peso medio de las manzanas es de 109 gramos. Determine mediante
# herramientas gráficas cuál sería la probabilidad de que el agricultor, quien obviamente no conoce
# este dato, cometa un error de tipo II

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

# Se trabajará con una prueba de hipótesis para una muestra (Prueba T de Student) ya que
# se quiere probar una hipótesis sobre la media poblacional a partir de una muestra
# aleatoria, por lo cual se verifican las condiciones:
# 1) Se cumplela independencia de observaciones y
# 2) La muestra sigue una distribución normal (con un tamaño superior a 30 muestras)

# Ha de considerarse/asumirse un nivel de significancia del 5%, en consecuencia alpha = 0.05
# Se realiza una prueba unilateral

alpha <- 0.05 

valorCritico <- qt(alpha, gradosLibertad, lower.tail = TRUE)
poderPrueba <- power.t.test(n = tamanioMuestra, delta = (mediaHipotetica - mediaVerdadera)/desviacionEstandar, sd = desviacionEstandar, sig.level = alpha, type = "one.sample", alternative = "one.sided")$power
# 1-poderPrueba = 0.9415 o 94.15% de que el agricultor no detecte una diferencia real en los pesos de las manzanas y acepte incorrectamente la hipótesis nula


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


# Teniendo en cuenta que en realidad no se conoce el verdadero peso medio, genere ahora un gr�fico del
# poder teniendo en cuenta que el agricultor piensa rechazar la hip�tesis nula si la muestra presenta un peso
# medio menor a 108,5 gramos o mayor a 111,5 gramos, pero suponiendo ahora que el peso volumen medio
# podr�a variar entre 109,5 y 110,5 gramos.


# Desviaci?n Est?ndar
desv_estandar <- 15

# Tama?o de la muestra
n <- 300


alfa <- 0.05

medias <- seq(109.5, 110.5, 0.01)
media_nula <- 110 # (109.5 + 110.5) / 2


# C�lculo del efecto
efecto <- (medias - media_nula) / desv_estandar

# C�lculo del poder
resultado <- power.t.test(n = n, 
                          delta = efecto, 
                          sd = desv_estandar, 
                          sig.level = alfa, 
                          type = "one.sample", 
                          alternative = "two.sided")$power


# Se define el gr�fico
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
g3 <- g3 + ylab ("Poder estad�stico")
g3 <- g3 + xlab ("Tama�o del efecto")

# T�tulo para el gr�fico
g3 <- g3 + theme_pubr ()
g3 <- g3 + ggtitle ("Poder v/s tama�o del efecto pesos")
g3 <- g3 + geom_vline ( xintercept = 0, linetype = "dashed")

print(g3)

# El grafico muestra c�mo el poder estad�stico cambia a medida que el tama�o del efecto cambia. 
# Cuanto mayor es el tama�o del efecto, mayor es el poder estad�stico. 
# Por lo tanto, si el gr�fico muestra que el poder estad�stico es alto para tama�os de efecto en el rango de 109.5 a 110.5, 
# esto quiere decir que el agricultor tiene una alta probabilidad de detectar si el peso medio de las manzanas cae dentro de ese rango, 
# dado su condicion de rechazo de la hip�tesis nula. Si el poder estad�stico es bajo, esto sugiere que el agricultor tiene una baja 
# probabilidad de detectar si el peso medio de las manzanas cae dentro de ese rango.

























