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


# El agricultor piensa rechazar la hipótesis nula cuando la muestra presente un peso medio menor a 108,5
# gramos o mayor a 111,5 gramos. Determine, usando herramientas gráficas, la probabilidad de que cometa
# un error de tipo I.

# Hipotesis
# H0: m = 110
# H1: m != 110

# Calcular la probabilidad de cometer un error de tipo I
# usar funciï¿½n pnorm para calcular la probabilidad acumulada de una variable aleatoria normal estï¿½ndar en un punto especï¿½fico de la distribuciï¿½n
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
g <-g + labs(title = "Ãrea Error tipo 1")
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

# Agregar una lï¿½nea vertical para el valor nulo .
g <- g + geom_vline(aes(xintercept = 110) ,
                           color = "steelblue", linetype = 1)

print(g)


# ////////////////////////// PREGUNTA 2 //////////////////////////


# Suponga ahora que el verdadero peso medio de las manzanas es de 109 gramos. Determine mediante
# herramientas grÃ¡ficas cuÃ¡l serÃ­a la probabilidad de que el agricultor, quien obviamente no conoce
# este dato, cometa un error de tipo II

# Primeramente se establecen las hipÃ³tesis nula y alternativa:
# H0: El peso medio de las manzanas es de 110 gramos 
# HA: El peso medio de las manzanas es menor a 110 gramos

# El equivalente matemÃ¡tico de las hipÃ³tesis anteriores:
# H0: Î¼ = 110
# HA: Î¼ < 110

# Los datos que se poseen son los siguientes:
# TamaÃ±o de la muestra   : 300 manzanas
# DesviaciÃ³n estÃ¡ndar    : 15  gramos
# Peso medio poblacional : 109 gramos

mediaHipotetica    <- 110
mediaVerdadera     <- 109 
tamanioMuestra     <- 300
gradosLibertad     <- tamanioMuestra - 1
valorNulo          <- 110
desviacionEstandar <- 15

# Se trabajarÃ¡ con una prueba de hipÃ³tesis para una muestra (Prueba T de Student) ya que
# se quiere probar una hipÃ³tesis sobre la media poblacional a partir de una muestra
# aleatoria, por lo cual se verifican las condiciones:
# 1) Se cumplela independencia de observaciones y
# 2) La muestra sigue una distribuciÃ³n normal (con un tamaÃ±o superior a 30 muestras)

# Ha de considerarse/asumirse un nivel de significancia del 5%, en consecuencia alpha = 0.05
# Se realiza una prueba unilateral

alpha <- 0.05 

valorCritico <- qt(alpha, gradosLibertad, lower.tail = TRUE)
poderPrueba <- power.t.test(n = tamanioMuestra, delta = (mediaHipotetica - mediaVerdadera)/desviacionEstandar, sd = desviacionEstandar, sig.level = alpha, type = "one.sample", alternative = "one.sided")$power
# 1-poderPrueba = 0.9415 o 94.15% de que el agricultor no detecte una diferencia real en los pesos de las manzanas y acepte incorrectamente la hipÃ³tesis nula


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


# Teniendo en cuenta que en realidad no se conoce el verdadero peso medio, genere ahora un gráfico del
# poder teniendo en cuenta que el agricultor piensa rechazar la hipótesis nula si la muestra presenta un peso
# medio menor a 108,5 gramos o mayor a 111,5 gramos, pero suponiendo ahora que el peso volumen medio
# podría variar entre 109,5 y 110,5 gramos.


# Desviaci?n Est?ndar
desv_estandar <- 15

# Tama?o de la muestra
n <- 300


alfa <- 0.05

medias <- seq(109.5, 110.5, 0.01)
media_nula <- 110 # (109.5 + 110.5) / 2


# Cálculo del efecto
efecto <- (medias - media_nula) / desv_estandar

# Cálculo del poder
resultado <- power.t.test(n = n, 
                          delta = efecto, 
                          sd = desv_estandar, 
                          sig.level = alfa, 
                          type = "one.sample", 
                          alternative = "two.sided")$power


# Se define el gráfico
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
g3 <- g3 + ylab ("Poder estadístico")
g3 <- g3 + xlab ("Tamaño del efecto")

# Título para el gráfico
g3 <- g3 + theme_pubr ()
g3 <- g3 + ggtitle ("Poder v/s tamaño del efecto pesos")
g3 <- g3 + geom_vline ( xintercept = 0, linetype = "dashed")

print(g3)

# El grafico muestra cómo el poder estadístico cambia a medida que el tamaño del efecto cambia. 
# Cuanto mayor es el tamaño del efecto, mayor es el poder estadístico. 
# Por lo tanto, si el gráfico muestra que el poder estadístico es alto para tamaños de efecto en el rango de 109.5 a 110.5, 
# esto quiere decir que el agricultor tiene una alta probabilidad de detectar si el peso medio de las manzanas cae dentro de ese rango, 
# dado su condicion de rechazo de la hipótesis nula. Si el poder estadístico es bajo, esto sugiere que el agricultor tiene una baja 
# probabilidad de detectar si el peso medio de las manzanas cae dentro de ese rango.


























