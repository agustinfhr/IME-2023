# Grupo 4
# Integrantes: 
# 
#  Nicolas Valdes
#  Agustín Henríquez


if (!require ( car )) {
  install.packages ("car" , dependencies=TRUE)
  require (car)
}


# Importación de las librerias 

library(car)


# ------------------------------ PROBLEMA --------------------------------------

# Grupo 4:
#   1. Tras el éxito obtenido recientemente por un equipo de agrónomos en la mejora del cultivo de manzanas de
# exportación, con la ayuda de algunos modelos estadísticos, un colega dedicado a la producción de peras
# desea estudiar algunas características de sus productos. Para ello, ha registrado los pesos (en gramos) de
# algunas unidades (cada una de un árbol diferente) de dos variedades distintas, Winter Nelly y Golden Bosc,
# durante la semana 15 de crecimiento. Desea saber si el peso de ambas variedades en esta etapa de su
# desarrollo es el mismo. ¿Qué puede concluir?

# -- Obtienen correctamente los datos necesarios para realizar la prueba solicitada, en un formato pertinente.
# ------------------------------- DATOS ----------------------------------------
# Primero, se necesita cargar los datos en R.

Winter_Nelly <- c(213.41, 185.9876, 221.9353, 320.1673, 201.3828, 180.1779, 168.4547, 222.9346, 183.4629)
Golden_Bosc <- c(170.8754, 144.3119, 166.2316, 114.3064, 112.9957, 126.6177, 117.5733, 128.7313, 143.4207, 210.4415, 138.7852, 272.3915)

# -- Argumentan convincentemente el incumplimiento de todas las condiciones requeridas para aplicar una prueba robusta adecuada.

# Prueba de Shapiro-Wilk para comprobar normalidad
print(shapiro.test(Winter_Nelly))
print(shapiro.test(Golden_Bosc))
# Ambos p-valores son menores que 0.05, se rechaza la hipótesis nula de que los datos 
# siguen una distribución normal. Por lo tanto, los datos no cumplen con una distribución normal.

# Se realiza un histograma y un Q-Q plot para visualizar si los datos siguen una distribución normal.
# Winter Nelly.
par(mfrow=c(2, 1))
hist(Winter_Nelly, main = "Winter Nelly", xlab = "Peso en gramos")
qqnorm(Winter_Nelly); qqline(Winter_Nelly)

# Golden Bosc.
par(mfrow=c(2, 1))
hist(Golden_Bosc, main = "Golden Bosc", xlab = "Peso en gramos")
qqnorm(Golden_Bosc); qqline(Golden_Bosc)

# A partir de ambos gráficos, se puede obversar que los datos no tienden a distribuirse de forma normal.

# -- Seleccionan, argumentando con gráficos u otras herramientas, una transformación apropiada para poder
# aplicar una prueba paramétrica a los datos.

# Los datos originales no siguen una distribución normal, como lo indica la prueba de Shapiro-Wilk. 
# Ante esta situación, decidimos realizar una transformación logarítmica, debido a que este tipo de transformación es útil 
# para manejar datos con distribuciones sesgadas, en este caso, presentan un sesgo positivo (derecho), 
# ayudando a estabilizar la varianza y a normalizar la distribución de los datos.

# Transformar logarítmica de los datos.
Winter_Nelly_log <- log(Winter_Nelly)
Golden_Bosc_log <- log(Golden_Bosc)

# Graficar los datos transformados.
par(mfrow = c(1, 2))
hist(Winter_Nelly_log, main = "Winter Nelly Log-Transformed", xlab = "Log-Weight")
hist(Golden_Bosc_log, main = "Golden Bosc Log-Transformed", xlab = "Log-Weight")

# -- Formulan con claridad y explícitamente hipótesis nulas y alternativas adecuadas para responder la pregunta
# planteada, tanto en lenguaje natural como en lenguaje matemático.

# Hipótesis propuesta para el problema
# Hipótesis nula (H0): No hay diferencia en las medias logarítmicas de peso entre las variedades Winter Nelly y Golden Bosc.
# 
# Hipótesis alternativa (H1): Existe una diferencia en las medias logarítmicas de peso entre las variedades Winter Nelly y Golden Bosc.

# Formulación Matemática
# H0: mu_WN = mu_GB

# HA: mu_WN != mu_GB

# Verificar la normalidad con la prueba de Shapiro-Wilk.
print(shapiro.test(Winter_Nelly_log))
print(shapiro.test(Golden_Bosc_log))

# Tras realizar la transformación logarítmica, las pruebas de Shapiro-Wilk indican una mejor aproximación a la normalidad,
# permitiéndonos así utilizar pruebas estadísticas paramétricas.
# Los p-valores son ahora mayores que 0.05, lo que sugiere que no podemos rechazar la hipótesis nula de que los datos siguen 
# una distribución normal. Por lo tanto, podemos aplicar una prueba paramétrica como la prueba t.

# -- Realizan de forma completa y correcta una prueba paramétrica adecuada para la pregunta planteada en el
# enunciado, utilizando los datos correctos.

# Prueba t de Student para evaluar la normalidad de los datos.
t.test(Winter_Nelly_log, Golden_Bosc_log, var.equal = TRUE)

# La prueba t para muestras independientes sobre los datos transformados logarítmicamente resulta en un p-valor de 0.004805. 
# Este es un resultado estadísticamente significativo (p < 0.05), lo que se puede rechazar la hipótesis nula de que las medias 
# logarítmicas de peso de las variedades Winter Nelly y Golden Bosc son iguales.
# 
# Por lo tanto, podemos concluir que, basándonos en la transformación logarítmica de los datos, existe una diferencia significativa 
# en las medias logarítmicas de peso entre las variedades Winter Nelly y Golden Bosc.

# -- Entregan una conclusión completa a la pregunta planteada en el enunciado, en el dominio original de los
# datos, basándose en el resultado correcto de una prueba adecuada y el contexto del problema.

# Conclusión:
# Los datos originales no se distribuyeron normalmente, lo que nos llevó a aplicar una transformación logarítmica. 
# Después de transformar los datos, realizamos una prueba t y obtuvimos un p-valor de 0.004805.
# Este valor es menor que 0.05, lo que nos lleva a rechazar la hipótesis nula de que no hay diferencia en las medias.
# Por lo tanto, aceptamos la hipótesis alternativa de que hay una diferencia significativa entre las medias logarítmicas de los pesos de las variedades de peras Winter_Nelly y Golden_Bosc.
# En el dominio original de los datos, esto implica que las peras de la variedad Winter_Nelly tienden a ser más pesadas que las de la variedad Golden_Bosc.
# Desde el punto de vista práctico, estos hallazgos podrían ser útiles para los productores de peras, quienes podrían considerar cultivar más peras de la variedad Winter_Nelly si el peso de las peras es un factor importante para la venta de sus productos.





