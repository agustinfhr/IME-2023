# # Grupo 2 EP06
# Integrantes: 
#   - Angel Vilches
#   - Nicolas Valdes
#   - Agustín Henríquez

# Librerias

if(!require(Hmisc)){
  install.packages("Hmisc",dependencies = TRUE)
  require(Hmisc)
}

# ////////////////////////// Enunciado ///////////////////////////

# ////////////////////////// Pregunta 1 //////////////////////////
# Estudios previos habían determinado que menos de 40% de los hombres encuestados son fanáticos
# moderados. ¿Respaldan estos datos tal estimación?

# Se plantea la hipótesis nula y alternativa con respectiva notación matemática:
# H0: La proporción de hombres fanáticos moderados es igual o menor al 40%.
# H1: La proporción de hombres fanáticos moderados es mayor al 40%.

# H0: p <= 0.4 
# H1: p >  0.4

# Se verifica el cumplimiento de condiciones:
# Se cumple la primera condición puesto que las observaciones de la muestra son independientes
# Se cumple la segunda condición puesto que se poseen al menos 10 observaciones asociadas a una
# situación de éxito y fracaso.

# Se declaran variables conocidas:
n         <- 793   # Cantidad total de hombres y mujeres que figuran en el fanátismo moderado
probExito <- 502/n # Probabilidad de elegir a un hombre fanatico moderado
valorNulo <- 0.4
alfa      <- 0.05  # Nivel de significación

# Se utilizará la prueba de Wilson dada sus ventajas, tales como la robustez, según la literatura:
exitos       <- probExito*n
pruebaWilson <- prop.test(exitos, n=n, alternative="less", conf.level=1-alfa)
print(pruebaWilson)

# De acuerdo con los resultados obtenidos, dado que p-valor es menor que alfa se
# rechaza la hipótesis nula en favor de la hipótesis alternativa.
# Por lo tanto, los datos respaldan que más del 40% de los hombres encuestados son
# fanáticos moderados

# ////////////////////////// Pregunta 2 //////////////////////////

# Seg�n estos datos, �es igual la proporci�n de fan�ticos (sean moderados o extremos) en hombres y en
# mujeres?

# Se plantea la hip�tesis nula y alternativa con respectiva notaci�n matem�tica:
# H0: La proporci�n de fan�ticos en hombres es igual a la de mujeres.
# H1: La proporci�n de fan�ticos en hombres no es igual a la de mujeres.

# H0: p1 = p2
# H1: p1 ??? p2

# Con respecto a las verificaciones, ya se cumplen para la pregunta 1, por lo cual se cumplen para la pregunta 2

# Declarar variables conocidas:
nHombres    <- 422 + 502 + 396  # Cantidad total de hombres 
nMujeres    <- 158 + 291 + 431  # Cantidad total de mujeres
nTotal      <- nHombres + nMujeres
alfa        <- 0.05  # Nivel de significaci�n

# Utilizar la prueba de proporciones de dos muestras:
exitosHombres <- 422 + 502 # hombres que son fan�ticos
exitosMujeres <- 158 + 291 # mujeres que son fan�ticos

pruebaProporciones <- prop.test(c(exitosHombres, exitosMujeres), 
                                n=c(nHombres, nMujeres), 
                                alternative="two.sided", 
                                conf.level=1-alfa)
print(pruebaProporciones)

# De acuerdo con los resultados obtenidos:
# El valor p es extremadamente peque�o, menor que el nivel de significancia (0.05),
# esto significa que se puede rechazar la hip�tesis nula de que las proporciones de 
# fan�ticos entre hombres y mujeres son iguales. Por lo tanto, hay una diferencia significativa
# entre las proporciones de fan�ticos entre hombres y mujeres, es decir, la proporci�n de fan�ticos de Star Wars  
# no es la misma para hombres y mujeres, siendo m�s alta para los hombres.
