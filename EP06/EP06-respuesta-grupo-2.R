# # Grupo 2 EP06
# Integrantes: 
#   - Angel Vilches
#   - Nicolas Valdes
#   - Agust铆n Henr铆quez

# Librerias

if(!require(Hmisc)){
  install.packages("Hmisc",dependencies = TRUE)
  require(Hmisc)
}

# ////////////////////////// Enunciado ///////////////////////////

# ////////////////////////// Pregunta 1 //////////////////////////
# Estudios previos hab铆an determinado que menos de 40% de los hombres encuestados son fan谩ticos
# moderados. 驴Respaldan estos datos tal estimaci贸n?

# Se plantea la hip贸tesis nula y alternativa con respectiva notaci贸n matem谩tica:
# H0: La proporci贸n de hombres fan谩ticos moderados es igual o menor al 40%.
# H1: La proporci贸n de hombres fan谩ticos moderados es mayor al 40%.

# H0: p <= 0.4 
# H1: p >  0.4

# Se verifica el cumplimiento de condiciones:
# Se cumple la primera condici贸n puesto que las observaciones de la muestra son independientes
# Se cumple la segunda condici贸n puesto que se poseen al menos 10 observaciones asociadas a una
# situaci贸n de 茅xito y fracaso.

# Se declaran variables conocidas:
n         <- 793   # Cantidad total de hombres y mujeres que figuran en el fan谩tismo moderado
probExito <- 502/n # Probabilidad de elegir a un hombre fanatico moderado
valorNulo <- 0.4
alfa      <- 0.05  # Nivel de significaci贸n

# Se utilizar谩 la prueba de Wilson dada sus ventajas, tales como la robustez, seg煤n la literatura:
exitos       <- probExito*n
pruebaWilson <- prop.test(exitos, n=n, alternative="less", conf.level=1-alfa)
print(pruebaWilson)

# De acuerdo con los resultados obtenidos, dado que p-valor es menor que alfa se
# rechaza la hip贸tesis nula en favor de la hip贸tesis alternativa.
# Por lo tanto, los datos respaldan que m谩s del 40% de los hombres encuestados son
# fan谩ticos moderados

# ////////////////////////// Pregunta 2 //////////////////////////

# Seg鷑 estos datos, 縠s igual la proporci髇 de fan醫icos (sean moderados o extremos) en hombres y en
# mujeres?

# Se plantea la hip髏esis nula y alternativa con respectiva notaci髇 matem醫ica:
# H0: La proporci髇 de fan醫icos en hombres es igual a la de mujeres.
# H1: La proporci髇 de fan醫icos en hombres no es igual a la de mujeres.

# H0: p1 = p2
# H1: p1 ??? p2

# Con respecto a las verificaciones, ya se cumplen para la pregunta 1, por lo cual se cumplen para la pregunta 2

# Declarar variables conocidas:
nHombres    <- 422 + 502 + 396  # Cantidad total de hombres 
nMujeres    <- 158 + 291 + 431  # Cantidad total de mujeres
nTotal      <- nHombres + nMujeres
alfa        <- 0.05  # Nivel de significaci髇

# Utilizar la prueba de proporciones de dos muestras:
exitosHombres <- 422 + 502 # hombres que son fan醫icos
exitosMujeres <- 158 + 291 # mujeres que son fan醫icos

pruebaProporciones <- prop.test(c(exitosHombres, exitosMujeres), 
                                n=c(nHombres, nMujeres), 
                                alternative="two.sided", 
                                conf.level=1-alfa)
print(pruebaProporciones)

# De acuerdo con los resultados obtenidos:
# El valor p es extremadamente peque駉, menor que el nivel de significancia (0.05),
# esto significa que se puede rechazar la hip髏esis nula de que las proporciones de 
# fan醫icos entre hombres y mujeres son iguales. Por lo tanto, hay una diferencia significativa
# entre las proporciones de fan醫icos entre hombres y mujeres, es decir, la proporci髇 de fan醫icos de Star Wars  
# no es la misma para hombres y mujeres, siendo m醩 alta para los hombres.

