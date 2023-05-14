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











