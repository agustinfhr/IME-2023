# # Grupo 2 EP06
# Integrantes: 
#   - Angel Vilches
#   - Nicolas Valdes
#   - Agustín Henríquez

# Librerias
library(pwr)

if(!require(Hmisc)){
  install.packages("Hmisc",dependencies = TRUE)
  require(Hmisc)
}

# ////////////////////////// Enunciado ///////////////////////////

# ////////////////////////// Pregunta 1 //////////////////////////
# Estudios previos habían determinado que menos de 40% de los hombres
# encuestados son fanáticos moderados. ¿Respaldan estos datos tal estimación?

# Se plantea la hipótesis nula y alternativa con respectiva notación matemática:
# H0: La proporción de hombres fanáticos moderados es igual a 40%.
# H1: La proporción de hombres fanáticos moderados es menor al 40%.

# H0: p = 0.4 
# H1: p < 0.4

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

# De acuerdo con los resultados obtenidos, dado que p-valor es mayor que alfa se
# falla en rechazar la hipótesis nula.
# Por lo tanto, los datos respaldan que, con un 95% de confianza, una proporción
# no menor del 40% de los hombres encuestados son fanáticos moderados

# ////////////////////////// Pregunta 2 //////////////////////////

# Según estos datos, ¿es igual la proporción de fanáticos (sean moderados
# o extremos) en hombres y en mujeres?

# Se plantea la hipítesis nula y alternativa con respectiva notación matemática:
# H0: La proporción de fanáticos en hombres es igual a la de mujeres.
# H1: La proporción de fanáticos en hombres no es igual a la de mujeres.

# H0: p1 =  p2
# H1: p1 != p2

# Con respecto a las verificaciones, ya se cumplen para la pregunta 1,
# por lo cual se cumplen para la pregunta 2

# Declarar variables conocidas:
nHombres    <- 422 + 502 + 396  # Cantidad total de hombres 
nMujeres    <- 158 + 291 + 431  # Cantidad total de mujeres
nTotal      <- nHombres + nMujeres
alfa        <- 0.05  # Nivel de significación

# Utilizar la prueba de proporciones de dos muestras:
exitosHombres <- 422 + 502 # hombres que son fanáticos
exitosMujeres <- 158 + 291 # mujeres que son fanáticos

pruebaProporciones <- prop.test(c(exitosHombres, exitosMujeres), 
                                n=c(nHombres, nMujeres), 
                                alternative="two.sided", 
                                conf.level=1-alfa)
print(pruebaProporciones)

# De acuerdo con los resultados obtenidos:
# El valor p es extremadamente pequeño, menor que el nivel de significancia (0.05),
# esto significa que se rechaza la hipótesis nula respecto de que las proporciones de 
# fanáticos entre hombres y mujeres son iguales. Por lo tanto, hay una diferencia significativa
# entre las proporciones de fanáticos entre hombres y mujeres, es decir, la proporción de fanáticos de Star Wars  
# no es la misma para hombres y mujeres, siendo más alta para los hombres.

# ////////////////////////// Pregunta 3 //////////////////////////

# Existe la creencia de que hay más fanáticos extremos entre los hombres que entre las mujeres y que dicha
# diferencia supera el 25%. ¿A cuántas personas (hombres y mujeres) se debería encuestar para obtener un
# intervalo de confianza del 97,5% y poder estadístico de 80%, si se intenta mantener aproximadamente la
# misma proporción de gente estudiada en cada caso?

int_confianza = 0.975
poder = 0.80

# Para poder desarrollar este ejercicio primero se debe calcular cual es la proporción tanto para fanáticos 
# extremos hombres como mujeres, los cuales se calcular de la siguiente forma:

prop_ext_mujeres <- 158 / (158 + 422)
prop_ext_hombres <- 422 / (158 + 422)

# Ahora se debe establecer una proporción esperada, para ello restaremos las proporciones de fanáticos hombres
# y mujeres, mostramos en pantalla su resultado para comprobar de que es mayor a 25%

prop_esperada = abs(prop_ext_hombres - prop_ext_mujeres)
cat("Valor esperado de proporción: ")
print(prop_esperada)

# Una vez calculada la proporcion esperada y rectificada de que sea mayor a 0.25, se utilizada la función 
# pwr.2p.test para calcular la cantidad de personas que se deben encuestar
n_3 <- pwr.2p.test(h = prop_esperada,
                   power = poder,
                   sig.level = 1 - int_confianza)$n

# En este caso h representa la diferencia de proporciones esperada, en este caso 45% aproximadamente,
# sig.level representa el nivel de significancia y power el poder

print(n_3)
  
# Observando los resultados anteriores se tiene que la cantidad de personas (hombres y mujeres) que se deben 
# encuestar para conservar una proporción mayor a 25%, un intervalo de confianza de 97,5% y un poder de 80%, es aproximadamente de 92 personas