# Grupo 3
# Integrantes: 
# Jaime Carrasco
# Pablo Villarreal Ortiz
# Agustin Henriquez Rojas

if (!require(ggpubr) ) {
  install.packages("ggpubr", dependencies = TRUE )
  require (ggpubr)
}

if (!require(dplyr) ) {
  install.packages("dplyr", dependencies = TRUE )
  require (dplyr)
}


# Explortacion de las librerias 

library(dplyr)
library(ggpubr)

# ------------------------------ PROBLEMA --------------------------------------

# Como habíamos visto a comienzos del semestre, la Encuesta de Caracterización Socioeconómica Nacional,
# Casen, es realizada por el Ministerio de Desarrollo Social de forma periódica para conocer la situación de los
# hogares chilenos con relación a aspectos demográficos, de educación, salud, vivienda, trabajo e ingresos. Es la
# principal fuente de información para estimar la magnitud de la pobreza y la distribución del ingreso en el país.
# Se pone a disposición el archivo EP11 Datos.csv, con un subconjunto de los datos obtenidos en la Encuesta
# Casen 2017. El equipo debe revisar las columnas disponibles en este archivo según la descripción en el libro de
# códigos de la encuesta, que también queda disponible para este ejercicio bajo el nombre de EP11 Diccionario de
# datos. Es importante notar que:
# ▪ En esta encuesta hay datos de carácter colectivo sobre “el hogar” del entrevistado, pero también hay datos
# de carácter individual, que se refieren “al jefe o la jefa de hogar” (no al entrevistado).
# ▪ El conjunto de datos entregado no incluye todas las variables descritas en el libro de códigos.
#  1. Copiar el enunciado de los problemas asignados como comentarios de un script R.
#  2. Descargar desde UVirtual el archivo EP11 Datos.csv con los datos a emplear.
#  3. Obtener las muestras que se piden, revisarlas gráficamente y comentar la necesidad de aplicar métodos
#  para datos problemáticos.
#  4. Independiente de las conclusiones anteriores, escribir código R que realice las pruebas con remuestreo en
#  cada caso.
#  5. Concluir de acuerdo con los resultados de la prueba realizada.

# ------------------------------- DATOS ----------------------------------------

dir <- "C:/Users/rowin/OneDrive/Escritorio/modelos estadistico/grupo_3.2/EP11"
basename <- "EP11 Datos.csv"
file <- file.path(dir, basename)
poblacion <- read.csv2(file = file)

# -------------------------- PREGUNTAS AL GRUPO (TODOS) --------------------------------

# 1. Propongan una pregunta de investigación original, que involucre la comparación de las medias de dos
# grupos independientes (más abajo se dan unos ejemplos). Fijando una semilla propia, seleccionen una
# muestra aleatoria de hogares (250 < n < 500) y respondan la pregunta propuesta utilizando una simulación
# Monte Carlo.
# 2. Propongan una pregunta de investigación original, que involucre la comparación de las medias de más de
# dos grupos independientes (más abajo se dan unos ejemplos). Fijando una semilla distinta a la anterior,
# seleccionen una muestra aleatoria de hogares (400 < n < 600) y respondan la pregunta propuesta utilizando
# bootstrapping. Solo por ejercicio académico, aplique un análisis post-hoc con bootstrapping aunque este no
# sea necesario.
# Algunos ejemplos (que no pueden ser ocupados en este ejercicio) son:
#   ▪ En promedio, el ingreso per cápita (ytotcorh / numper) en la Región Metropolitana (region) es el mismo
# entre hombres y mujeres (sexo) no heterosexuales (r23).
# ▪ El ingreso per cápita promedio es similar en las cuatro macro zonas (norte grande, norte chico, central, sur y
#                                                                        austral).
# ▪ El arriendo promedio que se paga por viviendas similares a la habitada (v19) tiene relación con el nivel
# educacional (educ) del jefe o la jefa del hogar.

# -------------------------- CRITERIOS DE EVALUACIÓN --------------------------------

# Pregunta 1:
# ▪ Proponen una pregunta de investigación, interesante y novedosa, que involucra la comparación de las
# medias de dos grupos independientes de personas encuestadas en la Encuesta Casen 2017.
# ▪ Obtienen una muestra de datos de acuerdo a lo solicitado, revisando su comportamiento con gráficos o
# pruebas estadísticas y pronunciándose explícitamente sobre la necesidad de utilizar métodos para datos
# problemáticos.
# ▪ Formulan explícitamente hipótesis nula y alternativa correctas, que involucran la comparación de las medias
# de una variable numérica de dos grupos independientes, para responder la pregunta de investigación que
# plantean.
# ▪ Basándose en el análisis anterior, proponen explícitamente un estadístico a remuestrear que permite
# docimar las hipótesis propuestas, justificando su elección apropiadamente.
# ▪ Realizan, de forma completa y sin errores, una simulación Monte Carlo de un estadístico que permite
# responder la pregunta de investigación que plantean, usando una muestra de datos adecuada, obteniendo
# un p valor o intervalo de confianza correcto.
# ▪ Entregan una conclusión correcta y completa a la pregunta de investigación que plantean, basándose en el
# resultado del p valor o intervalo de confianza obtenido a partir del estadístico remuestreado.
# ▪ Escriben código R -ordenado, bien indentado, sin sentencias espurias y bien comentado- que realiza de
# forma completa y correcta la prueba seleccionada con los datos adecuados en cada caso.
# ▪ Escriben con buena ortografía y redacción (<3 errores), usando vocabulario propio de la disciplina y el
# contexto del problema.

# Pregunta 2:
# ▪ Proponen una pregunta de investigación, interesante y novedosa, que involucra la comparación de las
# medias de más de dos grupos independientes de personas encuestadas en la Encuesta Casen 2017.
# ▪ Obtienen una muestra de datos de acuerdo a lo solicitado, revisando su comportamiento con gráficos o
# pruebas estadísticas y pronunciándose explícitamente sobre la necesidad de utilizar métodos para datos
# problemáticos.
# ▪ Formulan explícitamente hipótesis nula y alternativa correctas, que involucran la comparación de las medias
# de una variable numérica de más de dos grupos independientes, para responder la pregunta de
# investigación que plantean.
# ▪ Basándose en el análisis anterior, proponen explícitamente un estadístico a remuestrear que permite
# docimar las hipótesis ómnibus propuestas, justificando su elección apropiadamente.
# ▪ Aplican, de forma completa y sin errores, bootstrapping sobre un estadístico que permite docimar las
# hipótesis ómnibus propuestas, usando una muestra de datos adecuada, obteniendo un p valor o intervalo
# de confianza correcto.
# ▪ Proponen explícitamente un estadístico a remuestrear para comparar los grupos en un análisis post-hoc,
# justificando su elección apropiadamente.
# ▪ Aplican, de forma completa y sin errores, bootstrapping sobre un estadístico que permite realizar un análisis
# post-hoc, usando una muestra de datos adecuada, obteniendo un p valor o intervalo de confianza correcto.
# ▪ Entregan una conclusión correcta y completa a la pregunta de investigación que plantean, basándose en el
# resultado del p valor o intervalo de confianza obtenido a partir de los estadísticos remuestreados.
# ▪ Escriben código R -ordenado, bien indentado, sin sentencias espurias y bien comentado- que realiza, de
# forma completa y correcta, bootstrapping sobre estadísticos para comparaciones ómnibus y post-hoc
# adecuados.
# ▪ Escriben con buena ortografía y redacción (<3 errores), usando vocabulario propio de la disciplina y el
# contexto del problema.

# -------------------------- DESARROLLO --------------------------------

# 1. Propongan una pregunta de investigación original, que involucre la comparación de las medias de dos
# grupos independientes (más abajo se dan unos ejemplos). Fijando una semilla propia, seleccionen una
# muestra aleatoria de hogares (250 < n < 500) y respondan la pregunta propuesta utilizando una simulación
# Monte Carlo.

# Hipótesis de una simulación Monte Carlo
# H0: no hay relación causal entre las variables o no hay diferencias significativas entre grupos
# H1: hay relación causal o hay diferencias significativas entre grupos

# --------- FUNCIONES PARA USAR

# Un estudio de la Universidad de Santiago de Chile desea comprobar si el nivel
# de observación propia sobre el estado de salud de habitantes mayores a 25 años de dos
# regiones es similar, para ello se escoge la Región del Biobio y la Región Metropolitana.

# Hipótesis a Contrastar:
# H0: El promedio de la percepción propia del estado de salud los habitantes mayores a 25 años
# es el mismo en la región del Biobio y en la Región Metropolitana.
# H0: µa - µb = 0

# H1: El promedio de la percepción propia del estado de salud los habitantes mayores a 25 años
# no es el mismo en la región del Biobio y en la Región Metropolitana.
# H1: µa - µb =! 0


# Definición de Funciones

# Función para calcular la diferencia de medias.
# Argumentos :
# - muestra_1 , muestra_2: vectores numéricos con las muestras a comparar .
# - FUN: función del estadístico E para el que se calcula la diferencia .
# Valor :
# - diferencia E_1 - E_2.
calcular_diferencia <- function(muestra_1, muestra_2, FUN) {
  diferencia <- FUN(muestra_1) - FUN(muestra_2)
  return (diferencia)
}

# Función para hacer una permutación y calcular el estadístico
# de interés.
# Argumentos :
# - muestra_1 , muestra_2: vectores numéricos con las muestras a comparar .
# - FUN: función del estadístico E para el que se calcula la diferencia .
# Valor :
# - diferencia E_1 - E _2.
permutar <- function(muestra_1 , muestra_2, FUN) {
  n_1 <- length(muestra_1)
  n_2 <- length(muestra_2)
  
  # Hacer la permutación.
  permutacion <- sample (c( muestra_1 , muestra_2) , size = n_1 + n_2, replace = FALSE)
  
  # Asignar elementos a los dos grupos .
  
  permutacion_1 <- permutacion [1:n_1]
  permutacion_2 <- permutacion [n_1 + 1 : n_2]
  
  # Calcular y devolver la diferencia de medias .
  return (calcular_diferencia(permutacion_1, permutacion_2 , FUN))
}

# Función para calcular el valor p.
# Argumentos :
# - distribucion : distribución nula del estadístico de interés.
# - valor_observado : valor del estadístico de interés para las muestras
# originales .
# - repeticiones : cantidad de permutaciones a realizar .
# - alternative : tipo de hipótesis alternativa . "two.sided" para
# hipótesis bilateral , "greater" o "less" para hipótesis unilaterales .
# Valor :
# - el valor p calculado .
calcular_valor_p <- function(distribucion, valor_observado, repeticiones, alternative){
  if(alternative == "two.sided"){
    numerador <- sum(abs(distribucion) > abs(valor_observado)) + 1
    denominador <- repeticiones + 1
    valor_p <- numerador/denominador
  }
  else if(alternative == "greater"){
    numerador <- sum(distribucion > valor_observado) + 1
    denominador <- repeticiones + 1
    valor_p <- numerador/denominador
  }
  else{
    numerador <- sum( distribucion < valor_observado) + 1
    denominador <- repeticiones + 1
    valor_p <- numerador / denominador
  }
  return(valor_p)
}

# Función para graficar una distribución.
# Argumentos :
# - distribucion : distribución nula del estadístico de interés.
# - ...: otros argumentos a ser entregados a gghistogram y ggqqplot .
graficar_distribucion <- function(distribucion, ...) {
  
  observaciones <- data.frame(distribucion)
  
  histograma <- gghistogram(observaciones, x = "distribucion",
                            xlab = "Estadístico de interés",
                            ylab = "Frecuencia", ...)
  
  qq <- ggqqplot(observaciones , x = "distribucion", ...)
  
  # Crear una única figura con todos los gráficos de dispersión.
  figura <- ggarrange(histograma, qq ,ncol = 2 , nrow = 1)
  print(figura)
}

# Función para hacer la prueba de permutaciones .
# Argumentos :
# - muestra_1 , muestra_2: vectores numéricos con las muestras a comparar .
# - repeticiones : cantidad de permutaciones a realizar .
# - FUN : función del estadístico E para el que se calcula la diferencia .
# - alternative : tipo de hipó tesis alternativa . "two.sided" para
# hipótesis bilateral , "greater" o "less" para hipótesis unilaterales .
# - plot : si es TRUE , construye el gráfico de la distribución generada .
# - ...: otros argumentos a ser entregados a graficar_distribucion.
contrastar_hipotesis_permutaciones <- function(muestra_1 , muestra_2,
                                               repeticiones, FUN ,
                                               alternative, plot , ...){
  cat("Prueba de permutaciones\n\n")
  cat("Hipótesis alternativa :", alternative , "\n")
  observado <- calcular_diferencia(muestra_1, muestra_2 , FUN)
  cat("Valor observado :", observado , "\n")
  distribucion <- rep(NA, repeticiones)
  for(i in 1:repeticiones){
    distribucion[i] <- permutar(muestra_1, muestra_2, FUN)
  }
  if(plot){
    graficar_distribucion(distribucion, ...)
  }
  
  valor_p <- calcular_valor_p(distribucion, observado, repeticiones, "two.sided")
  cat("Valor p:", valor_p , "\n\n")
}


set.seed(486)
n <- 250
region_biobio <- poblacion %>% filter(region == "Region del Biobio", edad > 25, s13 != "NA", s13 != 9, s13 !="No sabe")
s13_region_biobio <- sample(region_biobio$s13, n)
a <- as.numeric(s13_region_biobio)

region_metropolitana <- poblacion %>% filter(region == "Region Metropolitana de Santiago", edad > 25, s13 != "NA", s13 != 9, , s13 !="No sabe")
s13_region_metropolitana <- sample(region_metropolitana$s13, n)
b <- as.numeric(s13_region_metropolitana)

print(shapiro.test(a))
print(shapiro.test(b))
# Para ambas muestras, el valor p es menor que alfa establecido, no cumpliendo con la normalidad.
# Por lo que se emplea prueba de permutaciones.

R = 4999
contrastar_hipotesis_permutaciones(a, b, repeticiones = R, 
                                   FUN = mean, 
                                   alternative = "two.sided", 
                                   plot = TRUE,
                                   color = "blue", fill = "blue")

# Se muestra además el histograma y gráfico Q-Q de la distribución para la 
# diferencia de medias generada mediante permutaciones.


# Conclusión:
# Con respecto a la prueba realizada y utilizando para ello una simulación de
# Monte Carlo, el resultado del valor p obtenido de 0.0001666667, inferior a un
# nivel de significación de 0.05, por lo que se rechaza la hipótesis nula a favor 
# de la hipótesis alternativa, de esta manera se concluye con 95% de confianza que
# el promedio del nivel de escolaridad de los habitantes mayores a 30 años es 
# distinto en la región metropolitana y en la Región del Maule.

# ------------------------------------------------------------------------------

# 2. Propongan una pregunta de investigación original, que involucre la comparación de las medias de más de
# dos grupos independientes (más abajo se dan unos ejemplos). Fijando una semilla distinta a la anterior,
# seleccionen una muestra aleatoria de hogares (400 < n < 600) y respondan la pregunta propuesta utilizando
# bootstrapping. Solo por ejercicio académico, aplique un análisis post-hoc con bootstrapping aunque este no
# sea necesario.