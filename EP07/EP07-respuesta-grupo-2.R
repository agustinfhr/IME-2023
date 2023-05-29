# # Grupo 2 EP07
# Integrantes: 
#   - Angel Vilches
#   - Nicolas Valdes
#   - Agustín Henríquez

library(tidyverse)
library(RVAideMemoire)
library(rcompanion)

######################################################################
############################# Pregunta 1 #############################
######################################################################

# Una joven emprendedora que crea y comercializa amigurumis desea analizar
# el interés que generan sus dos líneas de productos: Harry Potter y The
# Lord of the Rings, entre niños y adolescentes. 
# 
# Para ello, ha encuestado a 12 niños y 9 adolescentes. 7 de los primeros
# y 5 de los segundos prefirieron la línea de Harry Potter, mientras que
# los restantes optaron por la de The Lord of the Rings. ¿Influye el
# rango etario en la preferencia por Harry Potter?

#               Harry Potter	The Lord of the Rings	Total
#Niños	        7           	5	                    12
#Adolescentes	  5	            4         	          9


# Se procede al planteamiento de la hipótesis nula y alternativa:
# H0: La preferencia por Harry Potter es independiente del grupo de edad (sea niños o adolescentes).
# HA: La preferencia por Harry Potter depende del grupo de edad (niños o adolescentes).

# Se logra apreciar por enunciado, la existencia de celdas con menos de 5
# observaciones registradas. Por lo tanto, es preciso emplear una prueba
# que abarque una situación para muestras pequeñas.
# Al figurar con datos independientes, es decir, no pareados. Es correcto
# aplicar la prueba exacta de Fisher.

infante         <- c(7, 5)
adolescente     <- c(5, 4)

tabla           <- rbind(infante, adolescente)
colnames(tabla) <- c("Harry Potter", "The Lord of the Rings")
print(tabla)

# No existen indicios para tener precaución con los datos manejados, por ello,
# se establece un nivel de significación de 0.05

alfa            <- 0.05

# Se realiza la prueba exacta de Fisher:
prueba_fisher   <- fisher.test(tabla, conf.level = 1 - alfa)
cat("Resultado de la prueba exacta de Fisher:\n")
print(prueba_fisher)

# Considerando que el p-valor obtenido (1) es mayor que el nivel de signifación
# asignado (0.05) fallamos en rechazar la hipótesis nula. Por lo tanto, se
# concluye con un 95% de confianza que la preferencia por Harry Potter es 
# independiente del rango de edad (niños y adolescentes).



######################################################################
############################# Pregunta 2 #############################
######################################################################

# El gerente comercial de una importante plataforma de streaming está seguro de que a el interés por una
# serie de Star Trek cambió muchísimo durante la emisión de la segunda temporada. Sin embargo, ha
# preguntado a 20 personas si les ha gustado cada una de las temporadas, con el siguiente resultado:
#   
# ▪ A 4 personas les gustan ambas temporadas.
# ▪ A 11 personas solo les gusta la primera temporada.
# ▪ A 2 personas solo les gusta la segunda temporada.
# ▪ A 3 personas no les gusta ninguna temporada.
# 
# ¿Soportan estos datos la creencia del gerente comercial?


# Se pide determinar si hay un cambio significativo en el interés 
# por la serie de Star Trek entre la primera y la segunda temporada.

# Se procede al planteamiento de la hipótesis nula y alternativa:
# H0: No hay un cambio significativo en el interés por la serie entre la primera y la segunda temporada.
# HA: Existe un cambio significativo en el interés por la serie entre la primera y la segunda temporada.
# 
# H0: p1 = p2
# HA: p1 ≠ p2
# siento p la proporción.

# Al tratarse de una prueba de hipótesis para datos pareados, 
# la prueba de McNemar sería apropiada para este caso.

# Los datos se presentan en forma de tabla de contingencia de 2x2, donde
# cada entrada corresponde a la cantidad de personas que presentan una 
# cierta combinación de preferencias.

tabla <- matrix(c(4, 11, 2, 3), nrow = 2)

# Nombres de las filas y columnas.
colnames(tabla) <- c("Le gusta", "No le gusta")
rownames(tabla) <- c("Temporada 1", "Temporada 2")

# Prueba de McNemar
mcnemar_test <- mcnemar.test(tabla)

# Resultados
print(mcnemar_test)

# La prueba de McNemar se obtuvo un valor p de 0.0265. 
# Dado que este valor es menor que el nivel de significancia 0.05, 
# podemos rechazar la hipótesis nula en favor de la hipótesis alternativa.
# 
# Por lo tanto, con un 95% de confianza, podemos concluir que existe una diferencia significativa 
# entre la proporción de personas a las que les gustó la Temporada 1 
# en comparación con la Temporada 2 de Star Trek.


######################################################################
############################# Pregunta 3 #############################
######################################################################

# Minerva McGonagall, actual directora del Colegio Hogwarts de Magia y Hechicería, está haciendo un
# seguimiento de las actividades laborales y académicas de los egresados más recientes del colegio. Desea
# saber si los estudiantes de sanación son representan adecuadamente a los egresados del colegio ¿Qué
# puede inferir a partir de los siguientes datos?
  
#               Ravenclaw Slytherin Hufflepuff Gryffindor
# Sanación      38        14        43         5
# Egresados     387       171       400        238

# Se pide determinar si la proporción de estudiantes de sanación 
# entre los egresados es consistente en todas las casas de Hogwarts.

# Se procede al planteamiento de la hipótesis nula y alternativa:
# H0: La proporción de estudiantes de sanación es la misma en todas las casas de Hogwarts.
# HA: La proporción de estudiantes de sanación no es la misma en todas las casas de Hogwarts.

# Crear tabla de contingencia
sanacion <- c(38, 14, 43, 5)
total_egresados <- c(387, 171, 400, 238)

tabla <- as.table(rbind(sanacion,total_egresados))

dimnames(tabla)<-list(grupo = c("sanacion "," total_egresados"),
                      casa = c("Ravenclaw", "Slytherin", "Hufflepuff", "Gryffindor"))

print(tabla)

# Verificar si se esperan más de 5 observaciones por cada grupo .
n_total_egresados <- sum(total_egresados)
n_muestra <- 100
proporciones <- round(total_egresados/n_total_egresados, 3)
esperados <- round(proporciones*n_total_egresados, 3)
print(esperados)

# Hacer prueba chi - cuadrado de homogeneidad .
prueba <- chisq.test(tabla, correct = FALSE)
print(prueba)

# El resultado de la prueba Chi-cuadrado de Pearson indica que el valor del estadístico 
# Chi-cuadrado es 14.293 con 3 grados de libertad y un valor p de 0.002532.
#  
# El valor p es 0.002532, menor que 0.05
# 
# Por lo tanto, se rechaza la hipótesis nula, que afirma que la proporción de estudiantes 
# de sanación es la misma en todas las casas de Hogwarts. por lo que hay evidencia 
# suficiente para sugerir que la proporción de estudiantes de sanación no es la misma en todas 
# las casas de Hogwarts.

######################################################################
############################# Pregunta 4 #############################
######################################################################

# El Departamento de Educación Mágica del Ministerio de Magia desea saber si existen diferencias
# significativas en el desempeño de los estudiantes del Colegio Hogwarts de Magia y Hechicería en los TIMOS
# de asignaturas esenciales: Pociones, Defensa Contra las Artes Oscuras, Transformaciones y Encantamientos.
# Para ello, le ha entregado un archivo de datos que, para dichas asignaturas, indica si cada estudiante
# registrado obtuvo una Matrícula de Honor en Brujería (MHB) o falló (F). ¿Qué puede concluir el encargado
# del Departamento de Educación Mágica? Indicación: obtenga, a partir del archivo EP07 Datos.csv, una
# muestra de 100 estudiantes usando la semilla 1403.

#Hipotesis establecidas
#H0: La proporciOn de estudiantes aprobados es igual en todos los ramos
#H1: La proporcion de estudiantes aprobados es distinta en al menos un ramo

data4 <- read.csv2(file = 'C:/Users/Ekayn/Desktop/IME-2023/EP07/EP07 Datos.csv',
                      encoding = "UTF-8",
                      sep = ";")
set.seed(1403)
datos4<-sample_n(data4,100)

datos4<-datos4%>%pivot_longer(c("Pociones", "Defensa", "Transformaciones", "Encantamientos"),
                               names_to="Asignatura",
                               values_to="Estado")
datos4[["Asignatura"]]<-factor(datos4[["Asignatura"]])
datos4[["Estado"]]<-factor(datos4[["Estado"]])

prueba4<-cochran.qtest(Estado~Asignatura|Id,data=datos4,alpha=0.05)
cat("Resultado de prueba de Cochran: ")
print(prueba4)

# El resultado de las prueba de Cochran nos arroja un p-value = 0.06078, por lo que, con un nivel de confianza del 
# 95%, existe suficiente evidencia para rechazar la hipótesis alternativa a favor de la hipotesis nula. Por lo 
# anterior, se concluye que la proporción de aprovación de los ramos esenciales del Colegio Hogwarts de Magia 
# y Hechicería son similares.
