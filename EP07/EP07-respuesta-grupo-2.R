# # Grupo 2 EP07
# Integrantes: 
#   - Angel Vilches
#   - Nicolas Valdes
#   - Agustín Henríquez



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



