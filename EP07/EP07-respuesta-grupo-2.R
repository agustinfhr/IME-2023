


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