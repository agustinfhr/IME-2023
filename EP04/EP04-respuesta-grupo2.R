# # Grupo 2 EP04
# Integrantes: 
#   - Angel Vilches
#   - Nicolas Valdes
#   - Agustín Henríquez

#Definir ruta/directorio de trabajo
setwd("C:/Users/agust/Desktop/REPO IME/IME-2023/EP04")

# pregunta 1 ////////////////////////////////////////////////////////////////////

# 1. El criador a cargo del estudio cree que el peso medio de los pollitos alimentados con maravilla, a los 14 días
# de nacidos, es superior a 154,5 gramos. ¿Soportan los datos esta afirmación?

# Hipótesis nula: El peso medio de los pollitos alimentados con maravilla, 
# a los 14 días de nacidos, es igual a 154,5 gramos.

# Hipótesis alternativa: El peso medio de los pollitos alimentados con maravilla, 
# a los 14 días de nacidos, es superior a 154,5 gramos.

#Importar dplyr y ggpubr para utlizar funcionalidades del paquete.
library(dplyr)
library(tidyr)

#Leer datos del archivo csv, se importará con read.csv2() porque el archivo está en español.
datos <- read.csv2("EP04 Datos.csv",stringsAsFactors = TRUE)

# datosMaravilla <- filter(datos, DIETA == "Maravilla")

# Selección de los datos de interés
datos_maravilla_1 <- subset(datos, DIETA == "Maravilla" & DIA == 14)$PESO

# Verificar distribución muestral usando la prueba de normalidad
# de Shapiro - Wilk .
normalidad_1 <- shapiro.test(datos_maravilla_1)
print(normalidad_1)
# resultados arrojan datos con distribución normal 


# Fijar un nivel de significación.
alfa_1 <- 0.025
valor_nulo_1 <- 154.5

# Se utiliza el test t de Student.
prueba_1 <- t.test(datos_maravilla_1,
                       alternative = "less",
                       mu = valor_nulo_1,
                       conf.level = 1 - alfa_1)

print(prueba_1)

# La prueba t de la muestra es de 2.1107
# lo que significa que el valor de t calculado es mayor que el valor crítico, 
# por lo cual hay suficiente evidencia para rechazar la hipótesis nula 
# de que la media poblacional es menor o igual a 154.5


# pregunta 2 ////////////////////////////////////////////////////////////////////

# 2. ¿Sugieren los datos que, en promedio, los pollitos alimentados con soya
#    aumentan su peso en menos de 10,5 gramos a 2 días desde su nacimiento?

# Hipótesis nula: el peso promedio de los pollitos alimentados con soya 
# a los 2 días desde su nacimiento es igual a 10,5 gramos.
# 
# Hipótesis alternativa: el peso promedio de los pollitos alimentados con soya 
# a los 2 días desde su nacimiento es mayor a 10,5 gramos.

# Se seleccionan los datos de interes, estos son, el peso cuya dieta considera Soya en día 2
datos_soya <- subset(datos, DIETA == "Soya" & DIA == 2)$PESO
# En este sentido, la media aritmética y desviación estándar corresponden a:
media_datos_soya <- mean(datos_soya, na.rm = TRUE)              # 50.4
desviacion_estandar_datos_soya <- sd(datos_soya, na.rm = TRUE)  # 2.412928

# A continuación, y dada una muestra pequeña se emplea la distribución t de 
# Student para probar la veracidad de la hipótesis nula. Sin embargo, debemos
# veríficar la normalidad de la muestra
normalidad_2 <- shapiro.test(datos_soya)
print(normalidad_2)
# Luego, empleando un nivel de significancia de 0.05
alfa_2 <- 0.05
valor_nulo_2 <- 10.5
prueba_2 <- t.test(datos_soya,
                 alternative = "less",
                 mu = valor_nulo_2,
                 conf.level = 1 - alfa_2)
print(prueba_2)
# De acuerdo con los resultados obtenidos, se rechaza la hipótesis nula en favor
# de la hipótesis alternativa que señala un peso menor a 10.5 gramos a dos días
# desde su nacimiento con una dieta basada en soya. Esto dada la variabilidad
# evindenciada entre el valor t, media y valor de hipótesis, sumando al p valor
# que sugiere una probabilidad de diferencia de promedio atípico del 100%.

# pregunta 3 ////////////////////////////////////////////////////////////////////

# 3.  ¿Es posible afirmar que, en promedio, los pollitos alimentados con habas superan por 33,91 gramos a los
#     alimentados con maravilla a los 16 días de nacidos?

# Hipótesis nula(H0): en promedio, la diferencia de peso entre los pollitos alimentados con habas
# y los pollitos alimentados con maravilla no superan los 33,91 gramos a los 16 días de nacidos
# 
# Hipótesis alternativa(H1): en promedio, los pollitos alimentados con habas superan por 33,91 gramos a los
# alimentados con maravilla a los 16 días de nacidos

# Selección de los datos de interés
datos_habas_3 <- subset(datos, DIETA == "Habas" & DIA == 16)$PESO
datos_maravillas_3 <- subset(datos, DIETA == "Maravilla" & DIA == 16)$PESO

# Fijar un nivel de significación.
alfa_3 <- 0.05

# Verificar distribución muestral usando la prueba de normalidad
# de Shapiro - Wilk .
normalidad_habas_3 <- shapiro.test(datos_habas_3)
print(normalidad_habas_3)
# Dado a que el valor p-value arroja un resultado mayor a alfa, se comprueba
# de que la muestra tiene un comportamiento normal y no se rechaza la hipotesis
# nula

normalidad_marav_3 <- shapiro.test(datos_maravillas_3)
print(normalidad_marav_3)
# Dado a que el valor p-value arroja un resultado mayor a alfa, se comprueba
# que la muestra tiene un comportamiento normal y no se rechaza la hipótesis
# nula

# Se utiliza el test t de Student.
prueba_3 <- t.test(y=datos_habas_3,
                   x=datos_maravillas_3,
                   paired=FALSE,
                   alternative= "greater",
                   mu=33.91,
                   conf.level=1-alfa_3
)
print(prueba_3)

# Dado a que el valor de p-value = 0.1329 es mayor al nivel de significancia,
# la evidencia en favor de H0 es mas fuerte, por lo que rechazamos la hipótesis
# alternativa. Con lo anterior podemos confirmar con un grado de confianza del 95%
# que, en promedio, los pollitos alimentados con habas no pesan 33.91 gramos
# más que los pollitos alimentados con maravillas a los 16 días de nacidos
