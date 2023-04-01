# # Grupo 2
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
datos_maravilla <- subset(datos, DIETA == "Maravilla" & DIA == 14)$PESO

# Verificar distribución muestral usando la prueba de normalidad
# de Shapiro - Wilk .
normalidad <- shapiro.test(datos_maravilla)
print(normalidad)
# resultados arrojan datos con distribución normal 


# Fijar un nivel de significación.
alfa <- 0.025
valor_nulo <- 154.5

prueba <- t.test(datos_maravilla,
                       alternative = "less",
                       mu = valor_nulo,
                       conf.level = 1 - alfa)

print(prueba)

# prueba t de una muestra es de 2.1107

# lo que significa que el valor de t calculado es mayor que el valor crítico, 
# lo que sugiere que hay suficiente evidencia para rechazar la hipótesis nula 
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
normalidad <- shapiro.test(datos_soya)
print(normalidad)
# Luego, empleando un nivel de significancia de 0.05
alfa <- 0.05
valor_nulo <- 10.5
prueba <- t.test(datos_soya,
                 alternative = "less",
                 mu = valor_nulo,
                 conf.level = 1 - alfa)
print(prueba)
# De acuerdo con los resultados obtenidos, se rechaza la hipótesis nula en favor
# de la hipótesis alternativa que señala un peso menor a 10.5 gramos a dos días
# desde su nacimiento con una dieta basada en soya. Esto dada la variabilidad
# evindenciada entre el valor t, media y valor de hipótesis, sumando al p valor
# que sugiere una probabilidad de diferencia de promedio atípico del 100%.