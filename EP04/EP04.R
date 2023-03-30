
#Definir ruta/directorio de trabajo
setwd("C:/Users/agust/Desktop/REPO IME/IME-2023/EP04")

# pregunta 1 ////////////////////////////////////////////////////////////////////

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

# Verificar distribuci ón muestral usando la prueba de normalidad

# de Shapiro - Wilk .
normalidad <- shapiro.test(datos_maravilla)
print(normalidad)
# resultados arrojan datos con distribucion normal 


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


