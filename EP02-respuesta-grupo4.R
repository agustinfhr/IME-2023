

#Definir ruta/directorio de trabajo
setwd("C:/Users/agust/Desktop/Ramos 2023/IME/EP02")


#Importar dplyr para utlizar funcionalidades del paquete.
library(dplyr)
library(ggpubr)

#Leer datos del archivo csv, se importarÃ¡ con read.csv2() porque el archivo estÃ¡ en espaÃ±ol.

datos <- read.csv2("EP02 Datos.csv")

# Filtrar para trabajar con los datos solicitados, que sea hombre y sean de zona urbana o rural
datosHombre <- filter(datos, sexo == "Hombre")

datosUrbano <- filter(datos,zona == "Urbano")

datosRural <- filter(datos,zona == "Rural")

contadorUrbano <- datosUrbano %>% count(ch1, sort = TRUE)
contadorRural <- datosRural %>% count(ch1, sort = TRUE)

g1 <- gghistogram(contadorUrbano,
                  x = "n",
                  bins=4)
print(g1)




