# Grupo 4:
# Jaime Carrasco
# Agustín Henriquez
# David Valero


#Definir ruta/directorio de trabajo
setwd("C:/Users/david/OneDrive/Escritorio/universidad/10mo Semestre/IME/actividades semanales/IME-2023/EP02")


#Importar dplyr y ggpubr para utlizar funcionalidades del paquete.
library(dplyr)
library(ggpubr)
library(tidyr)

#Leer datos del archivo csv, se importará con read.csv2() porque el archivo está en español.

datos <- read.csv2("EP02 Datos.csv",stringsAsFactors = TRUE)

# Filtrar para trabajar con los datos solicitados, que sea hombre y sean de zona urbana o rural
datosHombre <- filter(datos, sexo == "Hombre")

datosUrbano <- filter(datos,zona == "Urbano")

datosRural <- filter(datos,zona == "Rural")

contadorUrbano <- datosUrbano %>% count(ch1)
contadorRural <- datosRural %>% count(ch1)

tabla <-table(datosHombre[["zona"]],datosHombre[["ch1"]])

tablaFrecuencia <- data.frame(tabla) 

f <- ggplot(tablaFrecuencia, aes(x = Var2, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "Situación Laboral", y = "Frecuencia", fill = "Zona")

f <- theme_pubr()

print(f)
# ¿Se distribuye de igual manera la situación ocupacional de los hombres que viven en áreas rurales y quienes
# viven en áreas urbanas de la RM?
  
# Observando el gráfico y la tabla de frecuencia, podemos concluir que la distribución de la 
# situación ocupacional de los hombres es diferente en las áreas rurales y urbanas de la RM. 
