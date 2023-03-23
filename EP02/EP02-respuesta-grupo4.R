# Grupo 4:
# Jaime Carrasco
# Agustín Henriquez
# David Valero


#Definir ruta/directorio de trabajo
setwd("C:/Users/david/OneDrive/Escritorio/universidad/10mo Semestre/IME/actividades semanales/IME-2023/EP02")


#Importar dplyr y ggpubr para utlizar funcionalidades del paquete.
library(dplyr)
library(ggpubr)

#Leer datos del archivo csv, se importará con read.csv2() porque el archivo está en español.

datos <- read.csv2("EP02 Datos.csv",stringsAsFactors = TRUE)

# Filtrar para trabajar con los datos solicitados, que sea hombre y sean de zona urbana o rural
datosHombre <- filter(datos, sexo == "Hombre")

datosUrbano <- filter(datos,zona == "Urbano")

datosRural <- filter(datos,zona == "Rural")

contadorUrbano <- datosUrbano %>% count(ch1)
contadorRural <- datosRural %>% count(ch1)

tabla <-table(datosHombre[["zona"]],datosHombre[["ch1"]])

#tabla <-table(datosHombre[["ch1"]],datosHombre[["zona"]])
tablaFrecuencia <- data.frame(tabla) %>% group_by(Var2)

# g1 <- gghistogram(contadorUrbano,
#                   x = "n",
#                   bins=4)
# print(g1)
# mis_colores <- c("blue", "red", "green", "purple")
# g1 <- ggbarplot(data = contadorUrbano,
#                 x = "ch1",
#                 y = "n",
#                 fill = mis_colores,
#                 title = "Ocupación en zona urbana (Hombres)",
#                 xlabel = "Ocupación",
#                 ylabel = "Cantidad")
# le pedí ayuda al chat gpt y me dijo lo mismo q ya tenía hecho, pero no encuentro el error KJDNKJBDF

# mis_colores <- c("blue", "red", "green", "purple")
# g1 <- ggbarplot(contadorUrbano,
#                 x = "ch1",
#                 y = "n",
#                 fill = mis_colores,
#                 title = "Ocupación en zona urbana (Hombres)",
#                 xlabel = "Ocupación",
#                 ylabel = "Cantidad")


# g2 <- ggbarplot(contadorRural,
#                 x = "ch1",
#                 y = "n",
#                 title = "Ocupación en zona rural (Hombres)",
#                 xlabel = "Ocupación",
#                 ylabel = "Cantidad")
# 
# 
# print(g1)
# print(g2)

g3 <- ggplot(tablaFrecuencia, aes(fill=Var1,y=Freq,x=Var2))
# g3 <- g3 + labs(y="proporción") + ggtitle("aaaaa")
# g3 <- g3 + theme_pubr()

print(g3)
# ¿Se distribuye de igual manera la situación ocupacional de los hombres que viven en áreas rurales y quienes
# viven en áreas urbanas de la RM?

# creo q igual necesitariamos ver los porcentajes o no? aaaaaa podría ser aplicar una distribución normal no sé djndkjb tiren ideas



