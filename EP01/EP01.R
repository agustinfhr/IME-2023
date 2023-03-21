# Grupo 4:
# Jaime Carrasco
# Agustín Henriquez
# David Valero


#Definir ruta/directorio de trabajo
setwd("C:/Users/Jx/Documents/Universidad/IME/EPs/EP01")

#Importar dplyr para utlizar funcionalidades del paquete.
library(dplyr)

#Leer datos del archivo csv, se importará con read.csv2() porque el archivo está en español.

datos <- read.csv2("EP01 Datos.csv")

datosPudahuel <- filter(datos, ESTACION=="PUDAHUEL")

datosOct<- filter(datosPudahuel,MES=="Oct")
datosNov<- filter(datosPudahuel,MES=="Nov")
datosDic<- filter(datosPudahuel,MES=="Dic")

#Se utiliza rbind para concatenar los datos obtenidos por mes.
datosTrimestre <- rbind(datosOct,datosNov,datosDic)

minimaAlta <- max(select(datosTrimestre,MINIMA))
# minimaAlta <- datosTrimestre %>% filter(DIA,MES,MINIMA==minimaAlta)

# Obtener la fila del día que se tuvo la mínima más alta
filaMinimaAlta <- filter(datosTrimestre, MINIMA == minimaAlta)
# Obtener el día y mes del día que se tuvo la mínima más alta
DiaMinimaAlta <- filaMinimaAlta %>% select(DIA,MES)

# 1. ¿Qué día se registró la mínima más alta del cuarto trimestre en Pudahuel?

#Se obtuvo que el día con la mínima más alta de Pudahuel fue el 9 de Diciembre


minimaPorMes <- c(min(select(datosOct,MINIMA)),min(select(datosNov,MINIMA)),min(select(datosDic,MINIMA)))

# 2. ¿Cuál fue la temperatura mínima registrada en Pudahuel para cada mes de este periodo?
# Las temperaturas minimas por mes obtenidas en el 4to trimestre en Pudahuel fueron de
# Octubre: 4.8
# Noviembre: 7.4
# Diciembre: 10.3


# ¿Qué variables se han cargado?
# Se cargaron las variables ESTACION, DIA, MES, MINIMA, MAXIMA

# ¿Qué tipo tiene cada una de estas variables?
# ESTACION -> Categórica nominal
# DIA -> Numérica discreta
# MES -> Categórica ordinal
# MINIMA -> Numérica continua
# MAXIMA -> Numérica continua

# ¿Qué escala parecen tener estas variables?
# ESTACION -> Escala Nominal
# DIA -> Escala Ordinal
# MES -> Escala Ordinal
# MINIMA -> Escala de Intervalo
# MAXIMA -> Escala de Intervalo

