# Grupo 3 - EP09
# Integrantes: 
# Jaime Carrasco
# Pablo Villarreal Ortiz
# Agustin Henriquez Rojas


if (!require ( dplyr )) {
  install.packages ("dplyr" , dependencies=TRUE)
  require (dplyr)
}

if (!require ( ggpubr )) {
  install.packages ("ggpubr" , dependencies=TRUE)
  require (ggpubr)
}

if (!require ( pwr )) {
  install.packages ("pwr" , dependencies=TRUE)
  require (pwr)
}

if (!require ( tidyverse )) {
  install.packages ("tidyverse" , dependencies=TRUE)
  require (tidyverse)
}

if (!require ( RVAideMemoire )) {
  install.packages ("RVAideMemoire" , dependencies=TRUE)
  require (RVAideMemoire)
}

if (!require ( rcompanion )) {
  install.packages ("rcompanion" , dependencies=TRUE)
  require (rcompanion)
}

if (!require ( car )) {
  install.packages ("car" , dependencies=TRUE)
  require (car)
}

# Exportacion de las librerias 

library(dplyr)
library(ggpubr)
library(pwr)
library(tidyverse)
library(RVAideMemoire)
library(rcompanion)
library(car)


# ------------------------------ PROBLEMA --------------------------------------

# Una veterinaria experta en nutrici髇 de gatos ha creado una nueva l韓ea de alimentos saludables para estas
# traviesas mascotas. Desea estudiar los efectos de cada una de sus variedades (pato, pollo, conejo, pescado y res)
# en el peso de los felinos, por lo que ha reclutado un grupo de humanos que, a cambio de varios meses de
# alimentos gratis para sus amigos peludos, han aceptado participar en el siguiente experimento. Para cada gato,
# se registra su peso inicial. Luego, se alimenta le alimenta durante un mes exclusivamente con una 鷑ica variedad
# de comida y se vuelve a registrar el peso. A continuaci髇, se modifica la dieta del gato para que recupere su
# peso inicial. Este proceso se repite para cada una de las variedades de comida.
# A fin de contar con informaci髇 no sesgada, ha repartido a los gatitos en distintos grupos, cada uno de los cuales
# debe recibir los alimentos mensuales en distinto orden (que tiene registrado en un cuaderno para an醠isis
# posteriores). Ha registrado la informaci髇 recolectada en un conjunto de datos con las siguientes variables:
# ??? Id: identificador 鷑ico del gato.
# ??? Grupo: grupo al que ha sido asignado el gato.
# ??? Inicial: peso del gato, en kilogramos, antes de iniciar el experimento.
# ??? Pollo: peso del gato, en kilogramos, al t閞mino del mes en que fue alimentado con pollo.
# ??? Res: peso del gato, en kilogramos, al t閞mino del mes en que fue alimentado con res.
# ??? Conejo: peso del gato, en kilogramos, al t閞mino del mes en que fue alimentado con conejo.
# ??? Pescado: peso del gato, en kilogramos, al t閞mino del mes en que fue alimentado con pescado.
# ??? Pato: peso del gato, en kilogramos, al t閞mino del mes en que fue alimentado con pato.

# ------------------------------- DATOS ----------------------------------------

dir <- "C:/Users/agust/Desktop/REPO IME/IME-2023/EP09"
basename <- "EP09 Datos.csv"
file <- file.path(dir, basename)
datos <- read.csv2(file = file)


# -------------------------- PREGUNTAS AL GRUPO --------------------------------

# Grupo 3:
# Determine si, para el grupo 7, existen diferencias significativas en el peso de los gatos para los distintos tipos de
# alimentos.

# 1. El equipo obtiene y manipula correctamente los datos entregados y propone hip髏esis pertinentes a
# contrastar para cada problema.

# Hip贸tesis nula H0: No existen diferencias significativas en el peso de los gatos 
# del grupo 7 para los distintos tipos de alimentos.

# Hip贸tesis alternativa HA: Existen diferencias significativas en el peso de los gatos
# del grupo 7 para los distintos tipos de alimentos.

# Formulaci贸n Matem谩tica

# H0: mu_pato = mu_pollo = mu_conejo = mu_pescado = mu_res

# HA: Al menos una de las igualdades en la hip髏esis nula es falsa {mu_pato != mu_pollo, mu_pato != mu_conejo, ....}

# Filtrar el dataframe para solo tener los gatos el grupo 7.
datos_grupo7 <- datos[datos$Grupo == 7, ]

# 2. El equipo se asegura que cada caso cumple las condiciones para utilizar ANOVA con validez, usando gr醘icos
# o pruebas estad韘ticas auxiliares disponibles en el entorno R.

# Comprobar las 4 Condiciones para ANOVA.

# Primero, la variable dependiente es el peso del gato despu閟 de haber sido alimentado 
# con un tipo espec韋ico de alimento. El peso es una variable que se mide en una escala de raz髇.
# (porque tiene un cero absoluto y los intervalos son iguales).
# por lo que si se cumple la primera condicion.

# Segundo, se asume que las observaciones son independientes entre si, cumpliendo la condici贸n de Independencia

# Tercero, comprobar que los datos siguen una distribuci贸n normal
# Para un mejor orden en la visualizacion, se mostraran los graficos en acoplados en 2x3.
# Al mismo tiempo, se realizan las pruebas de Shapiro-Wilk para comprobar la normalidad de los datos.
par(mfrow = c(2, 3))
for (alimento in c("Pollo", "Res", "Conejo", "Pescado", "Pato")) {
  qqnorm(datos_grupo7[[alimento]], main = paste("Q-Q plot de", alimento))
  qqline(datos_grupo7[[alimento]])
  print(paste("P-valor para el test de Shapiro-Wilk de", alimento, ":", shapiro.test(datos_grupo7[[alimento]])$p.value))
}

# Como se puede observar, todos los p-valores son mayores al nivel de significancia (p<0.05)
# No se rechaza la hip贸tesis nula de la prueba de Shapiro, por lo que el principio de normalidad
# se cumple para estas cuatro variedades.

# Ademas, a partir del grafico Q-Q, tambi茅n se puede obversar que los datos tienden a distribuirse de forma normal.

# Cuarto, verificar la condici贸n de homocedasticidad (tienen la misma varianza) en los grupos para poder aplicar el ANOVA.

# Para comprobar esta condici贸n, se puede utilizar la prueba de Levene
pesos <- c(datos_grupo7$Pollo, datos_grupo7$Res, datos_grupo7$Conejo, datos_grupo7$Pescado, datos_grupo7$Pato)
alimentos <- rep(c("Pollo", "Res", "Conejo", "Pescado", "Pato"), each = nrow(datos_grupo7))
leveneTest(pesos ~ alimentos)

# En este caso, el valor p obtenido por la prueba de Levene es de 1, mayor al nivel de 
# significancia 0.05, por lo que indica que no se puede rechazar la hip贸tesis nula de que las 
# varianzas son iguales en los grupos, de esta manera, cumpliendo con la condicion de homocedasticidad.

# Finalmente, los datos cumplen las cuatro condiciones para realizar una prueba ANOVA.

# 3. El equipo realiza de forma correcta y completa una prueba ANOVA 髆nibus para cada problema.

# Ahora procediendo a ejecutar la prueba ANOVA.

# Procedimiento ANOVA con aov().

# Convertir los datos al formato largo
datos_grupo7_largo <- datos_grupo7 %>%
  pivot_longer(cols = c(Pollo, Res, Conejo, Pescado, Pato), 
               names_to = "Alimento", 
               values_to = "Peso")
# Esto con el objetivo de tener una columna para la variable "peso" y 
# otra columna para el tipo de "alimento".

# Realizar el ANOVA
modelo_anova <- aov(Peso ~ Alimento, data = datos_grupo7_largo)
summary(modelo_anova)

# A partir de los resultados obtenido por la prueba ANOVA
# el p-valor es 0.879, mayor que 0.05. Esto significa que no se puede rechazar
# la hip贸tesis nula de que no hay diferencias entre los grupos de alimentos. 

# Por lo tanto, no hay evidencia suficiente para decir que el tipo de alimento
# tiene un efecto significativo en el peso de los gatos en el grupo 7.

# 4. El equipo determina correctamente si corresponde o no hacer un an醠isis post-hoc en cada caso, el que
# aplica de forma correcta y completa (cuando corresponda).

# En este caso, el p-valor de la prueba ANOVA fue 0.879, que es mayor que 0.05. 
# Esto significa que no se rechaza la hip贸tesis nula de que no hay diferencias significativas 
# entre las medias de los grupos. Por lo tanto, no ser谩 necesario realizar un an谩lisis post-hoc
# porque no se ha encontrado diferencias significativas entre los grupos en el ANOVA.
# 
# A este punto, se est谩 cumpliendo con el objetivo de la actividad en t茅rminos 
# de determinar si corresponde o no realizar un an谩lisis post-hoc. Sin embargo, si el p-valor 
# hubiera sido menor que 0.05, entonces se deber铆a proceder con un an谩lisis post-hoc, como la prueba
# de Tukey, para determinar d贸nde est谩n exactamente esas diferencias entre los grupos.

# 5. El equipo interpreta adecuadamente los resultados de las pruebas y responde las preguntas planteadas en
# cada caso, bas醤dose en la prueba 髆nibus y, si corresponde, considerando tambi閚 el an醠isis post-hoc.

# Bas醤donos en nuestro an醠isis ANOVA, concluimos que no hay diferencias significativas en el peso
# de los gatos del grupo 7 despu閟 de ser alimentados con pollo, res, conejo, pescado y pato.
# Esto se debe a que nuestro p-valor de 0.879 es mayor que el nivel de significaci髇 de 0.05,
# lo que significa que no podemos rechazar la hip髏esis nula de que las medias de los grupos son
# iguales. Por lo tanto, no hay evidencia de que el peso de los gatos var韊 significativamente entre 
# estos cinco tipos de alimentos.













