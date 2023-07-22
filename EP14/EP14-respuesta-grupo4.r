# ------------------- Grupo 4 EP-14 -------------------
#Integrantes:
#            - AGUSTÍN HENRIQUÉZ
#            - NICOLAS M. VALDES HERRERA 
#
#
# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# |||||||||||||||||||||||||||||||| ENUNCIADO |||||||||||||||||||||||||||||||||
# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
#
# ACTIVIDADES
# Para esta actividad usaremos los datos de medidas de atributos del vino que ya conocimos en el ejercicio
# práctico anterior.
#   
#   1. El equipo copia el enunciado del problema asignado como comentarios de un script R.
#   2. El equipo lee el enunciado, descarga el archivo de datos (EP13 Datos.csv) desde UVirtual y selecciona las
#       columnas para trabajar de acuerdo a las instrucciones.
#   3. El equipo construye los modelos solicitados usando la muestra correspondiente.
#   4. El equipo sube el script con las actividades anteriores comentando en detalle los pasos seguidos.
# 
# Fuera del horario de clases, cada equipo debe subir el script realizado UVirtual con el nombre "EP14-respuestagrupo-i", donde i es el número de grupo asignado. 
# 
# PREGUNTA (todos los grupos)
# Ahora podemos construir un modelo de regresión logística para predecir la variable clase, de acuerdo con las
# siguientes instrucciones:
#   1. Definir la semilla a utilizar, que corresponde a los últimos cuatro dígitos del RUN (sin considerar el dígito
#       verificador) del integrante de mayor edad del equipo.
#   2. Seleccionar una muestra de 120 vinos, asegurando que la mitad sean blancos y la otra mitad, tintos. Dividir
#      esta muestra en dos conjuntos: los datos de 80 vinos (40 con clase “Blanco”) para utilizar en la construcción 
#      de los modelos y 40 vinos (20 con clase “Blanco”) para poder evaluarlos
#   3. Seleccionar 6 variables predictoras de manera aleatoria (al igual que en el ejercicio anterior).
#   4. Seleccionar, de las otras variables, una que el equipo considere que podría ser útil para predecir la clase,
#      justificando bien esta selección.
#   5. Usando el entorno R y paquetes estándares, construir un modelo de regresión logística con el predictor
#      seleccionado en el paso anterior y utilizando de la muestra obtenida.
#   6. Agregue la variable seleccionada en el paso 4 al conjunto obtenido en el punto 3.
#   7. Usando herramientas estándares1 para la exploración de modelos del entorno R, buscar entre dos y cinco
#      predictores de entre las variables presentes en el conjunto obtenido en el paso anterior para construir un
#      modelo de regresión logística múltiple.
#   8. Evaluar la confiabilidad de los modelos (i.e. que tengan un buen nivel de ajuste y son generalizables) y
#      “arreglarlos” en caso de que tengan algún problema.
#   9. Usando herramientas del paquete caret, evaluar el poder predictivo de los modelos con los datos de los 40
#      vinos que no se incluyeron en su construcción en términos de sensibilidad y especificidad.

# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# |||||||||||||||||||||||||||||||| RESPUESTAS ||||||||||||||||||||||||||||||||
# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

# Librerías
if (!require(readxl) ) {
  install.packages("readxl", dependencies = TRUE )
  require (readxl)
}
if (!require(ggpubr) ) {
  install.packages("ggpubr", dependencies = TRUE )
  require (ggpubr)
}
if (!require(dplyr) ) {
  install.packages("dplyr", dependencies = TRUE )
  require (dplyr)
}
if (!require(car)){
  install.packages("car", dependencies = TRUE )
  require (car)
}
if (!require(corrplot)){
  install.packages("corrplot", dependencies = TRUE )
  require (corrplot)
}

install.packages("boot")
library(boot)

setwd("C:/Users/agust/Desktop/Repo IME/IME-2023/EP14")
data <- read.csv2("EP13 Datos.csv")
data[["id"]] <- factor(1:nrow(data))

# 1. Definir la semilla a utilizar, que corresponde a los últimos cuatro dígitos del RUN (sin considerar el dígito
#    verificador) del integrante de mayor edad del equipo.

set.seed(2704)

# 2.	Seleccionar una muestra de 120 vinos, asegurando que la mitad sean blancos y la otra mitad, tintos. Dividir esta muestra en
#     dos conjuntos: los datos de 80 vinos (40 con clase “Blanco”) para utilizar en la construcción de los modelos y 40 vinos (20 con clase 
#     “Blanco”) para poder evaluarlos.

# Separar el data frame en dos: uno para vinos blancos y otro para vinos tintos
vinos_blancos <- data[data$clase == 'Blanco', ]
vinos_tintos <- data[data$clase == 'Tinto', ]

# Seleccionar aleatoriamente 60 vinos de cada tipo
# Asegura que los resultados son reproducibles
vinos_blancos_muestra <- vinos_blancos[sample(nrow(vinos_blancos), 60), ]
vinos_tintos_muestra <- vinos_tintos[sample(nrow(vinos_tintos), 60), ]

# Combinar las dos muestras en un solo data frame
muestra <- rbind(vinos_blancos_muestra, vinos_tintos_muestra)

# Dividir la muestra en un conjunto de entrenamiento y uno de prueba
muestra_entrenamiento <- muestra[sample(nrow(muestra), 80), ] 

# Convertir los nombres de las filas a numéricos
indices_entrenamiento <- as.numeric(row.names(muestra_entrenamiento))

# Excluir las filas de entrenamiento
muestra_prueba <- muestra[-indices_entrenamiento, ] 

# 3. Seleccionar 6 variables predictoras de manera aleatoria (al igual que en el ejercicio anterior).

# Se extraen de forma aleatoria las variables para la prueba
opciones <-  c("calidad", "acidez.fija", "acidez.volatil", "acido.citrico", 
               "azucar.residual", "cloruros", "dioxido.azufre.libre", "dioxido.azufre.total",
               "densidad", "ph", "sulfatos", "alcohol")
variables <- sample(opciones, 6)

# variables 
# > print(variables)
# [1] "sulfatos"             "calidad"              "ph"                   "cloruros"            
# [5] "dioxido.azufre.total" "azucar.residual"  

# 4. Seleccionar, de las otras variables, una que el equipo considere que podría ser útil para predecir la clase,
#    justificando bien esta selección.

muestra_prueba[["clase"]] <- factor(muestra_prueba[["clase"]], levels = c("Blanco", "Tinto"),
                                    labels = c(0, 1))
muestra_prueba[["clase"]] <- as.integer(muestra_prueba[["clase"]])

clase <- muestra_prueba[["clase"]]

# Se analiza la correlación existente entre las variables no seleccionadas
acidesFija <- muestra_prueba[["acidez.fija"]]
acidesVolatil <- muestra_prueba[["acidez.volatil"]]
acidoCitrico <- muestra_prueba[["acido.citrico"]]
dioxidoAzufreLibre <- muestra_prueba[["dioxido.azufre.libre"]]
densidad <- muestra_prueba[["densidad"]]
alcohol <- muestra_prueba[["alcohol"]]

correlacion_lineal <- cor(acidesFija, clase)
cat("Acides fija: ", correlacion_lineal, "\n")
correlacion_lineal <- cor(acidesVolatil, clase)
cat("Acides Volatil: ", correlacion_lineal, "\n")
correlacion_lineal <- cor(acidoCitrico, clase)
cat("Acido citrico: ", correlacion_lineal, "\n")
correlacion_lineal <- cor(dioxidoAzufreLibre, clase)
cat("dioxido azufre libre: ", correlacion_lineal, "\n")
correlacion_lineal <- cor(densidad, clase)
cat("Densidad: ", correlacion_lineal, "\n")
correlacion_lineal <- cor(alcohol, clase)
cat("Alcohol: ", correlacion_lineal, "\n")

# Obtenemos que las variables con mayor correlación es la variable 'alcohol'
# y además se suele considerar que los vinos con un mayor contenido de alcohol tienden a ser de mayor calidad.
variables <- c(variables, "alcohol")

# > print(variables)
# [1] "sulfatos"             "clase"                "ph"                   "cloruros"             "dioxido.azufre.total"
# [6] "azucar.residual"      "alcohol" 

# 5. Usando el entorno R y paquetes estándares, construir un modelo de regresión logística con el predictor
#    seleccionado en el paso anterior y utilizando de la muestra obtenida.

# Asegurarse de que la variable de destino es un factor
muestra_entrenamiento[["clase"]] <- factor(muestra_entrenamiento[["clase"]], levels = c("Blanco", "Tinto"),
                                    labels = c(0, 1))

# Crear el modelo de regresión logística
modelo <- glm(clase ~ alcohol, data = muestra_entrenamiento, family = binomial)

# Resumen del modelo
summary(modelo)

# Observaciones:
# El "Estimate" para la variable alcohol es de -0.4261, lo que indica que con cada aumento de una unidad en el contenido 
# de alcohol, se espera que el logaritmo de las odds de que el vino sea blanco disminuya en 0.4261, manteniendo todas 
# las demás variables constantes. Esto se interpreta como, los vinos con mayor contenido de alcohol son menos probables de ser blancos, según el modelo.

# El valor P (Pr(>|z|)) para la variable alcohol es 0.0331, lo que es menor que 0.05, lo que sugiere que la relación entre el alcohol 
# y la clase del vino es estadísticamente significativa al nivel de significancia del 5%. Esto muestra que hay evidencia suficiente para
# rechazar la hipótesis nula de que la variable alcohol no tiene relación con la clase del vino.


# 6. Agregue la variable seleccionada en el paso 4 al conjunto obtenido en el punto 3.

variables <- c(variables, "alcohol")

# 7. Usando herramientas estándares1 para la exploración de modelos del entorno R, buscar entre dos y cinco
#     predictores de entre las variables presentes en el conjunto obtenido en el paso anterior para construir un
#     modelo de regresión logística múltiple.
#
# Creamos la formula para el modelo. Esta formula representa 'calidad ~ predictor1 + predictor2 + ... + predictorN'

f <- as.formula(paste("clase", paste(c("alcohol", "ph", "sulfatos"), collapse = "+"), sep = " ~ "))

completo <- glm(f, data = muestra_entrenamiento, family = binomial(link = "logit"))

print(summary(completo))

# 8. Evaluar la confiabilidad de los modelos (i.e. que tengan un buen nivel de ajuste y son generalizables) y
#   “arreglarlos” en caso de que tengan algún problema

# Ajusta modelo con regresión escalonada
cat("Modelo con regresión escalonada\n")

nulo <- glm(clase~ 1, family = binomial(link = "logit"), data = muestra_entrenamiento)

mejor <- step(nulo, scope = list(slower = nulo, upper = completo),
              direction = "both", trace = 0)

print(summary(mejor))


# Realizar una validación cruzada de 10 pliegues
set.seed(123) # Para reproducibilidad
cv.error <- cv.glm(muestra_entrenamiento, mejor, K = 10)
cv.error$delta

#   No debe existir multicolinealidad. Esto significa que no deben existir relaciones lineales fuertes entre dos o más predictores (coeficientes de correlación altos).
#    R: Observando los resultados, podemos ver que se cumple la condicion

vifs <- vif(mejor)
cat("\nVerificar la multicolinealidad :\n")
cat("- VIFs :\n")
print( vifs )

cat("- Tolerancias :\n")
print(1/vifs)

cat("- VIF medio :", mean (vifs), "\n")

# 9. Usando herramientas del paquete caret, evaluar el poder predictivo de los modelos con los datos de los 40
#    vinos que no se incluyeron en su construcción en términos de sensibilidad y especificidad.

if (!require(caret)){
  install.packages("caret", dependencies = TRUE )
  require (caret)
}

# Convertir 'muestra_prueba$clase' en un factor
muestra_prueba$clase <- as.factor(muestra_prueba$clase)

# Asegurarse de que los niveles de 'muestra_prueba$clase' y 'predicciones_clase' sean los mismos
predicciones_clase <- factor(predicciones_clase, levels = levels(muestra_prueba$clase))

# Crear la matriz de confusión
cm <- confusionMatrix(predicciones_clase, muestra_prueba$clase)

# Sensibilidad y especificidad
sensibilidad <- cm$byClass["Sensitivity"]
especificidad <- cm$byClass["Specificity"]

print(paste("Sensibilidad:", sensibilidad))
print(paste("Especificidad:", especificidad))




