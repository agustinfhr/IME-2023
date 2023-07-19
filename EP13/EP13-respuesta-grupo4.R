# ------------------- Grupo 4 -------------------
#Integrantes:
#            - AGUSTÍN HENRIQUÉZ
#            - NICOLAS M. VALDES HERRERA 
#
# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# |||||||||||||||||||||||||||||||| ENUNCIADO |||||||||||||||||||||||||||||||||
# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
#
# ACTIVIDADES
# Un estudio recolectó muestras de distintas botellas de vino de una importante viña francesa. Estas mediciones
# están disponibles en el archivo EP13 Datos.csv que acompaña a este enunciado. El estudio incluyó 12
# mediciones de características del vino y la indicación de si cada vino es tinto o blanco:
#   
#   1. El equipo copia el enunciado del problema asignado como comentarios de un script R.
#   2. El equipo lee el enunciado, descarga el archivo de datos (EP13 Datos.csv) desde UVirtual y selecciona las
#       columnas para trabajar de acuerdo a las instrucciones.
#   3. El equipo construye los modelos solicitados usando la muestra correspondiente.
#   4. El equipo sube el script con las actividades anteriores comentando en detalle los pasos seguidos.
# 
# Fuera del horario de clases, cada equipo debe subir el script realizado UVirtual con el nombre "EP13-respuestagrupo-i", donde i es el número de grupo asignado. Las respuestas deben subirse antes de las 23:30 del lunes 3
# de julio.
# 
# PREGUNTA (todos los grupos)
# Se pide construir un modelo de regresión lineal simple y otro de regresión lineal múltiple para predecir la
# variable calidad, de acuerdo con las siguientes instrucciones:
#   1. Definir la semilla a utilizar, que corresponde a los últimos cuatro dígitos del RUN (sin considerar el dígito
#       verificador) del integrante de menor edad del equipo.
#   2. Seleccionar una muestra de 100 vinos.
#   3. Seleccionar de forma aleatoria 6 posibles variables predictoras.
#   4. Seleccionar, entre las variables que no fueron escogidas en el punto anterior, una que el equipo considere
#       que podría ser útil para predecir la variable calidad, justificando bien esta selección.
#   5. Usando el entorno R, construir un modelo de regresión lineal simple con el predictor seleccionado en el
#       paso anterior.
#   6. Agregue la variable seleccionada en el paso 4 al conjunto obtenido en el punto 3.
#   7. Usando herramientas para la exploración de modelos del entorno R, escoger entre dos y cinco predictores
#       de entre las variables presentes en el conjunto obtenido en el paso anterior para construir un modelo de
#       regresión lineal múltiple.
#   8. Evaluar los modelos y “arreglarlos” en caso de que tengan algún problema con las condiciones que deben
#       cumplir.
#   9. Evaluar el poder predictivo del modelo en datos no utilizados para construirlo (o utilizando validación
#       cruzada).

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

library(ggpubr)
library(dplyr)
library(tidyr)

setwd("C:/Users/agust/Desktop/Repo IME/IME-2023/EP13")
data <- read.csv2("EP13 Datos.csv")
data[["id"]] <- factor(1:nrow(data))

set.seed(2184)
muestra <- sample_n(data, 100)

# Se extraen de forma aleatoria las variables para la prueba
opciones <-  c("clase", "acidez.fija", "acidez.volatil", "acido.citrico", 
               "azucar.residual", "cloruros", "dioxido.azufre.libre", "dioxido.azufre.total",
               "densidad", "ph", "sulfatos", "alcohol")
variables <- sample(opciones, 6)
print(variables)

calidad <- muestra[["calidad"]]

# Se analiza la correlación existente entre las variables no seleccionadas
densidad <- muestra[["densidad"]]
acidesFija <- muestra[["acidez.fija"]]
acidoCitrico <- muestra[["acido.citrico"]]
cloruros <- muestra[["cloruros"]]
dioxidoAzufreLibre <- muestra[["dioxido.azufre.libre"]]
dioxidoAzufreTotal <- muestra[["dioxido.azufre.total"]]

correlacion_lineal <- cor(densidad, calidad)
cat("Densidad: ", correlacion_lineal, "\n")
correlacion_lineal <- cor(acidesFija, calidad)
cat("Acides fija: ", correlacion_lineal, "\n")
correlacion_lineal <- cor(acidoCitrico, calidad)
cat("Acido citrico: ", correlacion_lineal, "\n")
correlacion_lineal <- cor(cloruros, calidad)
cat("Cloruros: ", correlacion_lineal, "\n")
correlacion_lineal <- cor(dioxidoAzufreLibre, calidad)
cat("dioxido azufre libre: ", correlacion_lineal, "\n")
correlacion_lineal <- cor(dioxidoAzufreTotal, calidad)
cat("dioxido azufre total: ", correlacion_lineal, "\n")

# Se realiza el RML mediante mínimos cuadrados, utilizando la variable "densidad" y "calidad",
# se escoje a variable densidad dado a que es la que posee la correlación mas fuerte con la 
# variable "calidad", además, es una característica propia para medir la calidad de los vinos
# Cabe destacar que la correlación que existe entre las variables "densidad" y "calidad" es 
# muy baja, siento ésta de -0.3173, por lo que desde un comienzo se sabe que la fuerza de relación
# lineal que existe es baja

# Se verifica que los datos cumplan con las condiciones para desarrollar RML de mínimos cuadrados

muestras <- data.frame(calidad, densidad)

modelo <- lm(calidad~densidad, data = muestras)
print(summary(modelo))

p <- ggscatter(muestras, x = "densidad", y = "calidad", color = "blue", fill = "blue",
               xlab = "Densidad total [g/L]", ylab = "Calidad del vino")
p <- p + geom_smooth(method = lm, se = FALSE, colour = "red")
print(p)

print(shapiro.test(modelo$residuals))

# 1. Los datos dben presentar una relación lineal
#     En este caso, los datos tienden a aglomerarse en torno a la rectificación, sin embargo,
#     se pueden observar muchos datos atípicos, ésto era de esperar dado a la correlación que 
#     existe entre los datos
# 2. La distribución de los residuos debe ser cercana a la normal
#     Si bien existen muchos datos atípicos, los residuos tienen una distribución cercana a la
#     normal, comprobado por el test de Shappiro Wilk
# 3. La variabilidad de los puntos en torno a la linea de mínimos cuadrados debe ser
#   aproximadamente constante
#     Se puede observar que, a pesar de que hayan muchos datos atípicos, existe una distribución
#     relativamente constante de datos, es decir, que no se aglomeran en torno a un extremo o sector
#     del grafo alejado de la rectificación
# 4. Las observaciones deben ser independientes entre si
#     Las observaciones son menos del 10% de los datos totales, por lo que pueden considerarse como
#     independientes
# 
# Con todo lo anterior, se tiene que una RLS para las variables de "calidad" y "densidad", 
# está dado por:
#   calidad = 107.06 + 101.85*densidad

# Se agrega la variable 'calidad' al conjunto obtenido en el paso 3
# variables <- c(variables, "calidad")
# print(variables)

# -- 7. Usando herramientas para la exploración de modelos del entorno R, escoger entre dos 
# y cinco predictores de entre las variables presentes en el conjunto obtenido en el paso anterior
# para construir un modelo de regresión lineal múltiple.

# Creamos la formula para el modelo. Esta formula representa 'calidad ~ predictor1 + predictor2 + ... + predictorN'
f <- as.formula(paste("calidad", paste(c("densidad", variables), collapse = "+"), sep = " ~ "))

print(f)

# Creamos un modelo de regresión lineal con todas las variables predictoras
modelo_completo <- lm(f, data = muestra)

# Selección de predictores hacia adelante (forward)
# Comenzamos con el modelo simple (modelo) y nos movemos hacia el modelo completo
modelo_nulo = lm(calidad ~ 1, data = muestra)
modelo_forward = step(modelo_nulo, scope = list(lower = modelo_nulo, upper = modelo_completo), direction = "forward", trace = 1)

# Resumen del modelo propuesto por el método Forward
summary(modelo_forward)

# Selección de predictores hacia atrás (backward)
# Comenzamos con el modelo completo y nos movemos hacia el modelo simple
modelo_backward = step(modelo_completo, scope = list(lower = modelo, upper = modelo_completo), direction = "backward", trace = 1)

# Resumen del modelo propuesto por el método Backward
summary(modelo_backward)

# Observamos, que para ambos metodos, el modelo propuesto incluye: "densidad", "dioxido.azufre.libre", "acido.citrico"
# Otras Observaciones
# El intercepto (valor cuando todas las variables predictoras son 0) es 108.2.
# La "densidad" tiene un coeficiente negativo de -103.8, lo que significa que a medida que aumenta la densidad, la calidad tiende a disminuir.
# El "dioxido.azufre.libre" tiene un coeficiente positivo de 0.01322, lo que indica que a medida que aumenta esta variable, la calidad también tiende a aumentar.
# El "acido.citrico" tiene un coeficiente positivo de 1.109, sugiriendo que a medida que este aumenta, la calidad también aumenta.


# -- 8. Evaluar los modelos y “arreglarlos” en caso de que tengan algún problema con las condiciones que deben cumplir.

# Condiciones RLM

# 1. Las variables predictoras deben ser cuantitativas o dicotómicas (de ahí la necesidad de variables indicadoras para manejar más de dos niveles).
#    R: Las variables predictoras son todas numéricas a nivel de intervalo y ninguna de ellas corresponde a una constante.

# 2. La variable de respuesta debe ser cuantitativa y continua, sin restricciones para su variabilidad.
#    R: Las variables "calidad" es cuantitativa y continua.

# 3. Los predictores deben tener algún grado de variabilidad (su varianza no debe ser igual a cero). En otras palabras, no pueden ser constantes.
#    R: Observando los resultados, ninguna varianza es igual a 0.

varianzas_densidad <- var(muestra$densidad)
varianzas_dioxido.azufre.libre <- var(muestra$dioxido.azufre.libre)
varianzas_acido.citrico <- var(muestra$acido.citrico)

print(varianzas_densidad) 
print(varianzas_dioxido.azufre.libre)
print(varianzas_acido.citrico)


# 4. No debe existir multicolinealidad. Esto significa que no deben existir relaciones lineales fuertes entre dos o más predictores (coeficientes de correlación altos).
#    R: Observando los resultados, podemos ver que se cumple la condicion

vifs <- vif(modelo_forward)
cat("\nVerificar la multicolinealidad :\n")
cat("- VIFs :\n")
print( vifs )

cat("- Tolerancias :\n")
print(1/vifs)

cat("- VIF medio :", mean (vifs), "\n")


# 5. Los residuos deben ser homocedásticos (con varianzas similares) para cada nivel de los predictores.
#    R: Obtenemos como resultado de la prueba: Chisquare = 0.4348471, Df = 1, p = 0.50962, por lo que el supuesto de homocedasticidad se cumple.

prueba.Ncv <- ncvTest(modelo_forward)
print(prueba.Ncv)

# 6. Los residuos deben seguir una distribución cercana a la normal centrada en cero.
#    R: Obtenemos como resultado de la prueba: p-value = 0.1485, por lo que podemos asumir que el supuesto se cumple.

prueba.shapiro = shapiro.test(modelo_forward$residuals)
print(prueba.shapiro)


# 7. Los valores de la variable de respuesta son independientes entre sí.
#    R: Obtenemos como resultado de la prueba: p = 0.334, por lo que podemos concluir que los residuos son, en efecto, independientes.

prueba.durbin <- durbinWatsonTest(modelo_forward)
print(prueba.durbin)


# 8. Cada predictor se relaciona linealmente con la variable de respuesta.
#    R: Como observamos en los resultados, el supuesto se cumple

datos_PR <- select(muestra, densidad, dioxido.azufre.libre, acido.citrico)
datos_PR$log_densidad <- log(datos_PR$densidad)
correlacion <- round(cor(datos_PR$log_densidad), 2)
correlacion
corrplot(correlacion, method="number", type="upper")


# -- 9.	Evaluar el poder predictivo del modelo en datos no utilizados para construirlo (o utilizando validación cruzada).

# Crear conjuntos de entrenamiento y prueba .
n <- nrow(muestra)
n_entrenamiento <- floor(0.7 * n)
muestra_sample <- sample.int(n = n, size = n_entrenamiento, replace = FALSE)
entrenamiento <- muestra[muestra_sample, ]
prueba <- muestra[-muestra_sample, ]

# Ajustar modelo con el conjunto de entrenamiento.
modelo_model <- lm(calidad ~ densidad, data = entrenamiento)
print(summary(modelo))

# Calcular error cuadrado promedio para el conjunto de entrenamiento.
mse_entrenamiento <- mean (modelo_model$residuals ** 2)
cat("MSE para el conjunto de entrenamiento: ", mse_entrenamiento, "\n")

# Hacer predicciones para el conjunto de prueba.
predicciones <- predict(modelo_model, prueba)

# Calcular error cuadrado promedio para el conjunto de prueba.
error <- sapply(prueba[["calidad"]],as.double) - predicciones
mse_prueba <- mean(error ** 2)
cat("MSE para el conjunto de prueba: ", mse_prueba)
# 
# El coeficiente de "densidad" es -101.85, lo que indica que hay una relación negativa entre "densidad" y "calidad". 
# Esto significa que a medida que aumenta la densidad, se espera que la calidad disminuya (según este modelo).










