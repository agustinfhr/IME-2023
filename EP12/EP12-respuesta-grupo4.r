# Grupo 4
# Integrantes: 
#            - AGUSTÍN FRANCISCO HENRÍQUEZ ROJAS
#            - NICOLAS M. VALDES HERRERA 


# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# |||||||||||||||||||||||||||||||| ENUNCIADO |||||||||||||||||||||||||||||||||
# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# 
# Copiar el enunciado de los problemas asignados como comentarios de un script R.
#   2. El equipo lee los enunciados entregados y revisa los datos gráficamente.
#   3. Transformación de datos:
#       a. El equipo discute las posibles transformaciones y se decide por alguna(s) de ellas.
#       b. El equipo aplica las transformaciones y realiza la prueba paramétrica de hipótesis correspondiente.
#   4. Métodos robustos:
#       a. El equipo discute qué prueba tradicional (paramétrica) correspondería en cada caso y, considerando
#         esto, concuerda una prueba no paramétrica que ha de aplicar.
#       b. El equipo aplica la prueba de hipótesis correspondiente usando el ambiente R.
#       c. El equipo identifica las hipótesis nula y alternativa en cada caso y entrega una conclusión,
#         interpretándola en el caso estudiado.
# 
# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# |||||||||||||||||||||||||||||||| PREGUNTAS |||||||||||||||||||||||||||||||||
# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# 
# 1. Tras el éxito obtenido recientemente por un equipo de agrónomos en la mejora del cultivo de manzanas de
#     exportación, con la ayuda de algunos modelos estadísticos, un colega dedicado a la producción de peras
#     desea estudiar algunas características de sus productos. Para ello, ha registrado los pesos (en gramos) de
#     algunas unidades (cada una de un árbol diferente) de dos variedades distintas, Winter Nelly y Golden Bosc,
#     durante la semana 15 de crecimiento. Desea saber si el peso de ambas variedades en esta etapa de su
#     desarrollo es el mismo. ¿Qué puede concluir?
#   
#     Winter Nelly  Golden Bosc
#     213,41        170,8754
#     185,9876      144,3119
#     221,9353      166,2316
#     320,1673      114,3064
#     201,3828      112,9957
#     180,1779      126,6177
#     168,4547      117,5733
#     222,9346      128,7313
#     183,4629      143,4207
#     210,4415
#     138,7852
#     272,3915
# 
# 2. Analice la primera pregunta abordada en el ejercicio práctico 11, con los mismos datos, utilizando un
#     método robusto adecuado.
# 3. Analice la segunda pregunta abordada en el ejercicio práctico 11, con los mismos datos, utilizando un
#     método robusto adecuado.
# 
# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# |||||||||||||||||||||||||||||||| PREGUNTA 1 ||||||||||||||||||||||||||||||||
# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

# 1. Tras el éxito obtenido recientemente por un equipo de agrónomos en la mejora del cultivo de manzanas de
# exportación, con la ayuda de algunos modelos estadísticos, un colega dedicado a la producción de peras
# desea estudiar algunas características de sus productos. Para ello, ha registrado los pesos (en gramos) de
# algunas unidades (cada una de un árbol diferente) de dos variedades distintas, Winter Nelly y Golden Bosc,
# durante la semana 15 de crecimiento. Desea saber si el peso de ambas variedades en esta etapa de su
# desarrollo es el mismo. ¿Qué puede concluir?
  

if (!require ( car )) {
  install.packages ("car" , dependencies=TRUE)
  require (car)
}

# Importación de las librerias 
library(car)

# -- Obtienen correctamente los datos necesarios para realizar la prueba solicitada, en un formato pertinente.

# Primero, se necesita cargar los datos en R.

Winter_Nelly <- c(213.41, 185.9876, 221.9353, 320.1673, 201.3828, 180.1779, 168.4547, 222.9346, 183.4629)
Golden_Bosc <- c(170.8754, 144.3119, 166.2316, 114.3064, 112.9957, 126.6177, 117.5733, 128.7313, 143.4207, 210.4415, 138.7852, 272.3915)

# -- Argumentan convincentemente el incumplimiento de todas las condiciones requeridas para aplicar una prueba robusta adecuada.

# Prueba de Shapiro-Wilk para comprobar normalidad
print(shapiro.test(Winter_Nelly))
print(shapiro.test(Golden_Bosc))
# Ambos p-valores son menores que 0.05, se rechaza la hipótesis nula de que los datos 
# siguen una distribución normal. Por lo tanto, los datos no cumplen con una distribución normal.

# Se realiza un histograma y un Q-Q plot para visualizar si los datos siguen una distribución normal.
# Winter Nelly.
par(mfrow=c(2, 1))
hist(Winter_Nelly, main = "Winter Nelly", xlab = "Peso en gramos")
qqnorm(Winter_Nelly); qqline(Winter_Nelly)

# Golden Bosc.
par(mfrow=c(2, 1))
hist(Golden_Bosc, main = "Golden Bosc", xlab = "Peso en gramos")
qqnorm(Golden_Bosc); qqline(Golden_Bosc)

# A partir de ambos gráficos, se puede obversar que los datos no tienden a distribuirse de forma normal.

# -- Seleccionan, argumentando con gráficos u otras herramientas, una transformación apropiada para poder
# aplicar una prueba paramétrica a los datos.

# Los datos originales no siguen una distribución normal, como lo indica la prueba de Shapiro-Wilk. 
# Ante esta situación, decidimos realizar una transformación logarítmica, debido a que este tipo de transformación es útil 
# para manejar datos con distribuciones sesgadas, en este caso, presentan un sesgo positivo (derecho), 
# ayudando a estabilizar la varianza y a normalizar la distribución de los datos.

# Transformación logarítmica de los datos.
Winter_Nelly_log <- log(Winter_Nelly)
Golden_Bosc_log <- log(Golden_Bosc)

# Graficar los datos transformados.
par(mfrow = c(1, 2))
hist(Winter_Nelly_log, main = "Winter Nelly Log-Transformado", xlab = "Log-Peso")
hist(Golden_Bosc_log, main = "Golden Bosc Log-Transformado", xlab = "Log-Peso")

# -- Formulan con claridad y explícitamente hipótesis nulas y alternativas adecuadas para responder la pregunta
# planteada, tanto en lenguaje natural como en lenguaje matemático.

# Hipótesis propuesta para el problema
# Hipótesis nula (H0): No hay diferencia en las medias logarítmicas de peso entre las variedades Winter Nelly y Golden Bosc.
# 
# Hipótesis alternativa (H1): Existe una diferencia en las medias logarítmicas de peso entre las variedades Winter Nelly y Golden Bosc.

# Formulación Matemática
# H0: mu_WN = mu_GB

# HA: mu_WN != mu_GB

# Verificar la normalidad con la prueba de Shapiro-Wilk.
print(shapiro.test(Winter_Nelly_log))
print(shapiro.test(Golden_Bosc_log))

# Tras realizar la transformación logarítmica, las pruebas de Shapiro-Wilk indican una mejor aproximación a la normalidad,
# permitiéndonos así utilizar pruebas estadísticas paramétricas.
# Los p-valores son ahora mayores que 0.05, lo que sugiere que no podemos rechazar la hipótesis nula de que los datos siguen 
# una distribución normal. Por lo tanto, podemos aplicar una prueba paramétrica como la prueba t.

# -- Realizan de forma completa y correcta una prueba paramétrica adecuada para la pregunta planteada en el
# enunciado, utilizando los datos correctos.

# Prueba t de Student para evaluar la normalidad de los datos.
t.test(Winter_Nelly_log, Golden_Bosc_log, var.equal = TRUE)

# La prueba t para muestras independientes sobre los datos transformados logarítmicamente resulta en un p-valor de 0.004805. 
# Este es un resultado estadísticamente significativo (p < 0.05), lo que se puede rechazar la hipótesis nula de que las medias 
# logarítmicas de peso de las variedades Winter Nelly y Golden Bosc son iguales.
# 
# Por lo tanto, podemos concluir que, basándonos en la transformación logarítmica de los datos, existe una diferencia significativa 
# en las medias logarítmicas de peso entre las variedades Winter Nelly y Golden Bosc.

# -- Entregan una conclusión completa a la pregunta planteada en el enunciado, en el dominio original de los
# datos, basándose en el resultado correcto de una prueba adecuada y el contexto del problema.

# Conclusión:
# Los datos originales no se distribuyeron normalmente, lo que nos llevó a aplicar una transformación logarítmica. 
# Después de transformar los datos, realizamos una prueba t y obtuvimos un p-valor de 0.004805.
# Este valor es menor que 0.05, lo que nos lleva a rechazar la hipótesis nula de que no hay diferencia en las medias.
# Por lo tanto, aceptamos la hipótesis alternativa de que hay una diferencia significativa entre las medias logarítmicas de los pesos de las variedades de peras Winter_Nelly y Golden_Bosc.
# En el dominio original de los datos, esto implica que las peras de la variedad Winter_Nelly tienden a ser más pesadas que las de la variedad Golden_Bosc.
# Desde el punto de vista práctico, estos hallazgos podrían ser útiles para los productores de peras, quienes podrían considerar cultivar más peras de la variedad Winter_Nelly si el peso de las peras es un factor importante para la venta de sus productos.



# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# |||||||||||||||||||||||||||||||| PREGUNTA 2 ||||||||||||||||||||||||||||||||
# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

# Como grupo, hemos tomado la desición de que el estadístico de interes serán las medias dos muestras que sean 
# independientes entre si, para este caso serán la edad para hombres y mujeres, y la 
# variable en común será el estado "salariado" para la variable "ch1". 
#  
# Con lo anterior se establece la siguiente pregunta:
# "Para hombres y mujeres de la región metropolitana, ¿la media de la edad es igual para hombres y mujeres que se 
# encuentren en un estado ocupacional "asalariados"?

# Se establecen las hipótesis a contrastar:
# H0: la media de edad para hombres y mujeres asalariados es igual. U1 = U2
# H1: la media de edad para hombres y mujeres asalariados es distinta U1 != U2
# U1: media de edad para hombres asalariados
# U2: media de edad para mujeres asalariados

if (!require(dplyr)) {
  install.packages("dplyr", dependencies = TRUE)
  require(dplyr) }

if (!require(WRS2)) {
  install.packages("WRS2", dependencies = TRUE)
  require(WRS2) }

if (!require( ggpubr)) {
  install.packages(" ggpubr", dependencies = TRUE)
  require( ggpubr) }

# SE ESTABLECE UNA SEMILLA
set.seed(123)

# SE IMPORTAN LOS DATOS
setwd("C:/Users/Ekayn/Desktop/IME/EP 12")
data <- read.csv2("EP12 Datos.csv")
data[["id.vivienda"]] <- factor(1:nrow(data))

# SE FILTRAN HOMBRES Y MUJERES DE LA REGIÓN METROPOLITANA ASALARIADOS
datos <- filter(data, region == "Region Metropolitana de Santiago")
datos <- filter(datos, ch1 == "Asalariado")

# SE EXTRAE UNA MUESTRA DE TAMAÑO 500
datos <- sample_n(datos, 500)

# SE FILTRAN HOMBRES Y MUJERES
mujeres <- filter(datos, sexo == "Mujer")
hombres <- filter(datos, sexo == "Hombre")

# SE OBTIENEN LAS EDADES DE HOMBRES Y MUJERES
edad_M <- mujeres[["edad"]]
edad_H <- hombres[["edad"]]

# SE CREA UN DATA CON LAS EDADES Y UN SEÑALADOR DE GENERO
edades <- c(edad_M, edad_H)
genero <- c(rep("M", length(edad_M)), rep("H", length(edad_H)))
datos_MyH <- data.frame(edades, genero)

# SE COMPRUEBA LA NORMALIDAD
g <- ggqqplot(datos_MyH, x = "edades", facet.by = "genero",
              palette = c("blue", "red") , color = "genero")

print(g)

# OBSERVACIÓN: OBSERVANDO LOS GRAFICOS SE PUEDE VER QUE LOS DATOS CLARAMENTE NO SIGUEN UNA DISTRIBUCIÓN
# NOMAL. ANALIZANDO LOS DATOS SE PUEDE OBSERVAR QUE SON INDEPENDIENTES ENTRE SI DADO A QUE SON DE PERSONAS
# DIFERENTES, CON ÉSTO, SE PROPONE REALIZAR UNA "PRUEBA DE YUEN DE DOS MUESTRAS INDEPENDIENTES"

# SE ESTABLECE EL NIVEL DE SIGNIFICANCIA
alfa <- 0.05

# SE ESTABLECE LA PODA DEL 20%
gamma <- 0.2
n_M <- length(edad_M)
n_H <- length(edad_H)

poda_M <- n_M * gamma
poda_H <- n_H * gamma

# SE APLICA LA PODA A LAS MUESTRAS
M_truncada <- edad_M[poda_M : (n_M - poda_M)]
H_truncada <- edad_H[poda_H : (n_H - poda_H)]

# SE CREA EL DATA PARA MUESTRAS PODADAS
edades_Yuen <- c(M_truncada, H_truncada)
genero_Yuen <- c(rep("M", length(M_truncada)), rep("H", length(H_truncada)))
datos_truncados <- data.frame(edades_Yuen, genero_Yuen)

g <- ggqqplot(datos_truncados, x = "edades_Yuen", facet.by = "genero_Yuen",
              palette = c("blue", "red") , color = "genero_Yuen")

print(g)

# SE PUEDE OBSERVAR QUE EXISTE UN PORCENTAJE SIGNIFICANTE DE DATOS QUE SIGUEN SIENDO ATÍPICOS, ES POR ELLO QUE, 
# EN VEZ DE REALIZAR LA PRUEBA DE YUEN PARA DOS MUESTRAS INDEPENDIENTES, SE REALIZARA UNA PRUEBA DE YUEN UTILIZANDO 
# BOOTSTRAPPING

# SE ESTABLECE LA CANTIDAD DE MUESTRAS A GENERAR CON BOOTSTRAPPING, EL ALFA SIGUE SIENDO 0.05
bootstrap <- 999

# SE APLICA LA PRUEBA CON LA MEDIA
prueba_media <- pb2gen(edades ~ genero,
                       data = datos_MyH,
                       est = "mean",
                       nboot = bootstrap)

cat ("\n\ nResultado al usar la media como estimador \n\n")
print(prueba_media)

# CONCLUSIÓN: LA PRUEBA DIO COMO RESULTADO UN P-VALUE SUPERIOR AL NIVEL DE SIGNIFICANCIA, ES POR ESTO QUE DETERMINAMOS
# QUE NO EXISTE EVIDENCIA SUFICIENTE PARA RECHAZAR LA HIPÓTESIS NULA A FAVOR DE LA ALTERNATIVA, POR LO QUE SE PUEDE 
# CONCLUÍR CON UN NIVEL DE CONFIANZA DEL 95% QUE LA DIFERENCIA DE LAS MEDIAS DE EDAD PARA HOMBRES Y MUJERES ASALARIADOS 
# SON IGUALES

# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# |||||||||||||||||||||||||||||||| PREGUNTA 3 ||||||||||||||||||||||||||||||||
# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

# 3. Analice la segunda pregunta abordada en el ejercicio práctico 11, con los mismos datos, utilizando un
# método robusto adecuado.


if (!require(boot)) {
  install.packages("boot", dependencies = TRUE )
  require (boot)
}
if (!require(simpleboot)) {
  install.packages("simpleboot", dependencies = TRUE )
  require (simpleboot)
}

if (!require(FSA)) {
  install.packages("FSA", dependencies = TRUE )
  require (FSA)
}

# Explortacion de las librerias 

library(dplyr)
library(ggpubr)
library(ggplot2)

# -- Proponen una pregunta de investigación, interesante y novedosa, que involucra la comparación de las
# medias de más de dos grupos independientes de personas encuestadas en la Encuesta Casen 2017.

# Se quiere realizar un estudio, donde se desea saber si promedio de los ingresos (ytot)  
# es igual según el nivel de estudios que tenga la población
# (Nivel Enseñanza Media, Nivel Técnico y Nivel Profesional o Superior)

# -- Formulan explícitamente hipótesis nula y alternativa correctas, que involucran la comparación de las medias
# de una variable numérica de más de dos grupos independientes, para responder la pregunta de
# investigación que plantean.

# Hipótesis a Contrastar:
# H0: El promedio de los ingresos de la población es el mismo para cada uno de los
#     Niveles de educación (Media, Técnico y Profesional)

# H0: El promedio de los ingresos es diferente en al menos uno de los niveles de 
#     educación (Media, Técnico y Profesional).

# Formulación Matemática

# H0: mu_1 = mu_2 = mu_3

# H1: mu_1 != mu_2 o mu_1 != mu_3 o mu_2 != mu_3  

# ------------------------------- DATOS ----------------------------------------

dir <- "C:/Users/agust/Desktop/Repo IME/IME-2023/EP12"
basename <- "EP11 Datos.csv"
file <- file.path(dir, basename)
poblacion <- read.csv2(file = file)

# -- Obtienen una muestra de datos de acuerdo a lo solicitado, revisando su comportamiento con gráficos o
# pruebas estadísticas y pronunciándose explícitamente sobre la necesidad de utilizar métodos para datos
# problemáticos.

set.seed(1234)
n <- 500
Ingresos_Media <- select(poblacion %>% filter(educ == "M. Hum. Completa"), contains("ytot"))
Ingresos_Tecnico <- select(poblacion %>% filter(educ == "Tecnico Nivel Superior Completo"), contains("ytot") )
Ingresos_Profesional <- select(poblacion %>% filter(educ == "Profesional Completo"), contains("ytot"))

# Se muestrean los ingresos según el nivel de estudios
m_Media <- sample(Ingresos_Media$ytotcorh, n)
m_Tecnico <- sample(Ingresos_Tecnico$ytotcorh, n)
m_Profesional <- sample(Ingresos_Profesional$ytotcorh, n)

# Combinar los datos en un solo data frame
df <- data.frame(
  Ingresos = c(m_Media, m_Tecnico, m_Profesional),
  Educacion = factor(c(rep("Media", n), rep("Tecnico", n), rep("Profesional", n)))
)

# Prueba de Shapiro-Wilk para comprobar la normalidad
shapiro_test_Media <- shapiro.test(m_Media)
shapiro_test_Tecnico <- shapiro.test(m_Tecnico)
shapiro_test_Profesional <- shapiro.test(m_Profesional)

# Imprimir los resultados 
print(paste("Test de Shapiro-Wilk para Educación Media: p-value =", shapiro_test_Media$p.value))
print(paste("Test de Shapiro-Wilk para Educación Técnica: p-value =", shapiro_test_Tecnico$p.value))
print(paste("Test de Shapiro-Wilk para Educación Profesional: p-value =", shapiro_test_Profesional$p.value))

# Gráfico QQ para Educación Media
ggplot(df[df$Educacion == "Media",], aes(sample = Ingresos)) +
  stat_qq() +
  stat_qq_line() +
  ggtitle("Gráfico QQ para Ingresos - Educación Media") +
  theme_minimal()

# Gráfico QQ para Educación Técnica
ggplot(df[df$Educacion == "Tecnico",], aes(sample = Ingresos)) +
  stat_qq() +
  stat_qq_line() +
  ggtitle("Gráfico QQ para Ingresos - Educación Técnica") +
  theme_minimal()

# Gráfico QQ para Educación Profesional
ggplot(df[df$Educacion == "Profesional",], aes(sample = Ingresos)) +
  stat_qq() +
  stat_qq_line() +
  ggtitle("Gráfico QQ para Ingresos - Educación Profesional") +
  theme_minimal()

# -- Basándose en el análisis anterior, proponen explícitamente una prueba robusta que permite docimar las
# hipótesis propuestas, justificando su elección apropiadamente.

# Dado que todos los valores p son significativamente menores a 0.05, se rechaza la hipótesis nula de que los datos 
# se distribuyen normalmente en cada uno de los grupos de educación. Esto significa que los ingresos en cada uno de 
# los grupos de educación no siguen una distribución normal, según la prueba de Shapiro-Wilk. Por lo tanto, es apropiado 
# utilizar métodos no paramétricos, como la prueba de Kruskal-Wallis.
# 
# Esta distribuciones también se pueden apreciar en los gráficos Q-Q. 
# Presentan desviaciones significativas de la línea recta teórica en los gráficos, 
# lo que también se interpreta que los datos no se distribuyen normalmente.

# -- Realizan, de forma completa y sin errores, una prueba robusta que permite responder la pregunta de
# investigación que plantean, usando una muestra de datos adecuada, obteniendo un p valor o intervalo de
# confianza correcto.

# Prueba de Kruskal-Wallis
kruskal_result <- kruskal.test(Ingresos ~ Educacion, data = df)
print(kruskal_result)

# El valor p es muy pequeño, menor a 2.2e-16, lo que es prácticamente 0, por lo que es una evidencia fuerte 
# de que al menos uno de los grupos difiere en ingresos de los otros.

# -- Aplican, de forma completa y sin errores, un procedimiento post-hoc robusto, usando una muestra de datos
# adecuada, obteniendo un p valor o intervalo de confianza correcto.

# Realizamos la prueba de Dunn post-hoc después de la prueba de Kruskal-Wallis
# Esto nos indicará qué grupos específicos son diferentes entre sí
dunn_result <- dunnTest(df$Ingresos, g=df$Educacion, method="bonferroni")

# Resultados de la prueba de Dunn
print(dunn_result)

# -- Entrega una conclusión correcta y completa a la pregunta planteada, basándose en el resultado de la prueba
# realizada y el contexto del problema.

# Conclusión:
# De acuerdo a los resultados obtenidos mediante la prueba de Kruskal-Wallis, se puede concluir que existen diferencias 
# significativas en los ingresos promedio entre los distintos niveles de educación (Media, Técnico, Profesional). 
# El valor p obtenido de la prueba fue muy cercano a cero, lo que indica que es muy poco probable que las diferencias 
# observadas en las medianas de ingresos se deban al azar. Por lo tanto, rechazamos la hipótesis nula de que todas las
# medianas de ingresos son iguales.
# 
# Además, la prueba post-hoc de Dunn proporcionó información sobre las diferencias específicas entre los grupos. 
# Según los resultados, todos los grupos difieren significativamente entre sí en términos de ingresos. Específicamente,
# las personas con una educación Profesional tienden a tener ingresos significativamente más altos que aquellas con educación 
# Técnica y Media, mientras que las personas con educación Técnica también tienen ingresos significativamente más altos que 
# aquellas con educación Media.
# 
# Estos resultados tienen implicaciones importantes en términos socioeconómicos. el nivel de educación puede 
# tener un impacto significativo en los ingresos de una persona, lo que a su vez puede afectar a una serie de otras variables.





















