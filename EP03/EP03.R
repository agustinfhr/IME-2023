#Grupo4: Agustín Henríquez, Jaime Carrasco, David Valero

library(ggpubr)
basename <- "EP03 Datos.csv"
#file <- file.path(dir, basename)
setwd("C:/Users/agust/Desktop/Repo IME/IME-2023/EP03")
poblacion <- read.csv2(basename)
tamano <- nrow(poblacion)
ingreso <- as.numeric(poblacion[["ytot"]])
poda <- 0.2
q20 <- quantile(ingreso, poda)
q80 <- quantile(ingreso, 1 - poda)
ingreso.podado <- ingreso[ingreso > q20 & ingreso < q80]
tamano.podado <- length(ingreso.podado)
media.ingreso <- mean(ingreso.podado)
sd.ingreso <- sqrt (sum((ingreso.podado - media.ingreso)^2) / tamano.podado )

#2 definir nuestra propia semilla
set.seed(420)
ingreso.normal <- rnorm(5000, mean = media.ingreso, sd = sd.ingreso)

#3 generar distribución Z
# z = x - u / sd
disZ <- (ingreso.normal - media.ingreso) / sd.ingreso

#4 definir grados de libertad
k = 6
k2 = 10

# generar distribucón chi cuadrado 
xi2.1 = disZ^((k/2)-1)*exp(1)^(-disZ/2)/2^(disZ/2)*gamma(k/2)
print (xi2.1)

xi2.2 = disZ^((k2/2)-1)*exp(1)^(-disZ/2)/2^(disZ/2)*gamma(k2/2)
print (xi2.2)

#5 generar distribución F
f <- (xi2.1/k)/(xi2.2/k2)
print (f)

#6.Gráfico Distribuciones
#Distribucion Z:
hist(disZ, plot=TRUE, 
     main = "Distribucion Z", 
     col = "purple",
     border = "blue", 
     xlab = "Distribucion Z", 
     ylab = "Frecuencia")

#Distribuciones xi2:
hist(xi2.1, plot=TRUE, 
     main = "Distribucion xi2", 
     col = "yellow",
     border = "orange", 
     xlab = "Distribucion xi2.1", 
     ylab = "Frecuencia")

hist(xi2.2, plot=TRUE, 
     main = "Distribucion xi2", 
     col = "yellow",
     border = "orange", 
     xlab = "Distribucion xi2.2", 
     ylab = "Frecuencia")

#Distribución F:
hist(f, plot=TRUE, 
     main = "Distribucion F", 
     col = "pink",
     border = "red", 
     xlab = "Distribucion F", 
     ylab = "Frecuencia")

#Distribuciones Discretas
#7. Se define la semilla y la cantidad de repeticiones.
set.seed(101)
n.repeticiones <- 100
ensayo <- function(x)
  ifelse(sample(poblacion[["sexo"]], 1) == "Mujer", 1, 0)

cien.repeticiones <- sapply(1:n.repeticiones, ensayo)

#8. Se genera la distribución binomial a partir de la distribución de bernoulli.
binomial <- function(x){
  return( sum(  sapply(1:100, ensayo)  )  )
}
dis.binomial <- sapply(1:1000,binomial)

#9. Se crea la distribución geométrica a partir de la distribución de bernoulli.
geometrica <- function(x){
  rep <- sapply(1:100, ensayo)
  return( which(rep == 1)[1]  )
}
dis.geometrica <- sapply(1:1000, geometrica)

#10. Se crea la distribución binomial negativa a partir de la distribución de bernoulli.
binomialnegativa <- function(x){
  rep <- sapply(1:100, ensayo)
  return( which(rep == 1)[5]  )
}
dis.binomialnegativa <- sapply(1:1000, binomialnegativa)

#11.Graficos

#Distribución binomial
hist(dis.binomial, plot=TRUE, 
     main = "Distribucion binomial", 
     col = "purple",
     border = "blue", 
     xlab = "Distribucion binomial", 
     ylab = "Frecuencia")

#Distribución geometrica
hist(dis.geometrica, plot=TRUE, 
     main = "Distribucion geometrica", 
     col = "yellow",
     border = "orange", 
     xlab = "Distribucion geometrica", 
     ylab = "Frecuencia")

#Distribución binomial negativa
hist(dis.binomialnegativa, plot=TRUE, 
     main = "Distribucion binomial negativa", 
     col = "pink",
     border = "red", 
     xlab = "Distribucion binomial negativa", 
     ylab = "Frecuencia")



