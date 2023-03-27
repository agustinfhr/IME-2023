basename <- "EP03 Datos.csv"
#file <- file.path(dir, basename)
setwd("C:/Users/agust/Desktop/Ramos 2023/IME/EP02/IME-2023/EP03")
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
# definir nuestra propia semilla
set.seed(420)
ingreso.normal <- rnorm(5000, mean = media.ingreso, sd = sd.ingreso)

#generar distribucion Z




set.seed(133)
n.repeticiones <- 20
ensayo <- function(x)
  ifelse(sample(poblacion[["sexo"]], 1) == "Mujer", 1, 0)
veinte.repeticiones <- sapply(1:n.repeticiones, ensayo)

