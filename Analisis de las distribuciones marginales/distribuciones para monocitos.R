###  librerias ###
library(readxl);library(GGally);library(ggplot2);library(patchwork)
library(rriskDistributions);library("fitdistrplus");library(stats)
library(MASS);library(nortest);library(goftest);library(rmutil); library(e1071)

#Base de datos
Datos <- read_excel("BD_ALGORITMOS_DENGUE_ACT.xlsx",sheet = "Hoja1")

#Se vuelve factor el patron de oro
Datos$DX<-factor(Datos$DX)
enfermos <- Datos[Datos$DX == "1", ]
noenfermos <- Datos[Datos$DX == "0", ]
############################################################################################3
fit.cont(Datos$nmono)
# Parámetros de la normal inversa gaussiana----
tu_params <- c(alpha = 1.47, beta = 0.832, delta = 0.48, mu = 0.39)
# Realizar la prueba de Anderson-Darling para la distribución NIG
resultado_ad <- ad.test(Datos$nmono, "dnig", alpha = 1.47, beta = 0.832, delta = 0.39, mu = 0.48)
# Mostrar los resultados
print(resultado_ad)

x11()
hist(datos$nmono,freq=F,ylim = c(0,1.5),breaks=50)
# Ajustar la ojiva de una distribución de Levy
x <- seq(0, 15, length.out = 1000)
y = fBasics::dnig(x,alpha = 1.47, beta = 0.832, delta = 0.39, mu = 0.48)
lines(x, y, type = "l", col = "red", lwd = 2)
legend("topright", legend = "distribucion normal inversa gaussiana", col = "red", lty = 1, lwd = 2)

# Calcular AIC y BIC
n <- length(Datos$linfocitos_neutrofilos)
llf <- sum(fBasics::dnig(Datos$linfocitos_neutrofilos, alpha = 1.47, beta = 0.832, delta = 0.39, mu = 0.48, log = TRUE))
aic <- 2 * 2 - 2 * llf
bic <- log(n) * 2 - 2 * llf

library(AdequacyModel)





log_likelihood <- function(params, data) {
  shape <- params[1]
  rate <- params[2]
  n <- length(data)
   likelihood = -sum(log(VGAM::derlang(data,mean,sd)))
  
  return(likelihood)  # Devuelve la verosimilitud
}


# Estimar los parámetros de máxima verosimilitud utilizando optim
initial_params <- c(shape = 1,rate = 1)  # Valores iniciales de los parámetros
upper_bounds <- c(mean = Inf, sd = Inf)
fit <- optim(initial_params, log_likelihood, data = Datos$linfocitos_neutrofilos, method = "L-BFGS-B", upper=upper_bounds)

fit$par












####qqplot

# Calcula los cuantiles de los datos observados y de la distribución NIG
cuantiles_datos <- quantile(Datos$nmono, probs = seq(0, 1, length.out = n))
cuantiles_nig <- qnig(seq(0, 1, length.out = n), alpha = 1.47, beta = 0.832, delta = 0.39, mu = 0.48)

# Grafica el Q-Q plot
plot(cuantiles_nig, cuantiles_datos, main = "Gráfico Q-Q para NIG", xlab = "Cuantiles de la distribución NIG",
     ylab = "Cuantiles de los datos observados", col = "blue")
abline(0, 1, col = "red") # Agrega una línea de referencia para una distribución perfecta












#laplace
log_likelihood <- function(params, data) {
  m <- params[1]
  s <- params[2]
  n <- length(data)
  likelihood = -sum(log(dlaplace(data,m,s)))
  
  return(likelihood)  # Devuelve la verosimilitud
}
# Estimar los parámetros de máxima verosimilitud utilizando optim
initial_params <- c(m= 0,s=0.5)  # Valores iniciales de los parámetros
upper_bounds <- c(m = Inf, s= Inf)
fit <- optim(initial_params, log_likelihood, data = Datos$linfocitos_neutrofilos, method = "L-BFGS-B", upper=upper_bounds)

fit$par
ad.test(Datos$nmono, "plaplace",m = 0.3729508,s=0.3585035)




# Calcula los cuantiles de los datos observados y de la distribución NIG
cuantiles_datos <- quantile(Datos$nmono, probs = seq(0, 1, length.out = n))
cuantiles_nig <- qlaplace(seq(0, 1, length.out = n), m = 0.6,s=0.38)

# Grafica el Q-Q plot
plot(cuantiles_nig, cuantiles_datos, main = "Gráfico Q-Q para NIG", xlab = "Cuantiles de la distribución NIG",
     ylab = "Cuantiles de los datos observados", col = "blue")
abline(0, 1, col = "red") # Agrega una línea de referencia para una distribución perfecta




x11()
hist(Datos$nmono,freq=F,ylim = c(0,2.5),breaks=50)
# Ajustar la ojiva de una distribución de Levy
x <- seq(0, 15, length.out = 1000)
y = dlaplace(x,m = 0.6,s=0.38)
lines(x, y, type = "l", col = "red", lwd = 2)
legend("topright", legend = "distribucion normal inversa gaussiana", col = "red", lty = 1, lwd = 2)








#dagum
#laplace
log_likelihood <- function(params, data) {
  scale <- params[1]
  shape1.a <- params[2]
  shape2.p <- params[3]
  n <- length(data)
  likelihood = -sum(log(VGAM::ddagum(data,scale,shape1.a,shape2.p)))
  
  return(likelihood)  # Devuelve la verosimilitud
}
# Estimar los parámetros de máxima verosimilitud utilizando optim
initial_params <- c(scale = 0.05,shape1.a=0.05, shape2.p = 0.05)  # Valores iniciales de los parámetros
upper_bounds <- c(scale = Inf,shape1.a=Inf, shape2.p = Inf)
fit <- optim(initial_params, log_likelihood, data = Datos$linfocitos_neutrofilos, method = "L-BFGS-B", upper=upper_bounds)

fit$par

ad.test(Datos$nmono, "pdagum")


ñ=VGAM::rdagum(1000,0.3255425, 2.0782794, 1.1147513)
summary(ñ)


# Calcula los cuantiles de los datos observados y de la distribución NIG
cuantiles_datos <- quantile(Datos$nmono, probs = seq(0, 1, length.out = n))
cuantiles_nig <- VGAM::qdagum(seq(0, 1, length.out = n),scale = 0.3255425,shape1.a=2.0782794, shape2.p = 1.1147513)

# Grafica el Q-Q plot
plot(cuantiles_nig, cuantiles_datos, main = "Gráfico Q-Q para NIG", xlab = "Cuantiles de la distribución NIG",
     ylab = "Cuantiles de los datos observados", col = "blue")
abline(0, 1, col = "red") # Agrega una línea de referencia para una distribución perfecta




x11()
hist(Datos$nmono,freq=F,ylim = c(0,1.5),breaks=50)
# Ajustar la ojiva de una distribución de Levy
x <- seq(0, 15, length.out = 1000)
y = VGAM::ddagum(x,scale = 0.3255425,shape1.a=2.0782794, shape2.p = 1.1147513)
lines(x, y, type = "l", col = "red", lwd = 2)
legend("topright", legend = "distribucion normal inversa gaussiana", col = "red", lty = 1, lwd = 2)






#pareto 
# Calcula los cuantiles de los datos observados y de la distribución NIG
cuantiles_datos <- quantile(Datos$nmono, probs = seq(0, 1, length.out = n))
cuantiles_nig <- qpareto(seq(0, 1, length.out = n),params = c(scale =0.01,shape = 0.2424699))

# Grafica el Q-Q plot
plot(cuantiles_nig, cuantiles_datos, main = "Gráfico Q-Q para NIG", xlab = "Cuantiles de la distribución NIG",
     ylab = "Cuantiles de los datos observados", col = "blue")
abline(0, 1, col = "red") # Agrega una línea de referencia para una distribución perfecta



#levy 
# Calcula los cuantiles de los datos observados y de la distribución NIG
cuantiles_datos <- quantile(nmono, probs = seq(0, 1, length.out = n))
cuantiles_nig <- qlevy(seq(0, 1, length.out = n),m = -0.01889886 ,s = 0.47482718  )

# Grafica el Q-Q plot
plot(cuantiles_nig, cuantiles_datos, main = "Gráfico Q-Q para NIG", xlab = "Cuantiles de la distribución NIG",
     ylab = "Cuantiles de los datos observados", col = "blue")
abline(0, 1, col = "red") # Agrega una línea de referencia para una distribución perfecta


which.max(Datos$nmono)
nmono = Datos$nmono[-c(190,106)]

x11()
hist(nmono,freq=F,ylim = c(0,2),breaks=50)
# Ajustar la ojiva de una distribución de Levy
x <- seq(0, 15, length.out = 1000)
y = VGAM::ddagum(x,scale = 0.3255425,shape1.a=2.0782794, shape2.p = 1.1147513)
lines(x, y, type = "l", col = "red", lwd = 2)
legend("topright", legend = "distribucion normal inversa gaussiana", col = "red", lty = 1, lwd = 2)

u<-rlevy(1000,0,0.0001)

summary(u)


             
fit$par
library(e1071)
skewness(Datos$nmono)
sd(Datos$nmono)
mean(Datos$nmono)


