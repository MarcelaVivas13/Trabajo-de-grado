###  librerias ###
library(readxl);library(GGally);library(ggplot2);library(patchwork)
library(rriskDistributions);library("fitdistrplus");library(stats)
library(MASS);library(nortest);library(goftest);library(rmutil); library(e1071)
library(copula);library(dplyr);library(VineCopula)


#Base de datos
Datos <- read_excel("BD_ALGORITMOS_DENGUE_ACT.xlsx",sheet = "Hoja1")
Datos <- Datos[-c(7,8)] #si salen dos variables mas

#Se vuelve factor el patron de oro
Datos$DX<-factor(Datos$DX)
enfermos <- Datos[Datos$DX == "1", ]
noenfermos <- Datos[Datos$DX == "0", ]
################################################################################
######################DAtos Completos###########################################
###############################################################################
#distribucion acumulada de las 5 variables para los datos completos
Leucocitos = plnorm(Datos$p9_Leuco,meanlog = 1.9945 ,sdlog = 0.4737)
Plaquetas= plogis(Datos$p10_Plaquetas,location = 237.989, scale = 45.044)
Monocitos = plnorm3(Datos$nmono,gamma = -0.01347245,meanlog = -0.40325803,sdlog =0.53709532)
Hematocritos_Hemoglobina= plnorm(Datos$hematocrito_hemoglobina,meanlog = 1.096 , sdlog = 0.0387 )
Linfocitos_Neutrofilos = plnorm(Datos$linfocitos_neutrofilos, meanlog = -1.042  , sdlog = 0.830)

matriz <- cbind(Leucocitos, Plaquetas, Monocitos, Hematocritos_Hemoglobina, Linfocitos_Neutrofilos) #matriz distribucion acumulada
x11()
pairs(matriz)


datos1<-data.frame(Datos$p9_Leuco,Datos$p10_Plaquetas,Datos$nmono,Datos$hematocrito_hemoglobina,Datos$linfocitos_neutrofilos)
u <- pobs(datos1) #transformación por rangos 
x11()
pairs(u)

datos2 <- as.matrix(datos1)
x11()
pairs(datos2)

cor(matriz,method = "spearman")
###############################################################################
##########################estimaciones con el fitcopula#########################
################################################################################


################################CLAYTHON########################################
#Estimación del parametro de la copula Clayton y prueba de bondad con datos 
#transformados por rangos 
g.copulaclaython  <- claytonCopula(dim = 5);N <-  26

fit.tauR <- fitCopula(g.copulaclaython, u,method="ml") #estimacion parametro dependencia
gofCopula(g.copulaclaython, u, N=N,method = "SnB",ties.method ="average", ties=TRUE,start = 0.1555)

#Estimación del parametro de la copula Clayton y prueba de bondad con las variables
#transformadas en distribuciones acumuladas
g.copulaclaython <- claytonCopula(dim = 5);N <-  26
fit.tauA <- fitCopula(g.copulaclaython, matriz,method="ml") #estimacion parametro dependencia
gofCopula(g.copulaclaython, matriz, N=N,method = "SnB",ties.method ="average", ties=TRUE,estim.method = "ml")

################################FRANK########################################
#Estimación del parametro de la copula Frank y prueba de bondad con datos 
#transformados por rangos
g.copulafrank <- frankCopula(dim = 5);N <-  26
fit.tauR1 <- fitCopula(g.copulafrank, u,method="ml") #estimacion parametro dependencia
gofCopula(g.copulafrank, u, N=N,method = "SnB",ties.method ="average", ties=TRUE, estim.method = "ml")

#Estimación del parametro de la copula Frank y prueba de bondad con las variables
#transformadas en distribuciones acumuladas
g.copulafrank <- frankCopula(dim = 5);N <-  26
fit.tauA1 <- fitCopula(g.copulafrank, matriz,method="ml") #estimacion parametro dependencia
gofCopula(g.copulafrank, matriz, N=N,method = "SnB",ties.method ="average", ties=TRUE,estim.method = "ml")

################################NORMAL########################################
#Estimación del parametro de la copula Normal y prueba de bondad con datos 
#transformados por rangos
g.copulanormal <- normalCopula(dim = 5);N <-  26
fit.tauR2 <- fitCopula(g.copulanormal, u,method="ml") #estimacion parametro dependencia
gofCopula(g.copulanormal, u, N=N,method = "SnB",ties.method ="average", ties=TRUE,estim.method = "ml")

#Estimación del parametro de la copula Normal y prueba de bondad con las variables
#transformadas en distribuciones acumuladas
g.copulanormal <- normalCopula(dim = 5);N <-  26
fit.tauA2 <- fitCopula(g.copulanormal, matriz,method="ml") #estimacion parametro dependencia
gofCopula(g.copulanormal, matriz, N=N,method = "SnB",ties.method ="average", ties=TRUE,estim.method = "ml")


###############################################################################
#Estimacion del parametro de la copula a traves de la funcion de densidad de la copula
################################################################################

# Definir los datos de las marginales y parámetros
parametros <- list(list(meanlog = 1.9945 , sdlog = 0.4737),  # Marginal lognormal
                   list(location = 237.989, scale = 45.044),  # Marginal logistica
                   list(gamma = -0.01347245, meanlog = -0.40325803, sdlog =0.53709532),  # Marginal lognormal de 3 parametros
                   list(meanlog = 1.096 , sdlog = 0.0387),  # Marginal lognormal
                   list(meanlog = -1.042 , sdlog = 0.830))  # Marginal lognormal

# Definir la copula Clayton
copula <- claytonCopula(dim = 5)

# Construir la distribución multivariable
mvdist <- mvdc(copula = copula, 
               margins = c("lnorm", "logis", "lnorm3", "lnorm", "lnorm"), 
               paramMargins = parametros,
               marginsIdentical = FALSE)


todos_parametros <- c(unlist(parametros), 0.1555)  # NA representa el parámetro de la copula, que se ajustará

# Ajustar el modelo multivariable con los valores iniciales proporcionados
ajuste <- fitMvdc(data = datos2, mvdc = mvdist, start = todos_parametros)
ajuste@estimate

copula = claytonCopula(dim = 5, param = 0.1707)
gofCopula(copula, datos2, N=N,method = "SnB",ties.method ="average", ties=TRUE)


copulaclaython  <- claytonCopula(dim = 5);N <-  26
gofCopula(copulaclaython , datos2, N=N,method = "SnB",ties.method ="average", ties=TRUE,estim.method= "ml")




#############################################################################
#############################3graficos########################################
###############################################################################
x11()
pairs(u)
cor(u, method = 'spearman')

copulaclaython = claytonCopula(dim = 5,param = 0.1555)
datos_aleatorios <- rCopula(n = 324, copula = copulaclaython)
x11()
pairs(datos_aleatorios) 


copulaclaython = claytonCopula(dim = 5,param = 0.17069434)
datos_aleatorios <- rCopula(n = 324, copula = copulaclaython)
x11()
pairs(datos_aleatorios)


##grafico en 3d
# Definir los datos de las marginales y parámetros
parametros <- list(list(meanlog = 1.9945 , sdlog = 0.4737),  # Marginal lognormal
                   list(location = 237.989, scale = 45.044),  # Marginal logistica
                   list(gamma = -0.01347245, meanlog = -0.40325803, sdlog =0.53709532))  # Marginal lognormal de 3 parametros
                  # Marginal lognormal

# Definir la copula Clayton
copula <- claytonCopula(dim = 3,0.1707)

# Construir la distribución multivariable
mvdist <- mvdc(copula = copula, 
               margins = c("lnorm", "logis", "lnorm3"), 
               paramMargins = parametros,
               marginsIdentical = FALSE)
n = 324
cloud2(mvdist, n = n)

x11()
plot3D:::points3D(datos2[,1],datos2[,2],datos2[,3], colvar = NULL, 
                  xlab = "u1", ylab = "u2",zlab="u3")



###############################################################################
#################parte de lo dicho por el profe Tovar #########################
##############################################################################

# Definir la función de densidad de la copula FGM multivariada
# Se basa en la normalCopula, pero se ajusta para representar la FGM
fgm_copula <- function(u, theta) {
  prod(u) - prod(1 - theta * (1 - outer(u,u,pmin)))
}

# Definir la función de log-verosimilitud para la estimación
log_likelihood <- function(params, datos) {
  theta <- params
  -sum(log(fgm_copula(datos, theta)))
}

# Estimar los parámetros de la copula FGM utilizando optim
initial_theta <- 0  # Valor inicial para theta
fit <- optim(initial_theta, log_likelihood, datos = u, method = "Brent", lower =0,upper=1)


fit <- optimize(log_likelihood, interval = c(0, 1), datos=u)


#Tranfosrmar la variable leucocitos y la razon linfocitosneutrofilos
#para mirar si cambia la correlacion
cor(Datos[-6],method = "spearman")
cor_original <- cor(Datos$p9_Leuco, Datos$linfocitos_neutrofilos)
cor_original

plot(Datos$p9_Leuco, Datos$linfocitos_neutrofilos, main = "Variables originales", xlab = "X", ylab = "Y")

modelo<-lm(Datos$linfocitos_neutrofilos~Datos$p9_Leuco)

x_log <- log(Datos$p9_Leuco)
y_log <- 1/log(Datos$linfocitos_neutrofilos)

x_log <- log(Datos$linfocitos_neutrofilos)
y_log <- 1/log(Datos$p9_Leuco)

x_log <- log(Datos$p10_Plaquetas)
y_log <- 1/log(Datos$linfocitos_neutrofilos)

x_log <- log(Datos$nmono)
y_log <- 1/log(Datos$linfocitos_neutrofilos)

x_log <- log(Datos$hematocrito_hemoglobina)
y_log <- 1/log(Datos$linfocitos_neutrofilos)

plot(x_log,y_log, main = "Variables transformada", xlab = "X", ylab = "Y")

cor_log <- cor(x_log,y_log)
cor_log

Datoscorpos <- Datos %>%
  mutate(p10_Plaquetas = x_log,
         linfocitos_neutrofilos = y_log)
View(Datoscorpos)

cor(Datoscorpos[-6],method = "spearman")


################################################################################
#############################mixtura de copulas ################################
################################################################################
#miro las correlaciones de spearman
cor(datos2, method = 'spearman')
x11()
corrplot::corrplot(cor(datos2,method = "spearman"),method = "number")
gp1 = data.frame(Datos$p9_Leuco,Datos$p10_Plaquetas)
gp1 = as.matrix(gp1)
gp2 = data.frame(Datos$p9_Leuco,Datos$nmono)
gp2 = as.matrix(gp2)
gp3 = data.frame(Datos$p9_Leuco,Datos$hematocrito_hemoglobina)
gp3 = as.matrix(gp3)
gp4 = data.frame(Datos$p9_Leuco,Datos$linfocitos_neutrofilos)
gp4 = as.matrix(gp4)
gp5 = data.frame(Datos$p10_Plaquetas,Datos$nmono)
gp5 = as.matrix(gp5)
gp6 = data.frame(Datos$p10_Plaquetas,Datos$hematocrito_hemoglobina)
gp6 = as.matrix(gp6)
gp7 = data.frame(Datos$p10_Plaquetas,Datos$linfocitos_neutrofilos)
gp7 = as.matrix(gp7)
gp8 = data.frame(Datos$nmono,Datos$hematocrito_hemoglobina)
gp8 = as.matrix(gp8)
gp9 = data.frame(Datos$nmono,Datos$linfocitos_neutrofilos)
gp9 = as.matrix(gp9)
gp10 = data.frame(Datos$linfocitos_neutrofilos,Datos$hematocrito_hemoglobina)
gp10 = as.matrix(gp10)




###############################################################################
#########################3copula quitando la razon linf_neu####################
###############################################################################

####hacemos esto con las distribuciones acumuladas de los datos 
### miramos el test ks 

#estos son los datos sin tener en cuenta a la razon linfocitos 
datos50<-data.frame(Datos$p9_Leuco,Datos$p10_Plaquetas,Datos$nmono,Datos$hematocrito_hemoglobina)
datos50 <- as.matrix(datos50)


#####Miramos la fgm----
fgm_copula <- function(u, theta) {
  prod(u) - prod(1 - theta * (1 - outer(u,u,pmin)))
}

# Definir la función de log-verosimilitud para la estimación
log_likelihood <- function(params, datos) {
  theta <- params
  -sum(log(fgm_copula(datos, theta)))
}

# Estimar los parámetros de la copula FGM utilizando optim
initial_theta <- 0  # Valor inicial para theta
fit <- optim(initial_theta, log_likelihood, datos = matriz1, method = "Brent", lower =0,upper=1)
fit <- optimize(log_likelihood, interval = c(0, 1), datos= matriz1)

#funcion para simular la fgm
simular_copula_fgm <- function(n, theta) {
  # Generar muestras uniformes
  u <- matrix(runif(n * 4), ncol = 4)
  # Aplicar la inversa de la distribución uniforme
  v <- matrix(apply(u, 1, function(x) -log(1 - x)^(1/theta)), ncol = 4)
  return(v)
}
simulacionfgm <- simular_copula_fgm(n = 324, theta = 1)
ks.test(simulacionfgm,matriz1) #test para una simulación

#####simulo 100 veces para ver cuantas acepto
# Define la función para realizar la prueba KS
set.seed(000)
realizar_prueba_ks <- function(simulacion, datos) {
  return(ks.test(simulacion, datos)$p.value)
}
num_simulaciones <- 100  
p_values <- numeric(num_simulaciones)
for (i in 1:num_simulaciones) {
  simulacionfgm <- simular_copula_fgm(n = 324, theta = 1)
  p_values[i] <- realizar_prueba_ks(simulacionfgm, matriz1)
}
print(p_values)
p_values<0.05
summenores = sum(p_values<0.05);summayores = sum(p_values>=0.05)
summenores/num_simulaciones;summayores/num_simulaciones
#grafico de dispersion 
cor(simulacionfgm,matriz1)
pairs(simulacionfgm)
pairs(matriz1)

#####Miramos la claython----
copulaclaython <- claytonCopula(dim = 4);N <-  26
matriz1  = matriz[,-5]
fit.tauR2 <- fitCopula(copulaclaython, matriz1,method="ml") #estimacion parametro dependencia
copulaclaython <- claytonCopula(0.1473, dim = 4)
simulacionclaython =  rCopula(n = 324, copula = copulaclaython)
ks.test(simulacionclaython,matriz1) #test para una simulación


#####simulo 100 veces para ver cuantas acepto
# Define la función para realizar la prueba KS
set.seed(111)
realizar_prueba_ks <- function(simulacion, datos) {
  return(ks.test(simulacion, datos)$p.value)
}
num_simulaciones <- 100  
p_values <- numeric(num_simulaciones)
for (i in 1:num_simulaciones) {
  simulacion_claython <- rCopula(n = 324, copula = copulaclaython)
  p_values[i] <- realizar_prueba_ks(simulacion_claython, matriz1)
}
print(p_values)
p_values<0.05
summenores = sum(p_values<0.05);summayores = sum(p_values>=0.05)
summenores/num_simulaciones;summayores/num_simulaciones
#grafico de dispersion 
cor(simulacionclaython,matriz1)
pairs(simulacionclaython)
pairs(matriz1)



#####Miramos la frank----
copulafrank <- frankCopula(dim = 4);N <-  26
matriz1  = matriz[,-5]
fit.tauR2 <- fitCopula(copulafrank, matriz1,method="ml") #estimacion parametro dependencia
copulafrank <- frankCopula(1.888 , dim = 4)
simulacionfrank =  rCopula(n = 324, copula = copulafrank) 
ks.test(simulacionfrank,matriz1) #test para una simulación

#####simulo 100 veces para ver cuantas acepto
# Define la función para realizar la prueba KS
set.seed(222)
realizar_prueba_ks <- function(simulacion, datos) {
  return(ks.test(simulacion, datos)$p.value)
}
num_simulaciones <- 100  
p_values <- numeric(num_simulaciones)

for (i in 1:num_simulaciones) {
  simulacionfrank <- rCopula(n = 324, copula = copulafrank)
  p_values[i] <- realizar_prueba_ks(simulacionfrank, matriz1)
}
print(p_values)
p_values<0.05
summenores = sum(p_values<0.05);summayores = sum(p_values>=0.05)
summenores/num_simulaciones;summayores/num_simulaciones
#grafico de dispersion 
cor(simulacionfrank,matriz1)
pairs(simulacionfrank)
pairs(matriz1)



#####Miramos la gumbel----
copulagumbel <- gumbelCopula(dim = 4);N <-  26
matriz1  = matriz[,-5]
fit.tauR2 <- fitCopula(copulagumbel, matriz1,method="ml") #estimacion parametro dependencia
copulagumbel <- gumbelCopula(1.203  , dim = 4)
simulaciongumbel =  rCopula(n = 324, copula = copulagumbel)
ks.test(simulaciongumbel,matriz1) #para una simulacion

#####simulo 100 veces para ver cuantas acepto
# Define la función para realizar la prueba KS
set.seed(333)
realizar_prueba_ks <- function(simulacion, datos) {
  return(ks.test(simulacion, datos)$p.value)
}
num_simulaciones <- 100 
p_values <- numeric(num_simulaciones)
for (i in 1:num_simulaciones) {
  simulaciongumbel <- rCopula(n = 324, copula = copulagumbel)
  p_values[i] <- realizar_prueba_ks(simulaciongumbel, matriz1)
}

print(p_values)
p_values<0.05
summenores = sum(p_values<0.05);summayores = sum(p_values>=0.05)
summenores/num_simulaciones;summayores/num_simulaciones
cor(simulaciongumbel,matriz1)
pairs(simulaciongumbel)
pairs(matriz1)



#####Miramos la t----
copulat <- tCopula(dim = 4);N <-  26
matriz1  = matriz[,-5]
fit.tauR2 <- fitCopula(copulat, matriz1,method="ml") #estimacion parametro dependencia
copulat <- tCopula(param = 0.2769,dim = 4, df = 21.7890) 
simulaciont =  rCopula(n = 324, copula = copulat)
ks.test(simulaciont,matriz1) #para una simulacion

#####simulo 100 veces para ver cuantas acepto
# Define la función para realizar la prueba KS
set.seed(444)
realizar_prueba_ks <- function(simulacion, datos) {
  return(ks.test(simulacion, datos)$p.value)
}
num_simulaciones <- 100 
p_values <- numeric(num_simulaciones)
for (i in 1:num_simulaciones) {
  simulaciont <- rCopula(n = 324, copula = copulat)
  p_values[i] <- realizar_prueba_ks(simulaciont, matriz1)
}
print(p_values)
p_values<0.05
summenores = sum(p_values<0.05);summayores = sum(p_values>=0.05)
summenores/num_simulaciones;summayores/num_simulaciones
cor(simulaciont,matriz1)
pairs(simulaciont)
pairs(matriz1)

pairs(simulacionclaython,matriz1)


###############################################################################
#########################Copula con todos los datos #########################
###############################################################################

####hacemos esto con las distribuciones acumuladas de los datos 
### miramos el test ks 

#estos son los datos de las distribuciones acumuladas
matriz


#####Miramos la fgm----
fgm_copula <- function(u, theta) {
  prod(u) - prod(1 - theta * (1 - outer(u,u,pmin)))
}

# Definir la función de log-verosimilitud para la estimación
log_likelihood <- function(params, datos) {
  theta <- params
  -sum(log(fgm_copula(datos, theta)))
}

# Estimar los parámetros de la copula FGM utilizando optim
initial_theta <- 0  # Valor inicial para theta
fit <- optim(initial_theta, log_likelihood, datos = matriz, method = "Brent", lower =0,upper=1)
fit <- optimize(log_likelihood, interval = c(0, 1), datos= matriz)

#funcion para simular la fgm
simular_copula_fgm <- function(n, theta) {
  # Generar muestras uniformes
  u <- matrix(runif(n * 5), ncol = 5)
  # Aplicar la inversa de la distribución uniforme
  v <- matrix(apply(u, 1, function(x) -log(1 - x)^(1/theta)), ncol = 5)
  return(v)
}
simulacionfgm <- simular_copula_fgm(n = 324, theta = 1)
ks.test(simulacionfgm,matriz) #test para una simulación

#####simulo 100 veces para ver cuantas acepto
# Define la función para realizar la prueba KS
set.seed(0000)
realizar_prueba_ks <- function(simulacion, datos) {
  return(ks.test(simulacion, datos)$p.value)
}
num_simulaciones <- 100  
p_values <- numeric(num_simulaciones)
for (i in 1:num_simulaciones) {
  simulacionfgm <- simular_copula_fgm(n = 324, theta = 1)
  p_values[i] <- realizar_prueba_ks(simulacionfgm, matriz)
}
print(p_values)
p_values<0.05
summenores = sum(p_values<0.05);summayores = sum(p_values>=0.05)
summenores/num_simulaciones;summayores/num_simulaciones
#grafico de dispersion 
cor(simulacionfgm,matriz)
pairs(simulacionfgm)
pairs(matriz)

#####Miramos la claython----
matriz = as.matrix(matriz)
copulaclaython <- claytonCopula(dim = 5);N <-  26
fit.tauR2 <- fitCopula(copulaclaython, matriz,method="ml") #estimacion parametro dependencia
copulaclaython <- claytonCopula(0.07971 , dim = 5)
simulacionclaython =  rCopula(n = 324, copula = copulaclaython)
ks.test(simulacionclaython,matriz) #test para una simulación


#####simulo 100 veces para ver cuantas acepto
# Define la función para realizar la prueba KS
set.seed(1111)
realizar_prueba_ks <- function(simulacion, datos) {
  return(ks.test(simulacion, datos)$p.value)
}
num_simulaciones <- 100  
p_values <- numeric(num_simulaciones)
for (i in 1:num_simulaciones) {
  simulacionclaython <- rCopula(n = 324, copula = copulaclaython)
  p_values[i] <- realizar_prueba_ks(simulacion_claython, matriz)
}
print(p_values)
p_values<0.05
summenores = sum(p_values<0.05);summayores = sum(p_values>=0.05)
summenores/num_simulaciones;summayores/num_simulaciones
#grafico de dispersion 
cor(simulacionclaython,matriz)
colnames(simulacionclaython) <- colnames(matriz)
pairs(simulacionclaython)
pairs(matriz)

factormatriz  = as.data.frame(matriz)
simulacionclaython = as.data.frame(simulacionclaython)

factormatriz$Origen <- "Real"
simulacionclaython$Origen <- "Simulada"

df_combinado_claython <- rbind(factormatriz, simulacionclaython)

# Cargar los paquetes necesarios
library(GGally)
library(ggplot2)

# Crear un gráfico de pares (scatter plot matrix)
ggpairs(df_combinado_claython,columns = 1:5, aes(color = Origen)) +
  ggtitle("Gráfico de dispersión de datos reales y simulados")




#####Miramos la frank----
copulafrank <- frankCopula(dim = 5);N <-  26
fit.tauR2 <- fitCopula(copulafrank, matriz,method="ml") #estimacion parametro dependencia
copulafrank <- frankCopula(0.6779 , dim = 5)
set.seed(2222)
simulacionfrank =  rCopula(n = 324, copula = copulafrank) 
ks.test(simulacionfrank,matriz) #test para una simulación

#esto era para mirar que hacia el test
simulacionfrank_uni <- c(simulacionfrank)
matriz_uni <-  c(matriz)
ks.test(simulacionfrank_uni,matriz_uni)

#####simulo 100 veces para ver cuantas acepto
# Define la función para realizar la prueba KS
set.seed(2222)
realizar_prueba_ks <- function(simulacion, datos) {
  return(ks.test(simulacion, datos)$p.value)
}
num_simulaciones <- 100  
p_values <- numeric(num_simulaciones)

for (i in 1:num_simulaciones) {
  simulacionfrank <- rCopula(n = 324, copula = copulafrank)
  p_values[i] <- realizar_prueba_ks(simulacionfrank, matriz)
}
print(p_values)
p_values<0.05
summenores = sum(p_values<0.05);summayores = sum(p_values>=0.05)
summenores/num_simulaciones;summayores/num_simulaciones
#grafico de dispersion 
cor(simulacionfrank,matriz)
colnames(simulacionfrank) <- colnames(matriz)
pairs(simulacionfrank)
pairs(matriz)


factormatriz  = as.data.frame(matriz)
simulacionfrank = as.data.frame(simulacionfrank)

factormatriz$Origen <- "Real"
simulacionfrank$Origen <- "Simulada"

df_combinado_frank <- rbind(factormatriz, simulacionfrank)

# Crear el gráfico pairs con colores personalizados
x11()
pairs(df_combinado_frank[, -ncol(df_combinado_frank)],  # Excluir la columna Origen
      labels = colnames(df_combinado_frank)[-ncol(df_combinado_frank)],  # Nombres de las variables
      pch = 21,                  # Símbolo para los puntos
      bg = ifelse(df_combinado_frank$Origen == "Real", "#2F4F4F", "#FF7256"),              # Color de fondo de los puntos
      col = ifelse(df_combinado_frank$Origen == "Real", "#2F4F4F", "#FF7256"),             # Color del borde de los puntos
      main = "Gráficos de correlación entre individuos enfermos para los 5 biomarcadores y la simulación utilizando una cópula Frank",  # Título del gráfico
      row1attop = TRUE,          # Si FALSE, cambia la dirección de la diagonal
      gap = 1,                   # Distancia entre subplots
      cex.labels = 1.5,          # Tamaño del texto de la diagonal
      font.labels = 2)           # Estilo de fuente del texto de la diagonal

#####Miramos la gumbel----
copulagumbel <- gumbelCopula(dim = 5);N <-  26
fit.tauR2 <- fitCopula(copulagumbel, matriz,method="ml") #estimacion parametro dependencia
copulagumbel <- gumbelCopula(1.072  , dim = 5)
simulaciongumbel =  rCopula(n = 324, copula = copulagumbel)
ks.test(simulaciongumbel,matriz) #para una simulacion

#####simulo 100 veces para ver cuantas acepto
# Define la función para realizar la prueba KS
set.seed(3333)
realizar_prueba_ks <- function(simulacion, datos) {
  return(ks.test(simulacion, datos)$p.value)
}
num_simulaciones <- 100 
p_values <- numeric(num_simulaciones)
for (i in 1:num_simulaciones) {
  simulaciongumbel <- rCopula(n = 324, copula = copulagumbel)
  p_values[i] <- realizar_prueba_ks(simulaciongumbel, matriz)
}

print(p_values)
p_values<0.05
summenores = sum(p_values<0.05);summayores = sum(p_values>=0.05)
summenores/num_simulaciones;summayores/num_simulaciones
cor(simulaciongumbel,matriz)
pairs(simulaciongumbel)
pairs(matriz)



#####Miramos la t----
copulat <- tCopula(dim = 5);N <-  26
fit.tauR2 <- fitCopula(copulat, matriz,method="ml") #estimacion parametro dependencia
copulat <- tCopula(param = 0.127,dim = 5, df = 19.046) 
simulaciont =  rCopula(n = 324, copula = copulat)
ks.test(simulaciont,matriz) #para una simulacion

#####simulo 100 veces para ver cuantas acepto
# Define la función para realizar la prueba KS
set.seed(4444)
realizar_prueba_ks <- function(simulacion, datos) {
  return(ks.test(simulacion, datos)$p.value)
}
num_simulaciones <- 100 
p_values <- numeric(num_simulaciones)
for (i in 1:num_simulaciones) {
  simulaciont <- rCopula(n = 324, copula = copulat)
  p_values[i] <- realizar_prueba_ks(simulaciont, matriz)
}
print(p_values)
p_values<0.05
summenores = sum(p_values<0.05);summayores = sum(p_values>=0.05)
summenores/num_simulaciones;summayores/num_simulaciones
cor(simulaciont,matriz)
pairs(simulaciont)
pairs(matriz)



################################################################################
############################ESTIMACIÓN COPULA PARA NO ENFERMOS #################
################################################################################

#distribucion acumulada de las 5 variables para los NO ENFERMOS
f = plnorm(noenfermos$p9_Leuco,meanlog = 2.0135001 ,sdlog = 0.4484493)
g= plogis(noenfermos$p10_Plaquetas,location = 241.35686, scale = 42.22504)
h = plnorm3(noenfermos$nmono,gamma = 0.05732773,meanlog = -0.50855315,sdlog =0.56999857)
i= plnorm(noenfermos$hematocrito_hemoglobina,meanlog = 1.09640318 , sdlog = 0.03879774 )
j = plnorm(noenfermos$linfocitos_neutrofilos, meanlog = -1.080398 , sdlog = 0.790767)
matriz3 <- cbind(f, g, h, i, j) #Datos

cor(matriz3,method = "spearman") #correlaciones

################
#####Miramos la fgm----
fgm_copula <- function(u, theta) {
  prod(u) - prod(1 - theta * (1 - outer(u,u,pmin)))
}

# Definir la función de log-verosimilitud para la estimación
log_likelihood <- function(params, datos) {
  theta <- params
  -sum(log(fgm_copula(datos, theta)))
}

# Estimar los parámetros de la copula FGM utilizando optim
initial_theta <- 0  # Valor inicial para theta
fit <- optim(initial_theta, log_likelihood, datos = matriz3, method = "Brent", lower =0,upper=1)
fit <- optimize(log_likelihood, interval = c(0, 1), datos= matriz3)

#funcion para simular la fgm
simular_copula_fgm <- function(n, theta) {
  # Generar muestras uniformes
  u <- matrix(runif(n * 5), ncol = 5)
  # Aplicar la inversa de la distribución uniforme
  v <- matrix(apply(u, 1, function(x) -log(1 - x)^(1/theta)), ncol = 5)
  return(v)
}
simulacionfgm <- simular_copula_fgm(n = 300, theta = 1)
ks.test(simulacionfgm,matriz3) #test para una simulación

#####simulo 100 veces para ver cuantas acepto
# Define la función para realizar la prueba KS
set.seed(00000)
realizar_prueba_ks <- function(simulacion, datos) {
  return(ks.test(simulacion, datos)$p.value)
}
num_simulaciones <- 100  
p_values <- numeric(num_simulaciones)
for (i in 1:num_simulaciones) {
  simulacionfgm <- simular_copula_fgm(n = 300, theta = 1)
  p_values[i] <- realizar_prueba_ks(simulacionfgm, matriz3)
}
print(p_values)
p_values<0.05
summenores = sum(p_values<0.05);summayores = sum(p_values>=0.05)
summenores/num_simulaciones;summayores/num_simulaciones
#grafico de dispersion 
cor(simulacionfgm,matriz3)
pairs(simulacionfgm)
pairs(matriz3)

#####Miramos la claython----
copulaclaython <- claytonCopula(dim = 5);N <-  26
fit.tauR2 <- fitCopula(copulaclaython, matriz3,method="itau") #estimacion parametro dependencia
copulaclaython <- claytonCopula(0.2983  , dim = 5)
simulacionclaython =  rCopula(n = 300, copula = copulaclaython)
ks.test(simulacionclaython,matriz3) #test para una simulación


#####simulo 100 veces para ver cuantas acepto
# Define la función para realizar la prueba KS
set.seed(1111)
realizar_prueba_ks <- function(simulacion, datos) {
  return(ks.test(simulacion, datos)$p.value)
}
num_simulaciones <- 100  
p_values <- numeric(num_simulaciones)
for (i in 1:num_simulaciones) {
  simulacion_claython <- rCopula(n = 324, copula = copulaclaython)
  p_values[i] <- realizar_prueba_ks(simulacion_claython, matriz3)
}
print(p_values)
p_values<0.05
summenores = sum(p_values<0.05);summayores = sum(p_values>=0.05)
summenores/num_simulaciones;summayores/num_simulaciones
#grafico de dispersion 
cor(simulacionclaython,matriz3)
pairs(simulacionclaython)
pairs(matriz)



#####Miramos la frank----


copulafrank <- frankCopula(dim = 5);N <-  26
fit.tauR2 <- fitCopula(copulafrank, matriz3,method="itau") #estimacion parametro dependencia
copulafrank <- frankCopula(0.8926  , dim = 5)
simulacionfrank =  rCopula(n = 300, copula = copulafrank) 
ks.test(simulacionfrank,matriz3) #test para una simulación

#####simulo 100 veces para ver cuantas acepto
# Define la función para realizar la prueba KS
set.seed(2222)
realizar_prueba_ks <- function(simulacion, datos) {
  return(ks.test(simulacion, datos)$p.value)
}
num_simulaciones <- 100  
p_values <- numeric(num_simulaciones)

for (i in 1:num_simulaciones) {
  simulacionfrank <- rCopula(n = 300, copula = copulafrank)
  p_values[i] <- realizar_prueba_ks(simulacionfrank, matriz3)
}
print(p_values)
p_values<0.05
summenores = sum(p_values<0.05);summayores = sum(p_values>=0.05)
summenores/num_simulaciones;summayores/num_simulaciones
#grafico de dispersion 
cor(simulacionfrank,matriz3)
colnames(simulacionfrank) <- colnames(matriz)
colnames(matriz3) = colnames(matriz)
pairs(simulacionfrank)
pairs(matriz3)


factormatriz3  = as.data.frame(matriz3)
simulacionfrank = as.data.frame(simulacionfrank)

factormatriz3$Origen <- "Real"
simulacionfrank$Origen <- "Simulada"

df_combinado_frank2 <- rbind(factormatriz3, simulacionfrank)

# Crear el gráfico pairs con colores personalizados
x11()
pairs(df_combinado_frank2[, -ncol(df_combinado_frank2)],  # Excluir la columna Origen
      labels = colnames(df_combinado_frank2)[-ncol(df_combinado_frank2)],  # Nombres de las variables
      pch = 21,                  # Símbolo para los puntos
      bg = ifelse(df_combinado_frank2$Origen == "Real", "#2F4F4F", "#FF7256"),              # Color de fondo de los puntos
      col = ifelse(df_combinado_frank2$Origen == "Real", "#2F4F4F", "#FF7256"),             # Color del borde de los puntos
      main = "Gráficos de correlación entre individuos no enfermos para los 5 biomarcadores y la simulación utilizando una cópula Frank",  # Título del gráfico
      row1attop = TRUE,          # Si FALSE, cambia la dirección de la diagonal
      gap = 1,                   # Distancia entre subplots
      cex.labels = 1.5,          # Tamaño del texto de la diagonal
      font.labels = 2
)


#####Miramos la gumbel----
copulagumbel <- gumbelCopula(dim = 5);N <-  26
fit.tauR2 <- fitCopula(copulagumbel, matriz3,method="ml") #estimacion parametro dependencia
copulagumbel <- gumbelCopula(1.182   , dim = 5)
simulaciongumbel =  rCopula(n = 324, copula = copulagumbel)
ks.test(simulaciongumbel,matriz3) #para una simulacion

#####simulo 100 veces para ver cuantas acepto
# Define la función para realizar la prueba KS
set.seed(3333)
realizar_prueba_ks <- function(simulacion, datos) {
  return(ks.test(simulacion, datos)$p.value)
}
num_simulaciones <- 100 
p_values <- numeric(num_simulaciones)
for (i in 1:num_simulaciones) {
  simulaciongumbel <- rCopula(n = 324, copula = copulagumbel)
  p_values[i] <- realizar_prueba_ks(simulaciongumbel, matriz3)
}

print(p_values)
p_values<0.05
summenores = sum(p_values<0.05);summayores = sum(p_values>=0.05)
summenores/num_simulaciones;summayores/num_simulaciones
cor(simulaciongumbel,matriz3)
pairs(simulaciongumbel)
pairs(matriz)



#####Miramos la t----
copulat <- tCopula(dim = 5);N <-  26
fit.tauR2 <- fitCopula(copulat, data = matriz3, method = "itau") #estimacion parametro dependencia
copulat <- tCopula(param = 0.133,dim = 5, df = 4) 
simulaciont =  rCopula(n = 324, copula = copulat)
ks.test(simulaciont,matriz) #para una simulacion

#####simulo 100 veces para ver cuantas acepto
# Define la función para realizar la prueba KS
set.seed(4444)
realizar_prueba_ks <- function(simulacion, datos) {
  return(ks.test(simulacion, datos)$p.value)
}
num_simulaciones <- 100 
p_values <- numeric(num_simulaciones)
for (i in 1:num_simulaciones) {
  simulaciont <- rCopula(n = 324, copula = copulat)
  p_values[i] <- realizar_prueba_ks(simulaciont, matriz3)
}
print(p_values)
p_values<0.05
summenores = sum(p_values<0.05);summayores = sum(p_values>=0.05)
summenores/num_simulaciones;summayores/num_simulaciones
cor(simulaciont,matriz)
pairs(simulaciont)
pairs(matriz)



#################################################################################
#################### Calculo del AIC y BIC ######################################
#################################################################################
#CLAYTHON 
copulaclaython = claytonCopula(dim = 5)
fit.CLAYTHON <- fitCopula(copulaclaython, matriz,method="ml")
copulaclaython = claytonCopula(0.07971,dim = 5)
log_likelihoodCLAYTHON <- sum(log(dCopula(matriz, copulaclaython)))
num_paramsCLAYTHON <- length(fit.CLAYTHON@estimate)
n <- nrow(matriz)
AIC_valuesCLAYTHON <- -2 * log_likelihoodCLAYTHON  + 2 * num_paramsCLAYTHON
BIC_valueCLAYTHON <- -2 * log_likelihoodCLAYTHON + num_paramsCLAYTHON* log(n)
AIC(fit.CLAYTHON)
BIC(fit.CLAYTHON)

#Frank
copulafrank = frankCopula(dim = 5)
fit.FRANK <- fitCopula(copulafrank, matriz,method="ml")
copulafrank = frankCopula(0.6779 ,dim = 5)
log_likelihoodFRANK <- sum(log(dCopula(matriz, copulafrank)))
num_paramsFRANK <- length(fit.FRANK@estimate)
n <- nrow(matriz)
AIC_valuesFRANK <- -2 * log_likelihoodFRANK   + 2 * num_paramsFRANK
BIC_valueFRANK <- -2 * log_likelihoodFRANK + num_paramsFRANK* log(n)
AIC(fit.FRANK)
BIC(fit.FRANK)

#GUMBEL
copulagumbel = gumbelCopula(dim = 5)
fit.gumbel <- fitCopula(copulagumbel , matriz,method="ml")
copulagumbel  = gumbelCopula(1.072,dim = 5)
log_likelihoodGUMBEL <- sum(log(dCopula(matriz, copulagumbel)))
num_paramsGUMBEL <- length(fit.gumbel@estimate)
n <- nrow(matriz)
AIC_valuesGUMBEL <- -2 * log_likelihoodGUMBEL  + 2 * num_paramsGUMBEL
BIC_valueGUMBEL <- -2 * log_likelihoodGUMBEL + num_paramsGUMBEL* log(n)
AIC(fit.gumbel)
BIC(fit.gumbel)


#T
copulat = tCopula(dim = 5)
fit.t <- fitCopula(copulat , matriz,method="ml")
copulat  = tCopula(param = 0.127,dim = 5, df = 19.046 )
log_likelihoodT <- sum(log(dCopula(matriz, copulat)))
num_paramsT <- length(fit.t@estimate)
n <- nrow(matriz)
AIC_valuesT <- -2 * log_likelihoodT  + 2 * num_paramsT
BIC_valueT <- -2 * log_likelihoodT + num_paramsT * log(n)
AIC(fit.t)
BIC(fit.t)


####### chiplot claython
simulacionclaython =  rCopula(n = 324, copula = copulaclaython)
op <- par(mfrow = c(2, 3))  # Cambia a 2 filas y 3 columnas para adaptarse a 5 variables

# Primero, crea chi-plots para todas las combinaciones posibles de pares de variables
for (i in 1:5) {
  for (j in 1:5) {
    if (i != j) {
      BiCopChiPlot(simulacionclaython[, i], simulacionclaython[, j], mode = "upper", 
                   main = paste("Chi-plot para variables", i, "y", j))
    }
  }
}

for (i in 1:5) {
  for (j in 1:5) {
    if (i != j) {
      BiCopKPlot(simulacionclaython[, i], simulacionclaython[, j],
                 main = paste("KPlot para variables", i, "y", j))
    }
  }
}

###################################################################################
############################Test chi cuadrado ###################################
################################################################################

##Claython
copulaclaython <- claytonCopula(dim = 5);N <-  26
fit.CLAYTHON <- fitCopula(copulaclaython, matriz,method="ml") #estimacion parametro dependencia
copulaclaython <- claytonCopula(0.07971 , dim = 5)
simulacionclaython =  rCopula(n = 324, copula = copulaclaython)

frecuencia_observadaclaython <- table(simulacionclaython)
num_clasesclaython <- length(frecuencia_observadaclaython)
frecuencia_esperadaclaython <- length(matriz) * dCopula(simulacionclaython, copulaclaython)
estadistico_chi_cuadradoclaython <- sum((frecuencia_observadaclaython - frecuencia_esperadaclaython)^2 / frecuencia_esperadaclaython)
p_valorclaython <- 1 - pchisq(estadistico_chi_cuadradoclaython , df = length(frecuencia_observadaclaython ) - 1)


#frank
copulafrank <- frankCopula(dim = 5);N <-  26
fit.frank <- fitCopula(copulafrank, matriz,method="ml") #estimacion parametro dependencia
copulafrank <- frankCopula(0.6779 , dim = 5)
simulacionfrank =  rCopula(n = 324, copula = copulafrank)

frecuencia_observadafrank <- table(simulacionfrank)
frecuencia_esperadafrank <- length(matriz) * dCopula(simulacionfrank, copulafrank)
estadistico_chi_cuadradofrank <- sum((frecuencia_observadafrank - frecuencia_esperadafrank)^2 / frecuencia_esperadafrank)
p_valorfrank <- 1 - pchisq(estadistico_chi_cuadradofrank , df = length(frecuencia_observadafrank) - 1)


#gumbel
copulagumbel <- gumbelCopula(dim = 5);N <-  26
fit.gumbel <- fitCopula(copulagumbel, matriz,method="ml") #estimacion parametro dependencia
copulagumbel <- gumbelCopula(1.072  , dim = 5)
simulaciongumbel =  rCopula(n = 324, copula = copulagumbel)

frecuencia_observadagumbel <- table(simulaciongumbel)
frecuencia_esperadagumbel <- length(matriz) * dCopula(simulaciongumbel, copulagumbel)
estadistico_chi_cuadradogumbel <- sum((frecuencia_observadagumbel - frecuencia_esperadagumbel)^2 / frecuencia_esperadagumbel)
p_valorgumbel <- 1 - pchisq(estadistico_chi_cuadradogumbel , df = length(frecuencia_observadagumbel) - 1)


#t
copulat <- tCopula(dim = 5);N <-  26
fit.t <- fitCopula(copulat, matriz,method="ml") #estimacion parametro dependencia
copulat <- tCopula(param = 0.127,dim = 5, df = 19.046)
simulaciont =  rCopula(n = 324, copula = copulat)


frecuencia_observadat <- table(simulaciont)
frecuencia_esperadat <- length(matriz) * dCopula(simulaciont, copulat)
estadistico_chi_cuadradot <- sum((frecuencia_observadat - frecuencia_esperadat)^2 / frecuencia_esperadat)
p_valort <- 1 - pchisq(estadistico_chi_cuadradot , df = length(frecuencia_observadat) - 1)

