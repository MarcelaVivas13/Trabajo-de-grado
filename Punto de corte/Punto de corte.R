###  librerias ###
library(readxl);library(GGally);library(ggplot2);library(patchwork)
library(rriskDistributions);library("fitdistrplus");library(stats)
library(MASS);library(nortest);library(goftest);library(rmutil); library(e1071)
library(copula);library(dplyr);library(VineCopula); library(naivebayes);library(PRROC);
library(GGally);library(gridExtra)



library(copula)
#todos #dengue
copulafrank1 <- frankCopula(0.6779 , dim = 5)
#no enfermos #no dengue
copulafrank2 <- frankCopula(0.8926  , dim = 5)

# 1000 Dengue
set.seed(123)
n1 <- 1000
DengueSimulada <-rCopula(n1,copulafrank1)
DengueSimulada <- data.frame(DengueSimulada)
fdensidad3  = dCopula(as.matrix(DengueSimulada), copulafrank1)
plot(fdensidad3)
# 9000 No dengue
set.seed(123)
n2 <- 9000
NoDengueSimulada<-rCopula(n2,copulafrank2)
NoDengueSimulada <- data.frame(NoDengueSimulada)
fdensidad4  = dCopula(as.matrix(NoDengueSimulada), copulafrank)
plot(fdensidad4)
par (mfrow= c(1,2))
hist(fdensidad4)
hist(fdensidad3)

Conjunta <- rbind(DengueSimulada,NoDengueSimulada)

#estimo los parametros de la frank conjunta
copulafrank3 <- frankCopula(dim = 5);N <-  26
fit.tauR <- fitCopula(copulafrank3, Conjunta,method="ml")

#encuentro la densidad de la copula conjunta
copulafrank4 <- frankCopula(0.863 , dim = 5)
fdensidad1  = dCopula(as.matrix(Conjunta), copulafrank4)

#Dx 
dxdengue <- c(rep(1,1000),rep(0,9000))
Conjunta_DX <- cbind(fdensidad1,dxdengue)
Conjunta_DX = data.frame(Conjunta_DX) 
Conjunta_DX$dxdengue <- as.factor(Conjunta_DX$dxdengue)


#curva de presicion y recall para un solo punto de corte 


pr<-pr.curve(scores.class0 = fdensidad4 , scores.class1 =  fdensidad3, curve = TRUE)
plot(pr)
pr$curve

recall <- pr$curve[, 1]  
precision <- pr$curve[, 2]     
thresholds <- pr$curve[, 3]
summary(thresholds)
plot(thresholds)

# Calcular F-score para cada punto
f_score <- 2 * precision * recall / (precision + recall)
plot(f_score)
# Obtener el umbral correspondiente al máximo F-score
umbral <- thresholds[which.max(f_score)]


#punto de corte por boostrap
PC <- c()
for (i in 1:1000) {
  # Sampleando datos para los dos grupos
  densidad_positivos <- sample(Conjunta_DX[Conjunta_DX$dxdengue == "1",]$fdensidad1, 1000, replace = TRUE)
  densidad_negativos <- sample(Conjunta_DX[Conjunta_DX$dxdengue == "0",]$fdensidad1, 9000, replace = TRUE)
  
  # Creando un dataframe con las muestras bootstrapped
  datos2 <- data.frame(dxdengue = factor(c(rep(1, 1000), rep(0, 9000))),
                       densidad = c(densidad_positivos, densidad_negativos))
  
  # Calculando la curva PR
  pr <- pr.curve(scores.class0 = datos2$densidad[datos2$dxdengue == "0"],
                 scores.class1 = datos2$densidad[datos2$dxdengue == "1"], curve = TRUE)
  
  recall <- pr$curve[, 1]  # Asume que la primera columna es recall
  precision <- pr$curve[, 2]  # Asume que la segunda columna es precision
  thresholds <- pr$curve[, 3]
  # Calculando F-score
  f_score <- 2 * precision * recall / (precision + recall)
  
  # Encontrando el umbral que maximiza el F-score
  max_index <- which.max(f_score)
  PC[i] <- thresholds[max_index]
}

# Puedes hacer algo con los PC calculados, como calcular la mediana
median_PC <- median(PC)



#grafico de la funcion de densidad dividido por dx
ggplot(data = BaseFinal,
                  mapping = aes(x = fdensidadfinal,
                                fill = factor(dxdengue),
                                color = factor(dxdengue))) +
  geom_histogram(aes(y = ..density..),
                 bins = 9,
                 position = 'identity',
                 alpha = 0.8,
                 color = 'black') +  
  geom_density(alpha = 0.2) +
  geom_freqpoly(stat = 'density',
                bins = 9,
                size = 0.2) +
  scale_fill_manual(values = c("0" = "#3F719E", "1" = "#B980A7")) +
  scale_color_manual(values = c("0" = "#2A5783", "1" = "#B81840")) +
  labs(title = 'Función de densidad conjunta',fill = 'dxdengue',color = 'dxdengue',x = 'Función de densidad conjunta',y = 'Densidad') +
  theme_minimal()
#--------------------------evaluación punto de corte--------------------------#

enfermos1 = data.frame(matriz,Datos$DX) #bd transformadas de la bd inicial y dx
enfermos0 = subset(enfermos1, enfermos1$Datos.DX == 1) #filtro la bd anterior solo por los dx =1 (enfermos)
enfermos0 = enfermos0[,-6]
enfermos0 = as.matrix(enfermos0) #bd transformada solo para los 1

fdensidadenfermos = dCopula(enfermos0, copulafrank1) #funcion copula para los enfermos (24 datos)
fdensidadnoenfermos = dCopula(matriz3, copulafrank2) #funcion de densidad no enfermos
fdensidadfinal = c(fdensidadenfermos,fdensidadnoenfermos) #funcion de densidad conjunta 


dxdengue <- c(rep(1,24),rep(0,300)) #gold standar
# Densidades para las variables originales
BaseFinal<-cbind(fdensidadfinal,dxdengue) #base final de densidades con los datos originales 
BaseFinal = as.data.frame(BaseFinal)
# Clasificando con el punto de corte
BaseFinal$ClMarcela <- as.factor(ifelse(BaseFinal$fdensidadfinal>median(PC),"0","1"))
table(BaseFinal$ClMarcela,BaseFinal$dxdengue)


#dxdengue5 <- c(rep(1,1000),rep(0,9000))
# Densidades para las variables originales
#fdensidadsimulada = c(fdensidad3,fdensidad4)
#BaseFinalsimulada<-cbind(fdensidadsimulada,dxdengue)

# Clasificando con el punto de corte
#clasificacionfinalsimulada <- as.factor(ifelse(BaseFinalsimulada[,1]>median(PC),"0","1")) 
#table(dxdengue5,clasificacionfinalsimulada)
#xtable(table(dxdengue5,clasificacionfinalsimulada))


#estraigo los algoritmos de mendez
Clasificaciones <- read_excel("clasifiacion_mendez_laboratorio.xlsx", sheet = "clasificacion", col_names = TRUE)
Clasificaciones <- Clasificaciones[,c(2,7:10)]
colnames(Clasificaciones) <- c('dxdengue','Mendez_Continuo_12','Mendez_Discreto_12','Mendez_continuo_8','Mendez_Discreto_8')

#establezco los dengue y los no dengue  de las clasificaciones de mendez
DengueCla<-Clasificaciones[Clasificaciones$dxdengue==1,]
NodengueCla<-Clasificaciones[Clasificaciones$dxdengue==0,]

Clasificaciones <- rbind(DengueCla,NodengueCla)

BaseFinal <- cbind(BaseFinal, Clasificaciones[,-1])
BaseFinal$dxdengue = as.factor(BaseFinal$dxdengue)


#------------------------------------------------------------------------------#
####          12. Prob predictivas de bayes                           ####
#------------------------------------------------------------------------------#
colnames(BaseFinal)
modelo1 <- naive_bayes(dxdengue~ClMarcela+Mendez_Discreto_12,data = BaseFinal,laplace = 1)
modelo2 <- naive_bayes(dxdengue~ClMarcela+Mendez_Continuo_12,data = BaseFinal,laplace = 1)
modelo3 <- naive_bayes(dxdengue~ClMarcela+Mendez_Discreto_8,data = BaseFinal,laplace = 1)
modelo4 <- naive_bayes(dxdengue~ClMarcela+Mendez_continuo_8,data = BaseFinal,laplace = 1)


#predictores = BaseFinal[,c(3,4,2)]
#predictores$dxdengue = as.factor(predictores$dxdengue)
#predictores$ClMarcela = as.numeric(predictores$ClMarcela)
#x <- as.matrix(predictores[, c(1, 2)])


#modelo1 = bernoulli_naive_bayes(x, predictores$dxdengue, prior = NULL, laplace = 1)
#Cla1 <- predict(modelo, x,type="prob")
# Probabilidades tanto para D=1 como para D=0

Cla1<-predict(modelo1,BaseFinal,type="prob");Cla1
Cla2<-predict(modelo2,BaseFinal,type="prob");Cla2
Cla3<-predict(modelo3,BaseFinal,type="prob");Cla3
Cla4<-predict(modelo4,BaseFinal,type="prob");Cla4

# Filtrando solo para las probabilidades D=1

Cla1 <- Cla1[,2]
Cla2 <- Cla2[,2]
Cla3 <- Cla3[,2]
Cla4 <- Cla4[,2]

# Guardando en la base

BaseFinal$PrNMD12 <-Cla1
BaseFinal$PrNMC12 <-Cla2
BaseFinal$PrNMD8 <-Cla3
BaseFinal$PrNMC8 <-Cla4


histcla1 = ggplot(data = BaseFinal, aes(x = PrNMD12, fill = factor(dxdengue))) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 5) +
  scale_fill_manual(values = c("0" = "#3F719E", "1" = "#B980A7")) +
  labs(title = "Probabilidades de tener Dengue segun Alg Marcela + Alg Mendez 1",
       x = "PrMMD12",
       y = "Frecuencia",
       fill = "Estado Dengue") +
  theme_minimal()

histcla2 = ggplot(data = BaseFinal, aes(x = PrNMC12, fill = factor(dxdengue))) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 5) +
  scale_fill_manual(values = c("0" = "#3F719E", "1" = "#B980A7")) +
  labs(title = "Probabilidades de tener Dengue segun Alg Marcela + Alg Mendez 2",
       x = "PrMMC12",
       y = "Frecuencia",
       fill = "Estado Dengue") +
  theme_minimal()


histcla3 = ggplot(data = BaseFinal, aes(x = PrNMD8, fill = factor(dxdengue))) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 5) +
  scale_fill_manual(values = c("0" = "#3F719E", "1" = "#B980A7")) +
  labs(title = "Probabilidades de tener Dengue segun Alg Marcela + Alg Mendez 3",
       x = "PrMMD8",
       y = "Frecuencia",
       fill = "Estado Dengue") +
  theme_minimal()

histcla4 = ggplot(data = BaseFinal, aes(x = PrNMC8, fill = factor(dxdengue))) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 5) +
  scale_fill_manual(values = c("0" = "#3F719E", "1" = "#B980A7")) +
  labs(title = "Probabilidades de tener Dengue segun Alg Marcela + Alg Mendez 4",
       x = "PrMMC8",
       y = "Frecuencia",
       fill = "Estado Dengue") +
  theme_minimal()

grid.arrange(histcla1, histcla2, histcla3, histcla4, ncol = 2, nrow = 2)
#------------------------------------------------------------------------------#
####          13. Curvas ROC para las probabilidades                        ####
#------------------------------------------------------------------------------#

# Punto de corte para ClMarcela y MendezDiscreto12

library(PRROC)
# Paso 1: Calcular la curva de precisión y recall
pr1 <- pr.curve(scores.class0 = BaseFinal$PrNMD12[BaseFinal$dxdengue == 0], 
               scores.class1 = BaseFinal$PrNMD12[BaseFinal$dxdengue == 1], curve = TRUE)
plot(pr1)

# Paso 2: Calcular el F-score para cada umbral
recall1 <- pr1$curve[, 1]
precision1 <- pr1$curve[, 2]
thresholds1 <- pr1$curve[, 3]
f_score1 <- 2 * precision1 * recall1 / (precision1 + recall1)
plot(f_score1, type = "l", main = "F-score vs Thresholds", xlab = "Threshold Index", ylab = "F-score")

# Paso 3: Obtener el umbral correspondiente al máximo F-score
umbral1 <- thresholds1[which.max(f_score1)]
cat("Umbral que maximiza el F-score:", umbral1, "\n")

# Paso 4: Bootstrap para obtener una distribución del punto de corte
PC1 <- c()
for (i in 1:10000) {
  datos2 <- data.frame(dxdengue = c(rep(1, sum(BaseFinal$dxdengue == 1)), 
                                    rep(0, sum(BaseFinal$dxdengue == 0))),
                       probabilidad = c(sample(BaseFinal$PrNMD12[BaseFinal$dxdengue == 1], 
                                               replace = TRUE),
                                        sample(BaseFinal$PrNMD12[BaseFinal$dxdengue == 0], 
                                               replace = TRUE)))
  
  pr1 <- pr.curve(scores.class0 = datos2$probabilidad[datos2$dxdengue == 0], 
                 scores.class1 = datos2$probabilidad[datos2$dxdengue == 1], curve = TRUE)
  
  recall1 <- pr1$curve[, 1]
  precision1 <- pr1$curve[, 2]
  thresholds1 <- pr1$curve[, 3]
  
  f_score1 <- 2 * precision1 * recall1 / (precision1 + recall1)
  PC1[i] <- thresholds1[which.max(f_score1)]
}
hist(PC1)

median(PC1)

# Paso 5: Clasificar los datos utilizando el punto de corte obtenido
BaseFinal$ClPrNMD12 <- as.factor(ifelse(BaseFinal$PrNMD12 > 0.0002, "1", "0"))
table(BaseFinal$ClPrNMD12, BaseFinal$dxdengue)
table( BaseFinal$dxdengue,BaseFinal$ClPrNMD12)

################################################################################
# Punto de corte para ClMarcela y MendezContinuo12
library(PRROC)

# Paso 1: Calcular la curva de precisión y recall para PrNMC12
pr2 <- pr.curve(scores.class0 = BaseFinal$PrNMC12[BaseFinal$dxdengue == 0], 
                      scores.class1 = BaseFinal$PrNMC12[BaseFinal$dxdengue == 1], curve = TRUE)
plot(pr2)

# Paso 2: Calcular el F-score para cada umbral
recall2 <- pr2$curve[, 1]
precision2 <- pr2$curve[, 2]
thresholds2 <- pr2$curve[, 3]
f_score2 <- 2 * precision2 * recall2 / (precision2 + recall2)
plot(f_score2, type = "l", main = "F-score vs Thresholds para PrNMC12", xlab = "Threshold Index", ylab = "F-score")

# Paso 3: Obtener el umbral correspondiente al máximo F-score
umbral2 <- thresholds2[which.max(f_score2)]
cat("Umbral que maximiza el F-score para PrNMC12:", umbral2, "\n")

# Paso 4: Bootstrap para obtener una distribución del punto de corte para PrNMC12
PC2 <- c()
for (i in 1:10000) {
  datos2 <- data.frame(dxdengue = c(rep(1, sum(BaseFinal$dxdengue == 1)), 
                                    rep(0, sum(BaseFinal$dxdengue == 0))),
                       probabilidad = c(sample(BaseFinal$PrNMC12[BaseFinal$dxdengue == 1], 
                                               replace = TRUE),
                                        sample(BaseFinal$PrNMC12[BaseFinal$dxdengue == 0], 
                                               replace = TRUE)))
  
  pr2 <- pr.curve(scores.class0 = datos2$probabilidad[datos2$dxdengue == 0], 
                 scores.class1 = datos2$probabilidad[datos2$dxdengue == 1], curve = TRUE)
  
  recall2 <- pr2$curve[, 1]
  precision2 <- pr2$curve[, 2]
  thresholds2 <- pr2$curve[, 3]
  
  f_score2 <- 2 * precision2 * recall2 / (precision2 + recall2)
  PC2[i] <- thresholds2[which.max(f_score2)]
}

# Visualizar la distribución del punto de corte
hist(PC2)
median(PC2)
# Paso 5: Clasificar los datos utilizando el punto de corte obtenido
BaseFinal$ClPrNMC12 <- as.factor(ifelse(BaseFinal$PrNMC12 > median(PC2), "1", "0"))
table(BaseFinal$ClPrNMC12, BaseFinal$dxdengue)
table(BaseFinal$dxdengue,BaseFinal$ClPrNMC12)


################################################################################
# Punto de corte para ClMarcela y Mendezdiscreto8
# Paso 1: Calcular la curva de precisión y recall para PrNMD8
pr3 <- pr.curve(scores.class0 = BaseFinal$PrNMD8[BaseFinal$dxdengue == 0], 
                     scores.class1 = BaseFinal$PrNMD8[BaseFinal$dxdengue == 1], curve = TRUE)
plot(pr3)

# Paso 2: Calcular el F-score para cada umbral
recall3 <- pr3$curve[, 1]
precision3 <- pr3$curve[, 2]
thresholds3 <- pr3$curve[, 3]
f_score3 <- 2 * precision3 * recall3 / (precision3 + recall3)
plot(f_score3, type = "l", main = "F-score vs Thresholds para PrNMD8", xlab = "Threshold Index", ylab = "F-score")

# Paso 3: Obtener el umbral correspondiente al máximo F-score
umbral3 <- thresholds3[which.max(f_score3)]
cat("Umbral que maximiza el F-score para PrNMD8:", umbral3, "\n")

# Paso 4: Bootstrap para obtener una distribución del punto de corte para PrNMD8
PC3 <- c()
for (i in 1:10000) {
  datos2 <- data.frame(dxdengue = c(rep(1, sum(BaseFinal$dxdengue == 1)), 
                                    rep(0, sum(BaseFinal$dxdengue == 0))),
                       probabilidad = c(sample(BaseFinal$PrNMD8[BaseFinal$dxdengue == 1], 
                                               replace = TRUE),
                                        sample(BaseFinal$PrNMD8[BaseFinal$dxdengue == 0], 
                                               replace = TRUE)))
  
  pr <- pr.curve(scores.class0 = datos2$probabilidad[datos2$dxdengue == 0], 
                 scores.class1 = datos2$probabilidad[datos2$dxdengue == 1], curve = TRUE)
  
  recall <- pr$curve[, 1]
  precision <- pr$curve[, 2]
  thresholds <- pr$curve[, 3]
  
  f_score <- 2 * precision * recall / (precision + recall)
  PC3[i] <- thresholds[which.max(f_score)]
}

# Visualizar la distribución del punto de corte
hist(PC3)
mean(PC3)
# Paso 5: Clasificar los datos utilizando el punto de corte obtenido
BaseFinal$ClPrNMD8 <- as.factor(ifelse(BaseFinal$PrNMD8 > mean(PC3), 1, 0))
table(BaseFinal$ClPrNMD8, BaseFinal$dxdengue)


###############################################################################3
# Punto de corte para ClMarcela y Mendezcontinuo8
# Paso 1: Calcular la curva de precisión y recall para PrNMC8
pr4 <- pr.curve(scores.class0 = BaseFinal$PrNMC8[BaseFinal$dxdengue == 0], 
                     scores.class1 = BaseFinal$PrNMC8[BaseFinal$dxdengue == 1], curve = TRUE)
plot(pr4)

# Paso 2: Calcular el F-score para cada umbral
recall4 <- pr4$curve[, 1]
precision4 <- pr4$curve[, 2]
thresholds4 <- pr4$curve[, 3]
f_score4 <- 2 * precision4 * recall4 / (precision4 + recall4)
plot(f_score4, type = "l", main = "F-score vs Thresholds para PrNMC8", xlab = "Threshold Index", ylab = "F-score")

# Paso 3: Obtener el umbral correspondiente al máximo F-score
umbral4 <- thresholds4[which.max(f_score4)]
cat("Umbral que maximiza el F-score para PrNMC8:", umbral4, "\n")

# Paso 4: Bootstrap para obtener una distribución del punto de corte para PrNMC8
PC4 <- c()
for (i in 1:10000) {
  datos2 <- data.frame(dxdengue = c(rep(1, sum(BaseFinal$dxdengue == 1)), 
                                    rep(0, sum(BaseFinal$dxdengue == 0))),
                       probabilidad = c(sample(BaseFinal$PrNMC8[BaseFinal$dxdengue == 1], 
                                               replace = TRUE),
                                        sample(BaseFinal$PrNMC8[BaseFinal$dxdengue == 0], 
                                               replace = TRUE)))
  
  pr <- pr.curve(scores.class0 = datos2$probabilidad[datos2$dxdengue == 0], 
                 scores.class1 = datos2$probabilidad[datos2$dxdengue == 1], curve = TRUE)
  
  recall <- pr$curve[, 1]
  precision <- pr$curve[, 2]
  thresholds <- pr$curve[, 3]
  
  f_score <- 2 * precision * recall / (precision + recall)
  PC4[i] <- thresholds[which.max(f_score)]
}

# Visualizar la distribución del punto de corte
hist(PC4)
median(PC4)
# Paso 5: Clasificar los datos utilizando el punto de corte obtenido
BaseFinal$ClPrNMC8 <- as.factor(ifelse(BaseFinal$PrNMC8 > median(PC4), "1", "0"))
table(BaseFinal$ClPrNMC8, BaseFinal$dxdengue)


#------------------------------------------------------------------------------#
####          14. Parametros de desempeño de los algoritmos                 ####
#------------------------------------------------------------------------------#

# Algunas modificaciones preliminares

BaseFinal$dxdengue <- factor(BaseFinal$dxdengue,levels = c("1","0"),ordered = TRUE)
BaseFinal$ClMarcela <- factor(BaseFinal$Marcela,levels = c("1","0"),ordered = TRUE)
BaseFinal$ClPrNMD12 <- factor(BaseFinal$ClPrNMD12,levels = c("1","0"),ordered = TRUE)
BaseFinal$ClPrNMC12 <- factor(BaseFinal$ClPrNMC12,levels = c("1","0"),ordered = TRUE)
BaseFinal$ClPrNMD8 <- factor(BaseFinal$ClPrNMD8,levels = c("1","0"),ordered = TRUE)
BaseFinal$ClPrNMC8 <- factor(BaseFinal$ClPrNMC8,levels = c("1","0"),ordered = TRUE)





































#