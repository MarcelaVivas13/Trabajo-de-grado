Conjunta <- rbind(DengueSimulada,NoDengueSimulada)
dxdengue <- c(rep(1,1000),rep(0,9000))
Conjunta <- cbind(Conjunta,fdensidad1,dxdengue)
Conjunta <- data.frame(Conjunta) 
Conjunta$dxdengue <- as.factor(Conjunta$dxdengue)




roc <- pROC::roc(Conjunta$dxdengue,Conjunta$fdensidad1, auc = TRUE, ci = TRUE)
print(roc)

windows(height=10,width=15)
pROC::plot.roc(roc, legacy.axes = TRUE, print.thres = "best", print.auc = TRUE,
               auc.polygon = FALSE, max.auc.polygon = FALSE, auc.polygon.col = "gainsboro",
               col = 2, grid = TRUE, main='Curva ROC - Punto de corte') 
rm(roc)



PC <- c()
for (i in 1:10000) {
  datos2 <- data.frame(dxdengue=c(rep("1",1000), rep("0",9000)), fdensidad1=c(sample(Conjunta[Conjunta$dxdengue == "1" ,]$fdensidad1, replace = TRUE), sample(Conjunta[Conjunta$dxdengue == "0" ,]$fdensidad1, replace = TRUE)))
  
  roc <- pROC::roc(datos2$dxdengue,datos2$fdensidad1, auc = TRUE, ci = TRUE)
  
  SensEspec <-roc$sensitivities + roc$specificities 
  maximo    <- max(SensEspec)
  numOrdenCutoff <- which(SensEspec==maximo)
  PC[i]<-roc$thresholds[numOrdenCutoff]
  
}

summary(PC)
hist(PC)
mean(PC)
plot(roc)

BaseFinal$ClMarcela <- as.factor(ifelse(BaseFinal$fdensidadfinal>mean(PC),"0","1"))



predictores = BaseFinal[,c(3,4,2)]
predictores$dxdengue = as.factor(predictores$dxdengue)
predictores$ClMarcela = as.numeric(predictores$ClMarcela)
x <- as.matrix(predictores[, c(1, 2)])
modelo1 = bernoulli_naive_bayes(x, predictores$dxdengue, prior = NULL, laplace = 1)
Cla1 <- predict(modelo, x,type="prob")

Cla1 <- Cla1[,2]
BaseFinal$PrNMD12 <-Cla1


# Bootstrap

# Punto de corte para ClNicolas y MendezDiscreto12

PC1 <- c()
for (i in 1:1000) {
  datos2 <- data.frame(dxdengue=c(rep("1",24), rep("0",300)), probabilidad=c(sample(BaseFinal[BaseFinal$dxdengue == "1" ,]$PrNMD12, replace = TRUE), sample(BaseFinal[BaseFinal$dxdengue == "0" ,]$PrNMD12, replace = TRUE)))
  
  roc <- pROC::roc(datos2$dxdengue,datos2$probabilidad, auc = TRUE, ci = TRUE)
  
  SensEspec <-roc$sensitivities + roc$specificities 
  maximo    <- max(SensEspec)
  numOrdenCutoff <- which(SensEspec==maximo)
  PC1[i]<-roc$thresholds[numOrdenCutoff]
}

mean(PC1)


windows(height=10,width=15)
par(mfrow = c(2,2))
pROC::plot.roc(roc, legacy.axes = TRUE, print.thres = "best", print.auc = TRUE,
               auc.polygon = FALSE, max.auc.polygon = FALSE, auc.polygon.col = "gainsboro",
               col = 2, grid = TRUE, main='Curva ROC - Punto de corte - Regla clasificadora + Mendez2') 


# Paso 5: Clasificar los datos utilizando el punto de corte obtenido
BaseFinal$ClPrNMD12 <- as.factor(ifelse(BaseFinal$PrNMD12 > mean(PC1), "1", "0"))
table(BaseFinal$ClPrNMD12, BaseFinal$dxdengue)





# Punto de corte para ClNicolas y Mendezcontinuo12

PC2 <- c()
for (i in 1:1000) {
  datos2 <- data.frame(dxdengue=c(rep("1",24), rep("0",300)), probabilidad=c(sample(BaseFinal[BaseFinal$dxdengue == "1" ,]$PrNMC12, replace = TRUE), sample(BaseFinal[BaseFinal$dxdengue == "0" ,]$PrNMC12, replace = TRUE)))
  
  roc2 <- pROC::roc(datos2$dxdengue,datos2$probabilidad, auc = TRUE, ci = TRUE)
  
  SensEspec <-roc2$sensitivities + roc2$specificities 
  maximo    <- max(SensEspec)
  numOrdenCutoff <- which(SensEspec==maximo)
  PC2[i]<-roc2$thresholds[numOrdenCutoff]
}

mean(PC2)



pROC::plot.roc(roc2, legacy.axes = TRUE, print.thres = "best", print.auc = TRUE,
               auc.polygon = FALSE, max.auc.polygon = FALSE, auc.polygon.col = "gainsboro",
               col = 2, grid = TRUE, main='Curva ROC - Punto de corte - Regla clasificadora + Mendez4') 


# Paso 5: Clasificar los datos utilizando el punto de corte obtenido
BaseFinal$ClPrNMC12 <- as.factor(ifelse(BaseFinal$PrNMC12 > mean(PC2), "1", "0"))
table(BaseFinal$ClPrNMC12, BaseFinal$dxdengue)






# Punto de corte para ClNicolas y Mendezdiscreto8

PC3 <- c()
for (i in 1:1000) {
  datos2 <- data.frame(dxdengue=c(rep("1",24), rep("0",300)), probabilidad=c(sample(BaseFinal[BaseFinal$dxdengue == "1" ,]$PrNMD8, replace = TRUE), sample(BaseFinal[BaseFinal$dxdengue == "0" ,]$PrNMD8, replace = TRUE)))
  
  roc3 <- pROC::roc(datos2$dxdengue,datos2$probabilidad, auc = TRUE, ci = TRUE)
  
  SensEspec <-roc3$sensitivities + roc3$specificities 
  maximo    <- max(SensEspec)
  numOrdenCutoff <- which(SensEspec == maximo)[1]
  PC3[i]<-roc3$thresholds[numOrdenCutoff]
}

mean(PC3)



pROC::plot.roc(roc3, legacy.axes = TRUE, print.thres = "best", print.auc = TRUE,
               auc.polygon = FALSE, max.auc.polygon = FALSE, auc.polygon.col = "gainsboro",
               col = 2, grid = TRUE, main='Curva ROC - Punto de corte - Regla clasificadora + Mendez1') 


# Paso 5: Clasificar los datos utilizando el punto de corte obtenido
BaseFinal$ClPrNMD8 <- as.factor(ifelse(BaseFinal$PrNMD8 > 0.462, "1", "0"))
table(BaseFinal$ClPrNMD8, BaseFinal$dxdengue)




# Punto de corte para ClNicolas y Mendezcontinuo8

PC4 <- c()
for (i in 1:1000) {
  datos2 <- data.frame(dxdengue=c(rep("1",24), rep("0",300)), probabilidad=c(sample(BaseFinal[BaseFinal$dxdengue == "1" ,]$PrNMC8, replace = TRUE), sample(BaseFinal[BaseFinal$dxdengue == "0" ,]$PrNMC8, replace = TRUE)))
  
  roc4 <- pROC::roc(datos2$dxdengue,datos2$probabilidad, auc = TRUE, ci = TRUE)
  
  SensEspec <-roc4$sensitivities + roc4$specificities 
  maximo    <- max(SensEspec)
  numOrdenCutoff <- which(SensEspec == maximo)[1]
  PC4[i]<-roc4$thresholds[numOrdenCutoff]
}

mean(PC4)



pROC::plot.roc(roc4, legacy.axes = TRUE, print.thres = "best", print.auc = TRUE,
               auc.polygon = FALSE, max.auc.polygon = FALSE, auc.polygon.col = "gainsboro",
               col = 2, grid = TRUE, main='Curva ROC - Punto de corte - Regla clasificadora + Mendez3') 


# Paso 5: Clasificar los datos utilizando el punto de corte obtenido
BaseFinal$ClPrNMC8 <- as.factor(ifelse(BaseFinal$PrNMC8 > mean(PC4), "1", "0"))
table(BaseFinal$ClPrNMC8, BaseFinal$dxdengue)









#############################3333333###########################################
###############################################################################

#Parametros de desempeño 
BaseFinal$dxdengue <- factor(BaseFinal$dxdengue,levels = c("1","0"),ordered = TRUE)
BaseFinal$ClMarcela <- factor(BaseFinal$ClMarcela,levels = c("1","0"),ordered = TRUE)
BaseFinal$ClPrNMD12 <- factor(BaseFinal$ClPrNMD12,levels = c("1","0"),ordered = TRUE)
BaseFinal$ClPrNMC12 <- factor(BaseFinal$ClPrNMC12,levels = c("1","0"),ordered = TRUE)
BaseFinal$ClPrNMD8 <- factor(BaseFinal$ClPrNMD8,levels = c("1","0"),ordered = TRUE)
BaseFinal$ClPrNMC8 <- factor(BaseFinal$ClPrNMC8,levels = c("1","0"),ordered = TRUE)

# Ahora si procedemos a calcular los parametros Theta1 = sensibilidad y Theta2 = especificidad
# Todo esto con una a priori no informativa

# Para ClMarcela

table(BaseFinal$ClMarcela, BaseFinal$dxdengue)

Theta11 <- qbeta(0.5,1,25)
print(c(qbeta(0.025,1,25),qbeta(0.025,1,25, lower.tail = FALSE))) # Region de credibilidad
Theta21 <- qbeta(0.5,296,6)
print(c(qbeta(0.025,296,6),qbeta(0.025,296,6, lower.tail = FALSE))) # Region de credibilidad
pROC::auc(BaseFinal$dxdengue,BaseFinal$ClMarcela)

# Para ClPrNMD12

table(BaseFinal$ClPrNMD12, BaseFinal$dxdengue)

Theta12 <- qbeta(0.5,10,16)
print(c(qbeta(0.025,10,16),qbeta(0.025,10,16, lower.tail = FALSE))) # Region de credibilidad
Theta22 <- qbeta(0.5,301,1)
print(c(qbeta(0.025,301,1),qbeta(0.025,301,1, lower.tail = FALSE))) # Region de credibilidad
pROC::auc(BaseFinal$dxdengue,BaseFinal$ClPrNMD12)


# Para ClPrNMC12

table(BaseFinal$ClPrNMC12, BaseFinal$dxdengue)

Theta13 <- qbeta(0.5,15,11)
print(c(qbeta(0.025,15,11),qbeta(0.025,15,11, lower.tail = FALSE))) # Region de credibilidad
Theta23 <- qbeta(0.5,232,70)
print(c(qbeta(0.025,232,70),qbeta(0.025,232,70, lower.tail = FALSE))) # Region de credibilidad
pROC::auc(BaseFinal$dxdengue,BaseFinal$ClPrNMC12)

# Para ClPrNMD8

table(BaseFinal$ClPrNMD8, BaseFinal$dxdengue)

Theta14 <- qbeta(0.5,7,20)
print(c(qbeta(0.025,2,20),qbeta(0.025,7,20, lower.tail = FALSE))) # Region de credibilidad
Theta24 <- qbeta(0.5,301,1)
print(c(qbeta(0.025,301,1),qbeta(0.025,301,1, lower.tail = FALSE))) # Region de credibilidad
pROC::auc(BaseFinal$dxdengue,BaseFinal$ClPrNMD8)

# Para ClPrNMC8

table(BaseFinal$ClPrNMC8, BaseFinal$dxdengue)

Theta15 <- qbeta(0.5,16,10)
print(c(qbeta(0.025,16,10),qbeta(0.025,16,10, lower.tail = FALSE))) # Region de credibilidad
Theta25 <- qbeta(0.5,245,54)
print(c(qbeta(0.025,245,54),qbeta(0.025,245,54, lower.tail = FALSE))) # Region de credibilidad
pROC::auc(BaseFinal$dxdengue,BaseFinal$ClPrNMC8)







# Ahora se calculan los parametros de desempeño con una a priori informativa
# Se usan los valores obtenidos por Mendez

#Sensibilidad a = 78 ; b = 33
Thetas1M <-c(0.666,0.672,0.722,0.757) #son los valores de las 4 sensibilidades (1 para cada algoritmo de mendez)
mean(Thetas1M);var(Thetas1M)

#Especificidad a = 13 ; b = 6
Thetas2M <-c(0.59,0.677,0.603,0.816) #son los valores de las 4 especificidades
mean(Thetas2M);var(Thetas2M)


# Para ClMarcela

table(BaseFinal$ClMarcela, BaseFinal$dxdengue)

Theta11M <- qbeta(0.5,78,57)
print(c(qbeta(0.025,78,57),qbeta(0.025,78,57, lower.tail = FALSE))) # Region de credibilidad
Theta21M <- qbeta(0.5,308,11)
print(c(qbeta(0.025,308,11),qbeta(0.025,308,11, lower.tail = FALSE))) # Region de credibilidad

# Para ClNMD12

table(BaseFinal$ClPrNMD12, BaseFinal$dxdengue)

Theta12M <- qbeta(0.5,87,48)
print(c(qbeta(0.025,87,48),qbeta(0.025,87,48, lower.tail = FALSE))) # Region de credibilidad
Theta22M <- qbeta(0.5,313,6)
print(c(qbeta(0.025,313,6),qbeta(0.025,313,6, lower.tail = FALSE))) # Region de credibilidad

# Para ClNMC12

table(BaseFinal$ClPrNMC12, BaseFinal$dxdengue)

Theta13M <- qbeta(0.5,92,43)
print(c(qbeta(0.025,92,43),qbeta(0.025,92,43, lower.tail = FALSE))) # Region de credibilidad
Theta23M <- qbeta(0.5,244,75)
print(c(qbeta(0.025,244,75),qbeta(0.025,244,75, lower.tail = FALSE))) # Region de credibilidad

# Para ClNMD8

table(BaseFinal$ClPrNMD8, BaseFinal$dxdengue)

Theta14M <- qbeta(0.5,83,52)
print(c(qbeta(0.025,83,52),qbeta(0.025,83,52, lower.tail = FALSE))) # Region de credibilidad
Theta24M <- qbeta(0.5,300,6)
print(c(qbeta(0.025,300,6),qbeta(0.025,300,6, lower.tail = FALSE))) # Region de credibilidad

# Para ClNMC8

table(BaseFinal$ClPrNMC8, BaseFinal$dxdengue)

Theta15M <- qbeta(0.5,93,42)
print(c(qbeta(0.025,93,42),qbeta(0.025,93,42, lower.tail = FALSE))) # Region de credibilidad
Theta25M <- qbeta(0.5,260,59)
print(c(qbeta(0.025,260,59),qbeta(0.025,260,59, lower.tail = FALSE))) # Region de credibilidad















################################################################################
################################################################################

#calculo de los valores predictivos 
library(ggplot2)
library(gridExtra)

# Crear una secuencia de prevalencias
prevalencia <- seq(0, 1, by = 0.01)


#Valores predictivos para la estimaciones por apriori no informativa 
# 
VPP_1<- (Theta11 * prevalencia) / 
  ((Theta11 * prevalencia) + ((1 - Theta21) * (1 - prevalencia)))
VPN_1 <- (Theta21* (1 - prevalencia)) / 
  (((1 - Theta11) * prevalencia) + (Theta21* (1 - prevalencia)))
# 
VPP_2 <- (Theta12 * prevalencia) / 
  ((Theta12 * prevalencia) + ((1 - Theta22) * (1 - prevalencia)))
VPN_2 <- (Theta22 * (1 - prevalencia)) / 
  (((1 - Theta12) * prevalencia) + (Theta22 * (1 - prevalencia)))
# 
VPP_3 <- (Theta13 * prevalencia) / 
  ((Theta13 * prevalencia) + ((1 - Theta23) * (1 - prevalencia)))
VPN_3 <- (Theta23 * (1 - prevalencia)) / 
  (((1 - Theta13) * prevalencia) + (Theta23 * (1 - prevalencia)))
# 
VPP_4 <- (Theta14 * prevalencia) / 
  ((Theta14 * prevalencia) + ((1 - Theta24) * (1 - prevalencia)))
VPN_4 <- (Theta24 * (1 - prevalencia)) / 
  (((1 - Theta14) * prevalencia) + (Theta24 * (1 - prevalencia)))
# 
VPP_5 <- (Theta15 * prevalencia) / 
  ((Theta15 * prevalencia) + ((1 - Theta25) * (1 - prevalencia)))
VPN_5 <- (Theta25 * (1 - prevalencia)) / 
  (((1 - Theta15) * prevalencia) + (Theta25 * (1 - prevalencia)))




# Crear una función para generar los gráficos
crear_grafico <- function(prevalencia, VPP, VPN, titulo) {
  data <- data.frame(
    Prevalencia = prevalencia,
    VPP = VPP,
    VPN = VPN
  )
  
  ggplot(data, aes(x = Prevalencia)) +
    geom_line(aes(y = VPP, color = "VPP"), size = 1) +
    geom_line(aes(y = VPN, color = "VPN"), size = 1) +
    labs(title = titulo, y = "Valores Predictivos", x = "Prevalencia") +
    scale_color_manual(name = "", values = c("VPP" = "skyblue", "VPN" = "red")) +
    theme_minimal()
}

#graficos individuales
#
grafico_1 <- crear_grafico(prevalencia, VPP_1, VPN_1, "Regla clasificación a partir de función cópula")
# 
grafico_2 <- crear_grafico(prevalencia, VPP_2, VPN_2, "Regla clasificadora + Alg 2 Méndez")
# 
grafico_3 <- crear_grafico(prevalencia, VPP_3, VPN_3, "Regla clasificadora + Alg 4 Méndez")
# 
grafico_4 <- crear_grafico(prevalencia, VPP_4, VPN_4, "Regla clasificadora + Alg 1 Méndez")
# 
grafico_5 <- crear_grafico(prevalencia, VPP_5, VPN_5, "Regla clasificadora + Alg 3 Méndez")

# Combinar los gráficos en una sola visualización
x11()
grid.arrange(grafico_1, grafico_4,grafico_2, grafico_5,grafico_3, ncol = 3, nrow = 2)



#Valores predictivos para la estimaciones por apriori informativa 
# 
VPP_1M<- (Theta11M * prevalencia) / 
  ((Theta11M * prevalencia) + ((1 - Theta21M) * (1 - prevalencia)))
VPN_1M <- (Theta21M* (1 - prevalencia)) / 
  (((1 - Theta11M) * prevalencia) + (Theta21M* (1 - prevalencia)))
# 
VPP_2M <- (Theta12M * prevalencia) / 
  ((Theta12M * prevalencia) + ((1 - Theta22M) * (1 - prevalencia)))
VPN_2M <- (Theta22M * (1 - prevalencia)) / 
  (((1 - Theta12M) * prevalencia) + (Theta22M * (1 - prevalencia)))
# 
VPP_3M <- (Theta13M * prevalencia) / 
  ((Theta13M * prevalencia) + ((1 - Theta23M) * (1 - prevalencia)))
VPN_3M <- (Theta23M * (1 - prevalencia)) / 
  (((1 - Theta13M) * prevalencia) + (Theta23M * (1 - prevalencia)))
# 
VPP_4M <- (Theta14M * prevalencia) / 
  ((Theta14M * prevalencia) + ((1 - Theta24M) * (1 - prevalencia)))
VPN_4M <- (Theta24M * (1 - prevalencia)) / 
  (((1 - Theta14M) * prevalencia) + (Theta24M * (1 - prevalencia)))
# 
VPP_5M <- (Theta15M * prevalencia) / 
  ((Theta15M * prevalencia) + ((1 - Theta25M) * (1 - prevalencia)))
VPN_5M <- (Theta25M * (1 - prevalencia)) / 
  (((1 - Theta15M) * prevalencia) + (Theta25M * (1 - prevalencia)))




# Crear una función para generar los gráficos
crear_grafico <- function(prevalencia, VPP, VPN, titulo) {
  data <- data.frame(
    Prevalencia = prevalencia,
    VPP = VPP,
    VPN = VPN
  )
  
  ggplot(data, aes(x = Prevalencia)) +
    geom_line(aes(y = VPP, color = "VPP"), size = 1) +
    geom_line(aes(y = VPN, color = "VPN"), size = 1) +
    labs(title = titulo, y = "Valores Predictivos", x = "Prevalencia") +
    scale_color_manual(name = "", values = c("VPP" = "skyblue", "VPN" = "red")) +
    theme_minimal()
}

#graficos individuales
#
grafico_1M <- crear_grafico(prevalencia, VPP_1M, VPN_1M, "Regla clasificación a partir de función cópula")
# 
grafico_2M <- crear_grafico(prevalencia, VPP_2M, VPN_2M, "Regla clasificadora + Alg 2 Méndez")
# 
grafico_3M <- crear_grafico(prevalencia, VPP_3M, VPN_3M, "Regla clasificadora + Alg 4 Méndez")
# 
grafico_4M <- crear_grafico(prevalencia, VPP_4M, VPN_4M, "Regla clasificadora + Alg 1 Méndez")
# 
grafico_5M <- crear_grafico(prevalencia, VPP_5M, VPN_5M, "Regla clasificadora + Alg 3 Méndez")

# Combinar los gráficos en una sola visualización
x11()
grid.arrange(grafico_1M, grafico_4M,grafico_2M, grafico_5M,grafico_3M, ncol = 3, nrow = 2)

