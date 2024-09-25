library(readxl)

BD_ALGORITMOS_DENGUE_ACT <- read_excel("BD_ALGORITMOS_DENGUE_ACT.xlsx",sheet = "Hoja1")


#Se vuelve factor el patron de oro
BD_ALGORITMOS_DENGUE_ACT$DX<-factor(BD_ALGORITMOS_DENGUE_ACT$DX)

library(ggplot2)
# Crea el histograma con densidad usando ggplot2
Uno<-ggplot(BD_ALGORITMOS_DENGUE_ACT, aes(x = p9_Leuco, fill = DX)) +
  geom_histogram(aes(y = ..density..), bins = 10, alpha = 0.5, position = "identity") +
  geom_density(alpha = 0.2) +
  scale_fill_manual(values = c("0" = "blue", "1" = "red")) +
  labs(title = "",
       x = "p9_Leuco", y = "Densidad") +
  theme_minimal()
Dos<-ggplot(BD_ALGORITMOS_DENGUE_ACT, aes(x = p10_Plaquetas, fill = DX)) +
  geom_histogram(aes(y = ..density..), bins = 10, alpha = 0.5, position = "identity") +
  geom_density(alpha = 0.2) +
  scale_fill_manual(values = c("0" = "blue", "1" = "red")) +
  labs(title = "",
       x = "p10_Plaquetas", y = "Densidad") +
  theme_minimal()
Tres<-ggplot(BD_ALGORITMOS_DENGUE_ACT, aes(x = linfocitos_neutrofilos, fill = DX)) +
  geom_histogram(aes(y = ..density..), bins = 10, alpha = 0.5, position = "identity") +
  geom_density(alpha = 0.2) +
  scale_fill_manual(values = c("0" = "blue", "1" = "red")) +
  labs(title = "",
       x = "linfocitos_neutrofilos", y = "Densidad") +
  theme_minimal()
Cuatro<-ggplot(BD_ALGORITMOS_DENGUE_ACT, aes(x = nmono, fill = DX)) +
  geom_histogram(aes(y = ..density..), bins = 10, alpha = 0.5, position = "identity") +
  geom_density(alpha = 0.2) +
  scale_fill_manual(values = c("0" = "blue", "1" = "red")) +
  labs(title = "",
       x = "nmono", y = "Densidad") +
  theme_minimal()
Quinto<-ggplot(BD_ALGORITMOS_DENGUE_ACT, aes(x = hematocrito_hemoglobina, fill = DX)) +
  geom_histogram(aes(y = ..density..), bins = 10, alpha = 0.5, position = "identity") +
  geom_density(alpha = 0.2) +
  scale_fill_manual(values = c("0" = "blue", "1" = "red")) +
  labs(title = "",
       x = "hematocrito_hemoglobina", y = "Densidad") +
  theme_minimal()
library(patchwork)
(Uno|Dos)/(Tres|Cuatro|Quinto)
library(FactoMineR)
library(factoextra)
ACP<- PCA(BD_ALGORITMOS_DENGUE_ACT[,-ncol(BD_ALGORITMOS_DENGUE_ACT)],graph=FALSE)
Var<- fviz_pca_var(ACP)
Biplot<- fviz_pca_biplot(ACP,habillage=BD_ALGORITMOS_DENGUE_ACT$DX)
(Var|Biplot)
col=c('pink','blue')
par(mfrow=c(3,2))
Boxplot<-lapply(BD_ALGORITMOS_DENGUE_ACT[,-ncol(BD_ALGORITMOS_DENGUE_ACT)],function(x){
  boxplot(x~BD_ALGORITMOS_DENGUE_ACT$DX,col=col,xlab='')
})
out.mult=function(Datos){
  n= nrow(Datos); p= ncol(Datos)
  Distance= mahalanobis(Datos,center=colMeans(Datos),cov=cov(Datos))
  Limit= qchisq(0.01, lower.tail=F,df=p)
  id.dist= which(Distance>Limit)
  Score_LOF = DMwR2::lofactor(Datos, k=5)
  id.LOF <- order(Score_LOF, decreasing=T)[1:ceiling(0.01*n)]
  
  windows()
  par(mfrow=c(2,1))
  plot(Distance,pch=20,ylim=c(0,max(Distance)*1.2))
  text(id.dist,Distance[id.dist],id.dist, col="red",pos=3,cex=0.8)
  abline(h=Limit,col="red",lwd=2,lty=2)
  plot(Score_LOF,pch=20,ylim=c(0,max(Score_LOF)*1.2))
  text(id.LOF,Score_LOF[id.LOF],id.LOF, col="red",pos=3,cex=0.8)
  return(list(Out_dist=id.dist,Out_LOF=id.LOF))
}
library(easypackages) #Librería especializada en carga de paquetes
lib<- c("MASS","lmtest","car","corrplot","ggplot2","plotly","scatterplot3d","GGally",
        "plot3D","rgl","scatterplot3d","plot3Drgl","alr4","effects","ggfortify","reshape2",
        "patchwork")
easypackages::libraries(lib)
out.mult(BD_ALGORITMOS_DENGUE_ACT[,-ncol(BD_ALGORITMOS_DENGUE_ACT)])
CorGraph<- function(x,method,colneg="aquamarine",colpos="purple"){
  cor<- round(cor(x,method = method),2)
  melted_cor <- melt(cor)
  l<-ggplot(data = melted_cor, aes(x=Var1, y=Var2, fill=value)) + 
    geom_tile() +
    geom_text(aes(Var2, Var1, label = value), size = 5) +
    scale_fill_gradient2(low = colneg, high = colpos,
                         limit = c(-1,1), name=paste0("Correlation ", method)) +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          panel.background = element_blank())
  l
}
Cor<-CorGraph(BD_ALGORITMOS_DENGUE_ACT[,sapply(BD_ALGORITMOS_DENGUE_ACT,is.numeric)],'kendall',colneg="aquamarine",colpos="purple")
library(plotly)
ggplotly(Cor)
distribucion<- function(X,distribution){
  x11()
  plotdist(X, histo=TRUE, demp=TRUE,col="aquamarine4",lwd=2)
  x11()
  descdist(X)
  x11()
  fw<- list()
  summar<- list()
  bendo.B <-list()
  summar2<- list()
  for(i in 1:length(distribution)){
    fw[[i]]<- fitdist(X, distribution[i])
    summar[[i]]<- summary(fw[[i]])
    bendo.B[[i]]<-bootdist(fw[[i]], niter = 1000)
    summar2[[i]]<- summary(bendo.B[[i]])
  }
  par(mfrow=c(2,2))
  plot.legend<-distribution
  denscomp(fw, legendtext=plot.legend);qqcomp(fw, legendtext=plot.legend)
  cdfcomp(fw, legendtext=plot.legend);ppcomp(fw, legendtext=plot.legend)
  print(gofstat(fw,fitnames = distribution))
  for(i in 1:length(distribution)){
    x11()
    plot(bendo.B[[i]])
    
  }
  print(summar)
  print(summar2)
}
library(fitdistrplus)
fw<-fitdist(BD_ALGORITMOS_DENGUE_ACT$p10_Plaquetas, "weibull",method='mle')
summary(fw)
plot(fw)
bendo.B <- bootdist(fw, niter = 100)
summary(bendo.B)
plot(bendo.B)
