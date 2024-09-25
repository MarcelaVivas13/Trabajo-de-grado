

library(readxl)
Datos <- read_excel("BD_ALGORITMOS_DENGUE_ACT.xlsx",sheet = "Hoja1")

#Se vuelve factor el patron de oro
Datos$DX<-factor(Datos$DX)
Datos = Datos[,-c(7,8)]

#Estadisticas descriptivas 
enfermos <- Datos[Datos$DX == "1", ]
noenfermos <- Datos[Datos$DX == "0", ]
summary(Datos); summary(enfermos); summary(noenfermos)


#desviacion y coeficiente de variacicon
lapply(enfermos[,-6], function(x) {
  desviacion <- sd(x, na.rm = TRUE) # Calcular desviación estándar
  media <- mean(x, na.rm = TRUE)    # Calcular media
  coef_variacion <- (desviacion / media)*100 # Calcular coeficiente de variación
  
  # Retornar una lista con ambos valores
  list(Desviacion = desviacion, Coef_Variacion = coef_variacion)
})

lapply(noenfermos[,-6], function(x) {
  desviacion <- sd(x, na.rm = TRUE) # Calcular desviación estándar
  media <- mean(x, na.rm = TRUE)    # Calcular media
  coef_variacion <- (desviacion / media)*100 # Calcular coeficiente de variación
  
  # Retornar una lista con ambos valores
  list(Desviacion = desviacion, Coef_Variacion = coef_variacion)
})

library(GGally)
library(ggplot2)
library(patchwork) #une los graficos en una sola imagen

##HISTOGRAMAS GENERALES----
histograma_leucocitos <- ggplot(data = Datos, aes(x = p9_Leuco)) +
  geom_histogram(aes(y = ..density..), bins = 9, fill = "#3F719E", color = "black", alpha = 0.8) +
  geom_density(color = "#B81840", linetype = "solid", size = 0.5) +
  labs(title = '',
       x = 'Leucocitos',
       y = 'Densidad') +
  theme_minimal()
histograma_plaquetas <- ggplot(data = Datos, aes(x = p10_Plaquetas)) +
  geom_histogram(aes(y = ..density..), bins = 9, fill = "#3F719E", color = "black", alpha = 0.8) +
  geom_density(color = "#B81840", linetype = "solid", size = 0.5) +
  labs(title = '',
       x = 'Plaquetas',
       y = 'Densidad') +
  theme_minimal()
histograma_monocitos <- ggplot(data = Datos, aes(x = nmono)) +
  geom_histogram(aes(y = ..density..), bins = 9, fill = "#3F719E", color = "black", alpha = 0.8) +
  geom_density(color = "#B81840", linetype = "solid", size = 0.5) +
  labs(title = '',
       x = 'Monocitos',
       y = 'Densidad') +
  theme_minimal()
histograma_hemahemo <- ggplot(data = Datos, aes(x = hematocrito_hemoglobina)) +
  geom_histogram(aes(y = ..density..), bins = 9, fill = "#3F719E", color = "black", alpha = 0.8) +
  geom_density(color = "#B81840", linetype = "solid", size = 0.5) +
  labs(title = '',
       x = 'Hematocrito / Hemoglobina',
       y = 'Densidad') +
  theme_minimal()
histograma_linneu <- ggplot(data = Datos, aes(x = linfocitos_neutrofilos)) +
  geom_histogram(aes(y = ..density..), bins = 9, fill = "#3F719E", color = "black", alpha = 0.8) +
  geom_density(color = "#B81840", linetype = "solid", size = 0.5) +
  labs(title = '',
       x = 'Linfocitos / Neutrofilos',
       y = 'Densidad') +
  theme_minimal()
histogramas_combinados = histograma_leucocitos + histograma_plaquetas + histograma_monocitos + histograma_hemahemo + histograma_linneu +
  plot_layout(guides = 'collect')
print(histogramas_combinados)


####HISTOGRAMA ENFERMOS NO ENFERMOS----
grafico1 = ggplot(data = Datos,
       mapping = aes(x = p9_Leuco,
                     fill = factor(DX),
                     color = factor(DX))) +
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
  labs(title = '',fill = 'DX',color = 'DX',x = 'Leucocitos',y = 'Densidad') +
  theme_minimal()
grafico2 = ggplot(data = Datos,
       mapping = aes(x = p10_Plaquetas,
                     fill = factor(DX),
                     color = factor(DX))) +
  geom_histogram(aes(y = ..density..),
                 bins = 9,
                 position = 'identity',
                 alpha = 0.8,
                 color = 'black') +  
  geom_density(alpha = 0.2) +
  geom_freqpoly(stat = 'density',
                bins = 9,
                size = 0.5) +
  scale_fill_manual(values = c("0" = "#3F719E", "1" = "#B980A7")) +
  scale_color_manual(values = c("0" = "#2A5783", "1" = "#B81840")) +
  labs(title = '',fill = 'DX',color = 'DX',x = 'Plaquetas',y = 'Densidad') +
  theme_minimal()
grafico3 = ggplot(data = Datos,
       mapping = aes(x = nmono,
                     fill = factor(DX),
                     color = factor(DX))) +
  geom_histogram(aes(y = ..density..),
                 bins = 9,
                 position = 'identity',
                 alpha = 0.8,
                 color = 'black') +  
  geom_density(alpha = 0.2) +
  geom_freqpoly(stat = 'density',
                bins = 9,
                size = 0.5) +
  scale_fill_manual(values = c("0" = "#3F719E", "1" = "#B980A7")) +
  scale_color_manual(values = c("0" = "#2A5783", "1" = "#B81840")) +
  labs(title = '',fill = 'DX',color = 'DX',x = 'Monocitos',y = 'Densidad') +
  theme_minimal()
grafico4 = ggplot(data = Datos,
       mapping = aes(x = hematocrito_hemoglobina,
                     fill = factor(DX),
                     color = factor(DX))) +
  geom_histogram(aes(y = ..density..),
                 bins = 9,
                 position = 'identity',
                 alpha = 0.8,
                 color = 'black') + 
  geom_density(alpha = 0.2) +
  geom_freqpoly(stat = 'density',
                bins = 9,
                size = 0.5) +
  scale_fill_manual(values = c("0" = "#3F719E", "1" = "#B980A7")) +
  scale_color_manual(values = c("0" = "#2A5783", "1" = "#B81840")) +
  labs(title = '', fill = 'DX',color = 'DX',x = 'Hematocrito / Hemoglobina',y = 'Densidad') +
  theme_minimal()
grafico5 = ggplot(data = Datos,
       mapping = aes(x = linfocitos_neutrofilos,
                     fill = factor(DX),
                     color = factor(DX))) +
  geom_histogram(aes(y = ..density..),
                 bins = 9,
                 position = 'identity',
                 alpha = 0.8,
                 color = 'black') +  
  geom_density(alpha = 0.2) +
  geom_freqpoly(stat = 'density',
                bins = 9,
                size = 0.5) +
  scale_fill_manual(values = c("0" = "#3F719E", "1" = "#B980A7")) +
  scale_color_manual(values = c("0" = "#2A5783", "1" = "#B81840")) +
  labs(title = '',fill = 'DX',color = 'DX',x = 'Linfocitos / Neutrofilos',y = 'Densidad') +
  theme_minimal()
graficos_combinados = grafico1 + grafico2 + grafico3 + grafico4 + grafico5 +
  plot_layout(guides = 'collect')
print(graficos_combinados)


##BOXPLOT---- 
#leo base de datos original para sacar outlers mas tarde
Datosoriginal <- read_excel("BD_ALGORITMOS_DENGUE_ACT.xlsx",sheet = "Hoja2")
Datosoriginal$DX<-factor(Datosoriginal$DX)
boxplot1 <- ggplot(Datosoriginal, aes(x = DX, y = p9_Leuco, fill = DX)) + 
  geom_boxplot() + labs( y='Leucocitos') +
  scale_fill_manual(values = c("0" = "#3F719E", "1" = "#B980A7")) +
  theme(legend.position = "none")
boxplot2 <- ggplot(Datosoriginal, aes(x = DX, y = p10_Plaquetas, fill = DX)) + 
  geom_boxplot() +  labs( y='Plaquetas') +
  scale_fill_manual(values = c("0" = "#3F719E", "1" = "#B980A7")) +
  theme(legend.position = "none")
boxplot3 <- ggplot(Datosoriginal, aes(x = DX, y = nmono, fill = DX)) + 
  geom_boxplot() +  labs( y='Monocitos') +
  scale_fill_manual(values = c("0" = "#3F719E", "1" = "#B980A7")) +
  theme(legend.position = "none")
boxplot4 <- ggplot(Datosoriginal, aes(x = DX, y = hematocrito_hemoglobina, fill = DX)) + 
  geom_boxplot() +  labs( y='Hematocrito / Hemoglobina') +
  scale_fill_manual(values = c("0" = "#3F719E", "1" = "#B980A7")) +
  theme(legend.position = "none")
boxplot5 <- ggplot(Datosoriginal, aes(x = DX, y = linfocitos_neutrofilos, fill = DX)) + 
  geom_boxplot() +  labs( y='Linfocitos / Neutrofilos') +
  scale_fill_manual(values = c("0" = "#3F719E", "1" = "#B980A7")) +
  theme(legend.position = "none")
boxplot_combinados = boxplot1 + boxplot2 + boxplot3 + boxplot4 + boxplot5 +
  plot_layout(guides = 'collect')
print(boxplot_combinados)


## VALORES ATIPICOS ----
### Leucocitos
statsLeu <- boxplot.stats(Datosoriginal$p9_Leuco)
which(Datosoriginal$p9_Leuco %in% statsLeu$out)
outLeu <- Datosoriginal[which(Datosoriginal$p9_Leuco %in% statsLeu$out), ]
### Plaquetas
statspla <- boxplot.stats(Datosoriginal$p10_Plaquetas)
which(Datosoriginal$p10_Plaquetas %in% statspla$out)
outpla <- Datosoriginal[which(Datosoriginal$p10_Plaquetas %in% statspla$out), ]
### Monocitos
statsmono <- boxplot.stats(Datosoriginal$nmono)
which(Datosoriginal$nmono %in% statsmono$out)
outmono <- Datosoriginal[which(Datosoriginal$nmono %in% statsmono$out), ]
### Hematocrito / Hemoglobina
statshemahemo<- boxplot.stats(Datosoriginal$hematocrito_hemoglobina)
which(Datosoriginal$hematocrito_hemoglobina %in% statshemahemo$out)
outhemahemo <- Datosoriginal[which(Datosoriginal$hematocrito_hemoglobina %in% statshemahemo$out), ]
### Linfocitos / Neutrofilos
statslineu <- boxplot.stats(Datosoriginal$linfocitos_neutrofilos)
which(Datosoriginal$linfocitos_neutrofilos %in% statslineu$out)
outlineu <- Datosoriginal[which(Datosoriginal$linfocitos_neutrofilos %in% statslineu$out), ]

##CORRELACIONES DE PEARSON----
library(dplyr)
colnames(Datos)[1:5] = c(
  "p9_Leuco" = "Leucocitos",
  "p10_Plaquetas" = "Plaquetas",
  "nmono" = "Monocitos",
  "hematocrito_hemoglobina" = "Hematocrito / Hemoglobina",
  "linfocitos_neutrofilos" = "Linfocitos / Neutrofilos"
) #Renombro las columnas 

correlaciones <- ggpairs(
  Datos[, 1:5],                           # Columnas a incluir
  mapping = aes(color = Datos$DX),        # Mapeo de colores por categoría DX
  lower = list(continuous = "points"),    # Mostrar puntos en la parte inferior
  diag = list(continuous = "barDiag", fill = Datos$DX),  # Histograma en la diagonal con colores según DX
  axisLabels = "show",                    # Mostrar etiquetas de los ejes
  columns = c("Leucocitos", "Plaquetas", "Monocitos", "Hematocrito / Hemoglobina", "Linfocitos / Neutrofilos"),  # Nombres personalizados de las variables
  upper = list(continuous = wrap("cor", method = "spearman")), # Líneas de correlación
  title = ""             # Título del gráfico
) +
  scale_color_manual(values = c("#3F719E", "#B980A7")) +
  scale_fill_manual(values = c("#3F719E", "#B980A7"))  # Establecer colores manualmente

print(correlaciones)



## CORRELACION DE SPEARMAN Y KENDALL ----

colnames(Datos)[1:5] = c(
  "p9_Leuco" = "Leucocitos",
  "p10_Plaquetas" = "Plaquetas",
  "nmono" = "Monocitos",
  "hematocrito_hemoglobina" = "Hema/Hemo",
  "linfocitos_neutrofilos" = "Linfo/Neutro"
)
library(dplyr)
#Spearman
correlation_matrix <- cor(Datos[, 1:5], method = "spearman")
cor_df <- as.data.frame(as.table(correlation_matrix))
names(cor_df) <- c("Variable1", "Variable2", "Correlation")
cor_df$Correlation <- as.numeric(cor_df$Correlation)
# Crear el gráfico de correlación
ggplot(cor_df, aes(x = Variable1, y = Variable2, fill = Correlation, label = ifelse(Variable1 == Variable2, as.character(Variable1), round(Correlation, 2)))) +
  geom_tile(color = "white") +
  geom_text(color = "black") +
  scale_fill_gradient2(low = "black", mid = "white", high = "#3F719E", midpoint = 0, limits = c(-1, 1)) +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid = element_blank()) +
  labs(title = "Matriz de Correlación de Spearman",
       fill = "Correlación")


#Kendall
matriz_corkendall <- cor(Datos[, 1:5], method = 'kendall')
cor_df2 <- as.data.frame(as.table(matriz_corkendall))
names(cor_df2) <- c("Variable1", "Variable2", "Correlation")
cor_df2$Correlation <- as.numeric(cor_df2$Correlation)
# Crear el gráfico de correlación
ggplot(cor_df2, aes(x = Variable1, y = Variable2, fill = Correlation, label = ifelse(Variable1 == Variable2, as.character(Variable1), round(Correlation, 2)))) +
  geom_tile(color = "white") +
  geom_text(color = "black") +
  scale_fill_gradient2(low = "black", mid = "white", high = "#B980A7", midpoint = 0, limits = c(-1, 1)) +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid = element_blank()) +
  labs(title = "Matriz de Correlación de Kendall",
       fill = "Correlación")





