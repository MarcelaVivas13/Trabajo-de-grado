#Distribucion normal

library(ggplot2)

set.seed(123) # Para reproducibilidad
mu_values <- c(0, 3, -3, 0) # Medias
sigma_values <- c(1, 0.7, 0.5, 4) # Desviaciones estándar
x <- seq(-6, 6, length.out = 1000) # Aumento de la resolución del eje x

# Crear una función para la densidad de probabilidad de la distribución normal
pdf_normal <- function(x, mu, sigma) {
  dnorm(x, mean = mu, sd = sigma)
}

# Graficar la función de densidad de probabilidad (PDF) de la distribución normal
ggplot() +
  geom_line(aes(x = x, y = pdf_normal(x, mu = mu_values[1], sigma = sigma_values[1]), color = "mu = 0, sd = 1"), size = 0.5) +
  geom_line(aes(x = x, y = pdf_normal(x, mu = mu_values[2], sigma = sigma_values[2]), color = "mu = 3, sd = 0.7"), size = 0.5) +
  geom_line(aes(x = x, y = pdf_normal(x, mu = mu_values[3], sigma = sigma_values[3]), color = "mu = -3, sd = 0.5"), size = 0.5) +
  geom_line(aes(x = x, y = pdf_normal(x, mu = mu_values[4], sigma = sigma_values[4]), color = "mu = 0, sd = 4"), size = 0.5) +
  labs(title = "Distribución Normal",
       x = "Valor",
       y = "Densidad de Probabilidad",
       color = "Parámetros") +
  scale_color_manual(values = c("mu = 0, sd = 1" = "red",
                                "mu = 3, sd = 0.7" = "blue",
                                "mu = -3, sd = 0.5" = "green",
                                "mu = 0, sd = 4" = "orange"),
                     name = "Parámetros") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        axis.line = element_line(color = "black"),
        legend.text = element_text(size = 12)) # Ajusta el tamaño del texto de la leyenda


#Distribución Logistica
library(ggplot2)

set.seed(123) # Para reproducibilidad
location_values <- c(0, 3, -3, 0) # Medias
scale_values <- c(1, 0.7, 0.5, 4) # Desviaciones estándar
x <- seq(-6, 6, length.out = 1000) # Aumento de la resolución del eje x

# Crear una función para la densidad de probabilidad de la distribución normal
pdf_logis <- function(x, location, scale) {
  dlogis(x, location = location, scale = scale)
}

# Graficar la función de densidad de probabilidad (PDF) de la distribución normal
ggplot() +
  geom_line(aes(x = x, y = pdf_logis(x, location = location_values[1], scale = scale_values[1]), color = "Location = 0, Scale = 1"), size = 0.5) +
  geom_line(aes(x = x, y = pdf_logis(x, location = location_values[2], scale = scale_values[2]), color = "Location = 3, Scale = 0.7"), size = 0.5) +
  geom_line(aes(x = x, y = pdf_logis(x, location = location_values[3], scale = scale_values[3]), color = "Location = -3, Scale = 0.5"), size = 0.5) +
  geom_line(aes(x = x, y = pdf_logis(x, location = location_values[4], scale = scale_values[4]), color = "Location = 0, Scale = 4"), size = 0.5) +
  labs(title = "Distribución Logística",
       x = "Valor",
       y = "Densidad de Probabilidad",
       color = "Parámetros") +
  scale_color_manual(values = c("Location = 0, Scale = 1" = "red",
                                "Location = 3, Scale = 0.7" = "blue",
                                "Location = -3, Scale = 0.5" = "green",
                                "Location = 0, Scale = 4" = "orange"),
                     name = "Parámetros") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        axis.line = element_line(color = "black"),
        legend.text = element_text(size = 12)) # Ajusta el tamaño del texto de la leyenda





#Distribución Gamma
library(ggplot2)

set.seed(123) # Para reproducibilidad
shape_values <- c(1, 2.5, 3, 5) # Medias
scale_values <- c(1, 0.7, 2, 1) # Desviaciones estándar
x <- seq(0, 15, length.out = 1000) # Aumento de la resolución del eje x

# Crear una función para la densidad de probabilidad de la distribución normal
pdf_gamma <- function(x, shape, scale) {
  dgamma(x, shape = shape, scale = scale)
}

# Graficar la función de densidad de probabilidad (PDF) de la distribución normal
ggplot() +
  geom_line(aes(x = x, y = pdf_gamma(x, shape = shape_values[1], scale = scale_values[1]), color = "Shape = 1, Scale = 1"), size = 0.5) +
  geom_line(aes(x = x, y = pdf_gamma(x, shape = shape_values[2], scale = scale_values[2]), color = "Shape = 2.5, Scale = 0.7"), size = 0.5) +
  geom_line(aes(x = x, y = pdf_gamma(x, shape = shape_values[3], scale = scale_values[3]), color = "Shape = 3, Scale = 2"), size = 0.5) +
  geom_line(aes(x = x, y = pdf_gamma(x, shape = shape_values[4], scale = scale_values[4]), color = "Shape = 5, Scale = 1"), size = 0.5) +
  labs(title = "Distribución Gamma",
       x = "Valor",
       y = "Densidad de Probabilidad",
       color = "Parámetros") +
  scale_color_manual(values = c("Shape = 1, Scale = 1" = "red",
                                "Shape = 2.5, Scale = 0.7" = "blue",
                                "Shape = 3, Scale = 2" = "green",
                                "Shape = 5, Scale = 1" = "orange"),
                     name = "Parámetros") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        axis.line = element_line(color = "black"),
        legend.text = element_text(size = 12)) # Ajusta el tamaño del texto de la leyenda







#Distribución Weibull
library(ggplot2)

set.seed(123) # Para reproducibilidad
shape_values <- c(2,1, 1.5, 2.5) # Medias
scale_values <- c(1.5,1,5, 1) # Desviaciones estándar
x <- seq(0, 15, length.out = 1000) # Aumento de la resolución del eje x

# Crear una función para la densidad de probabilidad de la distribución normal
pdf_weibull <- function(x, shape, scale) {
  dweibull(x, shape = shape, scale = scale)
}

# Graficar la función de densidad de probabilidad (PDF) de la distribución normal
ggplot() +
  geom_line(aes(x = x, y = pdf_weibull(x, shape = shape_values[1], scale = scale_values[1]), color = "Shape = 2, Scale = 1.5"), size = 0.5) +
  geom_line(aes(x = x, y = pdf_weibull(x, shape = shape_values[2], scale = scale_values[2]), color = "Shape = 1, Scale = 1"), size = 0.5) +
  geom_line(aes(x = x, y = pdf_weibull(x, shape = shape_values[3], scale = scale_values[3]), color = "Shape = 1.5, Scale = 5"), size = 0.5) +
  geom_line(aes(x = x, y = pdf_weibull(x, shape = shape_values[4], scale = scale_values[4]), color = "Shape = 2.5, Scale = 1"), size = 0.5) +
  labs(title = "Distribución Weibull",
       x = "Valor",
       y = "Densidad de Probabilidad",
       color = "Parámetros") +
  scale_color_manual(values = c("Shape = 2, Scale = 1.5" = "red",
                                "Shape = 1, Scale = 1" = "blue",
                                "Shape = 1.5, Scale = 5" = "green",
                                "Shape = 2.5, Scale = 1" = "orange"),
                     name = "Parámetros") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        axis.line = element_line(color = "black"),
        legend.text = element_text(size = 12)) # Ajusta el tamaño del texto de la leyenda







#Distribución Lognormal
set.seed(123) # Para reproducibilidad
mu_values <- c(0, 1, -1, 0) # Medias
sigma_values <- c(0.5, 1, 1.5, 1) # Desviaciones estándar
x <- seq(0, 6, length.out = 1000) # Aumento de la resolución del eje x

# Crear una función para la densidad de probabilidad de la distribución normal
pdf_lnorm <- function(x, mu, sigma) {
  dlnorm(x, mean = mu, sd = sigma)
}

# Graficar la función de densidad de probabilidad (PDF) de la distribución lognormal
ggplot() +
  geom_line(aes(x = x, y = pdf_lnorm(x, mu = mu_values[1], sigma = sigma_values[1]), color = "mu = 0, sd = 0.5"), size = 0.5) +
  geom_line(aes(x = x, y = pdf_lnorm(x, mu = mu_values[2], sigma = sigma_values[2]), color = "mu = 1, sd = 1"), size = 0.5) +
  geom_line(aes(x = x, y = pdf_lnorm(x, mu = mu_values[3], sigma = sigma_values[3]), color = "mu = -1, sd = 1.5"), size = 0.5) +
  geom_line(aes(x = x, y = pdf_lnorm(x, mu = mu_values[4], sigma = sigma_values[4]), color = "mu = 0, sd = 1"), size = 0.5) +
  labs(title = "Distribución Log-Normal",
       x = "Valor",
       y = "Densidad de Probabilidad",
       color = "Parámetros") +
  scale_color_manual(values = c("mu = 0, sd = 0.5" = "red",
                                "mu = 1, sd = 1" = "blue",
                                "mu = -1, sd = 1.5" = "green",
                                "mu = 0, sd = 1" = "orange"),
                     name = "Parámetros") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        axis.line = element_line(color = "black"),
        legend.text = element_text(size = 12)) # Ajusta el tamaño del texto de la leyenda







#Distribución Lognormal de 3p
set.seed(123) # Para reproducibilidad
mu_values <- c(0, 1, -1, 0) # Medias
sigma_values <- c(0.5, 1, 1.5, 1) # Desviaciones estándar
gamma_values <- c(1, 0.3, 1.2, 2)
x <- seq(0, 6, length.out = 1000) # Aumento de la resolución del eje x

# Crear una función para la densidad de probabilidad de la distribución normal
pdf_lnorm3 <- function(x, mu, sigma,gamma) {
  dlnorm3(x, meanlog = mu, sdlog = sigma, gamma = gamma)
}
# Graficar la función de densidad de probabilidad (PDF) de la distribución lognormal
ggplot() +
  geom_line(aes(x = x, y = pdf_lnorm3(x, mu = mu_values[1], sigma = sigma_values[1], gamma = gamma_values[1]), color = "mu = 0, sd = 0.5, gamma = 1"), size = 0.5) +
  geom_line(aes(x = x, y = pdf_lnorm3(x, mu = mu_values[2], sigma = sigma_values[2], gamma = gamma_values[2]), color = "mu = 1, sd = 1, gamma = 0.3"), size = 0.5) +
  geom_line(aes(x = x, y = pdf_lnorm3(x, mu = mu_values[3], sigma = sigma_values[3], gamma = gamma_values[3]), color = "mu = -1, sd = 1.5, gamma = 1.2"), size = 0.5) +
  geom_line(aes(x = x, y = pdf_lnorm3(x, mu = mu_values[4], sigma = sigma_values[4], gamma = gamma_values[4]), color = "mu = 0, sd = 1, gamma = 2"), size = 0.5) +
  labs(title = "Distribución Log-Normal de 3p",
       x = "Valor",
       y = "Densidad de Probabilidad",
       color = "Parámetros") +
  scale_color_manual(values = c("mu = 0, sd = 0.5, gamma = 1" = "red",
                                "mu = 1, sd = 1, gamma = 0.3" = "blue",
                                "mu = -1, sd = 1.5, gamma = 1.2" = "green",
                                "mu = 0, sd = 1, gamma = 2" = "orange"),
                     name = "Parámetros") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        axis.line = element_line(color = "black"),
        legend.text = element_text(size = 12)) # Ajusta el tamaño del texto de la leyenda



#------------------------------------------------------------------------------#
#----------------------------Copulas-------------------------------------------#
#------------------------------------------------------------------------------#
library(copula)
claython1 = claytonCopula(dim = 2,param = 1)
a <- rCopula(1000, claython1)
plot(a,xlab = expression(theta == 1),ylab = "", col = "#27408B",cex.lab = 1.5)
claython2 = claytonCopula(dim = 2,param = 5)
b <- rCopula(1000, claython2)
plot(b,xlab = expression(theta == 5),ylab = "", col = "#3A5FCD", main = "Cópula Claython",cex.lab = 1.5)
claython3 = claytonCopula(dim = 2,param = 10)
c <- rCopula(1000, claython3)
plot(c,xlab = expression(theta == 10),ylab = "", col = "#4876FF",cex.lab = 1.5)



frank1 = frankCopula(dim = 2,param = 1)
d <- rCopula(1000, frank1)
plot(d,xlab = expression(theta == 1),ylab = "", col = "#8B0000",cex.lab = 1.5)
frank2 = frankCopula(dim = 2,param = 5)
e <- rCopula(1000, frank2)
plot(e,xlab = expression(theta == 5),ylab = "", col = "#CD0000",main = "Cópula Frank",cex.lab = 1.5)
frank3 = frankCopula(dim = 2,param = 10)
f <- rCopula(1000, frank3)
plot(f,xlab = expression(theta == 10),ylab = "", col = "#FF0000",cex.lab = 1.5)



gumbel1 = gumbelCopula(dim = 2,param = 1)
g <- rCopula(1000, gumbel1)
plot(g,xlab = expression(theta == 1),ylab = "", col = "#7A378B",cex.lab = 1.5)
gumbel2 = gumbelCopula(dim = 2,param = 5)
h <- rCopula(1000, gumbel2)
plot(h,xlab = expression(theta == 5),ylab = "", col = "mediumorchid3",main = "Cópula Gumbel",cex.lab = 1.5)
gumbel3 = gumbelCopula(dim = 2,param = 10)
i <- rCopula(1000, gumbel3)
plot(i,xlab = expression(theta == 10),ylab = "", col = "#E066FF",cex.lab = 1.5)



t1 = tCopula(param = 0.1, dim = 2, df = 2)
j <- rCopula(1000, t1)
plot(j,xlab = bquote( df == 2 ~ "y" ~ rho == 0.1),ylab = "", col = "#030303",cex.lab = 1.5)
t2 = tCopula(param = 0.5, dim = 2, df = 5)
k <- rCopula(1000, t2)
plot(k,xlab = bquote( df == 5 ~ "y" ~ rho == 0.5),ylab = "", col = "#303030",main = "Cópula t",cex.lab = 1.5)
t3 = tCopula(param = 0.9, dim = 2, df = 10)
l <- rCopula(1000, t3)
plot(l,xlab = bquote( df == 10 ~ "y" ~ rho == 0.9),ylab = "", col = "#666666",cex.lab = 1.5)









