library(univariateML) #Librerias utilizadas
library(fitdistrplus)
library(tidyverse)
library(gamlss)
plotdist(Base_Ansiedad_1$Edad, histo = TRUE, demp =  TRUE)
comparacion_aic <- AIC(
       mlbetapr(Base_Ansiedad_1$Edad),
       mlnorm(Base_Ansiedad_1$Edad))#Metrica AIC para comparar Distribuciones

comparacion_aic %>% rownames_to_column(var = "distribucion") %>% arrange(AIC)#Tabla de los valores

comparacion_bic <- BIC(
  mlbetapr(Base_Ansiedad_1$Edad),
  mlnorm(Base_Ansiedad_1$Edad)) #Metrica BIC para comparar Distribuciones

comparacion_bic %>% rownames_to_column(var = "distribucion") %>% arrange(BIC) #Tabla de los valores

hist(Base_Ansiedad_1$Edad,
           main = "Distribución Edad",
            freq = FALSE,
          ylim = c(0, 0.00025))
lines(mlbetapr(Base_Ansiedad_1$Edad), lwd = 2, lty = 1, col = "blue")
lines(mlnorm(Base_Ansiedad_1$Edad), lwd = 2, lty = 2, col = "red")
legend(x = 15000, y = 0.0001, legend = c("betapr", "norm"),
              col = c("blue", "red"), lty = 1:2)
rug(Base_Ansiedad_1$Edad)  #Histograma

ggplot(data = Base_Ansiedad_1) +
  geom_histogram(aes(x = Edad, y =  after_stat(density)),
                 bins = 40,
                 alpha = 0.3, color = "black") +
  geom_rug(aes(x = Edad)) +
  stat_function(fun = function(.x){dml(x = .x, obj = mlbetapr(Base_Ansiedad_1$Edad))},
                aes(color = "betapr"),
                size = 1) +
  stat_function(fun = function(.x){dml(x = .x, obj = mlnorm(Base_Ansiedad_1$Edad))},
                aes(color = "normal"),
                size = 1) +
  scale_color_manual(breaks = c("betapr", "normal"),
                     values = c("betapr" = "red", "normal" = "blue")) +
  labs(title = "Distribución Edad",
       color = "Distribución") +
  theme_bw() +
  theme(legend.position = "bottom") # Grafico de comparacion 


ggplot(data = Base_Ansiedad_1) +
      stat_ecdf(aes(x = Edad), geom = "step", color = "black", size = 1) +
      geom_rug(aes(x = Edad)) +
       stat_function(fun = function(.x){pml(q = .x, obj = mlbetapr(Base_Ansiedad_1$Edad))},
                     aes(color = "betapr"),
                             size = 1) +
     stat_function(fun = function(.x){pml(q = .x, obj = mlnorm(Base_Ansiedad_1$Edad))},
                                     aes(color = "normal"),
                                        size = 1) +
     scale_color_manual(breaks = c("betapr", "normal"),
                       values = c("betapr" = "red", "normal" = "blue")) +
       labs(title = "Distribución Edad",
                      color = "Distribución",
                    y = "CDF") +
      theme_bw() +
  theme(legend.position = "bottom") # Grafico de los puntos en escala



distribucion <- mlbetapr(x = Base_Ansiedad_1$Edad) #Ajuste de la distribucion beta 
summary(distribucion)
plot(distribucion)
distribucionor <- mlnorm(x = Base_Ansiedad_1$Edad) #Ajuste de la distribucion normal
summary(distribucionor)
plot(distribucionor)

bootstrapml(distribucion, probs = c(0.05, 0.95), reps = 1000) #Intervalos de confianza

descdist(data = Base_Ansiedad_1$Edad,graph = FALSE ) #Descripcion de una distribucion



library(tidyverse) 
x <- seq(0, 1, length.out = 500)  #Zoib zero ajustado
y <- dZABB(x = x, mu = 0.1, sigma = 0.5, nu = 0.5)
ggplot(data = data.frame(x, y), aes(x,y)) +
  geom_point() +
  labs(title = "Zero-adjusted beta distribution") + 
  theme_bw()
 

hist(Base_Ansiedad_1$Edad)
shapiro.test(Base_Ansiedad_1$Edad) #test de normalidad
library(nortest)
lillie.test(Base_Ansiedad_1$Edad)

qqnorm(Base_Ansiedad_1$Edad) #Grafico de normalidad
qqline(Base_Ansiedad_1$Edad)


