library(tidyverse) #analizar datos
library(synthpop) #Generar version sintetica de la base original
library(cowplot) #Graficos
view(Base_Ansiedad_1)
vars<-c("Ciudad","Sexo","Edad","Seccion","Tipo","Puntuacion") #Filtrar las variables
Base_Ansiedad_1<-Base_Ansiedad_1[,vars]
Base_s<-syn(Base_Ansiedad_1,m=5,seed=1000) # Genera los datos sinteticos
compare(Base_s,Base_Ansiedad_1)

########################
graf <- compare(
  Base_s, Base_Ansiedad_1,
       breaks = 5,
      ncol = 7,
      nrow = 2,
      cols = c("#62B6CB", "#1B4965","Yellow") ) # Compara la base original con la sintetica
graf <- graf$plots
graf <- graf +
      scale_y_continuous(expand = c(0, 0)) + 
       theme_minimal_hgrid(12) # Apply theme

graf <- graf +
  theme(axis.text.x = element_text(angle = 60, hjust = 1),
        axis.title.x = element_blank()) +
  labs(fill = "Dataset")
graf #Grafico de comparacion
#####################

names(Base_Ansiedad_1)
Base_Ansiedad_1$Puntuacion

as.data.frame(table(Base_Ansiedad_1$Puntuacion))
tabla_punt<-as.data.frame(table(Base_Ansiedad_1$Puntuacion))
transform( tabla_punt,
           FreqAc= cumsum(Freq),
           Rel = round(prop.table(Freq),3),
           RelAc=round(cumsum(prop.table(Freq)),3))
####################3
names(Base_s[["syn"]][[2]])
Base_s[["syn"]][[2]]$Puntuacion

as.data.frame(table(Base_s[["syn"]][[2]]$Puntuacion))
tabla_punt<-as.data.frame(table(Base_s[["syn"]][[2]]$Puntuacion))
transform( tabla_punt,
           FreqAc= cumsum(Freq),
           Rel = round(prop.table(Freq),3),
           RelAc=round(cumsum(prop.table(Freq)),3))
library(nortest)
lillie.test(Base_s[["syn"]][[1]]$Puntuacion) # Prueba de normalidad de Kolmogorov
shapiro.test(Base_s[["syn"]][[1]]$Puntuacion) #Prueba de Shapiro
dbeta(Base_Ansiedad_1$Puntuacion, shape1 = 0.2, shape2 = 0.9)  #Resultados de la funcion de densidad
rbeta(991,shape1=0.2,shape2 = 0.9)  #Vector de valores de la distribucion beta
pbeta(Base_Ansiedad_1$Puntuacion,shape1 = 0.2,shape2 = 0.9) # Distribucion acumulada
qbeta(Base_Ansiedad_1$Puntuacion,shape1 = 0.2,shape2 = 0.9) # Cuantiles de la distribucion beta

curve(dbeta(x,shape1 = 0.4, shape2 = 0.6)) #Curvas de la puntuacion
curve(dbeta(x,shape1 = 0.1,shape2 = 0.9),col=2,lwd=4,add=T)
curve(pbeta(x,shape1 = 0.2,shape2 = 0.9))
curve(pbeta(x,shape1 = 0.1,shape2 = 0.8),col=4,lwd=4,add=T)
curve(qbeta(x,shape1 = 0.4, shape2 = 0.6))
