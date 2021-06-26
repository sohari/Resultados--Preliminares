niña = subset(Base_Ansiedad, Sexo=="Niña")\\ Selecciona el conjunto de niñas en la base de datos. 
niño = subset(Base_Ansiedad, Sexo=="Niño")
with(niño, mean(Edad))\\ calcula la media de las edades
with(niña,mean(Edad))
tip1= subset(Base_Ansiedad, Tipo=="TIP1")\\ calcula la muestra de niños en cada tipo de ansiedad. 
tip2= subset(Base_Ansiedad, Tipo=="TIP2")
tip3= subset(Base_Ansiedad, Tipo=="TIP3")
tip4= subset(Base_Ansiedad, Tipo=="TIP4")
library(brms)
Puntuacion ~ Sexo \\ el análisis del modelo se realiza la puntuación adquirida según el Sexo. 
family = gaussian
zoib_model<- bf(Puntuacion ~ Sexo, phi ~ Sexo,zoi ~ Sexo,coi ~ Sexo, family = zero_one_inflated_beta()) \\ regresión zoib beta inflado
fit<- brm(formula = zoib_model, data.frame(Base_Ansiedad,cores= 2, file = "brm-zoib"))
summary(fit)\\ calcula la estimaciones de los interceptó y coeficientes, así como los intervalos de confianza. 
plot(
  conditional_effects(fit, dpar = "mu"), 
  points = TRUE, 
  point_args = list(width = .05, shape = 1) 
)\\ genera el gráfico del análisis. 
