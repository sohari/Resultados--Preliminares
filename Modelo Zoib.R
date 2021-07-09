niña = subset(Base_Ansiedad_1, Sexo=="Niña")  #Niñas en la base 
niño = subset(Base_Ansiedad_1, Sexo=="Niño")  #Niños en la base
with(niño, mean(Edad)) #Media de las edades
with(niña,mean(Edad))
tip1= subset(Base_Ansiedad_1, Tipo=="TIP1") #Niños y Niñas con Ansiedad Alta
tip2= subset(Base_Ansiedad_1, Tipo=="TIP2")  #Niños y Niñas con Ansiedad tipo Escolar
tip3= subset(Base_Ansiedad_1, Tipo=="TIP3")   #Niños y Niñas con Ansiedad MOderada
tip4= subset(Base_Ansiedad_1, Tipo=="TIP4")  #Niños y Niñas con Ansiedad Baja
library(brms)
        
Puntuacion ~ Sexo #El analisis del modelo de la puntuación obtenida
family = gaussian
zoib_model <- bf(Puntuacion ~ Sexo, phi ~ Sexo,zoi ~ Sexo,coi ~ Sexo, family = zero_one_inflated_beta())
fit<- brm(formula = zoib_model, data.frame(Base_Ansiedad_1,cores= 2, file = "brm-zoib"))
summary(fit)  #calcula las estimaciones de los interceptos y coeficientes,asi como los intervalos de confianza
plot(
  conditional_effects(fit, dpar = "mu"), 
  points = TRUE, 
  point_args = list(width = .05, shape = 1) 
) #grafico del analisis



