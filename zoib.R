niña = subset(Base_Ansiedad, Sexo=="Niña")
niño = subset(Base_Ansiedad, Sexo=="Niño")
with(niño, mean(Edad))
with(niña,mean(Edad))
tip1= subset(Base_Ansiedad, Tipo=="TIP1")
tip2= subset(Base_Ansiedad, Tipo=="TIP2")
tip3= subset(Base_Ansiedad, Tipo=="TIP3")
tip4= subset(Base_Ansiedad, Tipo=="TIP4")
library(brms)
Puntuacion ~ Sexo
family = gaussian
zoib_model<- bf(Puntuacion ~ Sexo, phi ~ Sexo,zoi ~ Sexo,coi ~ Sexo, family = zero_one_inflated_beta())
fit<- brm(formula = zoib_model, data.frame(Base_Ansiedad,cores= 2, file = "brm-zoib"))
summary(fit)
plot(
  conditional_effects(fit, dpar = "mu"), 
  points = TRUE, 
  point_args = list(width = .05, shape = 1) 
)