nh= table(Base_Ansiedad_1$Edad) #Estratos de la variable edad.
ybar_h=tapply(Base_Ansiedad_1$Puntuacion, Base_Ansiedad_1$Edad, mean) #Media de la puntuación segun la edad por estratos.
names(Base_Ansiedad_1) #Nombres de las variables de la base 
aov(Edad~Sexo) 

anova1=aov(Edad~Sexo) #Anova de la edad segun el sexo
summary(anova1)
TukeyHSD(anova1) #Muestra las diferencias entre las media de grupos
boxplot(Edad~Sexo) #Grafico de caja 



#Autocritica

names(Base_Ansiedad_1)
aov(Puntuacion~Edad)
anova=aov(Puntuacion~Edad)
summary(anova)
boxplot(Puntuacion~Edad)
