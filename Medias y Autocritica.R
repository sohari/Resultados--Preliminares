nh= table(Base_Ansiedad_1$Edad) #Estratos de la variable edad.
ybar_h=tapply(Base_Ansiedad_1$Puntuacion, Base_Ansiedad_1$Edad, mean) #Media de la puntuación segun la edad por estratos.
attach(Base_Ansiedad_1) # LLamado a la base de datos.
names(Base_Ansiedad_1) #Nombres de las variables de la base 
aov(Edad~Sexo) 

anova1=aov(Edad~Sexo) #Anova de la edad segun el sexo
summary(anova1)
TukeyHSD(anova1) #Muestra las diferencias entre las media de grupos
boxplot(Edad~Sexo) #Grafico de caja 



#Autocritica
attach(Base_Ansiedad_1)
names(Base_Ansiedad_1)
aov(Puntuacion~Edad)
anova=aov(Puntuacion~Edad)
summary(anova)
