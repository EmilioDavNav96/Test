#Script Tarea 2
#Curso de R
#Equipo Dinamita: Brenda Hernandez, Emmanuel Garcia y Diego Davila

#Revisar directorio
getwd()
setwd("C:/Users/diem_/Documents/R/MisProyectos/Curso_R_Voyer-Rosell/CursoDeR-VoyerRosell/Datos")
remove(list=ls())
remove(list=ls())

#Cargar datos

datos <- read.csv("Vigilancia-ardillas.csv", header=T)

#Exploracion de datos
head(datos)
tail(datos)
names(datos)
class(datos)
str(datos)

#Factores
datos$Site <- as.factor(datos$Site)
datos$Area <- as.factor(datos$Area)
datos$Predator <- as.factor(datos$Predator)

str(datos)

hist(datos$AlertD) #Distancia de alerta
hist(datos$FID) #Distancia a la que se alejan/huyen

#Planteamos modelo de FID en función del tamaño corporal
mod.masa.fid <- lm(FID ~ BodyM, data = datos)

#Comprobamos supuestos del modelo
#supuesto de normalidad
hist(mod.masa.fid$residuals)
shapiro.test(mod.masa.fid$residuals)
#supuesto de homocedasticidad
par(mfrow=c(1,2))
plot(mod.masa.fid,1)
plot(mod.masa.fid,2)

#Summary modelo
summary(mod.masa.fid)
sum.mod.FidBodyM <- summary(mod.masa.fid)

#plot del modelo lineal FID~BodyM
par(mfrow=c(1,1))
plot(FID ~ BodyM, data = datos, xlab="Body mass")
predicted.fid.body <- predict(mod.masa.fid, datos)
abline(lm(predicted.fid.body ~ datos$BodyM), col="red")
mtext(paste0("r2 adj=", round(sum.mod.FidBodyM$adj.r.squared, 2)), side = 3, line=1,
      adj=0.07, cex=0.9, font=4)
mtext(paste0("p=", formatC(sum.mod.FidBodyM$coefficients[8], format="E", digits=2)), side=3, line=2,
      adj=0.07, cex=0.9, font=4)

#Planteamos modelo de Distancia de alerta en función del tamaño corporal
mod.masa.AlertD <- lm(AlertD ~ BodyM, data = datos)

#Comprobamos supuestos del modelo
#supuesto de normalidad
hist(mod.masa.AlertD$residuals)
shapiro.test(mod.masa.AlertD$residuals)
#supuesto de homocedasticidad
par(mfrow=c(1,2))
plot(mod.masa.AlertD,1)
plot(mod.masa.AlertD,2)

#Summary modelo
summary(mod.masa.AlertD)
sum.mod.AlertDodyM <- summary(mod.masa.AlertD)

#plot del modelo lineal FID~BodyM
par(mfrow=c(1,1))
plot(AlertD ~ BodyM, data = datos, xlab="Body mass")
predicted.AlertD.body <- predict(mod.masa.AlertD, datos)
abline(lm(predicted.AlertD.body ~ datos$BodyM), col="blue")
mtext(paste0("r2 adj=", round(sum.mod.AlertDodyM$adj.r.squared, 2)), side = 3, line=1,
      adj=0.07, cex=0.9, font=4)
mtext(paste0("p=", formatC(sum.mod.AlertDodyM$coefficients[8], format="E", digits=2)), side=3, line=2,
      adj=0.07, cex=0.9, font=4)

names(datos)
boxplot(FID~Area, data=datos)

#Pregunta 3
#Diferencias en la respuesta entre animales urbanos y rurales
mod.area.glm <- glm(sqrt(FID) ~ Area, data = datos)
#supuesto de normalidad
hist(mod.area.glm$residuals)
shapiro.test(mod.area.glm$residuals)

#supuesto de homocedasticidad
plot(mod.area.glm,1)
bartlett.test(mod.area.glm$residuals, mod.area.glm$fitted.values)

#Summary del modelo
summary(mod.area.glm)
boxplot(FID ~ Area, data = datos, notch=T)

#ANOVA para la pregunta 3
#El area (rural o urbana) influye en el FID?
anova.area <- aov(sqrt(FID) ~ Area, data = datos)
summary(anova.area)

#Verificacion de supuestos prueba AOV
hist(anova.area$residuals)
shapiro.test(anova.area$residuals)

#verificacion de homocedasticidad
plot(anova.area,1)

#Kruskal Wallis test
kruskal.area <- kruskal.test(FID ~ Area, data = datos)

#El area (rural o urbana) influye en la distancia de alerta ALERTD?
anova.area.alerta <- aov(sqrt(AlertD) ~ Area, data = datos)
summary(anova.area.alerta)

#supuesto de normalidad
hist(anova.area.alerta$residuals)
shapiro.test(anova.area.alerta$residuals)

#supuesto de heterocedasticidad 
bartlett.test(datos$AlertD~datos$Area)

#boxplot
boxplot(AlertD ~ Area, data=datos, notch=T)

#Pregunta 4
#Los animales de zonas rurales y urbanas responden de forma diferente al tipo de depredador
#modificando su FID?
anova.depredador.fid <- aov(sqrt(FID) ~ Area*Predator, data=datos)
summary(anova.depredador.fid)

boxplot(FID~Area*Predator, data=datos)

#supuesto de normalidad
hist(anova.depredador.fid$residuals)
shapiro.test(anova.depredador.fid$residuals)

#Pregunta 5
#Hacer modelo que mejor explique la variación en distancia de alerta AlertD para todos
#los individuos
#Modelo saturado con interacciones 
names(datos)
mod.satur.inter <- glm(AlertD ~ Area*Predator*BodyM, data=datos)
summary(mod.satur.inter)


#normalidad
hist(mod.satur.inter$residuals)
shapiro.test(mod.satur.inter$residuals)

#homocedasticidad
plot(mod.satur.inter, 1)

#Modelo saturado sin interacciones 
names(datos)
mod.satur <- glm(AlertD ~ Area+Predator+BodyM, data=datos)
summary(mod.satur)

#normalidad
hist(mod.satur$residuals)
shapiro.test(mod.satur$residuals)

#homocedasticidad
plot(mod.satur, 1)

#ModeloA - Area y peso corporal sin interaccion
mod.no.pred <- glm(AlertD ~ Area+BodyM, data=datos)
summary(mod.no.pred)

#normalidad
hist(mod.no.pred$residuals)
shapiro.test(mod.no.pred$residuals)

#homocedasticidad
plot(mod.no.pred, 1)

#Modelo B - Area y masa corporal con interaccion
mod.no.pred.inter <- glm(AlertD ~ Area*BodyM, data=datos)
summary(mod.no.pred.inter)

#normalidad
hist(mod.no.pred.inter$residuals)
shapiro.test(mod.no.pred.inter$residuals)

#homocedasticidad 
plot(mod.no.pred.inter, 1)

