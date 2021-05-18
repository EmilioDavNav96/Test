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

names(datos)
summary(datos) 

#ANOVA para la pregunta 3
#El area (rural o urbana) influye en el FID?
anova.area <- aov(sqrt(FID) ~ Area, data = datos)
summary(anova.area)

#Verificacion de supuestos prueba AOV
hist(anova.area$residuals)
shapiro.test(anova.area$residuals)

#verificacion de homocedasticidad
par(mfrow=c(1,2))
plot(anova.area,1)
plot(anova.area,2)

#Kruskal Wallis test
kruskal.area <- kruskal.test(FID ~ Area, data = datos)

#El area (rural o urbana) influye en la distancia de alerta ALERTD?
anova.area.alerta <- aov(sqrt(AlertD) ~ Area, data = datos)
summary(anova.area.alerta)
summary(anova.area)
#supuesto de normalidad
par(mfrow=c(1,1))
hist(anova.area.alerta$residuals)
shapiro.test(anova.area.alerta$residuals)

#verificacion de homocedasticidad
par(mfrow=c(1,2))
plot(anova.area.alerta,1)
plot(anova.area.alerta,2)

#supuesto de heterocedasticidad 
bartlett.test(datos$AlertD~datos$Area)

#boxplot
par(mfrow=c(1,1))
boxplot(AlertD ~ Area, data=datos, notch=T)

#Pregunta 4
#Los animales de zonas rurales y urbanas responden de forma diferente al tipo de depredador
#modificando su FID?
anova.depredador.fid <- aov(FID ~ Area*Predator, data=datos)
summary(anova.depredador.fid)

par(mfrow=c(1,1))
boxplot(FID~Area*Predator, data=datos)
interaction.plot(x.factor = datos$Predator,
                 trace.factor = datos$Area, # variable to plot on x-axis
                 response = datos$FID,    # variable to plot on y-axis
                 fun = median,  # summary statistic to be plotted for response variable
                 type = "l",     # type of plot, here "l" for lines
                 ylab = "Area:Predator",
                 xlab = "FID",
                 col = c("blue4", "red4"),
                 lty = 1,  # line type
                 lwd = 2,  # line width
                 trace.label = "Transmission",  # label for legend
                 xpd = FALSE) #,  # 'clip' legend at border

#supuesto de normalidad
hist(anova.depredador.fid$residuals)
shapiro.test(anova.depredador.fid$residuals)

#Homocedasticidad
par(mfrow=c(1,2))
plot(anova.depredador.fid,1)
plot(anova.depredador.fid,2)

#Pregunta 5
#Hacer modelo que mejor explique la variación en distancia de alerta AlertD para todos
#los individuos

#Modelo A
mod.b <- glm(AlertD ~ BodyM*Area, data=datos)
summary(mod.b)

#supuestos
#normalidad
hist(mod.b$residuals)
shapiro.test(mod.b$residuals)

#homocedasticidad
par(mfrow=c(1,2))
plot(mod.b,1)
plot(mod.b,2)


#Modelo B
mod.c <- glm(AlertD ~ BodyM + Area, data=datos)
summary(mod.c)

#supuestos
#normalidad
hist(mod.c$residuals)
shapiro.test(mod.c$residuals)

#homocedasticidad
par(mfrow=c(1,2))
plot(mod.c,1)
plot(mod.c,2)

names(datos)

#Modelo C
mod.d <- glm(AlertD ~ BodyM + Area + Predator, data=datos)
summary(mod.d)

#supuestos
#normalidad
hist(mod.d$residuals)
shapiro.test(mod.d$residuals)

#homocedasticidad
par(mfrow=c(1,2))
plot(mod.d,1)
plot(mod.d,2)

#Modelo D
mod.e <- glm(AlertD ~ BodyM*Predator+Area, data=datos)
summary(mod.e)

#supuestos
#normalidad
hist(mod.e$residuals)
shapiro.test(mod.e$residuals)

#homocedasticidad
par(mfrow=c(1,2))
plot(mod.e,1)
plot(mod.e,2)

names(datos)

#Modelo Brenda
par(mfrow=c(1,1))
bren<-glm(BodyM~Area, data=datos)
summary(bren)
boxplot(BodyM~Area, data=datos, notch=T)
boxplot(AlertD~Area*BodyM, data=datos, notch=T)
interaction.plot(x.factor = datos$Area,
                 trace.factor = datos$BodyM, # variable to plot on x-axis
                 response = datos$AlertD,    # variable to plot on y-axis
                 fun = median,  # summary statistic to be plotted for response variable
                 type = "l",     # type of plot, here "l" for lines
                 ylab = "AlertD",
                 xlab = "BodyM",
                 col = c("blue4", "red4"),
                 lty = 1,  # line type
                 lwd = 2,  # line width
                 trace.label = "Transmission",  # label for legend
                 xpd = FALSE) #,  # 'clip' legend at border


hist(bren$residuals)
par(mfrow=c(1,2))
plot(bren,1)
plot(bren,2)
