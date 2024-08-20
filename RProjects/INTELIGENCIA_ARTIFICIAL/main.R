# Title     : TODO
# Objective : TODO
# Created by: isa
# Created on: 01/07/21

getwd()
dprueba<-read.csv("Ejercicio.csv", header=TRUE)
View(dprueba$DATOS)
shapiro.test(dprueba$DATOS)
hist(dprueba$DATOS)
boxplot(dprueba$DATOS)
summary(dprueba)
head(dprueba)