# Proyecto Final
# Autor: David Segura #13-11341
#        Manuel Faria #15-10463
#        Juan Oropeza #15-11041

datos = read.table("Proyecto2A.txt",header=T)

# 1. Realice un analisis descriptivo y exploratorio de los datos. Incluya en este
# analisis la matriz de correlacion.

peso = datos$peso

hist(peso, xlab="Peso", ylab="Frecuencia", main= "Histograma de Peso")
boxplot(peso,ylab="Peso", main="Boxplot de Peso")
summary(peso)
sd(peso)s

estatura = datos$estatura

hist(estatura, xlab="Estatura", ylab="Frecuencia", main= "Histograma de Estatura")
boxplot(estatura,ylab="Estatura", main="Boxplot de Estatura")
summary(estatura)
sd(estatura)

pie = datos$pie

hist(pie, xlab="pie", ylab="Frecuencia", main= "Histograma de pie")
boxplot(pie,ylab="pie", main="Boxplot de pie")
summary(pie)
sd(pie)

lbrazo = datos$lbrazo

hist(lbrazo, xlab="lbrazo", ylab="Frecuencia", main= "Histograma de lbrazo")
boxplot(lbrazo,ylab="lbrazo", main="Boxplot de lbrazo")
summary(lbrazo)
sd(lbrazo)

anchoes = datos$anchoes

hist(estatura, xlab="Estatura", ylab="Frecuencia", main= "Histograma de Estatura")
boxplot(estatura,ylab="Estatura", main="Boxplot de Estatura")
summary(estatura)
sd(estatura)

estatura = datos$estatura

hist(estatura, xlab="Estatura", ylab="Frecuencia", main= "Histograma de Estatura")
boxplot(estatura,ylab="Estatura", main="Boxplot de Estatura")
summary(estatura)
sd(estatura)

estatura = datos$estatura

hist(estatura, xlab="Estatura", ylab="Frecuencia", main= "Histograma de Estatura")
boxplot(estatura,ylab="Estatura", main="Boxplot de Estatura")
summary(estatura)
sd(estatura)

estatura = datos$estatura

hist(estatura, xlab="Estatura", ylab="Frecuencia", main= "Histograma de Estatura")
boxplot(estatura,ylab="Estatura", main="Boxplot de Estatura")
summary(estatura)
sd(estatura)

estatura = datos$estatura

hist(estatura, xlab="Estatura", ylab="Frecuencia", main= "Histograma de Estatura")
boxplot(estatura,ylab="Estatura", main="Boxplot de Estatura")
summary(estatura)
sd(estatura)