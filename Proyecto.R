# Proyecto Final
# Autor: David Segura #13-11341
#        Manuel Faria #15-10463
#        Juan Oropeza #15-11041

datos = read.table("Proyecto2A.txt",header=T)

# 1. Realice un analisis descriptivo y exploratorio de los datos. Incluya en este
# analisis la matriz de correlacion.

peso = datos$peso

boxplot(peso,ylab="Peso", main="Boxplot de Peso")
summary(peso)
sd(peso)s

estatura = datos$estatura

boxplot(estatura,ylab="Estatura", main="Boxplot de Estatura")
summary(estatura)
sd(estatura)

pie = datos$pie

boxplot(pie,ylab="Pie", main="Boxplot de Pie")
summary(pie)
sd(pie)

lbrazo = datos$lbrazo

boxplot(lbrazo,ylab="Longitud de Brazo", main="Boxplot de Longitud de Brazo")
summary(lbrazo)
sd(lbrazo)

anchoes = datos$anchoes

boxplot(anchoes,ylab="Anchura de Espalda", main="Boxplot de Anchura de Espalda")
summary(anchoes)
sd(anchoes)

dcraneo = datos$dcraneo

boxplot(dcraneo,ylab="Diametro de Craneo", main="Boxplot de Diametro de Craneo")
summary(dcraneo)
sd(dcraneo)

lrodtob = datos$lrodtob

boxplot(lrodtob,ylab="Longitud entre Rodilla y el Tobillo", main="Boxplot de Longitud entre Rodilla y el Tobillo")
summary(lrodtob)
sd(lrodtob)

pais_de_procedencia = datos$pais_de_procedencia

boxplot(pais_de_procedencia,ylab="Pais de Procedencia", main="Boxplot de Pais de Procedencia")
summary(pais_de_procedencia)
sd(pais_de_procedencia)

#par(mfrow = c(1,1))
hist(peso, xlab="Peso", ylab="Frecuencia", main= "Histograma de Peso")
hist(estatura, xlab="Estatura", ylab="Frecuencia", main= "Histograma de Estatura")
hist(pie, xlab="Pie", ylab="Frecuencia", main= "Histograma de Pie")
hist(lbrazo, xlab="Longitud de Brazo", ylab="Frecuencia", main= "Histograma de Longitud de Brazo")
hist(anchoes, xlab="Anchura de Espalda", ylab="Frecuencia", main= "Histograma de Anchura de Espalda")
hist(dcraneo, xlab="Diametro de Craneo", ylab="Frecuencia", main= "Histograma de Diametro de Craneo")
hist(lrodtob, xlab="Longitud entre Rodilla y el Tobillo", ylab="Frecuencia", main= "Histograma de Longitud entre Rodilla y el Tobillo")
hist(pais_de_procedencia, xlab="Pais de Procedencia", ylab="Frecuencia", main= "Histograma de Pais de Procedencia")

# Encuentre el modelo de regresión simple que mejor se ajuste a los datos; realice las pruebas estadísticas que considere
# conveniente para justificar su respuesta, incluyendo un análisis de residuales.

par(mfrow = c(2,2))

# Peso
modeloPeso = lm(estatura~peso)
ysummary(modeloPeso)
plot(modeloPeso)

# Pie
modeloPie = lm(estatura~pie)
summary(modeloPie)
plot(modeloPie)

# Longitud de Brazo
modeloLbrazo = lm(estatura~lbrazo)
summary(modeloLbrazo)
plot(modeloLbrazo)

# Ancho Espalda
modeloAnchoes = lm(estatura~anchoes)
summary(modeloAnchoes)
plot(modeloAnchoes)

# Diametro de Craneo
modeloDcraneo = lm(estatura~dcraneo)
summary(modeloDcraneo)
plot(modeloDcraneo)

# Longitud entre Rodilla y el Tobillo
modeloLrodtob = lm(estatura~lrodtob)
summary(modeloLrodtob)
plot(modeloLrodtob)

