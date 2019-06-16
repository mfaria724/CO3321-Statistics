# Proyecto Final
# Autor: David Segura #13-11341
#        Manuel Faria #15-10463
#        Juan Oropeza #15-11041

library(corrplot)
datos = read.table("Proyecto2A.txt",header=T)
par(mfrow = c(2,2))

# 1. Realice un analisis descriptivo y exploratorio de los datos. Incluya en este
# analisis la matriz de correlacion.

# Peso
peso = datos$peso
summary(peso)
sd(peso)
boxplot(peso,main="Caja de Pesos",ylab="Pesos")

# Estatura
estatura = datos$estatura
summary(estatura)
sd(estatura)
boxplot(estatura,main="Caja de Estaturas",ylab="Estaturas")

# Longitud de Pie
pie = datos$pie
summary(pie)
sd(pie)
boxplot(pie,main="Caja de Longitudes de Pies",ylab="Longitudes de Pies")

# Longitud de Brazo
lbrazo = datos$lbrazo
summary(lbrazo)
sd(lbrazo)
boxplot(lbrazo,main="Caja de Longitudes de Brazos",ylab="Longitudes de Brazos")

# Ancho de Espalda
anchoes = datos$anchoes
summary(anchoes)
sd(anchoes)
boxplot(anchoes,main="Caja de Anchos de Espaldas",ylab="Anchos de Espaldas")

# Longitud entre la rodilla y el tobillo
lrodtob = datos$lrodtob
summary(lrodtob)
sd(lrodtob)
boxplot(lrodtob,main="Caja de Longitudes entre Rodilla-Tobillo",ylab="Longitudes entre Rodilla-Tobillo")

# Diametro del Craneo
dcraneo = datos$dcraneo
summary(dcraneo)
sd(dcraneo)
boxplot(dcraneo,main="Caja de Diametros de Craneos",ylab="Diametros de Craneos")

# Paises de procedencia
pais_de_procedencia = datos$pais_de_procedencia
summary(pais_de_procedencia)

# Histogramas
hist(peso,main="Histogramas de Pesos",ylab="Frecuencia",xlab="Pesos")
hist(estatura,main="Histogramas de Estaturas",ylab="Frecuencia",xlab="Estaturas")
hist(pie,main="Histogramas de Longitudes de Pies",ylab="Frecuencia",xlab="Longitudes de Pies")
hist(lbrazo,main="Histogramas de Longitudes de Brazos",ylab="Frecuencia",xlab="Longitudes de Brazos")
hist(anchoes,main="Histogramas de Anchos de Espalda",ylab="Frecuencia",xlab="Anchos de Espalda")
hist(lrodtob,main="Histogramas de Longitudes entre Rodilla-Tobillo",ylab="Frecuencia",xlab="Longitudes entre Rodilla-Tobillo")
hist(dcraneo,main="Caja de Diametros de Craneos",ylab="Frecuencia",xlab="Diametros de Craneos")

# Matriz de Correlacion
variables_cuant = datos[1:7]
datos.cor = cor(variables_cuant)
corrplot(datos.cor)

# 2. Encuentre el modelo de regresion simple que mejor se ajuste a
# los datos; realice las pruebas estadisticas que considere conveniente
# para justificar su respuesta, incluyendo un analisis de residuales.

# Peso
modeloPeso = lm(estatura~peso)
summary(modeloPeso)
plot(modeloPeso, main = "Estatura ~ Peso")

# Pie
modeloPie = lm(estatura~pie)
summary(modeloPie)
plot(modeloPie, main = "Estatura ~ Pie")

# Longitud de Brazo
modeloLbrazo = lm(estatura~lbrazo)
summary(modeloLbrazo)
plot(modeloLbrazo, main = "Estatura ~ Long. Brazo")

# Ancho Espalda
modeloAnchoes = lm(estatura~anchoes)
summary(modeloAnchoes)
plot(modeloAnchoes, main = "Estatura ~ Ancho Espalda")

# Diametro de Craneo
modeloDcraneo = lm(estatura~dcraneo)
summary(modeloDcraneo)
plot(modeloDcraneo, main = "Estatura ~ Diametro Craneo")

# Longitud entre Rodilla y el Tobillo
modeloLrodtob = lm(estatura~lrodtob)
summary(modeloLrodtob)
plot(modeloLrodtob, main = "Estatura ~ Long. Rodilla-Tobillo")

# 3. Emplee comparacion de modelos para establecer el modelo multiple mas
# apropiado. Realice, como en el inciso 2, todas las pruebas estadisticas
# que considere conveniente para justificar su respuesta, incluyendo un 
# analisis de residuos. Considere un nivel del 5%.

# Regresion paso a paso hacia atras
# Tenemos el modelo multiple con todas las variables
multiple1 = lm(estatura~peso + pie + lbrazo + anchoes + dcraneo + lrodtob)
summary(multiple1)
plot(multiple1,main="Multiple 1")
# Menos significativa es b0

multiple2 = lm(estatura~peso + pie + lbrazo + anchoes + dcraneo + lrodtob - 1)
summary(multiple2)
plot(multiple2,main="Multiple 2")
# Menos significativa es el peso

multiple3 = lm(estatura~pie + lbrazo + anchoes + dcraneo + lrodtob - 1)
summary(multiple3)
plot(multiple3,main="Multiple 3")
# Menos significativa es anchoes

multiple4 = lm(estatura~pie + lbrazo + dcraneo + lrodtob - 1)
summary(multiple4)
plot(multiple4,main="Multiple 4")
# Menos significativa es lrodtob

multiple5 = lm(estatura~pie + lbrazo + dcraneo - 1)
summary(multiple5)
plot(multiple5,main="Multiple 5")
# Aqui ya todos son significativos con nivel de 0.05 


# 4. Estudios previos indican que los estudiantes de doctorado en España
# muestran un peso promedio de 64 kg, aunque estudios en otros doctorados 
# suponen que dicho peso es superior al mostrado por este análisis. Con un
# nivel de confianza que usted cnsidere necesario, realice un código en el
# software estadístico R que mueste el resultado de dicho análisis. Analice
# los resultados y concluya.


# Dado que la muestr es pequeña se debe verificar si se distribuye de manera normal
españa = subset(datos,pais_de_procedencia == "España")$peso
qqnorm(españa, main = "Gráfica Cuantil-Cuantil Normal", xlab = "Cuantiles Teoricos", ylab = "Cuantiles de la muesta")
qqline(españa)
 
# Podemos afirmar que los datos se distribuyen de forma normal
n = length(españa)
t.test(españa, alternative = "greater", mu = 64, conf.level = 0.99)

