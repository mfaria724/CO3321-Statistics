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
par(mfrow=c(2,2))
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


# Dado que la muesta es pequeña se debe verificar si se distribuye de manera normal
españa = subset(datos,pais_de_procedencia == "España")$peso
qqnorm(españa, main = "Gráfica Cuantil-Cuantil Normal", xlab = "Cuantiles Teoricos", ylab = "Cuantiles de la muesta")
qqline(españa)
# Podemos afirmar que los datos se distribuyen de forma normal
# Tamaño de la muestra
n = length(españa)

# Prueba T-Estudent para obtenet estadistico y p-valor
t.test(españa, alternative = "greater", mu = 64, conf.level = 0.99)


# 5. Para el modelo de regresión lineal simple obtenido en el inciso 2, realice 
# la predicción correspondiente para la estatura de 5 estudiantes que se anexan
# a la muestra (Est=estudiantes), los datos se presentan en el Cuadro 1. Grafique
# los intervalos de predicción y de confianza respectivamente. Realice el 
# análisis respectivo.

# Como el mejor modeloque se ajustó fue el de los pies, usaremos ese para predecir
# la estura de los estudiantes.
modeloPie = lm(estatura~pie)
newPoints = data.frame(pie=c(34, 37, 39, 44, 36))

# Graficamos los datos
plot(pie, estatura)
abline(modeloPie)

# Predicción para los nuevos estudiantes
(predict(modeloPie, newPoints, interval = 'predict'))

# Generamos puntos necesarios para las bandas
sequence = data.frame(pie = seq(34, 44, 1))

# Intervalo de predicción
Pie1 = predict(modeloPie, sequence, interval = "prediction")
lines(sequence$pie, Pie1[,2], lty = 2, col = "red")
lines(sequence$pie, Pie1[,3], lty = 2, col = "red")

# Intervalo de confianza para el 95%
Pie2 = predict(modeloPie, sequence, interval = "confidence")
lines(sequence$pie, Pie2[,2], lty = 2, col = "blue")
lines(sequence$pie, Pie2[,3], lty = 2, col = "blue")

# 6. ¿Existe suficiente evidencia que permita concluir que la estatura media de los estudiantes 
# difiere con respecto al país de procedencia? Use el procedimiento de análisis de varianza para 
# un diseño de un factor. ¿Qué concluiría usted con un nivel de significancia de alpha = 0.03?

# H0: No hay diferencias entre las medias
# Ha: Hay diferencias entre las medias

# Obtenemos los datos de las estaturas para cada uno de los paises
estAme = datos$estatura[ pais_de_procedencia == 'América' ]
estChi = datos$estatura[ pais_de_procedencia == 'China' ]
estEsp = datos$estatura[ pais_de_procedencia == 'España' ]
estInd = datos$estatura[ pais_de_procedencia == 'India' ]

dat = c(estAme, estChi, estEsp, estInd)

# Realizamos el calculo de los factores
fac = c(replicate(length(estAme), "América"), replicate(length(estChi), "China"), 
        replicate(length(estEsp), "España"), replicate(length(estInd), "India"))
factr = factor(fac)
tapply(dat, factr, mean)

# Diagrama de caja del modelo
boxplot(dat~factr, xlab = "País", ylab = "Estaura en cm.")

# Creamos el modelo
mod = lm(dat~factr)

# Analisis de Varianza
anova(mod)

# Verificamos dos a dos
pairwise.t.test(dat, factr)
