# Proyecto Final
# Autor: David Segura #13-11341
#        Manuel Faria #15-10463
#        Juan Oropeza #15-11041

library(corrplot)
datos = read.table("Proyecto2A.txt",header=T)

# 1. Realice un analisis descriptivo y exploratorio de los datos. Incluya en este
# analisis la matriz de correlacion.

# Peso
peso = datos$peso
summary(peso)
sd(peso)

# Estatura
estatura = datos$estatura
summary(estatura)
sd(estatura)

# Longitud de Pie
pie = datos$pie
summary(pie)
sd(pie)

# Longitud de Brazo
lbrazo = datos$lbrazo
summary(lbrazo)
sd(lbrazo)

# Ancho de Espalda
anchoes = datos$anchoes
summary(anchoes)
sd(anchoes)

# Diametro del Craneo
dcraneo = datos$dcraneo
summary(dcraneo)
sd(dcraneo)

# Longitud entre la rodilla y el tobillo
lrodtob = datos$lrodtob
summary(lrodtob)
sd(lrodtob)

# Paises de procedencia
pais_de_procedencia = datos$pais_de_procedencia
summary(pais_de_procedencia)

# Matriz de Correlacion
variables_cuant = datos[1:7]
datos.cor = cor(variables_cuant)
corrplot(datos.cor)

# 2. Encuentre el modelo de regresion simple que mejor se ajuste a
# los datos; realice las pruebas estadisticas que considere conveniente
# para justificar su respuesta, incluyendo un analisis de residuales.

par(mfrow = c(2,2))

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