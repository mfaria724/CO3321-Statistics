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
sd(pais_de_procedencia)

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
