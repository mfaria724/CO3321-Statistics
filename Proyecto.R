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

# Matriz de Correlacion
variables_cuant = datos[1:7]
datos.cor = cor(variables_cuant)
corrplot(datos.cor)

# 2. Encuentre el modelo de regresion simple que mejor se ajuste a
# los datos; realice las pruebas estadisticas que considere conveniente
# para justificar su respuesta, incluyendo un analisis de residuales.

# Modelo Estatura - Peso
modelo_est_peso = lm(estatura ~ peso)
summary(modelo_est_peso)

# Modelo Estatura - Longitud de Pie
modelo_est_pie = lm(estatura ~ pie)
summary(modelo_est_pie)

# Modelo Estatura - Longitud de Brazo
modelo_est_lbrazo = lm(estatura ~ lbrazo)
summary(modelo_est_lbrazo)

# Modelo Estatura - Ancho de Espalda
modelo_est_anchoes = lm(estatura ~ anchoes)
summary(modelo_est_anchoes)

# Modelo Estatura - Diametro del Craneo
modelo_est_dcraneo = lm(estatura ~ dcraneo)
summary(modelo_est_dcraneo)

# Modelo Estatura - Longitud entre la rodilla y el tobillo
modelo_est_lrodtob = lm(estatura ~ lrodtob)
summary(modelo_est_lrodtob)

# Mejor estimacion > estatura ~ pie