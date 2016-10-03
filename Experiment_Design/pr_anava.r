setwd("/home/mariana/Documents/MestriaUNC/Applied_Statistics_assignments/Experiment_Design/")
require(ggplot2)
#-------------------------------------------------------------------------------------------------------
#-------------------------------------------Ejercicio 1 y 2 (juntos)------------------------------------
#-------------------------------------------------------------------------------------------------------

# Se estudian cuatro circuitos digitales diferentes de computadora. El objetivo es comparar el nivel de ruido presente en los 
# circuitos, a los fines de buscar el que menor ruido provea. ¿Qué circuito conviene seleccionar?
#
# a) Comente acerca del tipo de estudio, los factores y la variable de respuesta.

# En este experimento se estudia el factor "circuito" que tiene cuatro niveles, 1, 2, 3, y 4. Es un ANAVA de efectos fijos
# a un criterio de clasificación porque hay sólo un factor. La variable respuesta es la variación en el ruido.

df = read.csv('ej_1_anava.csv')
df = df[c('Circuito', 'Ruido')]
df$Circuito = factor(df$Circuito, labels = c('Circuito_1', 'Circuito_2', 'Circuito_3', 'Circuito_4'))
plot = ggplot(df, aes(x = Circuito, y= Ruido)) + xlab('Nr. de circuito') + ylab('Nivel de ruido')
plot + geom_boxplot()

# -----------------------------------------------------------------------------------------------------------------------
# b) Plantee las hipótesis estadísticas. 

# La hipótesis nula H0 es que las medias de cada uno de los circuitos son iguales. La hipótesis alternativa es que al menos dos
# de ellas difieren.

# -----------------------------------------------------------------------------------------------------------------------
# c) Complete el cuadro del análisis de la varianza (ANAVA) y discuta sobre las estimaciones correspondientes a cada fuente de 
# variación.

# Calculamos la suma de cuadrados entre tratamientos (SCE):

ll = levels(df$Circuito)
first_term = 0
a = length(ll)  # a = cantidad de tratamientos.
for (i in c(1:a)){
  level = ll[i]
  tratamiento = subset(df$Ruido, df$Circuito == level)
  n_i = length(tratamiento)  # n_i = cantidad de réplicas en el tratamiento i.
  sum_squared = sum(tratamiento)^2
  first_term = first_term + ((1/n_i) * sum_squared)
}

N = length(df$Circuito)
second_term = (1/N) * sum(df$Ruido)^2

SCE = first_term - second_term

# Grados de libertad = a - 1:
GLE = a - 1

# Cuadrados medios entre tratamientos:
CME = SCE/GLE

# Ahora la suma de cuadrados total (SCT):
first_term = sum(df$Ruido^2)
second_term = (1/N) * sum(df$Ruido)^2
SCT = first_term - second_term

# Grados de libertad total = N - 1:
GLT = N - 1

# Calculamos la suma de cuadrados dentro (SCD):
SCD = SCT - SCE

# Grados de libertad dentro = N - a:
GLD = N - a

# Cuadrados medios dentro:
CMD = SCD/GLD

# Calculamos el estadístico F:
F_ = CME/CMD

#Realizamos el análisis de la varianza:
fit = lm(Ruido ~ Circuito, data = df)
summary(fit)
anova(fit)

# -----------------------------------------------------------------------------------------------------------------------
# d) Compare las medias de interés utilizando la prueba de Tukey  y la de Fisher. 
a = aov(Ruido ~ Circuito, data = df)
tuk = TukeyHSD(a, conf.level = 0.95)

hand_tukey = 4.06 * sqrt(CMD/10)

# La prueba de Tukey revela que la diferencia de las medias entre los tratamientos 1 y 4 es significativa.

hand_fisher = 2.12 * sqrt(CMD * (10 + 9)/(10 * 9))
fisher.test()

# La prueba de Fisher arroja que la diferencia entre las medias 1 y 4 es significativa.

# -----------------------------------------------------------------------------------------------------------------------
# e) ¿Qué se puede concluir sobre el nivel de ruido?

# En general se concluye que no todas las medias son iguales, en particular el circuito 1 tiene un promedio de ruido menor, 
# por lo que es conveniente seleccionarlo.


#-------------------------------------------------------------------------------------------------------
#-------------------------------------------Ejercicio 3-------------------------------------------------
#-------------------------------------------------------------------------------------------------------
# Para comparar las producciones en grano de arroz (t/ha) que resultan al aplicar siete insecticidas, se trabajó con 28 parcelas 
# en las que se había sembrado una misma variedad. Se decidió aplicar cada insecticida en igual cantidad de parcelas y a cada
# parcela le fue asignado al azar uno de los productos. Cada parcela estuvo formada por cuatro surcos de 5 m de largo. 
# Para tener en cuenta el efecto de bordura se descartaron los surcos de cada lado de la parcela y medio metro en cada extremo. 
# Por lo tanto cada parcela neta, en la que se tomó el dato, consistió en dos surcos de 4 m. Los rendimientos por parcela se 
# indican a continuación en el esquema de campo:
  

# d) Construya el archivo de datos y realice el análisis estadístico.
df = read.csv('ex_3_insecticidas_parcelas.csv')
names(df)

# La primera columna corresponde a los bloques, que son cuatro, la segunda a los niveles del factor insecticida, y la tercera a
# los diferentes rendimientos.

df$Bloque = factor(df$Bloque, labels = c('Bloque_1', 'Bloque_2', 'Bloque_3', 'Bloque_4'))
df$Insecticida = factor(df$Insecticida, labels = c('A', 'B', 'C', 'D', 'E', 'F', 'G'))

model = lm(df$Rendimiento ~ df$Bloque + df$Insecticida)
analysis = aov(model)
summary(analysis)

# Con un p-valor de 0.0443 se rechaza la hipótesis nula de que las medias son todas iguales.

# -----------------------------------------------------------------------------------------------------------------------
# e) Verifique los supuestos requeridos para el análisis estadístico.

df_residuals = resid(model)

# Probamos el supuesto de normalidad:
qqnorm(df_residuals)
qqline(df_residuals)

# Probamos el supuesto de homocedasticidad de varianzas:
plot(fitted(model), df_residuals, xlab = 'Valores predichos', ylab = 'Residuos')

# Comprobamos que los supuestos de normalidad y homocedasticidad de la varianza se cumplen.
# Miramos ahora el supuesto de aditividad bloque-tratamiento:
ggplot(data = df, aes(x = Bloque, y = Rendimiento, shape = Insecticida, group = Insecticida, linetype = Insecticida)) +
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "line") + 
  xlab('Bloques') +
  ylab('Promedio por bloque')

ggplot(data = df, aes(x = Insecticida, y = Rendimiento, shape = Bloque, group = Bloque, linetype = Bloque)) +
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "line") + 
  xlab('Bloques') +
  ylab('Promedio por bloque')

# Los gráficos sugiere que hay múltiples interacciones entre los bloques y los tratamientos.

# -----------------------------------------------------------------------------------------------------------------------
# f) Obtenga las pruebas a “posteriori” de Fisher y DGC. Compare los resultados de ambas.

# Fisher:
library(agricolae)
t = LSD.test(df$Rendimiento, df$Bloque, 1.276, 0.07)
t

# DGC:
source("~/Documents/MestriaUNC/DiseñoExperimentos/Pr/R/MDSGC.r")
dgc = gDGC(df)

#-------------------------------------------------------------------------------------------------------
#-------------------------------------------Ejercicio 4-------------------------------------------------
#-------------------------------------------------------------------------------------------------------
# Se realizó un experimento en invernáculo con cinco macetas de trébol rojo por tratamiento. Los primeros cinco tratamientos están
# integrados por distintas cepas de Rhizobium trifolii y el sexto es una mezcla de las cinco cepas anteriores. 
# Se midió el contenido de nitrógeno en las plantas de cada maceta. Los datos obtenidos se presentan en la siguiente tabla.

library(reshape)
# d) Probar la desigualdad de las medias.
#Realizamos el análisis de la varianza:
df = read.csv('ex_4.csv')
df$Bloques = c('B1', 'B2', 'B3', 'B4', 'B5')
melted = melt(df)
names(melted) = c('Bloques', 'Tratamiento', 'Value')
fit = lm(Value ~ Bloques + Tratamiento, data = melted)
summary(fit)
anova(fit)

suma_cuadrados_bloques = function(df){
  num_tratamientos = length(names(df))
  medias_bloque = rowMeans(df)
  media_total = sum(df)/(length(df) * length(row.names(df)))
  desvio_bloque = medias_bloque - media_total
  scb = num_tratamientos * sum((medias_bloque - media_total)**2)
  scb
}

suma_cuadrados_tratamientos = function(df){
  num_bloques = length(row.names(df))
  medias_tratamiento = colMeans(df)
  media_total = sum(df)/(length(df) * length(row.names(df)))
  desvio_tratamientos = medias_tratamiento - media_total
  sctr = num_bloques * sum((medias_tratamiento - media_total)**2)
  sctr
}

#-------------------------------------------------------------------------------------------------------
# e) Verificar los supuestos.

# Normalidad:
df_residuals = resid(fit)

# Probamos el supuesto de normalidad:
qqnorm(df_residuals)
qqline(df_residuals)
# El gráfico luce normal.

# Probamos el supuesto de homocedasticidad de varianzas:
plot(fitted(fit), df_residuals, xlab = 'Valores predichos', ylab = 'Residuos')
# Los residuos lucen como uniformemente distribuidos sin ningún patrón a la vista.

# Ahora miramos la interacción bloque-tratamiento:
ggplot(data = melted, aes(x = Tratamiento, y = Value, shape = Bloques, group = Bloques, linetype = Bloques)) +
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "line") + 
  xlab('Bloques') +
  ylab('Promedio por bloque')
# Al correr el test de Tukey vemos que no hay interacción (¿en el gráfico si?):
tukey.add.test(melted$Value, melted$Tratamiento, melted$Bloques)
TukeyHSD(anova_fit, conf.level = 0.95)

#-------------------------------------------------------------------------------------------------------
# f) Conclusiones:
# Dado que el p-valor es de los tratamientos es menor a 0.05 concluimos que hay diferencias significativas entre los tratamientos.

#-------------------------------------------------------------------------------------------------------
# g) Gráfico de box-plot.
plot = ggplot(melted, aes(x = Tratamiento, y= Value)) + xlab('Tratamiento') + ylab('Contenido de nitrógeno')
plot + geom_boxplot()
