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

df$Insecticida = factor(df$Insecticida, labels = c('A', 'B', 'C', 'D', 'E', 'F', 'G'))

model = lm(df$Rendimiento ~ df$Insecticida)
analysis = aov(model)
summary(analysis)

# Con un p-valor menor a 0.05 se rechaza la hipótesis nula de que las medias son todas iguales.
# El siguiente gráfico ya sugiere grandes diferencias entre las medias de los tratamientos:
plot = ggplot(df, aes(x=Insecticida, y=Rendimiento))
plot + geom_boxplot()


# -----------------------------------------------------------------------------------------------------------------------
# e) Verifique los supuestos requeridos para el análisis estadístico.

df_residuals = resid(model)

# Probamos el supuesto de normalidad:
qqnorm(df_residuals)
qqline(df_residuals)
# Todo luce normal.

# Probamos el supuesto de homocedasticidad de varianzas:
plot(fitted(model), df_residuals, xlab = 'Valores predichos', ylab = 'Residuos')
# Todo luce homocedástico.

# -----------------------------------------------------------------------------------------------------------------------
# f) Obtenga las pruebas a “posteriori” de Fisher y DGC. Compare los resultados de ambas.

# Fisher:
library(agricolae)
t = LSD.test(df$Rendimiento, df$Insecticida, DFerror = 1.977, MSerror = 0.0942)
t

# Tukey:
TukeyHSD(analysis)

# DGC:
source("~/Documents/MestriaUNC/DiseñoExperimentos/Pr/R/MDSGC.r")
dgc = gDGC(df) # Esto no funciona bien, la matriz de distancias se llena de NaNs.

# Conclusión: muchas medias son significativamente distintas entre sí. El insecticida B fue el que mejor rendimiento tuvo, y el
# G el que peor funcionó, estando lejos de las medias de todos los demás grupos.


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
melted = melt(df)
names(melted) = c('Tratamiento', 'Nitrógeno')
fit = lm(Nitrógeno ~ Tratamiento, data = melted)
summary(fit)
anova(fit)

# Hacemos las cuentas manualmente:
# Fuente: tratamientos:
medias_tratamiento = colMeans(df)
media_total = sum(df)/(length(names(df)) * length(df$t1))
num_tratamientos = 6
num_repeticiones = 5
N = 30
SCTr = num_repeticiones * sum((medias_tratamiento - media_total)**2)

gl_tratamiento = num_tratamientos - 1
CMTr = SCTr/gl_tratamiento

# Fuente: error total:
error_total = sum((df - media_total) ** 2)
CMT = error_total/(N-1)

# Fuente: error experimental:
error_experimental = error_total - SCTr
gl_experimental = (N - num_tratamientos)
CME = error_experimental/gl_experimental

# Estadístico F:
F = CMTr/CME
# Todo salió bien.
# Miramos en la tabla F_5;24;0.05 = 2.10. Al ser nuestro estadístico mayor se rechaza la hipótesis nula.
# Además vemos que P(>F) es menor que 0.05, por lo que la hipótesis nula se rechaza para alfa = 0.05.

#-------------------------------------------------------------------------------------------------------
# e) Verificar los supuestos.

# Normalidad:
df_residuals = resid(fit)

# Probamos el supuesto de normalidad:
qqnorm(df_residuals)
qqline(df_residuals)
# El gráfico luce normal.

# Probamos el supuesto de homocedasticidad de varianzas con un s-l plot:
plot(fitted(fit), df_residuals, xlab = 'Valores predichos', ylab = 'Residuos')
# Los residuos lucen como uniformemente distribuidos sin ningún patrón a la vista.

# f) Conclusiones:
# Dado que el p-valor es de los tratamientos es menor a 0.05 concluimos que hay diferencias significativas entre los tratamientos.
# Esto quiere decir que los niveles de nitrógeno varían significativamente entre las cepas.

plot = ggplot(melted, aes(x = Tratamiento, y= Nitrógeno)) + xlab('Tratamiento') + ylab('Contenido de nitrógeno')
plot + geom_boxplot()
# En el gráfico de boxplot podemos ver cómo el nivel de nitrógeno de la cepa nro. 1 es mucho mayor en promedio que el de las 
# demás.

#-------------------------------------------------------------------------------------------------------
# g) Gráfico de box-plot.
plot = ggplot(melted, aes(x = Tratamiento, y= Value)) + xlab('Tratamiento') + ylab('Contenido de nitrógeno')
plot + geom_boxplot()


#-------------------------------------------------------------------------------------------------------
#-------------------------------------------Ejercicio 5-------------------------------------------------
#-------------------------------------------------------------------------------------------------------

df = read.csv('ex_5.csv')
df$Bloques = c('b1', 'b2', 'b3', 'b4')
melted = melt(df)
df$Bloques = NULL

# d) ANAVA
# Manualmente:
source("utils.r")
SCB = suma_cuadrados_bloques(df)
SCTr = suma_cuadrados_tratamientos(df)
SCT = suma_cuadrados_total(df)
SCE = SCT - SCTr - SCB

a = 6
b = 4
N = 24 

gl_bloques = b - 1
gl_tratamientos = a - 1
gl_total = a*b - 1
gl_error = (a-1) * (b-1)

CMB = SCB/gl_bloques
CMTr = SCTr/gl_tratamientos
CMT = SCT/gl_total
CME = SCE/gl_error

F_tratamiento = CMTr/CME

# Con las funciones de R:
model = lm(value~ Bloques + variable, melted)
an = anova(model)
# El anova muestra que hay diferencias significativas entre los pesos de las plantas para diferentes niveles de salinidad.

#-------------------------------------------------------------------------------------------------------
# e) Comprobamos los supuestos:
# Normalidad de los residuos:
residuos = resid(model)
qqnorm(residuos)
qqline(residuos)
# Los residuos no parecen ser normales.

# Homogeneidad de varianzas.
spread_location_plot(melted, 'Bloques', 'variable', 'value')
# Se puede percibir un patrón de embudo en los residuos que indica falta de homocedasticidad de varianzas.


# Comprobamos que los supuestos de normalidad y homocedasticidad de la varianza se cumplen.
# Miramos ahora el supuesto de aditividad bloque-tratamiento:
ggplot(data = melted, aes(x = Bloques, y = value, shape = variable, group = variable, linetype = variable)) +
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "line") + 
  xlab('Bloques') +
  ylab('Promedio por tratamiento')

ggplot(data = melted, aes(x = variable, y = value, shape = Bloques, group = Bloques, linetype = Bloques)) +
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "line") + 
  xlab('Salinidad') +
  ylab('Promedio por bloque')

#-------------------------------------------------------------------------------------------------------
# f) Dado que los supuestos no se cumplen concluimos que el análisis realizado no es adecuado para este problema.


#-------------------------------------------------------------------------------------------------------
#-------------------------------------------Ejercicio 6-------------------------------------------------
#-------------------------------------------------------------------------------------------------------
df = read.csv('ex_6.csv')
melted = melt(df)
names(melted) = c('Bloques', 'Tratamientos', 'Valores')
melted$Bloques = factor(melted$Bloques)
melted$Tratamientos = factor(melted$Tratamientos)


# d) Transformación arcoseno en los datos:
melted$Arcsin_tr = asin(sqrt(melted$Valores/100))
# Verificamos nuevamente los supuestos de normalidad y homocedasticidad de varianzas:
# Normalidad:
asin_fit = lm(Arcsin_tr ~ Bloques + Tratamientos, data = melted)
summary(asin_fit)
asin_residuals = resid(asin_fit)
qqnorm(asin_residuals)
qqline(asin_residuals)
# Luce bastante normal. Ahora varianza:
spread_location_plot(melted, 'Bloques', 'Tratamientos', 'Arcsin_tr')
# El comportamiento ahora sí luce aleatorio.

#-------------------------------------------------------------------------------------------------------
# e) Comparar el control con el promedio de los demás tratamientos.

# Lo hacemos mediante contrastes con coefficientes 3, -1, -1, -1 para el control y las demás respectivamente.
levels(melted$Tratamientos)  # El nivel A es el control.

c1 <- c(3, -1, -1, -1)

mat = cbind(c1)
contrasts(melted$Tratamientos) = mat
contrastes <- lm(Valores ~ Tratamientos, data=melted)
summary(contrastes)

attributes(contrastes$qr$qr)$contrasts


#-------------------------------------------------------------------------------------------------------
#-------------------------------------------Ejercicio 7-------------------------------------------------
#-------------------------------------------------------------------------------------------------------
df = read.csv('ex_7.csv')
melted = melt(df)
names(melted) = c('Insecticida', 'Bloque', 'Plantulas')

#d) ¿Los insecticidas producen distintos efectos?
anova(lm(Plantulas ~ Insecticida + Bloque, data = melted))
# El anova muestra que no hay diferencias significativas entre tratamientos, o sea, entre tipos de insecticidas.

#-------------------------------------------------------------------------------------------------------
# e) Eficiencia relativa:


#-------------------------------------------------------------------------------------------------------
#-------------------------------------------Ejercicio 9-------------------------------------------------
#-------------------------------------------------------------------------------------------------------
# d) ANAVA:

df = read.csv('ex_9.csv')
names(df) = c('Almacenamiento', 'Humedad', 'Viabilidad')
df$Almacenamiento = factor(df$Almacenamiento)
df$Humedad = factor(df$Humedad)

anova(lm(Viabilidad ~ Almacenamiento * Humedad, data=df))

# Vemos que no hay interacción, pero sí diferencias significativas entre las medias de cada tratamiento.

#-------------------------------------------------------------------------------------------------------
# e) Gráfico de valores medios de la respuesta según los factores.
# medias por humedad:
medias_humedad = aggregate(df$Viabilidad, by=list(df$Humedad), mean)
medias_almacen = aggregate(df$Viabilidad, by=list(df$Almacenamiento), mean)
plot(levels(df$Humedad), medias_humedad$x, col='red')
plot(levels(df$Almacenamiento), medias_almacen$x, col='blue')

#-------------------------------------------------------------------------------------------------------
# f) Comprobamos los supuestos:

# Normalidad de residuos:
model = lm(Viabilidad ~ Almacenamiento + Humedad + Almacenamiento * Humedad, data=df)
residuos = resid(model)
qqnorm(residuos)
qqline(residuos)

# Homocedasticidad de varianzas:
df$medias_celdas = ave(df$Viabilidad, df$Almacenamiento, df$Humedad)
df$Residuos = df$Viabilidad - df$medias_celdas
plot(df$medias_celdas, df$Residuos)
# PREGUNTAR ESTO.


#-------------------------------------------------------------------------------------------------------
#-------------------------------------------Ejercicio 11------------------------------------------------
#-------------------------------------------------------------------------------------------------------
df = read.csv('ex_11.csv')
melted = melt(df, id.vars = c('Cerdos', 'Cerdas'))

# d) ANAVA:
analysis = aov(lm(value ~ Cerdas + Cerdas/Cerdos, data = melted))
summary(analysis)

#-------------------------------------------------------------------------------------------------------
# e) Comprobamos los supuestos:

#-------------------------------------------------------------------------------------------------------
# d) 


#-------------------------------------------------------------------------------------------------------
#-------------------------------------------Ejercicio 13------------------------------------------------
#-------------------------------------------------------------------------------------------------------

# c) ANAVA:
df = read.csv('ex_13.csv')
anova_df = anova(lm(perimetro ~ virus*riego, data = df))

# d) Conclusiones:
# No resultó significativa la interacción pero sí cada factor.
# Test de fisher:
a = LSD.test(aov(perimetro ~ virus*riego, data = df), 'virus', group = TRUE)
b = LSD.test(aov(perimetro ~ virus*riego, data = df), c('virus', 'riego'), group = TRUE)
b


#-------------------------------------------------------------------------------------------------------
#-------------------------------------------Ejercicio 14------------------------------------------------
#-------------------------------------------------------------------------------------------------------

df = read.csv('ex_14.csv')

# Para realizar el análisis de la covarianza incluimos en el análisis una regresión con la variable vecinos como covariable:
mod_regr = lm(Increm. ~ vecinos, data = df)

# El modelo completo para el ancova es:
mod_completo = lm(Increm. ~ Especie + vecinos, data = df)

# ANAVA:
anova(mod_completo, mod_regr)
anova(mod_completo)


#-------------------------------------------------------------------------------------------------------
#-------------------------------------------Ejercicio 15------------------------------------------------
#-------------------------------------------------------------------------------------------------------

# a)
# Miremos qué pasa si no usamos la covariable:
anova(aov(Increm. ~ Especie, data = df))

# b)
# Notar que el error experimental es muchísimo más grande esta vez.

# c)
# Veamos qué relación hay entre la variable vecinos y la respuesta Increm.:
mod_regr
summary(mod_regr)

# Vemos que el test fue significativo, con lo que podemos concluir que hay una relación lineal entre los vecinos y 
# la variable respuesta de incremento.
plot(df$vecinos, df$Increm.)
