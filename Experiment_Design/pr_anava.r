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
# e)

# Probamos el supuesto de normalidad:
qqnorm(residuals(model))
qqline(residuals(model))

