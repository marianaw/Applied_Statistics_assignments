setwd("/home/mariana/Documents/MestriaUNC/Applied_Statistics_assignments/Experiment_Design/lmm/")
require(ggplot2)
library('nlme')

#------------------------------------------------------------------------------------------------
#-------------------------------------- Ejercicio 1 ---------------------------------------------
#------------------------------------------------------------------------------------------------
df = read.csv('ex_1.csv')
df$Cobayo = factor(df$Cobayo)

# a) Diagrama de dispersión de peso vs. tiempo separado por vitamina:
attach(df)
plot(Tiempo, Peso, col = c('red', 'blue', 'green')[VitE]);
detach(df)

# Se observan distintos comportamientos. Puede verse cómo con la alta dosis de vitamina el crecimiento en el peso es mayor.

#-------------------------------------------------------------------
# b) Diagrama de puntos y boxplot para cada tiempo:
plot = ggplot(df, aes(x = Tiempo, y= Peso, fill = VitE)) + xlab('Tiempo') + ylab('Peso')
plot + geom_boxplot()

medias_tiempo_porVit = aggregate(df$Peso, by = list(df$Tiempo, df$VitE), mean)
names(medias_tiempo_porVit) = c('Tiempo', 'VitE', 'Media_Peso')
plot(medias_tiempo_porVit$Tiempo, medias_tiempo_porVit$Media_Peso, col = c('red', 'blue', 'green')[medias_tiempo_porVit$VitE])
# Se observa un comportamiento cúbico.

# Se observa como la media de peso para la dosis alta de vitamina es mayor, y cómo la media para los que no reciben vitamina es
# menor.

# Hacemos un diagrama para mirar la interacción de tiempo y vitamina para cada cobayo:
attach(df)
interaction.plot(VitE, Cobayo, Peso)
detach(df)
#-------------------------------------------------------------------
# c) Ajustamos un modelo lineal mixto:

# # El siguiente modelo ajusta una linea sin distinción de tipos de vitamina (sólo estamos experimentando) con la misma pendiente
# # para todos los tiempos y errores aleatorios para los cobayos:
# modelo_1 = lme(Peso ~ Tiempo, data = df, random = ~ 1|Cobayo)
# summary(modelo_1)
# # R arroja que tanto el intercepto como el slope son significativos.
# 
# # Agregamos un efecto aleatorio sobre la pendiente del tiempo:
# modelo_2 = update(modelo_1, random = ~ Tiempo | Cobayo)
# summary(modelo_2)
# 
# # Comparemos ambos modelos:
# anova(modelo_1, modelo_2)
# # El p-valor muestra que el modelo_2 es mejor que el modelo_1.
# # Notar que aún no hemos considerado el factor vitamina, sólo analizamos el peso en función del tiempo.
# 
# # Ahora agregamos la interacción entre vitamina y tiempo:
# modelo_3 = update(modelo_2, fixed. = ~ Tiempo * VitE)

# Como el comportamiento es cúbico ajustamos un modelo lineal cúbico en el tiempo:
modelo = lme(Peso ~ poly(Tiempo, 3) * VitE, data = df, random = ~ Tiempo|Cobayo, method = 'ML')
summary(modelo)
# Vemos que con respecto a la base E0 del nivel VitE los niveles E1 y E2 no son significativos, es decir, no hacen diferencia.
# Este es un contraste de E1 y E2 con respecto a E0.
# Ahora vamos a correr otro modelo en el que consieramos E0 y E1 y E2, juntos, como otro nivel.

modelo_2 = lme(Peso ~ poly(Tiempo, 3) * E0vsE12, data = df, random = ~ Tiempo|Cobayo)
summary(modelo_2)

#-------------------------------------------------------------------
# d) Conclusiones sobre el modelo anterior:
# Vemos que el factor vitamina no es significativo, y que la correlación entre el nivel E1 de este factor y el tiempo, tampoco.

#-------------------------------------------------------------------
# e) Agregamos correlación:

# Correlación de simetría compuesta:
modelo_3_simcomp = lme(Peso ~ poly(Tiempo, 3) * VitE, data = df, random = ~Tiempo|Cobayo, correlation = corCompSymm())
summary(modelo_3_simcomp)

# Correlación sin estructura:
modelo_3_sinestr = lme(Peso ~ poly(Tiempo, 3) * VitE, data = df, random = ~Tiempo|Cobayo, correlation = corSymm(),
                       control = lmeControl(maxIter = 1000, opt = 'optim'))
summary(modelo_3_sinestr)

# Correlación autorregresiva de primer orden:
modelo_3_autoreg = lme(Peso ~ poly(Tiempo, 3) * VitE, data = df, random = ~Tiempo|Cobayo, correlation = corAR1(),
                       control = lmeControl(maxIter = 1000, opt = 'optim'))
summary(modelo_3_autoreg)

#-------------------------------------------------------------------
# f) Miramos la heterocedasticidad:
par(mfrow = c(2,1))
plot(modelo_3_autoreg)
qqnorm(modelo_3_autoreg)
plot(modelo)
# Todo luce homocedástico.

# g) Pruebas a posteriori:
# # Tukey:
# 
# library(multcomp)
# glht(modelo, linfct=mcp(Tiempo='Tukey'))
# 
# df$ints = interaction(df$VitE, df$Tiempo)
# modelo_posthoc = lme(Peso ~ 1 - ints, data = df, random = ~ Tiempo|Cobayo)
# comp = glht(modelo_posthoc, linfct = mcp(ints = 'Tukey'), test = adjusted(type = 'bonferroni'))

#------------------------------------------------------------------------------------------------
#-------------------------------------- Ejercicio 2 ---------------------------------------------
#------------------------------------------------------------------------------------------------
semillas_db = read.csv('ex_2_Semillas.csv')
semillas_db$cajas = factor(semillas_db$cajas)

# a) Gráfico de dispersión de biomasa vs. tiempo separado por tamaño de semilla:
plot = ggplot(semillas_db, aes(x = tiempo, y = biomasa, shape = semillas, color = semillas)) + 
  geom_point() +
  geom_smooth(method=lm)
plot
plot + facet_grid(. ~ semillas)

#------------------------------------------------------
# b) Dispersión de biomasa vs. tiempo particionado por semillas y cajas:
plot2 = ggplot(semillas_db, aes(x = tiempo, y = biomasa, shape = cajas, color = semillas,
                                group = interaction(semillas, cajas))) + 
  geom_point() +
  geom_line()
plot2
plot2 + facet_grid(. ~ semillas + cajas)

# Observar que en las semillas grandes hay poca variabilidad entre caja y caja, mientras que en las semillas pequeñas la caja
# cinco parece tener un comportamiento diferenciado, y la uno también. Las cajas 2, 3, y 4 se comportan de manera similar.

#------------------------------------------------------
# Modelo con estructura de correlación compuesta:

# Probamos un modelo con efectos aleatorios sólo en el intercepto:
sem_corr_comp_sim = corCompSymm(value = 0.2, form = ~ 1 | cajas)
sem_corr_comp_sim = Initialize(sem_corr_comp_sim, data = semillas_db)
corMatrix(sem_corr_comp_sim)


sem_cc_1 = gls(biomasa ~ semillas * tiempo, data = semillas_db, correlation = corCompSymm(form = ~1|cajas))
summary(sem_cc_1)

# sem_cc_1 = lme(biomasa ~ semillas * tiempo, data = semillas_db, random = ~ 1|cajas, correlation = corCompSymm(form = ~1|cajas))
# summary(sem_cc_1)
# 
# sem_cc_1_a = lme(biomasa ~ semillas * tiempo, data = semillas_db, random = ~ 1|cajas)
# summary(sem_cc_1_a)
# 
# anova(sem_cc_1, sem_cc_1_a)

# Heterocedasticidad del modelo que convergió:
plot(sem_cc_1)
qqnorm(sem_cc_1)
qqline(sem_cc_1)

#------------------------------------------------------
# Modelo de parcelas divididas:

sem_pardiv = lme(biomasa ~ semillas * tiempo, data = semillas_db, random = ~1|cajas)
summary(sem_pardiv)
plot(sem_pardiv)
qqnorm(sem_pardiv)

# Como hemos agregado un efecto aleatorio en este segundo modelo que, a diferencia del primero, es mixto, podemos ver
# cómo los errores lucen mucho mejor no obstante tener los mismos coeficientes.

# Prueba a posteriori: Tukey,
lsmeans(sem_pardiv, pairwise ~ semillas * tiempo, adjust='')

# Intento correr tukey con multcomp:
library(multcomp)

# Agregamos a los datos un término de interacción y corremos el modelo de nuevo:
semillas_db$semTiem = interaction(semillas_db$semillas, semillas_db$tiempo, drop = TRUE)
sem_pardiv_int = lme(biomasa ~ semTiem, data = semillas_db, random = ~1|cajas)
summary(sem_pardiv_int)
tukey = glht(sem_pardiv_int, linfct=mcp(semTiem = 'Tukey'))
summary(tukey)

# Fisher:

summary(tukey,test=univariate())

# Bonferroni:

summary(tukey, test=adjusted(type="bonferroni"))