setwd("/home/mariana/Documents/MestriaUNC/Applied_Statistics_assignments/Experiment_Design/regression/")

#------------------------------------------------------------------------------------------------
#-------------------------------------- Ejercicio 2 ---------------------------------------------
#------------------------------------------------------------------------------------------------
df = read.csv('ex_2.csv')
df$cerdo = factor(df$cerdo)

# a) Vemos si hay relación lineal:
# Lo que tenemos aquí es un modelo mixto en donde el factor "cerdo" tiene dos niveles e introduce efectos aleatorios, mientras
# el factor "mes" tiene seis niveles y es de efectos fijos.
library('nlme')
lm_cerdos = lme(peso ~ mes, data = df, random = ~ 1|cerdo)
summary(lm_cerdos)

# La varianza de la parte aleatoria es tan pequeña que lo que tenemos aquí es un modelo lineal simple en el que podemos ignorar
# la separación en dos grupos de los cerdos.
lm_cerdos_fijo = lm(peso ~ mes, data = df)
summary(lm_cerdos_fijo)

#---------------------------------------------
# b) Gráfico de la recta e interpretación del estimador de la pendiente:
# De un mes al otro el peso aumenta en 19.61gramos en promedio.
plot(peso ~ mes, data = df, col = cerdo)

#---------------------------------------------
# c) Verificación de supuestos y de capacidad predictiva del modelo:

# Residuos vs predichos:
residuos = residuals(lm_cerdos_fijo)
predichos = fitted(lm_cerdos_fijo)
plot(predichos, residuos)

# QQ-plot de residuos:
qqnorm(residuos)
qqline(residuos)

# Histograma de residuos:
hist(residuos, freq = FALSE)
curve(dnorm, add = TRUE)

# Residuos vs producto de variables predictoras ???

# Residuos vs tiempo
plot(df$mes, residuos)

#---------------------------------------------
# d) ¿Capacidad predictora?
# Miramos el R²:
summary(lm_cerdos_fijo)$r.squared
# Al ser 0.98 podemos concluir que casi toda la varianza es explicada por el modelo.
# ¿Significa que tiene capacidad predictora?

#---------------------------------------------
# e) ¿Podemos predecir el peso para un cerdo de diez meses de edad?
# Si:
x0 = 10
b0 = -1.9
b1 = 19.61
y0 = b0 + b1 * x0
y0


#------------------------------------------------------------------------------------------------
#-------------------------------------- Ejercicio 3 ---------------------------------------------
#------------------------------------------------------------------------------------------------

# a) Ajustar un modelo lineal:
df = read.csv('ex_3.csv')
lm_simple = lm(rend. ~ hum., data = df)
summary(lm_simple)

#---------------------------------------------
# b) Analizar los resultados:
plot(fitted(lm_simple), residuals(lm_simple))

# Podemos observar que los residuos tienen un patrón no aleatorio, más bien cuadrático, y que por lo tanto la regresión lineal
# no es adecuada. Además el R² indica que menos de la mitad de la varianza es explicada por dicho modelo.

#---------------------------------------------
# c) Ajustamos un modelo polinomial de segundo grado:
lm_poly = lm(rend. ~ poly(hum., 2), data = df)
summary(lm_poly)

#---------------------------------------------
# d) Comparación de resultados:

# Esta vez el R² es de casi 0.9, con lo que estamos cubriendo muchísima más varianza con el modelo. Además podemos ver que el
# coeficiente de grado dos es significativo y que el error del intercepto es mucho menor en el modelo polinómico.
# Miremos qué pasa con los residuos esta vez:

plot(fitted(lm_poly), residuals(lm_poly))
# Se observa un patrón aleatorio.

#---------------------------------------------
# e) Predecimos el rendimiento para una humedad del 11%:
predict(lm_poly, data.frame(hum. = c(11)))


#------------------------------------------------------------------------------------------------
#-------------------------------------- Ejercicio 4 ---------------------------------------------
#------------------------------------------------------------------------------------------------

#---------------------------------------------
# a) ANAVA:
anova(lm_poly)

df$pred = predict(lm_poly, data.frame(hum. = df$hum.))
mean_y = mean(df$rend.)
N = nrow(df)
p = 3
n = length(unique(df$hum.))

# Calculamos la SCR (regresión) y la SCE (error):
SCR = sum((df$pred - mean_y)**2)
GLR = p - 1
CMR = SCR/GLR

SCE = sum((df$rend. - df$pred)**2)
GLE = N - p
CME = SCE/GLE

F_reg = CMR/CME

# Calculamos ahora la suma de cuadrados para el lack of fit y el error puro:
df$means_groups = ave(df$rend., df$hum.)
SCLF = sum((df$means_groups - df$pred)^2)
GLLF = n - p
CMLF = SCLF/GLLF

SCEP = sum((df$rend. - df$means_groups)^2)
GLEP = N - n
CMEP = SCEP/GLEP

F_lof = CMLF/CMEP

# Comprobamos que los resultados están bien haciendo lo mismo con R:

lm_asfactor = lm(rend.~as.factor(hum.), data = df)
anova(lm_poly, lm_asfactor, test = 'F')
