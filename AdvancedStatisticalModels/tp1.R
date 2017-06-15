# Script para la primera evaluación de MOEA, Maestría en Estadística Aplicada, 2017.

setwd('Documents/Maestria/Applied_Statistics_assignments/AdvancedStatisticalModels/')
library(nlme)
library(plyr)
library(xtable)

# Cargamos los datos:
df = read.csv('cerezos.csv')
df$Bloque = as.factor(df$Bloque)
df$Tratamiento = as.factor(df$Tratamiento)
df$BloqueTratam = as.factor(paste(df$Bloque, df$Tratamiento, sep = '_'))

# Agrupamos y calculamos las medias de las alturas en cada intersección entre bloque, tratamiento, y año:
medias<-ddply(df, .(Bloque, Tratamiento, Anio), summarize, mean=mean(Altura))
names(medias) = c("Bloque", "Tratamiento", "Anio", "Altura")

# Colapsamos los factores Bloque y Tratamiento para conformar lo que serán nuestros sujetos de anidamiento
# en los datos longitudinales:
medias$BloqueTratam = as.factor(paste(medias$Bloque, medias$Tratamiento, sep = '_'))

# Detallamos el modelo (los resultados son muy parecidos a los obtenidos en SAS):
model = lme(Altura ~ Tratamiento + Anio + I(Anio^2) + I(Anio^2) * Tratamiento + Bloque,
            random = ~ 1|BloqueTratam,
            cor = corAR1(form = ~ 1|BloqueTratam),
            data = medias,
            method = "REML",
            na.action = na.omit)
summary(model)

# Ahora modelamos agrupando por árbol en vez de medias de altura:
model2 = lme(Altura ~ Tratamiento + Anio + I(Anio^2) + I(Anio^2) * Tratamiento + Bloque,
             random = ~1|BloqueTratam,
             cor = corAR1(form = ~ 1|BloqueTratam),
             data = df,
             method = "REML",
             na.action = na.omit)

# --------------------------------------------------------------------------
# Problemas 3 y 4. Decidimos qué plantación es mejor.
# Medias del séptimo año:
df_Anio7 = df[df$Anio == 7,]
medias_7 = aggregate(df_Anio7$Altura, list(df_Anio7$Tratamiento), mean)
names(medias_7) = c("Tratamiento", "Altura Promedio")
xtable(medias_7)

# Ajustamos un modelo lineal común para los datos del séptimo año.
l = lm(Altura ~ Tratamiento + Bloque , data = df_Anio7)
a = anova(l)

# Corremos Fisher LSD para estudiar la diferencia de medias:
lsd = LSD.test(df_Anio7$Altura, df_Anio7$Tratamiento, 264, 0.37316, console = TRUE)

# Usamos xtable para generar una tabla latex-friendly:
xtable(lsd$groups)
# --------------------------------------------------------------------------


# --------------------------------------------------------------------------
# Problemas 6 y 7.
# Agregamos la correlación espacial:

# Modelo sin correlación espacial:
me_nocor = gls(Altura ~ Tratamiento + Anio + I(Anio^2) + Anio * Tratamiento + I(Anio^2) * Tratamiento + Bloque - 1,
               data = df,
               cor = corAR1(form = ~ 1|BloqueTratam),
               method = "ML",
               na.action = na.omit)
summary(me_nocor)

# Modelo con correlación espacial:
me = update(me_nocor, 
            correlation = corExp(form = ~Fila + Columna|Anio, nugget = FALSE))
summary(me)

me_gauss = update(me_nocor,
                  correlation = corGaus(form = ~Fila + Columna|Anio, nugget = FALSE))

# Comparamos los modelos con y sin correlación espacial:
compar = anova(me_nocor, me_gauss, me)
# El de correlación espacial es el mejor.

# Estudiamos las diferencias de medias en ese modelo:

# Calculamos la suma de cuadrados de los residuos:
mean_sq_errors = mean(me$residuals^2); mean_sq_errors

# Ahora obtenemos los grados de libertad de los residuos:
a_me = anova(me)
gl_residuos = nrow(df) - sum(a_me$numDF) - 1; gl_residuos

# Hay 1904 observaciones, los coeficientes se llevan 18 grados de libertad, por lo tanto los
# residuos tienen 1885 grados de libertad.

# Ahora corremos Fisher LSD:
lsd_corr_esp = LSD.test(df$Altura, df$Tratamiento, gl_residuos, mean_sq_errors, console = TRUE)
# Vemos que la plantación de cerezo con alisos en baja densidad sigue siendo la mejor.

# Miramos el modelo que corresponde al séptimo año:
l_nocorr = gls(Altura ~ Tratamiento + Anio + I(Anio^2) + Anio * Tratamiento + I(Anio^2) * Tratamiento + Bloque - 1,
               data = df_Anio7,
               # cor = corAR1(form = ~ 1|BloqueTratam),
               method = "ML",
               na.action = na.omit)
l_corr = update(l, correlation = corGaus(form = ~Fila + Columna|Anio, nugget = FALSE))

# --------------------------------------------------------------------------


variog = Variogram(me, form = ~arbol|Anio, resType = "normalized", maxDist = 30)
# plot(variog, smooth = FALSE, ylim = c(0, 1.3))


me7= gls(Altura ~ Tratamiento + Bloque,
         correlation = corGaus(form = ~arbol, nugget = TRUE),
         data = df_Anio7,
         na.action = na.omit)
summary(me7)

variog7 = Variogram(me7, form = ~arbol, resType = "normalized")
plot(variog7, smooth = FALSE, ylim = c(0, 1.3))

df$Resid = residuals(me7)
coordinates(df) <- ~ Fila + Columna
bubble(df, 'Resid')


# me = gls(Altura ~ Tratamiento + Anio + I(Anio^2) + I(Anio^2) * Tratamiento + Bloque,
#          correlation = corGaus(form = ~arbol|Anio, nugget = FALSE),
#          data = df,
#          na.action = na.omit)

# model_espacial = lme(Altura ~ Tratamiento + Anio + I(Anio^2) + I(Anio^2) * Tratamiento + Bloque,
#                      random = ~1|BloqueTratam,
#                      cor = corGaus(form = ~ arbol * Anio),
#                      data = df,
#                      method = "REML",
#                      na.action = na.omit)