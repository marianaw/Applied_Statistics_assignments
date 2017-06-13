setwd('Documents/Maestria/Applied_Statistics_assignments/AdvancedStatisticalModels/')
library(nlme)
library(plyr)

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

# Agregamos la correlación espacial:
# model_espacial = lme(Altura ~ Tratamiento + Anio + I(Anio^2) + I(Anio^2) * Tratamiento + Bloque,
#                      random = ~1|BloqueTratam,
#                      cor = corGaus(form = ~ arbol * Anio),
#                      data = df,
#                      method = "REML",
#                      na.action = na.omit)

me = gls(Altura ~ Tratamiento + Anio + I(Anio^2) + I(Anio^2) * Tratamiento + Bloque,
         correlation = corGaus(form = ~arbol|Anio, nugget = FALSE),
         data = df,
         na.action = na.omit)
summary(me)

variog = Variogram(me, form = ~arbol|Anio, resType = "normalized", maxDist = 30)
# plot(variog, smooth = FALSE, ylim = c(0, 1.3))

df_Anio7 = df[df$Anio == 7,]
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


