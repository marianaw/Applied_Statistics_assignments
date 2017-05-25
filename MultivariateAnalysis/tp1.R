setwd('Documents/Maestria/Applied_Statistics_assignments/MultivariateAnalysis/')
library(gdata)
library(lattice)
library(moments)

df = read.xls('paises.xls')
df$X = NULL

# ----------------- EJERCICIO 1 -------------------------------------------------------------------------
# Medidas descriptivas y análisis exploratorio:
summary(df)
boxplot(df[,-1], las = 2)
histogram(~ expbser + tcrec + expaltec + impbser + partVAind + Crecpob + Inv, data = df)

numeric_cols = c("expbser", "tcrec", "expaltec", "impbser", "partVAind", "Crecpob", "Inv")
df_n = df[,numeric_cols]

Vdf_n = var(df_n)
cor(df_n)

m=mahalanobis(df_n, colMeans(df_n), Vdf_n)
mah_dist = cbind(as.vector(df$paises), m)

# ----------------- EJERCICIO 2 -------------------------------------------------------------------------
# Prueba de hipótesis de normalidad y pruebas gráficas.

# QQ-plots para las pruebas gráficas univariadas:
aux_qq = function(x){
  qqnorm(x)
  qqline(x)
}

apply(df_n, 2, aux_qq)

# Test de Shapiro-Wilks:
apply(df_n, 2, shapiro.test)

# Prueba multivariada gráfica de normalidad:
aux_qq(m)

# Asimetría y Kurtosis:
skew(df_n)
kurtosis(df_n)

# ----------------- EJERCICIO 3 -------------------------------------------------------------------------

# Prueba de Hotteling para diferencia de medias:
ms_h0 = c(30, 4, 15, 30, 27, 0.8, 20)
HotellingsT2(df_n, mu = ms_h0)  # Vemos que no se rechaza la hipótesis nula.


