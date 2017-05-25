setwd('Documents/Maestria/Applied_Statistics_assignments/MultivariateAnalysis/')
library(gdata)
library(lattice)
library(moments)
library(ICSNP)

df = read.xls('paises.xls')
df$X = NULL
n = nrow(df_n)
p = length(names(df_n))

# ----------------- EJERCICIO 1 -------------------------------------------------------------------------
# Medidas descriptivas y análisis exploratorio:
summary(df)
boxplot(df[,-1], las = 2)
histogram(~ expbser + tcrec + expaltec + impbser + partVAind + Crecpob + Inv, data = df)

numeric_cols = c("expbser", "tcrec", "expaltec", "impbser", "partVAind", "Crecpob", "Inv")
df_n = df[,numeric_cols]

S = var(df_n)
cor(df_n)

m=mahalanobis(df_n, colMeans(df_n), S)
qchi=c()
for (i in 1:n)
  qchi[i]=(i-(1/2))/n
chi=qchisq(q, p) 
gn= cbind(chi,sort(m));gn

plot(gn,pch=19,col = "blue",
     main="Grafico para evaluar Normalidad Multivariada",
     ylim=c(0,40),
     xlab="Chi-square con 7 gl",
     ylab="Distancias de Mahalanobis ordenadas");abline(0,1)



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
# skew(df_n)
# kurtosis(df_n)

Sinv = solve(S)
d = array(0,dim=c(n,n))
means = colMeans(df_n)
for (i in 1:n) 
  for (j in 1:n)   
    d[i,j]=(as.matrix(df_n[i,]-means))%*%Sinv%*%t(as.matrix(df_n[j,]-means))

d3 = d**3
Ap = sum(d3)/(n*n);Ap

App = Ap*(n/6);App
glApp = (p/6)*(p+1)*(p+2);glApp
chiAp = pchisq(App, glApp, ncp=0, lower.tail = FALSE);chiAp

Kp = sum(m**2)/n;Kp
Kpp = (Kp-p*(p+2))/(sqrt((8/n)*p*(p+2)));Kpp
normK = pnorm(Kpp,lower.tail = FALSE)*2;normK


# ----------------- EJERCICIO 3 -------------------------------------------------------------------------

# Prueba de Hotteling para diferencia de medias:
ms_h0 = c(30, 4, 15, 30, 27, 0.8, 20)
# HotellingsT2(df_n, mu = ms_h0)  # Vemos que no se rechaza la hipótesis nula.

T2=n*t(means-ms_h0)%*%Sinv%*%(means-ms_h0)
T2
FCrit=qf(0.95,p,(n-p))
FCrit
T2Crit=((n-1)*p)*FCrit/(n-p)
T2Crit
# No rechazamos la h0.

# Para un IC usar los cuantiles de una t con 25 grados de libertad (df).
qs = qt(c(0.025, 0.975), df=25)
svars = diag(S)

lowerIC = ms_h0 + qs[1] * s/n
upperIC = ms_h0 + qs[2] * s/n

# IC simultáneos:
# Hotelling (T2):
lowerW = means - sqrt(T2Crit*svars/n);lowerW
upperW = means + sqrt(T2Crit*svars/n);upperW

# Bonferroni:
prob=1-(0.05/(2*p))
tc = qt(prob,(n-1))
lowerB = means - tc * sqrt(svars/n);lowerB
upperB = means + tc * sqrt(svars/n);upperB

# ----------------- EJERCICIO 4 -------------------------------------------------------------------------
means = colMeans(df_n)
