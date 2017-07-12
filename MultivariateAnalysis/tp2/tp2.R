 setwd('Documents/Maestria/AnalisisMultivariado/Unidad2/')
# load('Documents/Maestria/AnalisisMultivariado/Unidad2/indice.rda')

# Definimos la condición como un factor:

indice$CONDICIO = as.factor(indice$CONDICIO)

# Seleccionamos las filas que nos interesan:

df = indice[, -(1:2)]

# ------------------------------------------------------------
# -------------- COMPONENTES PRINCIPALES ---------------------
# ------------------------------------------------------------
# Calculamos las componentes principales:

S = var(df);S
pc = prcomp(S, scale. = TRUE)
summary(pc)

rotation = pc$rotation
par(mfrow=c(1,2))
barplot(rotation[,1]); barplot(rotation[,2]); #barplot(rotation[,3])

# Graficamos las componentes más importantes:
screeplot(pc)
biplot(pc)
biplot(pc, choices = c(1,2))

# Correlación entre las variables originales y las componentes:
Cpc = diag(pc$sdev)
Corr = rotation %*% Cpc

# ------------------------------------------------------------
# Repetimos usando la matriz de correlación. Esta vez usamos otro comando:
# pc_corr = princomp(~AUTOFIN+INMACT+INMPN+LIQACID+PNOCOR+PROPACT+RENTECO+SOLVENC+MAREXP+REXP_INT,
#                    cor = TRUE,
#                    data = df)

C = cor(df)
pc_corr = prcomp(C, scale. = TRUE)
summary(pc_corr)

screeplot(pc_corr)  # La varianza se distribuye de un modo más parejo entre las componentes.

rotation_corr = pc_corr$rotation

par(mfrow=c(2,2))
biplot(pc_corr)
biplot(pc_corr, choices=c(1,2))
biplot(pc_corr, choices=c(1,3))
biplot(pc_corr, choices=c(2,3))
biplot(pc_corr, choices = c(1,4))

# Correlación entre componentes y variables:
C_corr=diag(pc_corr$sdev)
Corr_X_PC=rotation_corr%*%C_corr


# ------------------------------------------------------------
# -------------- ANÁLISIS DE CORRESPONDENCIA -----------------
# ------------------------------------------------------------
library(ca)
x=read.table("/home/mariana/Documents/Maestria/AnalisisMultivariado/Unidad2/eph.dat",header=TRUE)
mod1 = ca(x)

mod3 = mjca(x)
plot.mjca(mod3)
cacoord(mod3) ####extrae las coordenadas
biplot()
names(mod3)
mod3$colnames
rownames(W)=colnames(tabla)
mod3$Burt
mod3$levelnames
correlacion = sqrt(sum(mod3$inertia.e))
biplot(mod3$rowpcoord,mod3$colpcoord,var.axes = TRUE,xlab="dim 1", ylab="dim 2")



# ------------------------------------------------------------
# -------------- ANÁLISIS FACTORIAL --------------------------
# ------------------------------------------------------------
setwd('/home/mariana/Documents/Maestria/Applied_Statistics_assignments/MultivariateAnalysis/tp2/')
library(gdata)
Datos = read.xls('base_factorial.xlsx')
library(psych)

fa <- factanal(Datos[,5:16],factors=2, rotation="none", scores="Bartlett", data=Datos)
fa$converged
fa$loadings
unclass(fa$loadings)

fa_2_vmx <- factanal(Datos[,5:16],factors=2, rotation="varimax", scores="Bartlett", data=Datos)
fa_2_vmx$converged
fa_2_vmx$loadings
unclass(fa_2_vmx$loadings)


fa_7_var <- factanal(Datos[,5:16],factors=7, rotation="varimax", scores="Bartlett", data=Datos)
fa_7_var$converged
fa_7_var$loadings
fa_7_var

fa_5_var <- factanal(Datos[,5:16],factors=3, rotation="varimax", scores="Bartlett", data=Datos)
fa_5_var$converged
fa_5_var$loadings
fa_5_var

fa_2_var <- factanal(Datos[,5:16],factors=2, rotation="varimax", scores="Bartlett", data=Datos)
fa_2_var$converged
fa_2_var$loadings
fa_2_var
