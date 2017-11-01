library(lme4)
library(lattice)
library(agricolae)
library(multcomp)

setwd("Documents/Maestria/Applied_Statistics_assignments/AdvancedStatisticalModels/tp2/")
data = read.csv('GLUCEMIA.csv')

data$prop = data$GlucemiaAlta/data$Total
data$Toma = as.factor(data$Toma)
data$Persona = as.factor(data$Persona)

# How the data looks like:
g_mosaico = xtabs(~Persona + Toma, data = data)
mosaicplot(g_mosaico, color = T)

xyplot(prop ~ Toma | Persona, 
       data = data,
       fill.color = data$Programa.Alimentacion,
       panel = function(x, y,fill.color,...,subscripts) {
         fill = fill.color [subscripts]
         panel.xyplot(x, y,pch=19, col=fill)
         })


# m1 <- lmer(prop ~ Programa.Alimentacion + Toma + (1|Persona),
#            data = data)
# summary(m1)

# Modelo con Programa de Alimentación como efecto fijo y Persona como efecto aleatorio.
gm1 <- glmer(cbind(GlucemiaAlta, Total - GlucemiaAlta) ~ Programa.Alimentacion + Toma + (1|Persona),
             data = data,
             family = 'binomial')

# c_ijk = B_0i + B_1i prog.alim + b_0k + e_ijk 
gm2 <- glmer(cbind(GlucemiaAlta, Total - GlucemiaAlta) ~ Programa.Alimentacion + (1|Persona),
             data = data,
             family = 'binomial')

gm3 <- glmer(cbind(GlucemiaAlta, Total - GlucemiaAlta) ~ Programa.Alimentacion + (1|Toma) + (1|Persona),
             data = data,
             family = 'binomial')

gm4 <- glmer(cbind(GlucemiaAlta, Total - GlucemiaAlta) ~ Programa.Alimentacion + (1 | Toma/Persona),
             data = data,
             family = 'binomial')

gm5 <- glmer(cbind(GlucemiaAlta, Total - GlucemiaAlta) ~ Programa.Alimentacion + (1 + Toma|Persona),
             data = data,
             family = 'binomial')


# Prueba de diferencia de medias
# lsd = LSD.test(data$prop, data$Programa.Alimentacion, 93, 45.98); lsd  # Esto no tiene sentido.
res = glht(gm1, linfct = mcp(Programa.Alimentacion='Tukey'))
summary(res)

# Modelo con un intercepto aleatorio para cada persona (anidar Toma)

# ¿Modelo con tendencia polinomial?

# --------------------------------------------------------------------
# --------------------------- PROBLEMA 2 -----------------------------
# --------------------------------------------------------------------
library(minpack.lm)
library(nlme)
library(lattice)

df <- read.csv('TAMBOS.csv')
df$VACANUMERO <- as.factor(df$VACANUMERO)

#plots

xyplot(LTSLECHE ~ DLACT|VACANUMERO, data = df[c(1:100),])

xyplot(residuals(f) ~ fitted(f) | df[c(1:100),]$VACANUMERO,
        panel=function(x, y){
        panel.xyplot(x, y)
        panel.loess(x, y, span = 0.75)
        panel.lmline(x, y, lty = 2)  # Least squares broken line
        })


xyplot(residuals(f) ~ fitted(f) | df$TAMBO,
       panel=function(x, y){
         panel.xyplot(x, y)
         panel.loess(x, y, span = 0.75)
         panel.lmline(x, y, lty = 2)  # Least squares broken line
       })

# Fixed effects model:
f <- nls(LTSLECHE ~ a * DLACT^b * exp(-c * DLACT),
         start = list(a=10, b=1, c=0.1),
         data = df)

plot(LTSLECHE ~ DLACT, data = df[c(1:1000),])
lines(1:1000, predict(f, newdata = data.frame(DLACT = 1:1000)))

# Model with random effects grouped by cow:
frand <- nlme(model = LTSLECHE ~ a * DLACT^b * exp(-c * DLACT),
              fixed = list(a ~ 1, b~1, c~1),
              random = a ~ 1|TAMBO,
              data = df,
              start = c(a=10, b=1, c=-0.001))

# Comparison between the two models:
anova.lme(f, frand)
xyplot(residuals(frand) ~ fitted(frand) | df$TAMBO,
       panel=function(x, y){
         panel.xyplot(x, y)
         panel.loess(x, y, span = 0.75)
         panel.lmline(x, y, lty = 2)  # Least squares broken line
       })

# Model with random effects grouped by cow:
frand_tcow <- nlme(model = LTSLECHE ~ a * DLACT^b * exp(-c * DLACT),
              fixed = list(a ~ 1, b~1, c~1),
              random = a ~ 1|TAMBO/VACANUMERO,
              data = df,
              start = c(a=10, b=1, c=-0.001))

# Compare the two last models:
anova.lme(frand, frand_cow)
plot(fitted(frand_cow), residuals(frand_cow))

plot(LTSLECHE ~ DLACT, data = df[c(1:1000),])
lines(1:1000, predict(frand, newdata = data.frame(DLACT = 1:1000)))

# Model with random effects grouped by cow:
frand_cow <- nlme(model = LTSLECHE ~ a * DLACT^b * exp(-c * DLACT),
                  fixed = list(a ~ 1, b~1, c~1),
                  random = a ~ 1|VACANUMERO,
                  data = df,
                  start = c(a=10, b=1, c=-0.001))

# Second part:

a = 14.52
b = 0.219
c = 0.003
D = 305
days <- c(1:D)
re <- random.effects(frand_cow)$a

num_cows = length(re)
predicted = matrix(nrow = num_cows, ncol = D)
for(i in c(1:num_cows)){
  aux <- (a + re[i]) * days^b * exp(-c*days)
  predicted[i,] = aux
}

accumulated = apply(predicted, 1, sum)
lt = aggregate(df$Litrostotales, by = list(df$VACANUMERO), mean)
acc_pred = as.data.frame(cbind(accumulated, lt$x, re))
names(acc_pred) = c('Accumulated_prediction', 'Litrostotales', 'RE')
acc_pred = acc_pred[order(acc_pred$Accumulated_prediction),]

accumulated = acc_pred$Accumulated_prediction
original = acc_pred$Litrostotales

# Intervalos:

normal_q = 1.96
sd = 2.5941  # Estimated standard deviation for random effect a ~ 1|VACANUMERO.
q = sd * normal_q

low_cut = length(which(re < -q))
middle_cut = length(which(re < 0))  # Because random effects are assumed to have a centered normal distribution.
upper_cut = length(which(re < q))

original_2_5 = original[c(1:low_cut)]
q_2_5 = accumulated[c(1:low_cut)]
cor(original_2_5, q_2_5)

original_5 = original[c(1:middle_cut)]
q_5 = accumulated[c(1:middle_cut)]
cor(original_5, q_5)

original_97_5 = original[c(1:upper_cut)]
q_97_5 = accumulated[c(1:upper_cut)]
cor(original_97_5, q_97_5)


# frand <- nlsLM(LTSLECHE ~ wood(DLACT, a, b, c), start = list(a=1, b=0, c=0), data = df)
# wood = deriv(~a * DLACT^b * exp(-c * DLACT), namevec = c('a', 'b', 'c'), function.arg = c('DLACT', 'a', 'b', 'c'))
# fit_wood = nlmer(LTSLECHE ~ wood(DLACT, a, b, c), start = list(nlpars = c(a=1, b=0, c=0)), data = df)

# 
# wood_model <- function(input, a, b, c){
#   a * input^b * exp(-c * input)
# }
