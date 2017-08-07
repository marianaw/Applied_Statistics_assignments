library(lme4)

setwd("Documents/Maestria/Applied_Statistics_assignments/AdvancedStatisticalModels/tp2/")
data = read.csv('GLUCEMIA.csv')

data$prop = data$GlucemiaAlta/data$Total
data$Toma = as.factor(data$Toma)
data$Persona = as.factor(data$Persona)

m1 <- lmer(prop ~ Programa.Alimentacion + Toma + (1|Persona),
           data = data)
summary(m1)

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


# Modelo con un intercepto aleatorio para cada persona (anidar Toma)

# ¿Modelo con tendencia polinomial?

# --------------------------------------------------------------------
# --------------------------- PROBLEMA 2 -----------------------------
# --------------------------------------------------------------------
library(minpack.lm)
library(nlme)

df <- read.csv('TAMBOS.csv')
df$VACANUMERO <- as.factor(df$VACANUMERO)

# Fixed effects model:
f <- nls(LTSLECHE ~ a * DLACT^b * exp(-c * DLACT),
         start = list(a=1, b=0, c=0),
         data = df)

plot(LTSLECHE ~ DLACT, data = df[c(1:1000),])
lines(1:1000, predict(f, newdata = data.frame(DLACT = 1:1000)))

# Model with random effects grouped by cow:
frand <- nlme(model = LTSLECHE ~ a * DLACT^b * exp(-c * DLACT),
              fixed = list(a ~ 1, b~1, c~1),
              random = a ~ 1|VACANUMERO,
              data = df,
              start = c(a=10, b=1, c=-0.001))


plot(LTSLECHE ~ DLACT, data = df[c(1:1000),])
lines(1:1000, predict(frand, newdata = data.frame(DLACT = 1:1000)))

# We compare both models:
anova.lme(f, frand)


# frand <- nlsLM(LTSLECHE ~ wood(DLACT, a, b, c), start = list(a=1, b=0, c=0), data = df)
# wood = deriv(~a * DLACT^b * exp(-c * DLACT), namevec = c('a', 'b', 'c'), function.arg = c('DLACT', 'a', 'b', 'c'))
# fit_wood = nlmer(LTSLECHE ~ wood(DLACT, a, b, c), start = list(nlpars = c(a=1, b=0, c=0)), data = df)

# 
# wood_model <- function(input, a, b, c){
#   a * input^b * exp(-c * input)
# }
