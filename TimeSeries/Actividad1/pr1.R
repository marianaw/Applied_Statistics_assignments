# Primer práctico de Series de Tiempo.
library(TSA)

setwd('Documents/Maestria/Applied_Statistics_assignments/TimeSeries/Actividad1/')


# ----------------------------------------------------------------
# ---------------------- PROBLEMA 3 ------------------------------
# ----------------------------------------------------------------

y <- rnorm(48,0,1)
plot(y)
tiempo <- seq(1,48,by=1)
plot(tiempo,y)

y_ts <- ts(y)
plot.ts(y_ts)

# Simulamos 30 series de tiempo normales generadas por ruido blanco.
C=30
for(i in c(1:C)){
  y_ts <- ts(rnorm(48, 0, 1))
  plot.ts(y_ts)
}
# Podemos observar que el patrón cambia en cada serie.


# ----------------------------------------------------------------
# ---------------------- PROBLEMA 4 ------------------------------
# ----------------------------------------------------------------

y <- rt(48, df = 5)
plot(y)
tiempo <- seq(1,48,by=1)
plot(tiempo,y)

y_ts <- ts(y)
plot.ts(y_ts)

C=30
for(i in c(1:C)){
  y_ts <- ts(rt(48, df = 5))
  plot.ts(y_ts)
}


# ----------------------------------------------------------------
# ---------------------- PROBLEMA 4 ------------------------------
# ----------------------------------------------------------------

years = 2017 - 1963
months = years * 12 - 6  # Restamos los meses que corresponden a enero-marzo de 1963, y octubre-diciembre de 2017.

ys = ts(rnorm(months),
        frequency = 12,
        start = c(1963, 04),
        end = c(2017, 09))
plot.ts(ys)
