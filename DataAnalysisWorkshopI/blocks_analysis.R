library(plyr)
setwd('/home/mariana/Documents/Maestria/ProyectoDatosI/')

# Funciones auxiliares:
factorize <- function(element, obs_str, nobs_str){
  if (is.na(element)){
    nobs_str
  }
  else{
    obs_str
  }
}

bin_factorize_col <- function(col, obs_str="O", nobs_str="NO"){
  as.factor(sapply(col, factorize, obs_str=obs_str, nobs_str=nobs_str))
}

plot_hist <- function(col){
  c = count(col)
  c = c[complete.cases(c),]
  c$x = as.factor(c$x)
  vec = as.vector(rep(c$x, c$freq))
  barplot(prop.table(table(vec)))
}

merge_cols <- function(cols){
  cols[is.na(cols)] <- 0
  rowSums(cols)
}



# contingency <- function(col1, col2, name1, name2){  #FIXME!
#   c = cbind(col1, col2)
#   c = c[rowSums(!is.na(v2_v14)) > 0, ]  # Eliminamos filas con sólo nas.
#   names(c) = c(name1, name2)
#   assign("n1", name1)
#   assign(n2, name2)
#   c = transform(c,
#                 n1 = bin_factorize_col(n1),
#                 n2 = bin_factorize_col(n2))
#   table(c)
# }
# -----------------------------------------------------------------

# Cargamos los datos y conservamos aquellos para los que sí hay respuesta.
bbdd = read.csv('BloqueIVyVII_CORREGIDO.csv')
bbdd = bbdd[bbdd$ENTREVISTA.REALIZADA == 'S',]

# Obtenemos los bloques IV y VII:
blVII_names = grep("VII\\.", names(bbdd), value=TRUE)
blVII = bbdd[blVII_names]

blIV_names = grep("IV\\.", names(bbdd), value=TRUE)
blIV = bbdd[blIV_names]


# -----------------------------------------------------------------
# ----------------------------BLOQUE IV----------------------------
# -----------------------------------------------------------------
# Hacemos tablas de frecuencia para la variable que cuenta intento vs.
# robo concreto.
plot_hist(blIV$IV.1.)
# Hay 17 personas que sufrieron un intento de robo y 26 que fueron concretamente
# asaltadas.

# Hacemos tablas de contingencia para verificar que no haya cruzamientos sin
# sentido.
# Cruzamos V.2 y V.16, que si bien son la misma pregunta, como variables deberían
# ser disjuntas.
v2_v14 = blIV[,c("IV.2", "IV.14")]
v2_v14 = v2_v14[rowSums(!is.na(v2_v14)) > 0, ]  # Eliminamos las filas con sólo NAs.
v2_v14 = transform(v2_v14,
                   IV.2 = bin_factorize_col(IV.2),
                   IV.14 = bin_factorize_col(IV.14))
table(v2_v14)
# Observamos que no se intersectan, lo cual es correcto.

# Hay muchas columnas vacías:
empty <- blIV[,colSums(is.na(blIV))==nrow(blIV)]
length(names(empty))
# Hay 31 preguntas no respondidas.

# Vemos un histograma de la frecuencia de violencia física:
plot_hist(blIV$IV.8)
# La respuesta debería ser binaria y sin embargo hay tres niveles.


# -----------------------------------------------------------------
# ----------------------------BLOQUE VII---------------------------
# -----------------------------------------------------------------
# Nuevamente chequeamos que no haya cruzamiento entre dos variables disjuntas.
lugar = blVII[,c("VII.2", "VII.16")]
lugar = lugar[rowSums(!is.na(lugar)) > 0, ]  # Eliminamos las filas con sólo NAs.
lugar = transform(lugar,
                  VII.2 = bin_factorize_col(VII.2),
                  VII.16 = bin_factorize_col(VII.16))
table(lugar)

# Se cruzan en un lugar, lo que implica que una persona respondió dos veces la misma
# pregunta cuando debió haber usado la columna que le correspondía a su experiencia de 
# hurto concretado vs. no concretado.

# Miramos las frecuencias de hurtos de partes concretadas y no concretadas:
plot_hist(blVII$VII.1)
# Las concretadas son más frecuentes.

# Vemos la frecuencia de meses:
plot_hist(blVII$VII.19)

# Contingencias del tipo de robo (mirar esto, no luce bien.)
aux = function(element, s){
  if (!is.na(element)){
    s
  }
  else{
    element
  }
}

f = function(col, s){
  as.factor(sapply(col, aux, s))
}

tipo_robo = blVII[, c("VII.13.1", "VII.13.2", "VII.13.3", "VII.13.4")]
tipo_robo = tipo_robo[rowSums(!is.na(tipo_robo)) > 0, ]
tipo_robo = transform(tipo_robo,
                      VII.13.1 = f(VII.13.1, 'vidrio_roto'),
                      VII.13.2 = f(VII.13.2, 'puerta_forzada'),
                      VII.13.3 = f(VII.13.3, 'alarma_inhibida'),
                      VII.13.4 = f(VII.13.4, 'otro'))
factor_tipo_robo = apply(tipo_robo, 1, function(x) x[!is.na(x)])

cosas_robadas = blVII[, c("VII.14.1", "VII.14.2", "VII.14.3", "VII.14.4")]
cosas_robadas = cosas_robadas[rowSums(!is.na(cosas_robadas)) > 0 ,]
cosas_robadas = transform(cosas_robadas,
                          VII.14.1 = f(VII.14.1, 'ruedas'),
                          VII.14.2 = f(VII.14.2, 'accesorios'),
                          VII.14.3 = f(VII.14.3, 'otros'),
                          VII.14.4 = f(VII.14.4, 'ns/nc'))
factor_cosas_robadas = apply(cosas_robadas, 1, function(x) x[!is.na(x)])
