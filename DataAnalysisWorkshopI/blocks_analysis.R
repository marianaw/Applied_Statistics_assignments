library(plyr)
setwd('/home/mariana/Documents/Maestria/ProyectoDatosI/')

# Cargamos los datos y conservamos aquellos para los que sí hay respuesta.
bbdd = read.csv('BloqueIVyVII_CORREGIDO.csv')
bbdd = bbdd[bbdd$ENTREVISTA.REALIZADA == 'S',]

# Obtenemos los bloques IV y VII:
blVII_names = grep("VII\\.", names(bbdd), value=TRUE)
blVII = bbdd[blVII_names]

blIV_names = grep("IV\\.", names(bbdd), value=TRUE)
blIV = bbdd[blIV_names]

# Hacemos tablas de frecuencia para cada variable:
c = count(blIV, names(blIV)[2])  # Esto nos da la tabla de frecuencias para la segunda variable.
hist(table(c))  # Esto nos da un histograma.
