vec <- readline("indique el número total de datos que ingresará: ")
vec <- as.numeric(vec)
vec1 <- c()
while(vec != length(vec1)){
  datardos <- readline("ingrese su dato (desde el inicio de su tabla y de izquierda a derecha)")
  vec1 <- c(vec1, datardos)
}
vec1

jojofu <- function(datos, manual){
  if(manual == FALSE){
    print("kiubo")
    print(datos[1:10, 1:3])
  } else{
    print("me lleva")
  }
}
jojofu(manual = TRUE)


ELISA4 <- function(datos, manual){
  if(manual == FALSE){
    limite <- 0
    limite <- readline("indique el renglón hasta el que llegan los pos o pre-inmunizados: ")
    limite <- as.numeric(limite)
    negati <- c()
    negati <- readline("Indique el valor del control negativo: ")
    negati <- as.numeric(negati)
    negati <- rep(negati, length(datos))
  } else{
    columnas <- readline("indique el número de columnas de su base de datos: ")
    columnas <- as.numeric(columnas)
    vec <- readline("indique el número total de datos que ingresará: ")
    vec <- as.numeric(vec)
    vec1 <- c()
    while(vec != length(vec1)){
      datardos <- readline("ingrese su dato (desde el inicio de su tabla y de izquierda a derecha)")
      vec1 <- c(vec1, datardos)
    }
    vec1 <- as.numeric(vec1)
    datos <- matrix(vec1, ncol= columnas, byrow = TRUE)
    print(datos)
  }
}
ELISA4(manual = TRUE)
datos

x <- 1
if(x == 2){
  print(x)
}
print("kiubo")

k <- readline("kiubo: ")
k <- as.integer(k)
k 

head(ChickWeight)
menos <- ChickWeight[-1, -1]
head(menos)
chik <- drop(ChickWeight[1,1])
chik
n <- 2
nrow(bioinfo)
kiubo <- bioinfo[4, 2 ]

kiubo
kiubo <- matrix(kiubo, ncol = 1 )
kiubo
con1 <- 1
con2 <- 1
n <- 3
primero <- c()
while(con1 <= n){
primero <- c(primero, bioinfo[con1, con2])
con2 <- con2 + 1
if(con2 > length(bioinfo)){
  con2 <- 1
  con1 <- con1 + 1
}
}
length(primero)
primero
print(bioinfo)
primero <- c(primero, bioinfo[1, 2])
primero
primero <- c(primero, bioinfo[2,2])
primero <- matrix(primero, ncol=1)
primero


### Analisis estadistico
promedios3 <- matrix(promedios3, ncol=1)
promedios4 <- matrix(promedios4, ncol=1)
promedios3
negati <- matrix(negati, ncol=1)
datos2 <- cbind(promedios3, promedios4, negati)
datos2 <- as.data.frame(datos2)
print(datos2)
dimension <- dim(datos2)
modelo <-aov(datos2[1:dimension[1], 3]~ datos2[1:dimension[1], 1], datos2)
resultado1 <- summary.aov(modelo)
print(resultado1)
modelo2 <-aov(datos2[1:dimension[1], 3]~ datos2[1:dimension[1], 2], datos2)
resultado2 <- summary.aov(modelo)
print(resultado2)

### Grafica
### indique el nombre de la columna que contiene sus datos


huevos <- matrix(bioinfo[ , 2], ncol=1)
huevos
huevos <- cbind(huevos, bioinfo[ , 1])
huevos


ELISA3(bioinfo)
limite <- 3
promedios <- c()
promedios <- c(promedios, (mean(bioinfo[ limite+1:6, 12])))
promedios

######## pruebas
long <- length(bioinfo)
promedios <- c()
promedios2 <- c()
limite2 <- 3

while(long > 0){
  promedios <- c(promedios, (mean(bioinfo[ 1:limite, long])))
  promedios2 <- c(promedios2, (mean(bioinfo[limite2:6, long])))
  print("hola")
  limite2 <- limite + 1
  long <- long - 1
}

print("hola2")
promedios
promedios <- rev(promedios)
promedios2 <- rev(promedios2)
matriz <- rbind(promedios, promedios2, negati)
matriz <- as.matrix(matriz)
matriz
print("hola3")
print(promedios)
print(promedios2)
print(matriz)
promedios3 <- as.vector(promedios)
promedios4 <- as.vector(promedios2)

negati <- c()
negati <- readline("Indique el valor del control negativo: ")
negati <- as.numeric(negati)
negati <- rep(negati, length(bioinfo))  

### Analisis estadistico
promedios3 <- matrix(promedios3, ncol=1)
promedios4 <- matrix(promedios4, ncol=1)
negati <- matrix(negati, ncol=1)
datos2 <- cbind(promedios3, promedios4, negati)
datos2 <- as.data.frame(datos2)
print(datos2)
dimension <- dim(datos2)
modelo <-aov(datos2[1:dimension[1], 3]~ datos2[1:dimension[1], 1], datos2)
resultado1 <- summary.aov(modelo)
print(resultado1)
modelo2 <-aov(datos2[1:dimension[1], 3]~ datos2[1:dimension[1], 2], datos2)
resultado2 <- summary.aov(modelo)
print(resultado2)

### grafica
matrizgrafica1 <- cbind(promedios3, dilucion, pos)
matrizgrafica1
dilucion <- c(1:length(bioinfo))
dilucion <- as.character(dilucion)
matrizgrafica2 <- cbind(promedios4, dilucion,pre)
matrizgrafica2
matrizgrafica3 <- cbind(negati, dilucion,negati2 )
matrizgrafinal <- rbind(matrizgrafica1, matrizgrafica2, matrizgrafica3)
matrizgrafinal
matrizgrafinal <- as.data.frame(matrizgrafinal)
colnames(matrizgrafinal) <- c("Absorbancia", "Dilucion", "Muestra")
matrizgrafinal
pos <- rep("posinmunizado", length(bioinfo))
pos
pre <- rep("preinmunizado", length(bioinfo))
pre
negati2 <- rep("controlnegativo", length(bioinfo))
negati2

library(Rmisc)
resumen <- summarySE(matrizgrafinal, measurevar = "Absorbancia", groupvars = c("Dilucion", "Muestra"))
resumen
summarySE()



##### tienes que utilizar la base de datos original para el aov y el summary, acomodala como el oscar ademas acuerdate de crear
### dos ifs al inicio para meter tu base de datos y para crear la tuya, los promedios se ocupan para hacer la grafica con ggplot
ELISA4 <- function(datos, manual){
  library(Rmisc)
  library(ggplot2)
  
  if(manual == FALSE){
    limite <- 0
    limite <- readline("indique el renglón hasta el que llegan los pos o pre-inmunizados: ")
    limite <- as.numeric(limite)
    inmuni <- readline("indique si se trata de los pos (pos) o pre-inmunizados (pre)")
    negati <- readline("Indique el valor del control negativo: ")
    negati <- as.numeric(negati)
    negati <- rep(negati, length(datos))
  } else{
    columnas <- readline("indique el número de columnas de su base de datos: ")
    columnas <- as.numeric(columnas)
    vec <- readline("indique el número total de datos que ingresará: ")
    vec <- as.numeric(vec)
    vec1 <- c()
    while(vec != length(vec1)){
      datardos <- readline("ingrese su dato (desde el inicio de su tabla y de izquierda a derecha)")
      vec1 <- c(vec1, datardos)
    }
    vec1 <- as.numeric(vec1)
    datos <- matrix(vec1, ncol= columnas)
    datos <- as.data.frame(datos)
    # pregunta <- readline("¿Estás satisfecho con tu base de datos?y/n")
    #if(pregunta == "n"){
    # pregunta2 <- readline("indique el número de casillas que quiere quitar")
    #pregunta2 <- as.numeric(pregunta2)
    #for(i in 1:pregunta2)
    #seleccionr <- readline("indique el número de renglón que quiere quitar")
    #seleccionc <- readline("indique el número de columna que quiere quitar")
    #seleccionc <- as.numeric(seleccionc); seleccionr <- as.numeric(seleccionr)
    
    #}
    limite <- 0
    limite <- readline("indique el renglón hasta el que llegan los pos o pre-inmunizados: ")
    inmuni <- readline("indique si se trata de los pos (pos) o pre-inmunizados (pre): ")
    limite <- as.numeric(limite)
    negati <- readline("Indique el valor del control negativo: ")
    negati <- as.numeric(negati)
    negati <- rep(negati, length(datos))
  }
  
  #### enderezar matriz
  print("AQUI ESTA NEGATI")
  print(negati)
  con1 <- 1
  con2 <- 1
  primero <- c()
  while(con1 <= limite){
    primero <- c(primero, bioinfo[con1, con2])
    con2 <- con2 + 1
    if(con2 > length(bioinfo)){
      con2 <- 1
      con1 <- con1 + 1
    }
  }
  con3 <- limite + 1
  con4 <- 1
  segundo <- c()
  while(con3 <= nrow(datos)){
    segundo <- c(segundo, bioinfo[con3, con4])
    con4 <- con4 + 1
    if(con4 > length(bioinfo)){
      con4 <- 1
      con3 <- con3 + 1
    }
  }
  primero <- matrix(primero, ncol=1)
  segundo <- matrix(segundo, ncol= 1)
  negati2 <- rep(negati, 1)
  negati2 <- matrix(negati2, ncol = 1)
  matriz_orden <- rbind(primero, segundo, negati2)
  print("PRIMER INTENTO")
  print(matriz_orden)
  pretxt <- "pre-inmunizado"
  postxt <- "pos-inmunizado"
  dil <- c(1:length(datos))
  if(inmuni == "pre"){
    preinmuno <- rep(pretxt, (length(datos)*nrow(datos))/2)
    posinmuno <- rep(postxt, (length(datos)*nrow(datos))/2)
    preinmuno <- matrix(preinmuno, ncol = 1)
    posinmuno <- matrix(posinmuno, ncol = 1)
    negati3 <- rep("negativo", length(negati))
    negati3 <- matrix(negati3, ncol = 1)
    todos <- rbind(posinmuno, preinmuno, negati3)
    dil2 <- rep(dil, nrow(datos)+ 1)
    dil2 <- matrix(dil2, ncol=1)
    #### checkpoint
    print("LONGITUDES")
    print(length(preinmuno))
    print(length(posinmuno))
    print(length(matriz_orden))
    print(length((dil2)))
    print(length(todos))
    matriz_orden <- cbind(matriz_orden, todos, dil2)
    matriz_orden <- as.data.frame(matriz_orden)
    print("YA QUEDO CREO")
    colnames(matriz_orden) <- c("Abosrbancia", "Muestra", "Dilucion")
    print(matriz_orden)
  } else{
    preinmuno <- rep(pretxt, (length(datos)*nrow(datos))/2)
    posinmuno <- rep(postxt, (length(datos)*nrow(datos))/2)
    preinmuno <- matrix(preinmuno, ncol = 1)
    posinmuno <- matrix(posinmuno, ncol = 1)
    negati3 <- rep("negativo", length(negati))
    negati3 <- matrix(negati3, ncol = 1)
    todos <- rbind(posinmuno, preinmuno, negati3)
    dil2 <- rep(dil, nrow(datos)+ 1)
    dil2 <- matrix(dil2, ncol=1)
    matriz_orden <- cbind(matriz_orden, todos, dil2)
    matriz_orden <- as.data.frame(matriz_orden)
    print("YA QUEDO CREO")
    colnames(matriz_orden) <- c("Abosrbancia", "Muestra", "Dilucion")
    print(matriz_orden)
  }
  
  
  #### matriz con los promedios     
  long <- length(datos)
  promedios <- c()
  promedios2 <- c()
  limite2 <- 0
  limite2 <- limite + 1
  while(long > 0){
    promedios <- c(promedios, (mean(datos[ 1:limite, long])))
    promedios2 <- c(promedios2, (mean(datos[limite2:nrow(datos), long])))
    print("hola")
    limite2 <- limite + 1
    long <- long - 1
  }
  
  print("hola2")
  promedios3 <- rev(promedios)
  promedios4 <- rev(promedios2)
  promedios3 <- matrix(promedios3, nrow = length(datos))
  promedios4 <- matrix(promedios4, nrow = length(datos))
  negati <- matrix(negati, nrow = length(datos))
  
  posi <- rep("posinmunizacion", length(datos))
  prei <- rep("preinmunizacion", length(datos))
  negati4 <- rep("negativo", length(datos))
  
  posi <- matrix(posi, ncol = 1)
  posi <- matrix(posi, ncol = 1)
  posi <- matrix(posi, ncol = 1)
  
  dilucion <- rep(1:length(datos), 3)
  dilucion <- matrix(dilucion, ncol = 1)
  
  if(inmuni == "pre"){
    pro1 <- cbind(promedios3, prei)
    pro2 <- cbind(promedios4, posi)
    pro3 <- cbind(negati, negati4)
    matfinal <- rbind(pro1,pro2, pro3)
    matfinal <-  cbind(mat1, dilucion)
    print("AVER SI JALA ESTOS")
    print(mat1)
  }else{
    pro1 <- cbind(promedios3, posi)
    pro2 <- cbind(promedios4, prei)
    pro3 <- cbind(negati, negati4)
    matfinal <- rbind(pro1,pro2, pro3) 
    matfinal <-  cbind(mat1, dilucion)
    print("AVER SI JALA ESTOS")
    print(mat1)
  }
  
  ### Grafica
  ### indique el nombre de la columna que contiene sus datos
  
}


ELISA4(bioinfo, manual = FALSE)
