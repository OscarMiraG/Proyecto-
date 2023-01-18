datos <- bioinfo
print("AQUI ESTA NEGATI")
print(negati)
con1 <- 1
con2 <- 1
primero <- c()
while(con1 <= limite){
  primero <- c(primero, datos[con1, con2])
  con2 <- con2 + 1
  if(con2 > length(datos)){
    con2 <- 1
    con1 <- con1 + 1
  }
}
con3 <- limite + 1
con4 <- 1
segundo <- c()
while(con3 <= nrow(datos)){
  segundo <- c(segundo, datos[con3, con4])
  con4 <- con4 + 1
  if(con4 > length(datos)){
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
dil <- letters[seq( from = 1, to = length(datos) )]

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
  matriz_orden2 <- as.data.frame(matriz_orden)
  print("YA QUEDO CREO")
  colnames(matriz_orden2) <- c("Abosrbancia", "Muestra", "Dilucion")
  print(matriz_orden2)
  matriz_ordenaov <- matriz_orden2[1:length(primero)+length(segundo), ]
  matriz_ordenaov <- as.data.frame(matriz_ordenaov)
  matriz_ordenaov
  
  
  
  anco <- aov(as.numeric(paste(matriz_ordenaov$Abosrbancia)) ~ unlist(matriz_ordenaov$Muestra) + 
                 unlist(matriz_ordenaov$Dilucion, matriz_ordenaov))
  anco
  anco <- aov(as.numeric(paste(matriz_orden$Absorbancia)) ~ unlist(matriz_orden$Muestra)
              + unlist(matriz_orden$Dilucion), matriz_orden)
  
  summary.aov(anco)
  matriz_orden$Absorbancia
  
  testtukey <- TukeyHSD(anco)
  print(testtukey)
  print("Valor de p obtenido para cada grupo de tu muestra")
  print(testtukey$`unlist(matriz_orden$Muestra)`)
  
  summary.aov(anco)
  matriz_orden$Absorbancia
  
  testtukey <- TukeyHSD(anco)
  print(testtukey)
  print("Valor de p obtenido para cada grupo de tu muestra")
  print(testtukey$`unlist(matriz_orden$Muestra)`)
  
  length(matriz_ordenaov$Abosrbancia)
  length(matriz_ordenaov$Muestra)
  length(matriz_ordenaov$Dilucion)

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
tres <- 
prei <- rep("preinmunizacion", length(datos))
negati4 <- rep("negativo", length(datos))

posi <- matrix(posi, ncol = 1)
posi <- matrix(posi, ncol = 1)
posi <- matrix(posi, ncol = 1)


dilucion <- letters[seq( from = 1, to = length(datos) )]
dilucion2<- rep(dilucion, 3)
dilucion2 <- matrix(dilucion, ncol = 1)

  pro1 <- cbind(promedios3, prei)
  pro2 <- cbind(promedios4, posi)
  pro3 <- cbind(negati, negati4)
  matfinal <- rbind(pro1,pro2, pro3)
  matfinal <-  cbind(matfinal, dilucion)
  colnames(matfinal) <- c("Absorbancia", "Muestra", "Dilucion")
  matfinal <- as.data.frame(t(matfinal))
  
  print("AVER SI JALA ESTOS")
  print(matfinal)
  matfinal2 <- as.data.frame(t(matfinal))
  matfinal2
library(ggplot2)
  plot(as.numeric(paste(matfinal2$Absorbancia)))
grafica <- ggplot(data= matfinal2, aes(x=matfinal2$Dilucion ,y= as.numeric(paste(matfinal2$Absorbancia)), group= matfinal2$Muestra,
                            colour= matfinal2$Muestra)) +
        geom_line()+
        geom_point()
grafica

anco <- aov(as.numeric(paste(matriz_orden$Absorbancia)) ~ unlist(matriz_orden$Muestra)
            + unlist(matriz_orden$Dilucion), matriz_orden)

summary.aov(anco)
matriz_orden$Absorbancia

testtukey <- TukeyHSD(anco)
print(testtukey)
print("Valor de p obtenido para cada grupo de tu muestra")
print(testtukey$`unlist(matriz_orden$Muestra)`)



#########################333
############################ elisa final final
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
    primero <- c(primero, datos[con1, con2])
    con2 <- con2 + 1
    if(con2 > length(datos)){
      con2 <- 1
      con1 <- con1 + 1
    }
  }
  con3 <- limite + 1
  con4 <- 1
  segundo <- c()
  while(con3 <= nrow(datos)){
    segundo <- c(segundo, datos[con3, con4])
    con4 <- con4 + 1
    if(con4 > length(datos)){
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
  dil <- letters[seq( from = 1, to = length(datos) )]
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
    matriz_orden2 <- data.frame(matriz_orden)
    print("YA QUEDO CREO")
    colnames(matriz_orden2) <- c("Abosrbancia", "Muestra", "Dilucion")
    print(matriz_orden2)
    matriz_orden3
    
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
    print(class(matriz_orden))
    matriz_ordenaov <- matriz_orden2[1:length(primero)+length(segundo)+1, ]
    matriz_ordenaov <- as.data.frame(matriz_ordenaov)
    matriz_ordenaov
    ##### analisis estadistico
    anco <- aov(as.numeric(paste(matriz_ordenaov$Abosrbancia)) ~ unlist(matriz_ordenaov$Muestra) + 
                  unlist(matriz_ordenaov$Dilucion, matriz_ordenaov))
    anco
    anco <- aov(as.numeric(paste(matriz_orden$Absorbancia)) ~ unlist(matriz_orden$Muestra)
                + unlist(matriz_orden$Dilucion), matriz_orden)
    
    summary.aov(anco)
    matriz_orden$Absorbancia
    
    testtukey <- TukeyHSD(anco)
    print(testtukey)
    print("Valor de p obtenido para cada grupo de tu muestra")
    print(testtukey$`unlist(matriz_orden$Muestra)`)
    
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
  
  
  dilucion <- letters[seq( from = 1, to = length(datos) )]
  dilucion2<- rep(dilucion, 3)
  dilucion2 <- matrix(dilucion, ncol = 1)
  
  if(inmuni == "pre"){
    pro1 <- cbind(promedios3, prei)
    pro2 <- cbind(promedios4, posi)
    pro3 <- cbind(negati, negati4)
    matfinal <- rbind(pro1,pro2, pro3)
    matfinal <-  cbind(matfinal, dilucion)
    print("AVER SI JALA ESTOS")
    print(matfinal)
    
    ##### grafica
    matfinal2 <- as.data.frame(t(matfinal))
    matfinal2
    colnames(matfinal2) <- c("Absorbancia", "Muestra", "Dilucion")
    grafica <- ggplot(data= matfinal2, aes(x=matfinal2$Dilucion ,y= as.numeric(paste(matfinal2$Absorbancia)), group= matfinal2$Muestra,
                                           colour= matfinal2$Muestra)) +
      geom_line()+
      geom_point()+
      ylab("Absorcion")+
    xlab("Dilucion")+
       labs(color='Muestra') 
    grafica
    
  }else{
    pro1 <- cbind(promedios3, posi)
    pro2 <- cbind(promedios4, prei)
    pro3 <- cbind(negati, negati4)
    matfinal <- rbind(pro1,pro2, pro3) 
    matfinal <-  cbind(matfinal, dilucion)
    print("AVER SI JALA ESTOS")
    print(matfinal)
    
    ##### grafica
    matfinal2 <- as.data.frame(t(matfinal))
    matfinal2
    colnames(matfinal2) <- c("Absorbancia", "Muestra", "Dilucion")
    grafica <- ggplot(data= matfinal2, aes(x=matfinal2$Dilucion ,y= as.numeric(paste(matfinal2$Absorbancia)), group= matfinal2$Muestra,
                                           colour= matfinal2$Muestra)) +
      geom_line()+
      geom_point()
    grafica
    
  }
  
  
}

ELISA4(bioinfo, manual = FALSE)


letters[seq( from = 1, to = 10 )]
datos <- Exponer1
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
matriz_orden
colnames(matriz_orden) <- c("Abosrbancia", "Muestra", "Dilucion")
##### analisis estadistico
matriz_ordenaov <- as.data.frame(matriz_orden)
matriz_ordenaov
aovchido <- aov(as.numeric(paste(matriz_ordenaov$Abosrbancia)) ~ unlist(matriz_ordenaov$Muestra) + 
                  unlist(matriz_ordenaov$Dilucion, matriz_ordenaov))
resultadoaov <- summary.aov(aovchido)
resultado
test <- TukeyHSD(aovchido)
print(test)
print("Valor de p obtenido para cada grupo de tu muestra")
print(test$`unlist(matriz_orden$Muestra)`)




datos <- Exponer1
con1 <- 1
con2 <- 1
primero <- c()
while(con1 <= limite){
  primero <- c(primero, bioinfo[con1, con2])
  con2 <- con2 + 1
  if(con2 == length(bioinfo)){
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
  if(con4 == length(bioinfo)){
    con4 <- 1
    con3 <- con3 + 1
  }
}
primero <- matrix(primero, ncol=1)
segundo <- matrix(primero, ncol= 1)
matriz_orden <- rbind(primero, segundo, negati)
print("PRIMER INTENTO")
print(matriz_orden)
pretxt <- "pre-inmunizado"
postxt <- "pos-inmunizado"
dil <- letters[seq( from = 1, to = 10 )]
  preinmuno <- rep(pretxt, length(primero))
  posinmuno <- rep(postxt, length(segundo))
  preinmuno <- matrix(preinmuno, ncol = 1)
  posinmuno <- matrix(posinmuno, ncol = 1)
  todos <- rbind(posinmuno, preinmuno)
  dil2 <- rep(dil, 3)
  dil2 <- matrix(dil2, ncol=1)
  matriz_orden <- cbind(matriz_orden, todos, dil2)
  matriz_orden <- as.data.frame(matriz_orden)
} else{
  preinmuno <- rep(pretxt, length(segundo))
  posinmuno <- rep(postxt, length(primero))
  preinmuno <- matrix(preinmuno, ncol = 1)
  posinmuno <- matrix(posinmuno, ncol = 1)
  todos <- rbind(posinmuno, preinmuno)
  dil2 <- rep(dil, 3)
  dil2 <- matrix(dil2, ncol=1)
  matriz_orden <- cbind(matriz_orden, todos, dil2)
  matriz_orden <- as.data.frame(matriz_orden)
  print("YA QUEDO CREO")
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
promedios <- rev(promedios)
promedios2 <- rev(promedios2)
matriz <- rbind(promedios, promedios2, negati,pos)
matriz <- as.matrix(matriz)
print("hola3")
print(promedios)
print(promedios2)
print(matriz)
