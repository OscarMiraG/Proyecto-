
### ESTA ES LA VERSION CHIDA ###

Datosos <- read.csv("Datos/Datos_Proyecto.csv")
Datosos 

Datosos2 <- read.csv ("Datos/Datos_Proyecto_2.csv")
Datosos2

ELISA <- function(x){
  numero <- readline(prompt = "Cuántas replicas se realizaron?: ")
  as.numeric(numero)
  if (numero == 3){
  library (ggplot2)
  library (Rmisc)
  Promedios <- matrix (nrow = 12, ncol = 2)
  colnames(Promedios) <- c ("Preinmunizado","Postinmunzado")
  row.names(Promedios) <- c ("1:500", "1:100", "1:2000", "1:4000", "1:8000", 
                             "1;16000", "1:32000", "1:64000", "1:128000", 
                             "1:256000", "1:512000", "1:1024000")
  
  
  Promedios [1,1] <- (x[1,1] + x [2,1] + x [3,1])/3
  Promedios [2,1] <- (x[1,2] + x [2,2] + x [3,2])/3
  Promedios [3,1] <- (x[1,3] + x [3,3] + x [3,3])/3
  Promedios [4,1] <- (x[1,4] + x [2,4] + x [3,4])/3
  Promedios [5,1] <- (x[1,5] + x [2,5] + x [3,5])/3
  Promedios [6,1] <- (x[1,6] + x [2,6] + x [3,6])/3
  Promedios [7,1] <- (x[1,7] + x [2,7] + x [3,7])/3
  Promedios [8,1] <- (x[1,8] + x [2,8] + x [3,8])/3
  Promedios [9,1] <- (x[1,9] + x [2,9] + x [3,9])/3
  Promedios [10,1] <- (x[1,10] + x [2,10] + x [3,10])/3
  Promedios [11,1] <- (x[1,11] + x [2,11] + x [3,11])/3
  Promedios [12,1] <- (x[1,11] + x [2,11] + x [3,12])/3
  
  Promedios [1,2] <- (x[4,1] + x [5,1] + x [6,1])/3
  Promedios [2,2] <- (x[4,2] + x [5,2] + x [6,2])/3
  Promedios [3,2] <- (x[4,3] + x [5,3] + x [6,3])/3
  Promedios [4,2] <- (x[4,4] + x [5,4] + x [6,4])/3
  Promedios [5,2] <- (x[4,5] + x [5,5] + x [6,5])/3
  Promedios [6,2] <- (x[4,6] + x [5,6] + x [6,6])/3
  Promedios [7,2] <- (x[4,7] + x [5,7] + x [6,7])/3
  Promedios [8,2] <- (x[4,8] + x [5,8] + x [6,8])/3
  Promedios [9,2] <- (x[4,9] + x [5,9] + x [6,9])/3
  Promedios [10,2] <- (x[4,10] + x [5,10] + x [6,10])/3
  Promedios [11,2] <- (x[4,11] + x [5,11] + x [6,11])/3
  Promedios [12,2] <- (x[4,12] + x [5,12] + x [6,12])/3
  
  Control_Positivo <- (x[7,1] + x [7,2] + x [8,1] + x [8,2])/4
  Control_Negativo <- (x[7,3] + x [7,4] + x [8,3] + x [8,4])/4
  Conjugado <- (x[7,5] + x [7,6] + x [8,5] + x [8,6])/4
  
  print (Promedios)
  print (Control_Negativo)
  print (Control_Positivo)
  print (Conjugado)
  
  Promedios2 <- matrix (nrow = 84, ncol = 3)
  colnames(Promedios2) <- c ("Absorbancia", "Muestra", "Dilucion")
  
  Promedios2 <- data.frame(Promedios2)
  
  Promedios2 [1:36,1] <- c (x [4,1], x [5,1], x [6,1], x [4,2], x [5,2], x [6,2], x[4,3], x [5,3],
                            x [6,3], x [4,4], x [5,4], x [6,4], x [4,5], x [5,5], x [6,5], x [4,6], 
                            x [5,6], x [6,6], x [4,7], x [5,7], x [6,7], x [4,8], x [5,8], x [6,8],
                            x [4,9], x [5,9], x [6,9], x [4,10], x [5,10], x [6,10], x [4,11], x [5,11], 
                            x [6,11], x [4,12], x [5,12], x [6,12])
  
  Promedios2 [37:84,1] <- c (x[7,3], x [7,4], x [8,3], x [8,4])
  
  
  
  Promedios2 [1:84,3] <- c ("A", "A", "A", 
                            "B", "B", "B",
                            "C", "C", "C",
                            "D", "D", "D", 
                            "E", "E", "E", 
                            "F", "F", "F", 
                            "G", "G", "G",
                            "H", "H", "H",
                            "I",  "I",  "I", 
                            "J", "J", "J",
                            "K", "K", "K", 
                            "L", "L", "L", 
                            "A", "A", "A", "A", 
                            "B", "B", "B", "B",
                            "C", "C", "C", "C",
                            "D", "D", "D", "D", 
                            "E", "E", "E", "E", 
                            "F", "F", "F", "F", 
                            "G", "G", "G", "G",
                            "H", "H", "H", "H",
                            "I",  "I",  "I", "I", 
                            "J", "J", "J", "J",
                            "K", "K", "K", "K", 
                            "L", "L", "L", "L")
  
  for (i in 1:36){
    Frase <- "PostInmunizado"
    Promedios2 [1:36,2] <- Frase
    
  }
  
  for (i in 1:47){
    Frase2 <- "ControlNegativo"
    Promedios2 [37:84,2] <- Frase2
  }
  
  print (Promedios2)
  
  Resumen <- summarySE (Promedios2, measurevar = "Absorbancia", groupvars = c ("Dilucion", "Muestra"))
  
  print (Resumen)
  
  pd <- position_dodge(0.1)
  Grafica_Perrona <- ggplot(data= Resumen, aes(x = Dilucion, y = Absorbancia, group = Muestra, colour=Muestra)) +
    geom_errorbar(aes(ymin = Absorbancia - se, ymax = Absorbancia + se), width=.1, position=pd) +
    geom_line(position=pd) +
    geom_point(position=pd)
  
  print (Grafica_Perrona)
}else if (numero == 2){
  library (ggplot2)
  library (Rmisc)
  Promedios <- matrix (nrow = 12, ncol = 2)
  colnames(Promedios) <- c ("Preinmunizado","Postinmunzado")
  row.names(Promedios) <- c ("1:500", "1:100", "1:2000", "1:4000", "1:8000", 
                             "1;16000", "1:32000", "1:64000", "1:128000", 
                             "1:256000", "1:512000", "1:1024000")
  
  
  Promedios [1,1] <- (x[1,1] + x [2,1])/2
  Promedios [2,1] <- (x[1,2] + x [2,2])/2
  Promedios [3,1] <- (x[1,3] + x [3,3])/2
  Promedios [4,1] <- (x[1,4] + x [2,4])/2
  Promedios [5,1] <- (x[1,5] + x [2,5])/2
  Promedios [6,1] <- (x[1,6] + x [2,6])/2
  Promedios [7,1] <- (x[1,7] + x [2,7])/2
  Promedios [8,1] <- (x[1,8] + x [2,8])/2
  Promedios [9,1] <- (x[1,9] + x [2,9])/2
  Promedios [10,1] <- (x[1,10] + x [2,10])/2
  Promedios [11,1] <- (x[1,11] + x [2,11])/2
  Promedios [12,1] <- (x[1,11] + x [2,11])/2
  
  Promedios [1,2] <- (x[3,1] + x [4,1])/2
  Promedios [2,2] <- (x[3,2] + x [4,2])/2
  Promedios [3,2] <- (x[3,3] + x [4,3])/2
  Promedios [4,2] <- (x[3,4] + x [4,4])/2
  Promedios [5,2] <- (x[3,5] + x [4,5])/2
  Promedios [6,2] <- (x[3,6] + x [4,6])/2
  Promedios [7,2] <- (x[3,7] + x [4,7])/2
  Promedios [8,2] <- (x[3,8] + x [4,8])/2
  Promedios [9,2] <- (x[3,9] + x [4,9])/2
  Promedios [10,2] <- (x[3,10] + x [4,10])/2
  Promedios [11,2] <- (x[3,11] + x [4,11])/2
  Promedios [12,2] <- (x[3,12] + x [4,12])/2
  
  Control_Positivo <- (x[5,1] + x [5,2] + x [6,1] + x [6,2])/4
  Control_Negativo <- (x[5,3] + x [5,4] + x [6,3] + x [6,4])/4
  Conjugado <- (x[5,5] + x [5,6] + x [6,5] + x [6,6])/4
  
  print (Promedios)
  print (Control_Negativo)
  print (Control_Positivo)
  print (Conjugado)
  
  Promedios2 <- matrix (nrow = 72, ncol = 3)
  colnames(Promedios2) <- c ("Absorbancia", "Muestra", "Dilucion")
  
  Promedios2 <- data.frame(Promedios2)
  
  Promedios2 [1:24,1] <- c (x [3,1], x [4,1], x [3,2], x [4,2], x[3,3], x [4,3],
                            x [3,4], x [4,4], x [3,5], x [4,5], x [3,6], 
                            x [4,6], x [3,7], x [4,7], x [3,8], x [4,8],
                            x [3,9], x [4,9], x [3,10], x [4,10], x [3,11], x [4,11], 
                            x [3,12], x [4,12])
  
  Promedios2 [25:72,1] <- c (x[5,3], x [5,4], x [6,3], x [6,4])
  
  
  
  Promedios2 [1:72,3] <- c ("A", "A", 
                            "B", "B",
                            "C", "C",
                            "D", "D", 
                            "E", "E", 
                            "F", "F", 
                            "G", "G",
                            "H", "H",
                            "I",  "I", 
                            "J", "J",
                            "K", "K", 
                            "L", "L", 
                            "A", "A", "A", "A", 
                            "B", "B", "B", "B",
                            "C", "C", "C", "C",
                            "D", "D", "D", "D", 
                            "E", "E", "E", "E", 
                            "F", "F", "F", "F", 
                            "G", "G", "G", "G",
                            "H", "H", "H", "H",
                            "I",  "I",  "I", "I", 
                            "J", "J", "J", "J",
                            "K", "K", "K", "K", 
                            "L", "L", "L", "L")
  
  for (i in 1:24){
    Frase <- "PostInmunizado"
    Promedios2 [1:36,2] <- Frase
    
  }
  
  for (i in 1:47){
    Frase2 <- "ControlNegativo"
    Promedios2 [25:72,2] <- Frase2
  }
  
  print (Promedios2)
  
  Resumen <- summarySE (Promedios2, measurevar = "Absorbancia", groupvars = c ("Dilucion", "Muestra"))
  
  print (Resumen)
  
  pd <- position_dodge(0.1)
  Grafica_Perrona <- ggplot(data= Resumen, aes(x = Dilucion, y = Absorbancia, group = Muestra, colour=Muestra)) +
    geom_errorbar(aes(ymin = Absorbancia - se, ymax = Absorbancia + se), width=.1, position=pd) +
    geom_line(position=pd) +
    geom_point(position=pd)
  
  print (Grafica_Perrona)
 } 
}





ELISA(Datosos)
















    attach(Datosos)
    
    ############PROPUESTA

    ELISA2 <- function(datos, controlneg, controlpos, coadyuv){
      #### ESTO ES POR SI QUEREMOS FLEXEAR, TODAVIA NO ESTA LISTO
      #dec <- readline(prompt = "¿Quieres armar tu propia base de datos?/n/y")
      #nombres <- readline("¿Quieres ponerle nombres a las columnas y renglones?/n/y")
      #if(dec == "y"){
        #datardos <- readline("Ingrese sus datos: ")
        #colum <- readline("cuantas columnas tendra tu tabla: ")
        #reng <- readline("cuantos renglones tendra tu tabla: ")
        #datos <- as.numeric(datardos); colum <- as.numeric(colum); reng <- as.numeric(reng)
        #basedatos <- matrix(datardos,nrow = reng, ncol = colum, byrow = TRUE)
        #}else if(nombres == "y"){
          #nombresrow <- c()
          #nombrescol <- c()
          #basedatos <- colnames(nombrescol)
          #basedatos <- rownames(nombresrow)
        #} else{ break;}
      #}
      controlneg <- as.numeric(controlneg);controlpos <- as.numeric(controlpos);
      coadyuv <- as.numeric(coadyuv)
      long <- length(datos)
      promedios <- c()
      while(long > 0){
        promedios <- c(promedios, (mean(datos[ , long])))
        print("hola")
        long <- long - 1
      }
      promedios <- rev(promedios)
      print(promedios)
      
      ### graficas
      library(Rmisc)
      library(ggplot2)
      
    }
ELISA2(datos = bioinfo, controlneg = c(1,3,4), controlpos = c(23,33,2), coadyuv = c(1,23,3))    
promedios
long
ELISA2(datos = bioinfo)
ELISA2()


ELISA3 <- function(datos){
  negati <-c()
  pos <- c()
  limite <- 0
  limite <- readline("indique el renglón hasta el que llegan los pos o pre-inmunizados: ")
  limite <- as.numeric(limite)
  
  pregunta <- readline("Quieres poner los datos de tus controles de forma manual?/y/n")
  pos <- c()
  negati <- c()
  if(pregunta == "y"){
  for (i in 1:length(datos)){
  negati <- readline("Indique los valores del control negativo: ")
  negati <- as.numeric(negati)
  }
  for (i in 1:length(datos)){
    pos <- readline("Indique los valores del control positivo: ")
    pos <- as.numeric(pos)
  }
  } else{
    negati <- readline("Indique el valor del control negativo: ")
    pos <- readline("Indique el valor del control positivo: ")
    negati <- as.numeric(negati)
    pos <- as.numeric(pos)
    negati <- rep(negati, length(datos))  
    pos <- rep(pos, length(datos)) 
  }
  negati <- negati/length(datos)
  
  pos <- pos/length(datos)
  
  
  long <- length(datos)
  promedios <- c()
  promedios2 <- c()
  
  while(long > 0){
    promedios <- c(promedios, (mean(datos[ limite, long])))
    promedios2 <- c(promedios2, (mean(datos[limite + 1, long])))
    print("hola")
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
  promedios3 <- as.vector(promedios)
  promedios4 <- as.vector(promedios2)
  
  ### Analisis estadistico
  promedios3 <- matrix(promedios3, ncol=1)
  promedios4 <- matrix(promedios4, ncol=1)
  promedios5 <- rbind(promedios3, promedios4)
  negati <- matrix(negati, ncol=1)
  datos2 <- cbind(datos, promedios3, negati)
  datos2 <- as.data.frame(datos2)
  print(datos2)
  modelo <-lm(datos2[limite, length(datos)+1]~ datos2[limite +1, length(datos)+2], datos2)
  resultado <- summary(modelo)
  
  print(resultado)
  }
  

ELISA3(bioinfo)
negati2 <- c()
negati2 <- readline("Indique el valor del control negativo: ")
negati2 <- as.numeric(negati2)
negati2 <- rep(negati2, times= length(bioinfo))
vec1 <- c(1,2,3,4)
vec2 <- c(5,6,7,8)
vec8 <- matrix(vec1, ncol=1)
vec8
vec3 <- matrix(vec1,nrow = length(vec1), ncol =1)
vec4 <- matrix(vec2,nrow = length(vec2), ncol =1)
vec3
vec4
vec5 <- rbind(vec3,vec4)
vec5
negati2 
lengt
vec6 <- matrix(vec1, nrow= length(vec1), ncol =1)

data3 <- cbind(vec3,vec4,vec6)
data3
k <- cbind(data3, vec1)
k <- as.data.frame(k)
k
vec6 <- vec6/length(vec1)
vec6

<<<<<<< Updated upstream
ELISA3 <- function(datos){
  negati <-c()
  pos <- c()
  limite <- 0
  limite <- readline("indique el renglón hasta el que llegan los pos o pre-inmunizados: ")
  limite <- as.numeric(limite)
  
  pregunta <- readline("Quieres poner los datos de tus controles de forma manual?/y/n")
  pos <- c()
  negati <- c()
  if(pregunta == "y"){
    for (i in 1:length(datos)){
      negati <- readline("Indique los valores del control negativo: ")
      negati <- as.numeric(negati)
    }
    for (i in 1:length(datos)){
      pos <- readline("Indique los valores del control positivo: ")
      pos <- as.numeric(pos)
    }
  } else{
    negati <- readline("Indique el valor del control negativo: ")
    pos <- readline("Indique el valor del control positivo: ")
    negati <- as.numeric(negati)
    pos <- as.numeric(pos)
    negati <- rep(negati, length(datos))  
    pos <- rep(pos, length(datos)) 
  }
  negati <- negati/length(datos)
  
  pos <- pos/length(datos)
  
  
  long <- length(datos)
  promedios <- c()
  promedios2 <- c()
  
  while(long > 0){
    promedios <- c(promedios, (mean(datos[ limite, long])))
    promedios2 <- c(promedios2, (mean(datos[limite + 1, long])))
    print("hola")
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
  promedios3 <- as.vector(promedios)
  promedios4 <- as.vector(promedios2)
  
  ### Analisis estadistico
  promedios3 <- matrix(promedios3, ncol=1)
  promedios4 <- matrix(promedios4, ncol=1)
  promedios5 <- rbind(promedios3, promedios4)
  negati <- matrix(negati, ncol=1)
  datos2 <- cbind(datos, promedios3, negati)
  datos2 <- as.data.frame(datos2)
  print(datos2)
  modelo <-lm(datos2[limite, length(datos)+1]~ datos2[limite +1, length(datos)+2], datos2)
  resultado <- summary(modelo)
  
  print(resultado)
  
  ### Grafica
  ### indique el nombre de la columna que contiene sus datos
 
  
  print (Grafica_Perrona)
  pregunta2 <- readline("Quiere crear una gráfica con los datos de su tabla?/y/n ")
  pregunta3 <- readline("Quiere que las imagenes se guarden como pdf?/y/n  ")
  
  if(pregunta2 == "y" & pregunta3 == "y"){
    ruta <- readline("Indique la ruta donde quiere guardar su gráfica: ")
    nombre <- readline("Indique el nombre de su gráfica: ")
    ancho <- readline("Indique el ancho que quiere para su imagen: ")
    alto <- readline("Indique el ancho que quiere para su imagen: ")
    setwd(ruta)
    
    y <- readline("Indique el nombre de la columna que contiene los datos para la variable y: ")
    x <- readline("Indique el nombre de la columna que contiene los datos para la variable x: ")
    g <- readline("Indique el nombre de la variable de agrupamiento: ")
    y2 <- subset(datos, select= y)
    x2 <- subset(datos, select= x)
    g2 <- subset(datos, select= g)
    pd <- position_dodge(0.1)
    
    Grafica_Perrona <- ggplot(data= datos, aes(x = x2, y = y2, group = g2, colour=g2)) +
      geom_errorbar(aes(ymin = Absorbancia - se, ymax = Absorbancia + se), width=.1, position=pd) +
      geom_line(position=pd) +
      geom_point(position=pd)
    pdf(file = nombre,
        width = ancho,
        height = alto)
    dev.off()
  } else if(pregunta2 == "y"){
    y <- readline("Indique el nombre de la columna que contiene los datos para la variable y: ")
    x <- readline("Indique el nombre de la columna que contiene los datos para la variable x: ")
    g <- readline("Indique el nombre de la variable de agrupamiento: ")
    y2 <- subset(datos, select= y)
    x2 <- subset(datos, select= x)
    g2 <- subset(datos, select= g)
    pd <- position_dodge(0.1)
    
    Grafica_Perrona <- ggplot(data= datos, aes(x = x2, y = y2, group = g2, colour=g2)) +
      geom_errorbar(aes(ymin = Absorbancia - se, ymax = Absorbancia + se), width=.1, position=pd) +
      geom_line(position=pd) +
      geom_point(position=pd)
    print(Grafica_Perrona)
  }
}
ELISA3(bioinfo)
=======
model <- lm(iris[ , 1]~ iris[ ,2], iris)
summary(model)
>>>>>>> Stashed changes
