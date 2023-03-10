
### ESTA ES LA VERSION CHIDA ###


Datosos2 <- read.csv ("Datos/Datos_Proyecto_2.csv")
Datosos2

Datosos3 <- read.csv("Datos/Exponer1.csv")

Datosos4 <- read.csv("Datos/Exponer2.csv")
print (Datosos4)

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
  Promedios [12,1] <- (x[1,12] + x [2,12] + x [3,12])/3
  
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
  
  print ("Acontinuación se te muestra una tabla con los promedios de tus 3 replicas para cada dilucion:")
  print (Promedios)
  
  print(paste ("Valor del control negativo: ",Control_Negativo))
  
  print (paste ("Valor del control positivo: ", Control_Positivo))
  
  print (paste ("Valor del conjugado: ", Conjugado))
  
  Promedios2 <- matrix (nrow = 120, ncol = 3)
  colnames(Promedios2) <- c ("Absorbancia", "Muestra", "Dilucion")
  
  Promedios2 <- data.frame(Promedios2)
  
  Promedios2 [1:36,1] <- c (x [4,1], x [5,1], x [6,1], x [4,2], x [5,2], x [6,2], x[4,3], x [5,3],
                            x [6,3], x [4,4], x [5,4], x [6,4], x [4,5], x [5,5], x [6,5], x [4,6], 
                            x [5,6], x [6,6], x [4,7], x [5,7], x [6,7], x [4,8], x [5,8], x [6,8],
                            x [4,9], x [5,9], x [6,9], x [4,10], x [5,10], x [6,10], x [4,11], x [5,11], 
                            x [6,11], x [4,12], x [5,12], x [6,12])
  
  Promedios2 [37:72,1] <- c (x[1,1], x [2,1], x [3,1], x[1,2], x [2,2], x [3,2], x[1,3], x [3,3], x [3,3], 
                             x[1,4], x [2,4], x [3,4], x[1,5], x [2,5], x [3,5], x[1,6], x [2,6], x [3,6], 
                             x[1,7], x [2,7], x [3,7], x[1,8], x [2,8], x [3,8], x[1,9], x [2,9], x [3,9],
                             x[1,10], x [2,10], x [3,10], x[1,11], x [2,11], x [3,11], x[1,12], x [2,12], x [3,12])
  
  Promedios2 [73:120,1] <- c (x[7,3], x [7,4], x [8,3], x [8,4])
  
  
  
  Promedios2 [1:120,3] <- c ("A", "A", "A", 
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
                            "A", "A", "A", 
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
  
  for (i in 1:36){
    Frase <- "Preinmunizado"
    Promedios2 [37:72,2] <- Frase
    
  }
  
  for (i in 1:47){
    Frase2 <- "ControlNegativo"
    Promedios2 [73:120,2] <- Frase2
  }
  
  Resumen <- summarySE (Promedios2, measurevar = "Absorbancia", groupvars = c ("Dilucion", "Muestra"))
  
  pd <- position_dodge(0.1)
  Grafica_Perrona <- ggplot(data= Resumen, aes(x = Dilucion, y = Absorbancia, group = Muestra, colour=Muestra)) +
    geom_errorbar(aes(ymin = Absorbancia - se, ymax = Absorbancia + se), width=.1, position=pd) +
    geom_line(position=pd) +
    geom_point(position=pd)
  
  print (Grafica_Perrona)
  
  ANOVA <- aov (Absorbancia ~ Dilucion + Muestra, Promedios2)
  
  Resultado <- summary.aov(ANOVA)
  
  print ("Se muestra el resultado de la prueba estadística ANOVA para comparar tus grupos")
  print (Resultado)
  
  Tukey <- TukeyHSD(ANOVA)
  
  print ("Valor de p obtenido para cada grupo de tu muestra")
  print (Tukey$Muestra)
  
 
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
  Promedios [3,1] <- (x[1,3] + x [2,3])/2
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
  
  Promedios2 <- matrix (nrow = 96, ncol = 3)
  colnames(Promedios2) <- c ("Absorbancia", "Muestra", "Dilucion")
  
  Promedios2 <- data.frame(Promedios2)
  
  Promedios2 [1:24,1] <- c (x [3,1], x [4,1], x [3,2], x [4,2], x[3,3], x [4,3],
                            x [3,4], x [4,4], x [3,5], x [4,5], x [3,6], 
                            x [4,6], x [3,7], x [4,7], x [3,8], x [4,8],
                            x [3,9], x [4,9], x [3,10], x [4,10], x [3,11], x [4,11], 
                            x [3,12], x [4,12])
  
  Promedios2 [25:48, 1] <- c (x[1,1], x [2,1], x[1,2], x [2,2], x[1,3], x [2,3], x[1,4], x [2,4], 
                              x[1,5], x [2,5], x[1,6], x [2,6], x[1,7], x [2,7], x[1,8], x [2,8], 
                              x[1,9], x [2,9], x[1,10], x [2,10], x[1,11], x [2,11], x[1,11], x [2,11])
  
  Promedios2 [49:96,1] <- c (x[5,3], x [5,4], x [6,3], x [6,4])
  
  
  
  Promedios2 [1:96,3] <- c ("A", "A", 
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
                            "A", "A", 
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
  
 
  for (i in 1:36){
    Frase2 <- "PostInmunizado"
    Promedios2 [1:24,2] <- Frase2
    
  }
  
  for (i in 1:36){
    Frase2 <- "Preinmunizado"
    Promedios2 [25:48,2] <- Frase2
    
  }
  
  for (i in 1:47){
    Frase2 <- "ControlNegativo"
    Promedios2 [49:96,2] <- Frase2
  }
  
  Resumen <- summarySE (Promedios2, measurevar = "Absorbancia", groupvars = c ("Dilucion", "Muestra"))
  
  pd <- position_dodge(0.1)
  Grafica_Perrona <- ggplot(data= Resumen, aes(x = Dilucion, y = Absorbancia, group = Muestra, colour=Muestra)) +
    geom_errorbar(aes(ymin = Absorbancia - se, ymax = Absorbancia + se), width=.1, position=pd) +
    geom_line(position=pd) +
    geom_point(position=pd)
  
  print (Grafica_Perrona)
  
  ANOVA2 <- aov (Absorbancia ~ Dilucion + Muestra, Promedios2)
  
  Resultado2 <- summary.aov(ANOVA2)
  
  print (Resultado2)
  
  Tukey2 <- TukeyHSD(ANOVA2)
  
  print (Tukey2$Muestra)

 } 
}



ELISA(Datosos2)
ELISA(Datosos3)
ELISA(Datosos4)















    

