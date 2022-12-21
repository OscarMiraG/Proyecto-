Datosos <- read.csv("Datos/Datos_Proyecto.csv")
Datosos 

ELISA <- function(x){
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
  
  Control_Negativo <- (x[7,1] + x [7,2] + x [8,1] + x [8,2])/4
  Control_Positivo <- (x[7,3] + x [7,4] + x [8,3] + x [8,4])/4
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
  
  Promedios2 [37:84,1] <- c (x[7,1], x [7,2], x [8,1], x [8,2])
  
  
  Promedios2 [1:84,3] <- c ("1:500", "1:500", "1:500", 
                            "1:1000", "1:1000","1:1000",
                            "1:2000", "1:2000","1:2000",
                            "1:4000", "1:4000", "1:4000", 
                            "1:8000", "1:8000", "1:8000", 
                            "1:16000", "1:16000", "1:16000", 
                            "1:32000", "1:32000", "1:32000",
                            1-64000,  1-64000,  1-64000, 
                            1-128000, 1-128000,1-128000,
                            1-256000, 1-256000, 1-256000, 
                            1-512000, 1-512000, 1-512000, 
                            1-1024000, 1-1024000, 1-1024000,
                            1-500, 1-500, 1-500, 1-500,
                            1-1000, 1-1000,1-1000,1-1000,
                            1-2000, 1-2000,1-2000,1-2000,
                            1-4000, 1-4000, 1-4000,  1-4000,
                            1-8000, 1-8000, 1-8000, 1-8000,
                            1-16000, 1-16000, 1-16000,  1-16000,
                            1-32000, 1-32000,1-32000, 1-32000,
                            1-64000,  1-64000,  1-64000, 1-64000, 
                            1-128000, 1-128000,1-128000, 1-128000,
                            1-256000, 1-256000, 1-256000, 1-256000,
                            1-512000, 1-512000, 1-512000, 1-512000,
                            1-1024000, 1-1024000, 1-1024000, 1-1024000)

  Promedios2 [1:84,2] <- c ("PostInmunizado","PostInmunizado","PostInmunizado",
                            "PostInmunizado","PostInmunizado","PostInmunizado",
                            "PostInmunizado","PostInmunizado","PostInmunizado",
                            "PostInmunizado","PostInmunizado","PostInmunizado",
                            "PostInmunizado","PostInmunizado","PostInmunizado",
                            "PostInmunizado","PostInmunizado","PostInmunizado",
                            "PostInmunizado","PostInmunizado","PostInmunizado",
                            "PostInmunizado","PostInmunizado","PostInmunizado",
                            "PostInmunizado","PostInmunizado","PostInmunizado",
                            "PostInmunizado","PostInmunizado","PostInmunizado",
                            "PostInmunizado","PostInmunizado","PostInmunizado",
                            "PostInmunizado","PostInmunizado","PostInmunizado",
                            "ControlNegativo","ControlNegativo","ControlNegativo",
                            "ControlNegativo","ControlNegativo","ControlNegativo",
                            "ControlNegativo","ControlNegativo","ControlNegativo",
                            "ControlNegativo","ControlNegativo","ControlNegativo",
                            "ControlNegativo","ControlNegativo","ControlNegativo",
                            "ControlNegativo","ControlNegativo","ControlNegativo",
                            "ControlNegativo","ControlNegativo","ControlNegativo",
                            "ControlNegativo","ControlNegativo","ControlNegativo",
                            "ControlNegativo","ControlNegativo","ControlNegativo",
                            "ControlNegativo","ControlNegativo","ControlNegativo",
                            "ControlNegativo","ControlNegativo","ControlNegativo",
                            "ControlNegativo","ControlNegativo","ControlNegativo",
                            "ControlNegativo","ControlNegativo","ControlNegativo",
                            "ControlNegativo","ControlNegativo","ControlNegativo",
                            "ControlNegativo","ControlNegativo","ControlNegativo",
                            "ControlNegativo", "ControlNegativo","ControlNegativo")
  
  Resumen <- summarySE (Promedios2, measurevar = "Absorbancia", groupvars = c ("Dilucion", "Muestra"))
  
  print (Resumen)
  
  pd <- position_dodge(0.1)
  Grafica_Perrona <- ggplot(data= Resumen, aes(x = Dilucion, y = Absorbancia, group = Muestra, colour=Muestra)) +
    geom_errorbar(aes(ymin = Absorbancia - se, ymax = Absorbancia + se), colour="black", width=.1, position=pd) +
    geom_line(position=pd) +
    geom_point(position=pd)
  
  
  
  print (Grafica_Perrona)
  
}

ELISA(Datosos)
