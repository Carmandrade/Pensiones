library(dplyr)
library(tidyr)
library(tibble)
library(purrr)
library(janitor)
#library(rJava)
library(remotes)


########## Para una compilación más fácil y sin necesidad de instalar programas adicionales, recomiendo cargar el r.Data

# #setwd("C:/Users/escan/Desktop/Proyecto Contin ")
# 
# remotes::install_github(c("ropensci/tabulizerjars", "ropensci/tabulizer"))
# 
# library(tabulizer)
# 
# # Citar a https://blog.djnavarro.net/posts/2023-06-16_tabulizer/
# 
# #Cargar el pdf y extraer las tablas
# 
# pdf_tables <- extract_tables(file = "Modelo de australia.pdf" ,
#                              output = "data.frame") 

# Guardarlas como tibble

# pdf_tables2 <- map(pdf_tables,as_tibble)

#Sacar la tabla que tiene los valores de probabilidades de ambos generos
# 
# tabla_probs_hom <- pdf_tables2[[3]] 
# 
# head(tabla_probs_hom,10)
# 
# tabla_probs_muj <- pdf_tables2[[4]]
# 
# head(tabla_probs_muj,10)

#Darle formato de matriz

tabla_probs_hom <- as.matrix(tabla_probs_hom,)

tabla_probs_muj <- as.matrix(tabla_probs_muj,)


#Poner la primer columna en los nombres de filas

rownames(tabla_probs_hom)  <- tabla_probs_hom[,1]

rownames(tabla_probs_muj)  <- tabla_probs_muj[,1]


#Eliminar primer columna

tabla_probs_hom <- tabla_probs_hom[,2:ncol(tabla_probs_hom)]

tabla_probs_muj <- tabla_probs_muj[,2:ncol(tabla_probs_muj)]


#Eliminar los NA del pdf

tabla_probs_hom <- na.omit(tabla_probs_hom)

tabla_probs_muj <- na.omit(tabla_probs_muj)

#Funcion que interpola todo de golpe

interpolacion_lineal <- function(xx,yy){
  
  # elimina los 80 20-70
  xx1 <- xx[xx != 80]
  # elimina los 20 30-80 
  xx2 <- xx[xx != 20]
  # esto son los y de los x en los 20-70
  yy1 <- yy[xx != 80]
  # esto son los y de los x en los 30-80
  yy2 <- yy[xx != 20]
  
  # cantidad de veces que se esta repitiendo alguno de esos dos vectores
  
  cant_secuencias <-length(xx1)/6
  
  matriz <- matrix(rep(c(rep(c(seq(1,10,1)-1,
                               rep(0,20)),5),
                         seq(1,30,1)-1),
                       cant_secuencias),
                   ncol=30,
                   nrow=length(xx1), 
                   byrow = T)
  
  matriz <- yy1 + (matriz) * (yy2-yy1)/(xx2-xx1)
  
  #i=2
  
  for (i in 1:(cant_secuencias)) {
    if(i == 1){
      matriz2 <- rbind(matriz[(1:6),1:10],
                       matriz[6,c(1:10)+10],
                       matriz[6,c(1:10)+20])    
    }else{
      matriz2 <- rbind(matriz2,
                       matriz[(1:6)+6*(i-1),1:10],
                       matriz[6*(i),c(1:10)+10],
                       matriz[6*(i),c(1:10)+20])
    }
      
    
  }
  
  matriz2 <-  t(matriz2)

  matriz2 <- as.numeric(matriz2)
  
  matriz3 <- matrix(matriz2, ncol = 6)
  
  
  return(matriz2)
  
}

#Crear los parametros de la funcion

vect_yy_hom <- as.numeric(tabla_probs_hom)

vect_xx_hom <- rep(seq(from = 20,length.out = 7, by = 10),length(vect_yy_hom)/7)

xx <- vect_xx_hom

yy <- vect_yy_hom

vect_yy_muj <- as.numeric(tabla_probs_muj)

vect_xx_muj <- rep(seq(from = 20,length.out = 7, by = 10),length(vect_yy_muj)/7)

#Calculamos la interpolacion

vect_yy_hom_final <- interpolacion_lineal(vect_xx_hom,vect_yy_hom)

vect_yy_muj_final <- interpolacion_lineal(vect_xx_muj,vect_yy_muj)

#Se reordenan de nuevo

tabla_probs_hom_final <- matrix(vect_yy_hom_final, ncol = 6)

tabla_probs_muj_final <- matrix(vect_yy_muj_final, ncol = 6)

#Declaramos variables

cantidad_edades <- length(20:99)

cantidad_annos_modelados <- length(1:100)

#Inicializo las matrices

matriz_hombres <- array(0,dim = c(6,6,cantidad_edades,cantidad_annos_modelados))

matriz_mujeres <- array(0,dim = c(6,6,cantidad_edades,cantidad_annos_modelados))

# j=1
# i=1

# Se hacen las de 1 paso

for (i in 1:cantidad_edades) {
  
  matriz_hombres[,,i,1] <- tabla_probs_hom_final[1:nrow(tabla_probs_hom_final)%% 80 == i %% 80,]
  matriz_mujeres[,,i,1] <- tabla_probs_muj_final[1:nrow(tabla_probs_muj_final)%% 80 == i %% 80,]
  print(i)
}


# Se hace el proceso de markov para esa edad

for (i in 1:cantidad_edades) {
  
  for (j in 2:cantidad_annos_modelados) {
    
    if( i+j-1 <= 80 ){
      
      matriz_hombres[,,i,j] <- matriz_hombres[,,i,j-1] %*% matriz_hombres[,,i+j-1,1]
      matriz_mujeres[,,i,j] <- matriz_mujeres[,,i,j-1] %*% matriz_mujeres[,,i+j-1,1]
    }else{
      # supuesto toda persona con mas de 99 de años tendría matriz de transicion de 1 paso igual a la de 99
      matriz_hombres[,,i,j] <- matriz_hombres[,,i,j-1] %*% matriz_hombres[,,99-20+1,1]
      matriz_mujeres[,,i,j] <- matriz_mujeres[,,i,j-1] %*% matriz_mujeres[,,99-20+1,1]
    }
  }
  print(i)
}








