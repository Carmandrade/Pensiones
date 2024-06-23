#### Proyección estocástica
### Se utliza función rmultinom propia de R base que facilita efectuar simulaciones.
## Se escoge una distribución multinomial por ser una distribución de varios estados.

# px[sexo (1 H 2 M ), ynacimi (1950:2010), x (edad 0:115),  estado_inicial (1:6), estado_final (1:5) ]
### cotizaciones [añodeProy, edad]


#### HOMBRES

# Creacion de matriz 3d para guardar las simulaciones
simulaciones_H_9 <- array(0,dim = c(115,5,100))   #(proyec, estados, simulaciones)




# Se hacen las simulaciones 
for (s in 1:100) {
  
  matriz_esto_1 <- matrix(0, ncol = 5, nrow = 115) # crea matriz vacía ############### cuantos años proyecctamos????
  matriz_esto_1[1,] <- c(personas_de_la_primerafila,0,0,0,0,0) # llena la primera fila con la población inicial de 20 años
  edad_naci <- 2023-20
  for (i in 2:115) { #llena las demás filas para la personas de 20 años ############## cuantos años proyectamos????
    
    if(cotizaciones[i, 20] >= 15 || cotizaciones[i, 20] - cotizaciones[i-2, 20] >= 0.5){   #### REVISAR INDICE i PARA COTIZACIONES
      
      matriz_esto_1[i,]<- (rmultinom(1,cantidad_de_personas_del_estado1_un_añoatras, px[1, edad_naci, 20+i-2, 1, ])) #### REVISAR SI SE QUIERE PROBS DE 20 PARA PASAR A 21 O DE 21 PARA PASAR A 21
      
    }else{
      
      matriz_esto_1[i,]<- (rmultinom(1,cantidad_de_personas_del_estado1_un_añoatras, px[1, edad_naci, 20+i-2, 6, ]))
    }
    
    matriz_esto_1[i,]<-(rmultinom(1,cantidad_de_personas_del_estado2_un_añoatras, px[1, edad_naci, 20+i-2, 2, ])) + matriz_esto_1[i,]
    matriz_esto_1[i,]<-(rmultinom(1,cantidad_de_personas_del_estado3_un_añoatras, px[1, edad_naci, 20+i-2, 3, ])) + matriz_esto_1[i,]
    matriz_esto_1[i,]<-(rmultinom(1,cantidad_de_personas_del_estado4_un_añoatras, px[1, edad_naci, 20+i-2, 4, ])) + matriz_esto_1[i,]
    matriz_esto_1[i,]<-(rmultinom(1,cantidad_de_personas_del_estado5_un_añoatras, px[1, edad_naci, 20+i-2, 5, ])) + matriz_esto_1[i,]
    
  } 
  
  matrizFinal_Estoca_9H <- matriz_esto_1 
  
  # Itera las edades y los guarda en la matriz final, llena matrices para cada una de las edades restantes.
  for (j in 21:115) {
    
    matriz_esto_2 <- matrix(0, ncol = 5, nrow = 115) # crea matriz vacía ############### cuantos años proyecctamos????
    matriz_esto_2[1,] <- c(personas_de_la_primerafila,0,0,0,0,0)
    edad_naci2 <- 2023-j
    for (i in 2:115) {
      
      if(cotizaciones[i,j] >= 15 || cotizaciones[i,j] - cotizaciones[i-2,j] >= 0.5){
        
        matriz_esto_2[i,]<- (rmultinom(1,cantidad_de_personas_del_estado1_un_añoatras,  px[1, edad_naci2, j+i-2, 1, ])) ### IGUAL REVISAR CUALES PROBS
        
      }else{
        
        matriz_esto_2[i,]<- (rmultinom(1,cantidad_de_personas_del_estado1_un_añoatras, px[1, edad_naci2, j+i-2, 6, ]))
      }
      
      matriz_esto_2[i,]<-(rmultinom(1, cantidad_de_personas_del_estado2_un_añoatras, px[1, edad_naci2, j+i-2, 2, ])) + matriz_esto_2[i,] 
      matriz_esto_2[i,]<-(rmultinom(1, cantidad_de_personas_del_estado3_un_añoatras, px[1, edad_naci2, j+i-2, 3, ])) + matriz_esto_2[i,]
      matriz_esto_2[i,]<-(rmultinom(1, cantidad_de_personas_del_estado4_un_añoatras, px[1, edad_naci2, j+i-2, 4, ])) + matriz_esto_2[i,]
      matriz_esto_2[i,]<-(rmultinom(1, cantidad_de_personas_del_estado4_un_añoatras, px[1, edad_naci2, j+i-2, 5, ])) + matriz_esto_2[i,]
    }
    
    matrizFinal_Estoca_9H <-  matrizFinal_Estoca_9H + matriz_esto_2   # suma las matrices
    
  }
  
  # Rellena cada tercera dimensión con una simulación  
  simulaciones_H_9[,,s] <- matrizFinal_Estoca_9H
  
}


# Creación de matriz de 2d para sacar esperanza de las simulaciones
matriz_resumenEsperanza_H_9 <- array(0,dim = c(115,5)) 

for (i in 1:115) {
  for (j in 1:5) {
    matriz_resumenEsperanza_H_9[i,j] <- mean(simulaciones_H_9[i,j,])
    
  }
  
}








#### Mujeres 
# Hace lo mismo que la anterior pero con mujeres

# Creacion de matriz 3d para guardar las simulaciones
simulaciones_M_9 <- array(0,dim = c(115,5,100))  #(proyec, estados, simulaciones)

for (s in 1:100) {
  
  
  
  matriz_esto_3 <- matrix(0, ncol = 5, nrow = 115)
  matriz_esto_3[1,] <- c(personas_de_la_primerafila,0,0,0,0,0) 
  edad_naci3 <- 2023-20
  
  for (i in 2:100) {
    
    if(cotizaciones[i, 20] >= 15 || cotizaciones[i, 20] - cotizaciones[i-2, 20] >= 0.5){   #### REVISAR INDICE i PARA COTIZACIONES
      
      matriz_esto_3[i,]<- (rmultinom(1,cantidad_de_personas_del_estado1_un_añoatras, px[2, edad_naci3, 20+i-2, 1, ])) #### REVISAR SI SE QUIERE PROBS DE 20 PARA PASAR A 21 O DE 21 PARA PASAR A 21
      
    }else{
      
      matriz_esto_3[i,]<- (rmultinom(1,cantidad_de_personas_del_estado1_un_añoatras, px[2, edad_naci3, 20+i-2, 6, ]))
    }
    
    matriz_esto_3[i,]<-(rmultinom(1,cantidad_de_personas_del_estado2_un_añoatras, px[2, edad_naci3, 20+i-2, 2, ])) + matriz_esto_3[i,]
    matriz_esto_3[i,]<-(rmultinom(1,cantidad_de_personas_del_estado3_un_añoatras, px[2, edad_naci3, 20+i-2, 3, ])) + matriz_esto_3[i,]
    matriz_esto_3[i,]<-(rmultinom(1,cantidad_de_personas_del_estado4_un_añoatras, px[2, edad_naci3, 20+i-2, 4, ])) + matriz_esto_3[i,]
    matriz_esto_3[i,]<-(rmultinom(1,cantidad_de_personas_del_estado5_un_añoatras, px[2, edad_naci3, 20+i-2, 5, ])) + matriz_esto_3[i,]
    
  }
  matrizFinal_Estoca_9M <- matriz_esto_3 
  
  # Iterar las edades y guardarlo en la matriz final
  for (j in 21:99) {
    matriz_esto_4 <- matrix(0, ncol = 5, nrow = 115)
    matriz_esto_4[1,] <- c(personas_de_la_primerafila,0,0,0,0,0)
    edad_naci4 <- 2023-j
    
    for (i in 2:115) {
      
      if(cotizaciones[i,j] >= 15 || cotizaciones[i,j] - cotizaciones[i-2,j] >= 0.5){
        
        matriz_esto_4[i,]<- (rmultinom(1,cantidad_de_personas_del_estado1_un_añoatras,  px[2, edad_naci4, j+i-2, 1, ])) ### IGUAL REVISAR CUALES PROBS
        
      }else{
        
        matriz_esto_4[i,]<- (rmultinom(1,cantidad_de_personas_del_estado1_un_añoatras, px[2, edad_naci4, j+i-2, 6, ]))
      }
      
      matriz_esto_4[i,]<-(rmultinom(1, cantidad_de_personas_del_estado2_un_añoatras, px[2, edad_naci4, j+i-2, 2, ])) + matriz_esto_4[i,] 
      matriz_esto_4[i,]<-(rmultinom(1, cantidad_de_personas_del_estado3_un_añoatras, px[2, edad_naci4, j+i-2, 3, ])) + matriz_esto_4[i,]
      matriz_esto_4[i,]<-(rmultinom(1, cantidad_de_personas_del_estado4_un_añoatras, px[2, edad_naci4, j+i-2, 4, ])) + matriz_esto_4[i,]
      matriz_esto_4[i,]<-(rmultinom(1, cantidad_de_personas_del_estado4_un_añoatras, px[2, edad_naci4, j+i-2, 5, ])) + matriz_esto_4[i,]
      
    }
    
    matrizFinal_Estoca_9M <- matrizFinal_Estoca_9M + matriz_esto_4  
    
  }
  
  simulaciones_M_9[,,s] <- matrizFinal_Estoca_9M 
  
}

# Creación de matriz de 2d para sacar esperanza de las simulaciones
matriz_resumenEsperanza_M_9 <- array(0,dim = c(115,5)) 

for (i in 1:115) {
  for (j in 1:5) {
    matriz_resumenEsperanza_M_9[i,j] <- mean(simulaciones_M_9[i,j,])
    
  }
  
}