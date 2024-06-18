############# Punto 9
### Se utliza función rmultinom propia de R base que facilita efectuar simulaciones.
## Se escoge una distribución multinomial por ser una distribución de varios estados.

#base <- datosFinal_v2 %>% filter(Edades>=20 & Edades<=99)



#### Hombres

# Creacion de matriz 3d para guardar las simulaciones
simulaciones_H_9 <- array(0,dim = c(100,6,100)) 



# Se hacen las simulaciones 
for (s in 1:100) {
  
  matriz_esto_1 <- matrix(0, ncol = 6, nrow = 100) # crea matriz vacía
  matriz_esto_1[1,] <- c(base$Hombres[20-19],0,0,0,0,0) # llena la primera fila para hombres de 20 años
 
   for (i in 2:100) { #llena las demás filas para hombres de 20 años
    matriz_esto_1[i,]<-(rmultinom(1,matriz_esto_1[i-1,1], matriz_hombres[1,,min(20-19+i-2,80),1])) 
    matriz_esto_1[i,]<-(rmultinom(1,matriz_esto_1[i-1,2], matriz_hombres[2,,min(20-19+i-2,80),1])) + matriz_esto_1[i,]
    matriz_esto_1[i,]<-(rmultinom(1,matriz_esto_1[i-1,3], matriz_hombres[3,,min(20-19+i-2,80),1])) + matriz_esto_1[i,]
    matriz_esto_1[i,]<-(rmultinom(1,matriz_esto_1[i-1,4], matriz_hombres[4,,min(20-19+i-2,80),1])) + matriz_esto_1[i,]
    matriz_esto_1[i,]<-(rmultinom(1,matriz_esto_1[i-1,5], matriz_hombres[5,,min(20-19+i-2,80),1])) + matriz_esto_1[i,]
    matriz_esto_1[i,]<-(rmultinom(1,matriz_esto_1[i-1,6], matriz_hombres[6,,min(20-19+i-2,80),1])) + matriz_esto_1[i,]
    
  } 
  
  matrizFinal_Estoca_9H <- matriz_esto_1 
  
  # Itera las edades y los guarda en la matriz final, llena matrices para cada una de las edades restantes.
  for (j in 21:99) {
    
    matriz_esto_2 <- matrix(0, ncol = 6, nrow = 100)
    matriz_esto_2[1,] <- c(base$Hombres[j-19],0,0,0,0,0)
    for (i in 2:100) {
      matriz_esto_2[i,]<-(rmultinom(1, matriz_esto_2[i-1,1], matriz_hombres[1,,min(j-19+i-2,80),1])) 
      matriz_esto_2[i,]<-(rmultinom(1, matriz_esto_2[i-1,2], matriz_hombres[2,,min(j-19+i-2,80),1])) + matriz_esto_2[i,]
      matriz_esto_2[i,]<-(rmultinom(1, matriz_esto_2[i-1,3], matriz_hombres[3,,min(j-19+i-2,80),1])) + matriz_esto_2[i,]
      matriz_esto_2[i,]<-(rmultinom(1, matriz_esto_2[i-1,4], matriz_hombres[4,,min(j-19+i-2,80),1])) + matriz_esto_2[i,]
      matriz_esto_2[i,]<-(rmultinom(1, matriz_esto_2[i-1,5], matriz_hombres[5,,min(j-19+i-2,80),1])) + matriz_esto_2[i,]
      matriz_esto_2[i,]<-(rmultinom(1, matriz_esto_2[i-1,6], matriz_hombres[6,,min(j-19+i-2,80),1])) + matriz_esto_2[i,]
      
    }
    
    matrizFinal_Estoca_9H <-  matrizFinal_Estoca_9H + matriz_esto_2   # suma las matrices
    
  }

  # Rellena cada tercera dimensión con una simulación  
  simulaciones_H_9[,,s] <- matrizFinal_Estoca_9H
  
}

# Creación de matriz de 2d para sacar esperanza de las simulaciones
matriz_resumenEsperanza_H_9 <- array(0,dim = c(100,6)) 

for (i in 1:100) {
  for (j in 1:6) {
    matriz_resumenEsperanza_H_9[i,j] <- mean(simulaciones_H_9[i,j,])
    
  }
  
}

# Creación de matriz de 2d para sacar epercentil de las simulaciones
matriz_resumenPercentil_H_9 <- array(0,dim = c(100,6)) 

for (i in 1:100) {
  for (j in 1:6) {
    matriz_resumenPercentil_H_9[i,j] <- quantile(simulaciones_H_9[i,j,], 0.995)
    
  }
  
}








#### Mujeres 
# Hace lo mismo que la anterior pero con mujeres

# Creacion de matriz 3d para guardar las simulaciones
simulaciones_M_9 <- array(0,dim = c(100,6,100)) 

for (s in 1:100) {
  


matriz_esto_3 <- matrix(0, ncol = 6, nrow = 100)
matriz_esto_3[1,] <- c(base$Mujeres[20-19],0,0,0,0,0) 

for (i in 2:100) {
  matriz_esto_3[i,]<-(rmultinom(1, matriz_esto_3[i-1,1], matriz_mujeres[1,,min(20-19+i-2,80),1])) 
  matriz_esto_3[i,]<-(rmultinom(1, matriz_esto_3[i-1,2], matriz_mujeres[2,,min(20-19+i-2,80),1])) + matriz_esto_3[i,]
  matriz_esto_3[i,]<-(rmultinom(1, matriz_esto_3[i-1,3], matriz_mujeres[3,,min(20-19+i-2,80),1])) + matriz_esto_3[i,]
  matriz_esto_3[i,]<-(rmultinom(1, matriz_esto_3[i-1,4], matriz_mujeres[4,,min(20-19+i-2,80),1])) + matriz_esto_3[i,]
  matriz_esto_3[i,]<-(rmultinom(1, matriz_esto_3[i-1,5], matriz_mujeres[5,,min(20-19+i-2,80),1])) + matriz_esto_3[i,]
  matriz_esto_3[i,]<-(rmultinom(1, matriz_esto_3[i-1,6], matriz_mujeres[6,,min(20-19+i-2,80),1])) + matriz_esto_3[i,]
  
}
matrizFinal_Estoca_9M <- matriz_esto_3 

# Iterar las edades y guardarlo en la matriz final
for (j in 21:99) {
  matriz_esto_4 <- matrix(0, ncol = 6, nrow = 100)
  matriz_esto_4[1,] <- c(base$Mujeres[j-19],0,0,0,0,0)
  for (i in 2:100) {
    matriz_esto_4[i,]<-(rmultinom(1, matriz_esto_4[i-1,1], matriz_mujeres[1,,min(j-19+i-2,80),1])) 
    matriz_esto_4[i,]<-(rmultinom(1, matriz_esto_4[i-1,2], matriz_mujeres[2,,min(j-19+i-2,80),1]))  + matriz_esto_4[i,]
    matriz_esto_4[i,]<-(rmultinom(1, matriz_esto_4[i-1,3], matriz_mujeres[3,,min(j-19+i-2,80),1]))  + matriz_esto_4[i,]
    matriz_esto_4[i,]<-(rmultinom(1, matriz_esto_4[i-1,4], matriz_mujeres[4,,min(j-19+i-2,80),1]))  + matriz_esto_4[i,]
    matriz_esto_4[i,]<-(rmultinom(1, matriz_esto_4[i-1,5], matriz_mujeres[5,,min(j-19+i-2,80),1]))  + matriz_esto_4[i,]
    matriz_esto_4[i,]<-(rmultinom(1, matriz_esto_4[i-1,6], matriz_mujeres[6,,min(j-19+i-2,80),1]))  + matriz_esto_4[i,]
    
  }
  
  matrizFinal_Estoca_9M <- matrizFinal_Estoca_9M + matriz_esto_4  
  
}

simulaciones_M_9[,,s] <- matrizFinal_Estoca_9M 

}

# Creación de matriz de 2d para sacar esperanza de las simulaciones
matriz_resumenEsperanza_M_9 <- array(0,dim = c(100,6)) 

for (i in 1:100) {
  for (j in 1:6) {
    matriz_resumenEsperanza_M_9[i,j] <- mean(simulaciones_M_9[i,j,])
    
  }
  
}



# Creación de matriz de 2d para sacar el percentil de las simulaciones
matriz_resumenPercentil_M_9 <- array(0,dim = c(100,6)) 

for (i in 1:100) {
  for (j in 1:6) {
    matriz_resumenPercentil_M_9[i,j] <- quantile(simulaciones_M_9[i,j,], 0.995)
    
  }
  
}




################### Punto 11

#supuestos
interes=0.07246 #i
inflacion=0.0279 #j

#se crea el j equivalente
j=(interes-inflacion)/(1+inflacion)

#calcula la v
v=1/(1+j)


SA_6 = 150000
Anua_5 = 30000

#gastos
Gasto_inicial= 200
Gasto_mantenimiento= 0.05 #de las primas
Gasto_Terminacion = 500

######### Hombres
## Primero para hombres con edad 20
## Filas cantidad de años de proyeccion

#Creacion de matriz 3d para guardar las simulaciones
simulaciones_H_11 <- array(0,dim = c(100,6,100)) 



# Se hacen las simulaciones 
for (s in 1:100) {
  

  matriz_esto_5 <- matrix(0, ncol = 6, nrow = 100) # Crea matriz vacía
  matriz_esto_5[1,] <- c(base$Hombres[20-19],0,0,0,0,0) # LLena primera fila
  
  matriz1H_11_pagos <- matrix(0, ncol = 6, nrow = 100) # Crea matriz vacía
  matriz1H_11_pagos[1,] <- (c(base$Hombres[20-19],0,0,0,0,0)*prima_ponderadaH)-(c(base$Hombres[20-19],0,0,0,0,0)*Gasto_inicial) # LLena primera fila
  
  
  vector_Montos <- c(prima_ponderadaH,prima_ponderadaH,prima_ponderadaH*0.6, -0, -Anua_5, -(SA_6 + Gasto_Terminacion) ) # vector de Montos con primas ponderadas
  
  
  # Llena las demás filas
  # Para el estado 6 se evita el "doble conteo", se restan los beneficios que se hayan pagado el año anterior para quienes se mantengan en el estado 6 luego de 1 año
  # Solo se toman en cuenta los beneficios de gente nueva entrando en el estado 6
  for (i in 2:100) { 
    
    matriz_esto_5[i,]<-(rmultinom(1,matriz_esto_5[i-1,1], matriz_hombres[1,,min(20-19+i-2,80),1])) 
    matriz_esto_5[i,]<-(rmultinom(1,matriz_esto_5[i-1,2], matriz_hombres[2,,min(20-19+i-2,80),1]))  + matriz_esto_5[i,]
    matriz_esto_5[i,]<-(rmultinom(1,matriz_esto_5[i-1,3], matriz_hombres[3,,min(20-19+i-2,80),1]))  + matriz_esto_5[i,]
    matriz_esto_5[i,]<-(rmultinom(1,matriz_esto_5[i-1,4], matriz_hombres[4,,min(20-19+i-2,80),1]))  + matriz_esto_5[i,]
    matriz_esto_5[i,]<-(rmultinom(1,matriz_esto_5[i-1,5], matriz_hombres[5,,min(20-19+i-2,80),1]))  + matriz_esto_5[i,]
    matriz_esto_5[i,]<-(rmultinom(1,matriz_esto_5[i-1,6], matriz_hombres[6,,min(20-19+i-2,80),1]))  + matriz_esto_5[i,]
    
    # Solo quitar los dobles conteos del estado 6
    matriz_esto_5[i,6] <- matriz_esto_5[i,6]- (matriz_esto_5[i-1,6]) # Resta el las personas pagadas el año anterior
    
    matriz1H_11_pagos[i,]<- matriz_esto_5[i,]*vector_Montos * (1+inflacion)^(100-i)
    
  }
  matrizFinal_Estoca_11H <- matriz1H_11_pagos
  
  
  
  #Itera las edades y los guardar en la matriz final, llena matrices para cada una de las edades restantes.
  
  # j edades
  for (j in 21:99) { # Hasta el 99 por supuesto según la base de datos
    
    matriz_esto_6 <- matrix(0, ncol = 6, nrow = 100) # Crea matriz vacía
    matriz_esto_6[1,] <- c(base$Hombres[j-19],0,0,0,0,0) # LLena primera fila
    
    matriz2H_11_pagos <- matrix(0, ncol = 6, nrow = 100) # Crea matriz vacía
    matriz2H_11_pagos[1,] <- (c(base$Hombres[j-19],0,0,0,0,0)*prima_ponderadaH)-(c(base$Hombres[j-19],0,0,0,0,0)*Gasto_inicial) # LLena primera fila
    
    vector_Montos <- c(prima_ponderadaH,prima_ponderadaH,prima_ponderadaH*0.6, -0, -Anua_5, -(SA_6 + Gasto_Terminacion) ) # vector de Montos con primas ponderadas
    
    # i pasos
    for (i in 2:100) {
      
      matriz_esto_6[i,]<-(rmultinom(1, matriz_esto_6[i-1,1], matriz_hombres[1,,min(j-19+i-2,80),1])) 
      matriz_esto_6[i,]<-(rmultinom(1, matriz_esto_6[i-1,2], matriz_hombres[2,,min(j-19+i-2,80),1])) + matriz_esto_6[i,]
      matriz_esto_6[i,]<-(rmultinom(1, matriz_esto_6[i-1,3], matriz_hombres[3,,min(j-19+i-2,80),1])) + matriz_esto_6[i,]
      matriz_esto_6[i,]<-(rmultinom(1, matriz_esto_6[i-1,4], matriz_hombres[4,,min(j-19+i-2,80),1])) + matriz_esto_6[i,]
      matriz_esto_6[i,]<-(rmultinom(1, matriz_esto_6[i-1,5], matriz_hombres[5,,min(j-19+i-2,80),1])) + matriz_esto_6[i,]
      matriz_esto_6[i,]<-(rmultinom(1, matriz_esto_6[i-1,6], matriz_hombres[6,,min(j-19+i-2,80),1])) + matriz_esto_6[i,]
      
      
      matriz_esto_6[i,6] <- matriz_esto_6[i,6]- (matriz_esto_6[i-1,6])
      
      matriz2H_11_pagos[i,] <- matriz_esto_6[i,] * vector_Montos * (1+inflacion)^(100-i)
    }
    
    matrizFinal_Estoca_11H <- matrizFinal_Estoca_11H + matriz2H_11_pagos  
    
  }
  
  simulaciones_H_11[,,s] <- matrizFinal_Estoca_11H
  
}
  
  
# Creación de matriz de 2d para sacar esperanza de las simulaciones
matriz_resumenEsperanza_H_11 <- array(0,dim = c(100,6)) 

for (i in 1:100) {
  for (j in 1:6) {
    matriz_resumenEsperanza_H_11[i,j] <- mean(simulaciones_H_11[i,j,])
    
  }
  
}


# Creación de matriz de 2d para sacar el percentil de las simulaciones
matriz_resumenPercentil_H_11 <- array(0,dim = c(100,6)) 

for (i in 1:100) {
  for (j in 1:6) {
    matriz_resumenPercentil_H_11[i,j] <- quantile(simulaciones_H_11[i,j,], 0.995)
    
  }
  
}

#apply(matriz_resumenEsperanza_H_11,1,sum)




######### Mujeres
## Se aplica lo mismo que el anterior pero para los datos de mujeres
simulaciones_M_11 <- array(0,dim = c(100,6,100)) 


for (s in 1:100) {
  

  
  matriz_esto_7 <- matrix(0, ncol = 6, nrow = 100) # Crea matriz vacía
  matriz_esto_7[1,] <- c(base$Mujeres[20-19],0,0,0,0,0) # LLena primera fila
  
  matriz1M_11_pagos <- matrix(0, ncol = 6, nrow = 100) # Crea matriz vacía
  matriz1M_11_pagos[1,] <- (c(base$Mujeres[20-19],0,0,0,0,0)*prima_ponderadaM)-(c(base$Mujeres[20-19],0,0,0,0,0)*Gasto_inicial) # LLena primera fila
  
  
  vector_Montos <- c(prima_ponderadaM,prima_ponderadaM,prima_ponderadaM*0.6, -0, -Anua_5, -(SA_6 + Gasto_Terminacion) ) # vector de Montos con primas ponderadas
  
  for (i in 2:100) { 
    
    matriz_esto_7[i,]<-(rmultinom(1, matriz_esto_7[i-1,1], matriz_mujeres[1,,min(20-19+i-2,80),1])) 
    matriz_esto_7[i,]<-(rmultinom(1, matriz_esto_7[i-1,2], matriz_mujeres[2,,min(20-19+i-2,80),1]))  + matriz_esto_7[i,]
    matriz_esto_7[i,]<-(rmultinom(1, matriz_esto_7[i-1,3], matriz_mujeres[3,,min(20-19+i-2,80),1]))  + matriz_esto_7[i,]
    matriz_esto_7[i,]<-(rmultinom(1, matriz_esto_7[i-1,4], matriz_mujeres[4,,min(20-19+i-2,80),1]))  + matriz_esto_7[i,]
    matriz_esto_7[i,]<-(rmultinom(1, matriz_esto_7[i-1,5], matriz_mujeres[5,,min(20-19+i-2,80),1]))  + matriz_esto_7[i,]
    matriz_esto_7[i,]<-(rmultinom(1, matriz_esto_7[i-1,6], matriz_mujeres[6,,min(20-19+i-2,80),1]))  + matriz_esto_7[i,]
    
    
    matriz_esto_7[i,6] <- matriz_esto_7[i,6]- (matriz_esto_7[i-1,6]) # Resta las personas pagadas el año anterior
    
    matriz1M_11_pagos[i,]<- matriz_esto_7[i,]*vector_Montos * (1+inflacion)^(100-i)
    
  }
  matrizFinal_Estoca_11M <- matriz1M_11_pagos
  
  
  #Itera las edades y los guardar en la matriz final, llena matrices para cada una de las edades restantes.
  
  # j edades
  for (j in 21:99) { # Hasta el 99 por supuesto según la base de datos
    matriz_esto_8 <- matrix(0, ncol = 6, nrow = 100) # Crea matriz vacía
    matriz_esto_8[1,] <- c(base$Mujeres[j-19],0,0,0,0,0) # LLena primera fila
    
    matriz2M_11_pagos <- matrix(0, ncol = 6, nrow = 100) # Crea matriz vacía
    matriz2M_11_pagos[1,] <- (c(base$Mujeres[j-19],0,0,0,0,0)*prima_ponderadaM)-(c(base$Mujeres[j-19],0,0,0,0,0)*Gasto_inicial) # LLena primera fila
    
    vector_Montos <- c(prima_ponderadaM,prima_ponderadaM,prima_ponderadaM*0.6, -0, -Anua_5, -(SA_6 + Gasto_Terminacion) ) # vector de Montos con primas ponderadas
    
    # i pasos
    for (i in 2:100) {
      
      matriz_esto_8[i,]<-(rmultinom(1, matriz_esto_8[i-1,1], matriz_mujeres[1,,min(j-19+i-2,80),1])) 
      matriz_esto_8[i,]<-(rmultinom(1, matriz_esto_8[i-1,2], matriz_mujeres[2,,min(j-19+i-2,80),1])) + matriz_esto_8[i,]
      matriz_esto_8[i,]<-(rmultinom(1, matriz_esto_8[i-1,3], matriz_mujeres[3,,min(j-19+i-2,80),1])) + matriz_esto_8[i,]
      matriz_esto_8[i,]<-(rmultinom(1, matriz_esto_8[i-1,4], matriz_mujeres[4,,min(j-19+i-2,80),1])) + matriz_esto_8[i,]
      matriz_esto_8[i,]<-(rmultinom(1, matriz_esto_8[i-1,5], matriz_mujeres[5,,min(j-19+i-2,80),1])) + matriz_esto_8[i,]
      matriz_esto_8[i,]<-(rmultinom(1, matriz_esto_8[i-1,6], matriz_mujeres[6,,min(j-19+i-2,80),1])) + matriz_esto_8[i,]
      
      
      matriz_esto_8[i,6] <- matriz_esto_8[i,6]- (matriz_esto_8[i-1,6])
      
      matriz2M_11_pagos[i,] <- matriz_esto_8[i,] * vector_Montos * (1+inflacion)^(100-i)
    }
    
    matrizFinal_Estoca_11M <- matrizFinal_Estoca_11M + matriz2M_11_pagos  
    
  }

  
  simulaciones_M_11[,,s] <- matrizFinal_Estoca_11M
}

# Creación de matriz de 2d para sacar esperanza de las simulaciones
matriz_resumenEsperanza_M_11 <- array(0,dim = c(100,6)) 

for (i in 1:100) {
  for (j in 1:6) {
    matriz_resumenEsperanza_M_11[i,j] <- mean(simulaciones_M_11[i,j,])
    
  }
  
}


# Creación de matriz de 2d para sacar el percentil de las simulaciones
matriz_resumenPercentil_M_11 <- array(0,dim = c(100,6)) 

for (i in 1:100) {
  for (j in 1:6) {
    matriz_resumenPercentil_M_11[i,j] <- quantile(simulaciones_M_11[i,j,], 0.995)
    
  }
  
}


#### Graficación

###Punto 9

##Hombres
df_EstocaHombres_9 <- as.data.frame(matriz_resumenEsperanza_H_9)
df_EstocaHombres_9 <- df_EstocaHombres_9 %>% mutate(Año= c(0:99))
colnames(df_EstocaHombres_9) <- c("Estado_0", "Estado_1", "Estado_2", "Estado_3", "Estado_4", "Estado_5", "Año" )



grafico_Hombres_9 <- ggplot(data = df_EstocaHombres_9,mapping= aes(x = Año)) +
  geom_line(aes(y = Estado_0, color = "Estado 0"), linewidth=1.05) +
  geom_line(aes(y = Estado_1, color = "Estado 1"), linewidth=1.05) +
  geom_line(aes(y = Estado_2, color = "Estado 2"), linewidth=1.05) +
  geom_line(aes(y = Estado_3, color = "Estado 3"), linewidth=1.05) +
  geom_line(aes(y = Estado_4, color = "Estado 4"), linewidth=1.05) +
  geom_line(aes(y = Estado_5, color = "Estado 5"), linewidth=1.05) +
  scale_x_continuous(breaks = seq(0, 100, by = 20)) +
  scale_y_continuous(labels= comma, breaks = seq(0, max(df_EstocaHombres_9$Estado_0), by = 500000)) +
  labs( y="Población", color="Estado", x="Año de proyección")+
  scale_color_manual(values = c("Estado 0" = "green", "Estado 1" = "blue", "Estado 2"="yellow", "Estado 3" = "cyan", "Estado 4"="purple", "Estado 5"= "red"))+
  theme_cowplot(12)

grafico_Hombres_9


###Mujeres
df_EstocaMujeres_9 <- as.data.frame(matriz_resumenEsperanza_M_9)
df_EstocaMujeres_9 <- df_EstocaMujeres_9 %>% mutate(Edad= c(0:99))
colnames(df_EstocaMujeres_9) <- c("Estado_0", "Estado_1", "Estado_2", "Estado_3", "Estado_4", "Estado_5", "Edad" )


grafico_Mujeres_9 <- ggplot(data = df_EstocaMujeres_9,mapping= aes(x = Edad)) +
  geom_line(aes(y = Estado_0, color = "Estado 0"), linewidth=1.05) +
  geom_line(aes(y = Estado_1, color = "Estado 1"), linewidth=1.05) +
  geom_line(aes(y = Estado_2, color = "Estado 2"), linewidth=1.05) +
  geom_line(aes(y = Estado_3, color = "Estado 3"), linewidth=1.05) +
  geom_line(aes(y = Estado_4, color = "Estado 4"), linewidth=1.05) +
  geom_line(aes(y = Estado_5, color = "Estado 5"), linewidth=1.05) +
  scale_x_continuous(breaks = seq(0, 100, by = 20)) +
  scale_y_continuous(labels= comma, breaks = seq(0, max(df_EstocaMujeres_9$Estado_0), by = 500000)) +
  labs( y="Población", color="Estado", x="Año de proyección")+
  scale_color_manual(values = c("Estado 0" = "green", "Estado 1" = "blue", "Estado 2"="yellow", "Estado 3" = "cyan", "Estado 4"="purple", "Estado 5"= "red"))+
  theme_cowplot(12)


grafico_Mujeres_9

###Punto 11

## Hombres
df_EstocaHombres_11 <- as.data.frame(matriz_resumenEsperanza_H_11)
df_EstocaHombres_11 <- df_EstocaHombres_11 %>% mutate(Año= c(0:99))
colnames(df_EstocaHombres_11) <- c("Estado_0", "Estado_1", "Estado_2", "Estado_3", "Estado_4", "Estado_5", "Año" )



grafico_Hombres_11 <- ggplot(data = df_EstocaHombres_11,mapping= aes(x = Año)) +
  geom_line(aes(y = Estado_0, color = "Estado 0"), linewidth=1.05) +
  geom_line(aes(y = Estado_1, color = "Estado 1"), linewidth=1.05) +
  geom_line(aes(y = Estado_2, color = "Estado 2"), linewidth=1.05) +
  geom_line(aes(y = Estado_3, color = "Estado 3"), linewidth=1.05) +
  geom_line(aes(y = Estado_4, color = "Estado 4"), linewidth=1.05) +
  geom_line(aes(y = Estado_5, color = "Estado 5"), linewidth=1.05) +
  scale_x_continuous(breaks = seq(0, 100, by = 20)) +
  scale_y_continuous(labels= comma, breaks = seq(-20000000000, max(df_EstocaHombres_11$Estado_0), by =25000000000)) +
  #scale_y_log10()+
  labs( y="Montos", color="Estado", x="Año de proyección")+
  scale_color_manual(values = c("Estado 0" = "green", "Estado 1" = "blue", "Estado 2"="yellow", "Estado 3" = "cyan", "Estado 4"="purple", "Estado 5"= "red"))+
  theme_cowplot(12)

grafico_Hombres_11

### Mujeres

df_EstocaMujeres_11 <- as.data.frame(matriz_resumenEsperanza_M_11)
df_EstocaMujeres_11 <- df_EstocaMujeres_11 %>% mutate(Edad= c(0:99))
colnames(df_EstocaMujeres_11) <- c("Estado_0", "Estado_1", "Estado_2", "Estado_3", "Estado_4", "Estado_5", "Edad" )


grafico_Mujeres_11 <- ggplot(data = df_EstocaMujeres_11,mapping= aes(x = Edad)) +
  geom_line(aes(y = Estado_0, color = "Estado 0"), linewidth=1.05) +
  geom_line(aes(y = Estado_1, color = "Estado 1"), linewidth=1.05) +
  geom_line(aes(y = Estado_2, color = "Estado 2"), linewidth=1.05) +
  geom_line(aes(y = Estado_3, color = "Estado 3"), linewidth=1.05) +
  geom_line(aes(y = Estado_4, color = "Estado 4"), linewidth=1.05) +
  geom_line(aes(y = Estado_5, color = "Estado 5"), linewidth=1.05) +
  scale_x_continuous(breaks = seq(0, 100, by = 20)) +
  scale_y_continuous(labels= comma, breaks = seq(-20000000000, max(df_EstocaMujeres_11$Estado_0), by =25000000000)) +
  labs( y="Población", color="Estado", x="Año de proyección")+
  scale_color_manual(values = c("Estado 0" = "green", "Estado 1" = "blue", "Estado 2"="yellow", "Estado 3" = "cyan", "Estado 4"="purple", "Estado 5"= "red"))+
  theme_cowplot(12)


grafico_Mujeres_11

