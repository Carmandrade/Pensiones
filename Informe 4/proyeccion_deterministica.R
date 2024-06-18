library(readxl)
library(ggplot2)
library(expm)
library(Matrix)
library(matrixcalc)
library(tidyverse)
library(dplyr)
library(ggpubr)
library(scales)
library(cowplot)

 base <- datosFinal_v2 %>% filter(Edades>=20 & Edades<=99)



##################### Punto 8   
##### Hombres
 ## matriz filas tienen años modelados y en las columnas los estados
 ## oportunidades de mejora de que las  probabilidades se pueden asumir distintas para que la poblacion se reduzca mas rapido
 
 
 matriz1 <- matrix(0, ncol = 6, nrow = 100) # crea matriz vacía
 matriz1[1,] <- c(base$Hombres[20-19],0,0,0,0,0) # llena la primera fila para hombres de 20 años
 
 for (i in 2:100) { #llena las demás filas para hombres de 20 años
   matriz1[i,]<-(matriz1[i-1,1]*matriz_hombres[1,,min(20-19+i-2,80),1]) 
   matriz1[i,]<-(matriz1[i-1,2]*matriz_hombres[2,,min(20-19+i-2,80),1]) + matriz1[i,]
   matriz1[i,]<-(matriz1[i-1,3]*matriz_hombres[3,,min(20-19+i-2,80),1]) + matriz1[i,]
   matriz1[i,]<-(matriz1[i-1,4]*matriz_hombres[4,,min(20-19+i-2,80),1]) + matriz1[i,]
   matriz1[i,]<-(matriz1[i-1,5]*matriz_hombres[5,,min(20-19+i-2,80),1]) + matriz1[i,]
   matriz1[i,]<-(matriz1[i-1,6]*matriz_hombres[6,,min(20-19+i-2,80),1]) + matriz1[i,]
   
 }
 
 matrizFinal_Hombres_8 <- matriz1 
 
 # Itera las edades y los guarda en la matriz final, llena matrices para cada una de las edades restantes.
 for (j in 21:99) {
   
   matriz2 <- matrix(0, ncol = 6, nrow = 100)
   matriz2[1,] <- c(base$Hombres[j-19],0,0,0,0,0)
   for (i in 2:100) {
     matriz2[i,]<-(matriz2[i-1,1]*matriz_hombres[1,,min(j-19+i-2,80),1]) 
     matriz2[i,]<-(matriz2[i-1,2]*matriz_hombres[2,,min(j-19+i-2,80),1]) + matriz2[i,]
     matriz2[i,]<-(matriz2[i-1,3]*matriz_hombres[3,,min(j-19+i-2,80),1]) + matriz2[i,]
     matriz2[i,]<-(matriz2[i-1,4]*matriz_hombres[4,,min(j-19+i-2,80),1]) + matriz2[i,]
     matriz2[i,]<-(matriz2[i-1,5]*matriz_hombres[5,,min(j-19+i-2,80),1]) + matriz2[i,]
     matriz2[i,]<-(matriz2[i-1,6]*matriz_hombres[6,,min(j-19+i-2,80),1]) + matriz2[i,]
     
   }
   
   matrizFinal_Hombres_8 <-  matrizFinal_Hombres_8 + matriz2  # suma las matrices
   
 }
 

 
 round(matrizFinal_Hombres_8,3)
 
 
 # matriz2[i-1,6]
 # matriz_hombres[6,,min(j-19+i-2,80),82]
 

### Mujeres 
 # Hace lo mismo que la anterior pero con mujeres
 matriz1M <- matrix(0, ncol = 6, nrow = 100)
 matriz1M[1,] <- c(base$Mujeres[20-19],0,0,0,0,0) 
 
 for (i in 2:100) {
   matriz1M[i,]<-(matriz1M[i-1,1]*matriz_mujeres[1,,min(20-19+i-2,80),1]) 
   matriz1M[i,]<-(matriz1M[i-1,2]*matriz_mujeres[2,,min(20-19+i-2,80),1]) + matriz1M[i,]
   matriz1M[i,]<-(matriz1M[i-1,3]*matriz_mujeres[3,,min(20-19+i-2,80),1]) + matriz1M[i,]
   matriz1M[i,]<-(matriz1M[i-1,4]*matriz_mujeres[4,,min(20-19+i-2,80),1]) + matriz1M[i,]
   matriz1M[i,]<-(matriz1M[i-1,5]*matriz_mujeres[5,,min(20-19+i-2,80),1]) + matriz1M[i,]
   matriz1M[i,]<-(matriz1M[i-1,6]*matriz_mujeres[6,,min(20-19+i-2,80),1]) + matriz1M[i,]
   
 }
 matrizFinal_Mujeres_8 <- matriz1M 
 
 # Iterar las edades y guardarlo en la matriz final
 for (j in 21:99) {
   matriz2M <- matrix(0, ncol = 6, nrow = 100)
   matriz2M[1,] <- c(base$Mujeres[j-19],0,0,0,0,0)
   for (i in 2:100) {
     matriz2M[i,]<-(matriz2M[i-1,1]*matriz_mujeres[1,,min(j-19+i-2,80),1]) 
     matriz2M[i,]<-(matriz2M[i-1,2]*matriz_mujeres[2,,min(j-19+i-2,80),1]) + matriz2M[i,]
     matriz2M[i,]<-(matriz2M[i-1,3]*matriz_mujeres[3,,min(j-19+i-2,80),1]) + matriz2M[i,]
     matriz2M[i,]<-(matriz2M[i-1,4]*matriz_mujeres[4,,min(j-19+i-2,80),1]) + matriz2M[i,]
     matriz2M[i,]<-(matriz2M[i-1,5]*matriz_mujeres[5,,min(j-19+i-2,80),1]) + matriz2M[i,]
     matriz2M[i,]<-(matriz2M[i-1,6]*matriz_mujeres[6,,min(j-19+i-2,80),1]) + matriz2M[i,]
     
   }
   
   matrizFinal_Mujeres_8 <- matrizFinal_Mujeres_8 + matriz2M  
   
 }
round(matrizFinal_Mujeres_8,3)

 
 ################### Punto 10

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
 
 ### Hombres
 ## Primero para hombres con edad 20
 ## Filas cantidad de años de proyeccion
 
 matriz1H_10_pob <- matrix(0, ncol = 6, nrow = 100) # Crea matriz vacía
 matriz1H_10_pob[1,] <- c(base$Hombres[20-19],0,0,0,0,0) # LLena primera fila
 
 matriz1H_10_pagos <- matrix(0, ncol = 6, nrow = 100) # Crea matriz vacía
 matriz1H_10_pagos[1,] <- (c(base$Hombres[20-19],0,0,0,0,0)*prima_ponderadaH)-(c(base$Hombres[20-19],0,0,0,0,0)*Gasto_inicial) # LLena primera fila
 
 
 vector_Montos <- c(prima_ponderadaH,prima_ponderadaH,prima_ponderadaH*0.6, -0, -Anua_5, -(SA_6 + Gasto_Terminacion) ) # vector de Montos con primas ponderadas
 
 
 # Llena las demás filas
 # Estado 3 = columna 4
 # Para el estado 6 se evita el "doble conteo", se restan los beneficios que se hayan pagado el año anterior para quienes se mantengan en el estado 6 luego de 1 año
 # Solo se toman en cuenta los beneficios de gente entrando en el estado 6
 for (i in 2:100) { 
   
   matriz1H_10_pob[i,]<-(matriz1H_10_pob[i-1,1]*matriz_hombres[1,,min(20-19+i-2,80),1]) 
   matriz1H_10_pob[i,]<-(matriz1H_10_pob[i-1,2]*matriz_hombres[2,,min(20-19+i-2,80),1]) + matriz1H_10_pob[i,]
   matriz1H_10_pob[i,]<-(matriz1H_10_pob[i-1,3]*matriz_hombres[3,,min(20-19+i-2,80),1]) + matriz1H_10_pob[i,]
   matriz1H_10_pob[i,]<-(matriz1H_10_pob[i-1,4]*matriz_hombres[4,,min(20-19+i-2,80),1]) + matriz1H_10_pob[i,]
   matriz1H_10_pob[i,]<-(matriz1H_10_pob[i-1,5]*matriz_hombres[5,,min(20-19+i-2,80),1]) + matriz1H_10_pob[i,]
   matriz1H_10_pob[i,]<-(matriz1H_10_pob[i-1,6]*matriz_hombres[6,,min(20-19+i-2,80),1]) + matriz1H_10_pob[i,]
   
   # Solo quitar los dobles conteos del estado 6
   matriz1H_10_pob[i,6] <- matriz1H_10_pob[i,6]- (matriz_hombres[6,6,min(20-19+i-2,80),1]*matriz1H_10_pob[i-1,6]) # Resta el monto total menos los montos pagados el año anterior
   
   
   matriz1H_10_pagos[i,]<- matriz1H_10_pob[i,]*vector_Montos * (1+inflacion)^(100-i)
   
 }
 matrizFinal_Hombres_10 <- matriz1H_10_pagos

options(scipen = 99) 
  

 
 #Itera las edades y los guardar en la matriz final, llena matrices para cada una de las edades restantes.
 
 # j edades
 for (j in 21:99) { # Hasta el 99 por supuesto según la base de datos
   
   matriz2H_10_pob <- matrix(0, ncol = 6, nrow = 100) # Crea matriz vacía
   matriz2H_10_pob[1,] <- c(base$Hombres[j-19],0,0,0,0,0) # LLena primera fila
   
   matriz2H_10_pagos <- matrix(0, ncol = 6, nrow = 100) # Crea matriz vacía
   matriz2H_10_pagos[1,] <- (c(base$Hombres[j-19],0,0,0,0,0)*prima_ponderadaH)-(c(base$Hombres[j-19],0,0,0,0,0)*Gasto_inicial) # LLena primera fila
   
   vector_Montos <- c(prima_ponderadaH,prima_ponderadaH,prima_ponderadaH*0.6, -0, -Anua_5, -(SA_6 + Gasto_Terminacion) ) # vector de Montos con primas ponderadas
   
   # i pasos
   for (i in 2:100) {
     
     matriz2H_10_pob[i,]<-(matriz2H_10_pob[i-1,1]*matriz_hombres[1,,min(j-19+i-2,80),1]) 
     matriz2H_10_pob[i,]<-(matriz2H_10_pob[i-1,2]*matriz_hombres[2,,min(j-19+i-2,80),1]) + matriz2H_10_pob[i,]
     matriz2H_10_pob[i,]<-(matriz2H_10_pob[i-1,3]*matriz_hombres[3,,min(j-19+i-2,80),1]) + matriz2H_10_pob[i,]
     matriz2H_10_pob[i,]<-(matriz2H_10_pob[i-1,4]*matriz_hombres[4,,min(j-19+i-2,80),1]) + matriz2H_10_pob[i,]
     matriz2H_10_pob[i,]<-(matriz2H_10_pob[i-1,5]*matriz_hombres[5,,min(j-19+i-2,80),1]) + matriz2H_10_pob[i,]
     matriz2H_10_pob[i,]<-(matriz2H_10_pob[i-1,6]*matriz_hombres[6,,min(j-19+i-2,80),1]) + matriz2H_10_pob[i,]
     
     matriz2H_10_pob[i,6] <- matriz2H_10_pob[i,6]- (matriz_hombres[6,6,min(j-19+i-2,80),1]*matriz2H_10_pob[i-1,6])
     
     matriz2H_10_pagos[i,] <- matriz2H_10_pob[i,] * vector_Montos * (1+inflacion)^(100-i)
   }
   
   matrizFinal_Hombres_10 <- matrizFinal_Hombres_10 + matriz2H_10_pagos  
   
 }
 
 round(matrizFinal_Hombres_10,3)
 
 #apply(matrizFinal_Hombres_10,1,sum)
 
 
######### Mujeres
## Se aplica lo mismo que el anterior pero para los datos de mujeres
 matriz1M_10_pob <- matrix(0, ncol = 6, nrow = 100) # Crea matriz vacía
 matriz1M_10_pob[1,] <- c(base$Mujeres[20-19],0,0,0,0,0) # LLena primera fila
 
 matriz1M_10_pagos <- matrix(0, ncol = 6, nrow = 100) # Crea matriz vacía
 matriz1M_10_pagos[1,] <- (c(base$Mujeres[20-19],0,0,0,0,0)*prima_ponderadaM)-(c(base$Mujeres[20-19],0,0,0,0,0)*Gasto_inicial) # LLena primera fila
 
 
 vector_Montos <- c(prima_ponderadaM,prima_ponderadaM,prima_ponderadaM*0.6, -0, -Anua_5, -(SA_6 + Gasto_Terminacion) ) # vector de Montos con primas ponderadas
 
 for (i in 2:100) { 
   
   matriz1M_10_pob[i,]<-(matriz1M_10_pob[i-1,1]*matriz_mujeres[1,,min(20-19+i-2,80),1]) 
   matriz1M_10_pob[i,]<-(matriz1M_10_pob[i-1,2]*matriz_mujeres[2,,min(20-19+i-2,80),1]) + matriz1M_10_pob[i,]
   matriz1M_10_pob[i,]<-(matriz1M_10_pob[i-1,3]*matriz_mujeres[3,,min(20-19+i-2,80),1]) + matriz1M_10_pob[i,]
   matriz1M_10_pob[i,]<-(matriz1M_10_pob[i-1,4]*matriz_mujeres[4,,min(20-19+i-2,80),1]) + matriz1M_10_pob[i,]
   matriz1M_10_pob[i,]<-(matriz1M_10_pob[i-1,5]*matriz_mujeres[5,,min(20-19+i-2,80),1]) + matriz1M_10_pob[i,]
   matriz1M_10_pob[i,]<-(matriz1M_10_pob[i-1,6]*matriz_mujeres[6,,min(20-19+i-2,80),1]) + matriz1M_10_pob[i,]
   
   
   matriz1M_10_pob[i,6] <- matriz1M_10_pob[i,6]- (matriz_mujeres[6,6,min(20-19+i-2,80),1]*matriz1M_10_pob[i-1,6]) # Resta el monto total menos los montos pagados el año anterior
   
   
   matriz1M_10_pagos[i,]<- matriz1M_10_pob[i,]*vector_Montos * (1+inflacion)^(100-i)
   
 }
 matrizFinal_Mujeres_10 <- matriz1M_10_pagos

 
 #Itera las edades y los guardar en la matriz final, llena matrices para cada una de las edades restantes.
 
 # j edades
 for (j in 21:99) { # Hasta el 99 por supuesto según la base de datos
   matriz2M_10_pob <- matrix(0, ncol = 6, nrow = 100) # Crea matriz vacía
   matriz2M_10_pob[1,] <- c(base$Mujeres[j-19],0,0,0,0,0) # LLena primera fila
   
   matriz2M_10_pagos <- matrix(0, ncol = 6, nrow = 100) # Crea matriz vacía
   matriz2M_10_pagos[1,] <- (c(base$Mujeres[j-19],0,0,0,0,0)*prima_ponderadaM)-(c(base$Mujeres[j-19],0,0,0,0,0)*Gasto_inicial) # LLena primera fila
   
   vector_Montos <- c(prima_ponderadaM,prima_ponderadaM,prima_ponderadaM*0.6, -0, -Anua_5, -(SA_6 + Gasto_Terminacion) ) # vector de Montos con primas ponderadas
   
   # i pasos
   for (i in 2:100) {
     
     matriz2M_10_pob[i,]<-(matriz2M_10_pob[i-1,1]*matriz_mujeres[1,,min(j-19+i-2,80),1]) 
     matriz2M_10_pob[i,]<-(matriz2M_10_pob[i-1,2]*matriz_mujeres[2,,min(j-19+i-2,80),1]) + matriz2M_10_pob[i,]
     matriz2M_10_pob[i,]<-(matriz2M_10_pob[i-1,3]*matriz_mujeres[3,,min(j-19+i-2,80),1]) + matriz2M_10_pob[i,]
     matriz2M_10_pob[i,]<-(matriz2M_10_pob[i-1,4]*matriz_mujeres[4,,min(j-19+i-2,80),1]) + matriz2M_10_pob[i,]
     matriz2M_10_pob[i,]<-(matriz2H_10_pob[i-1,5]*matriz_mujeres[5,,min(j-19+i-2,80),1]) + matriz2M_10_pob[i,]
     matriz2M_10_pob[i,]<-(matriz2M_10_pob[i-1,6]*matriz_mujeres[6,,min(j-19+i-2,80),1]) + matriz2M_10_pob[i,]
     
     
     matriz2M_10_pob[i,6] <- matriz2M_10_pob[i,6]- (matriz_mujeres[6,6,min(j-19+i-2,80),1]*matriz2M_10_pob[i-1,6])
     
     matriz2M_10_pagos[i,] <- matriz2M_10_pob[i,] * vector_Montos * (1+inflacion)^(100-i)
   }
   
   matrizFinal_Mujeres_10 <- matrizFinal_Mujeres_10 + matriz2M_10_pagos  
   
 }
 
 round(matrizFinal_Mujeres_10,3)
 

### Graficacion de cada una de las matrices
 
## Punto 8
 
 #Hombres
 df_Hombres_8 <- as.data.frame(matrizFinal_Hombres_8)
 df_Hombres_8 <- df_Hombres_8 %>% mutate(Año= c(0:99))
 colnames(df_Hombres_8) <- c("Estado_0", "Estado_1", "Estado_2", "Estado_3", "Estado_4", "Estado_5", "Año" )
 
 

 grafico_Hombres_8 <- ggplot(data = df_Hombres_8,mapping= aes(x = Año)) +
   geom_line(aes(y = Estado_0, color = "Estado 0"), linewidth=1.05) +
   geom_line(aes(y = Estado_1, color = "Estado 1"), linewidth=1.05) +
   geom_line(aes(y = Estado_2, color = "Estado 2"), linewidth=1.05) +
   geom_line(aes(y = Estado_3, color = "Estado 3"), linewidth=1.05) +
   geom_line(aes(y = Estado_4, color = "Estado 4"), linewidth=1.05) +
   geom_line(aes(y = Estado_5, color = "Estado 5"), linewidth=1.05) +
   scale_x_continuous(breaks = seq(0, 100, by = 20)) +
   scale_y_continuous(labels= comma, breaks = seq(0, max(df_Hombres_8$Estado_0), by = 500000)) +
   labs( y="Población", color="Estado", x="Año de proyección")+
   scale_color_manual(values = c("Estado 0" = "green", "Estado 1" = "blue", "Estado 2"="yellow", "Estado 3" = "cyan", "Estado 4"="purple", "Estado 5"= "red"))+
   theme_cowplot(12)
 
grafico_Hombres_8
 

 #Mujeres 
 
 df_Mujeres_8 <- as.data.frame(matrizFinal_Mujeres_8)
 df_Mujeres_8 <- df_Mujeres_8 %>% mutate(Edad= c(0:99))
 colnames(df_Mujeres_8) <- c("Estado_0", "Estado_1", "Estado_2", "Estado_3", "Estado_4", "Estado_5", "Edad" )
 
 
 grafico_Mujeres_8 <- ggplot(data = df_Mujeres_8,mapping= aes(x = Edad)) +
   geom_line(aes(y = Estado_0, color = "Estado 0"), linewidth=1.05) +
   geom_line(aes(y = Estado_1, color = "Estado 1"), linewidth=1.05) +
   geom_line(aes(y = Estado_2, color = "Estado 2"), linewidth=1.05) +
   geom_line(aes(y = Estado_3, color = "Estado 3"), linewidth=1.05) +
   geom_line(aes(y = Estado_4, color = "Estado 4"), linewidth=1.05) +
   geom_line(aes(y = Estado_5, color = "Estado 5"), linewidth=1.05) +
   scale_x_continuous(breaks = seq(0, 100, by = 20)) +
   scale_y_continuous(labels= comma, breaks = seq(0, max(df_Mujeres_8$Estado_0), by = 500000)) +
   labs( y="Población", color="Estado", x="Año de proyección")+
   scale_color_manual(values = c("Estado 0" = "green", "Estado 1" = "blue", "Estado 2"="yellow", "Estado 3" = "cyan", "Estado 4"="purple", "Estado 5"= "red"))+
   theme_cowplot(12)
 
 

 grafico_Mujeres_8
  

 
 ###### Punto 10
 
##Hombres 
 
 df_Hombres_10 <- as.data.frame(matrizFinal_Hombres_10)
 df_Hombres_10 <- df_Hombres_10 %>% mutate(Año= c(0:99))
 colnames(df_Hombres_10) <- c("Estado_0", "Estado_1", "Estado_2", "Estado_3", "Estado_4", "Estado_5", "Año" )

 
 
 grafico_Hombres_10 <- ggplot(data = df_Hombres_10,mapping= aes(x = Año)) +
   geom_line(aes(y = Estado_0, color = "Estado 0"), linewidth=1.05) +
   geom_line(aes(y = Estado_1, color = "Estado 1"), linewidth=1.05) +
   geom_line(aes(y = Estado_2, color = "Estado 2"), linewidth=1.05) +
   geom_line(aes(y = Estado_3, color = "Estado 3"), linewidth=1.05) +
   geom_line(aes(y = Estado_4, color = "Estado 4"), linewidth=1.05) +
   geom_line(aes(y = Estado_5, color = "Estado 5"), linewidth=1.05) +
   scale_x_continuous(breaks = seq(0, 100, by = 20)) +
   scale_y_continuous(labels= comma, breaks = seq(-20000000000, max(df_Hombres_10$Estado_0), by =25000000000)) +
   #scale_y_log10()+
   labs( y="Montos", color="Estado", x="Año de proyección")+
   scale_color_manual(values = c("Estado 0" = "green", "Estado 1" = "blue", "Estado 2"="yellow", "Estado 3" = "cyan", "Estado 4"="purple", "Estado 5"= "red"))+
   theme_cowplot(12)
 
 grafico_Hombres_10
 
 
 ### Mujeres
 
 df_Mujeres_10 <- as.data.frame(matrizFinal_Mujeres_10)
 df_Mujeres_10 <- df_Mujeres_10 %>% mutate(Edad= c(0:99))
 colnames(df_Mujeres_10) <- c("Estado_0", "Estado_1", "Estado_2", "Estado_3", "Estado_4", "Estado_5", "Edad" )
 
 
 grafico_Mujeres_10 <- ggplot(data = df_Mujeres_10,mapping= aes(x = Edad)) +
   geom_line(aes(y = Estado_0, color = "Estado 0"), linewidth=1.05) +
   geom_line(aes(y = Estado_1, color = "Estado 1"), linewidth=1.05) +
   geom_line(aes(y = Estado_2, color = "Estado 2"), linewidth=1.05) +
   geom_line(aes(y = Estado_3, color = "Estado 3"), linewidth=1.05) +
   geom_line(aes(y = Estado_4, color = "Estado 4"), linewidth=1.05) +
   geom_line(aes(y = Estado_5, color = "Estado 5"), linewidth=1.05) +
   scale_x_continuous(breaks = seq(0, 100, by = 20)) +
   scale_y_continuous(labels= comma, breaks = seq(-20000000000, max(df_Mujeres_10$Estado_0), by =25000000000)) +
   labs( y="Población", color="Estado", x="Año de proyección")+
   scale_color_manual(values = c("Estado 0" = "green", "Estado 1" = "blue", "Estado 2"="yellow", "Estado 3" = "cyan", "Estado 4"="purple", "Estado 5"= "red"))+
   theme_cowplot(12)
 
 
 grafico_Mujeres_10
 
 
 
 