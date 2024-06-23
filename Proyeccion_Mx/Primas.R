library(readxl)
library(ggplot2)
library(expm)
library(Matrix)
library(matrixcalc)
library(tidyverse)
library(dplyr)
library(ggpubr)
library(cowplot)

#supuestos
interes=0.07246 #i
inflación=0.0279 #j

#se crea el j equivalente
j=(interes-inflación)/(1+inflación)
  
#calcula la v
v=1/(1+j)




# Funcion para el calculo de anualidades
# Recibe como parámetros estado inicial, final, edad y sexo
# Toma en cuenta que los individuos comienzan en estado 0
# Suma de los v^k*kpx^{ij} para cada ij 

Anualidades<-function(inicial,final,edad,sexo){
  
  #Elije en que tabla trabajar
  if (sexo=="H"){
    matrix=matriz_hombres
  }else{if(sexo=="M"){
    matrix=matriz_mujeres
  }}
  
  valor=c()
  #incializa la sumatoria tomando en cuenta los casos de i=j con k=0
  if (inicial==final) {
    valor[1]=v^0*1
  }else{
    valor[1]=0
  }
  
  #continua la sumatoria llegando hasta la edad-1 ya que es vitalicia
  for (k in 1:100) {
    valor[k+1]=v^(k)*matrix[inicial,final, (edad-19), k]
  }
  suma=sum(valor)
  return(suma)
}




# Funcion para el calculo del seguro
# Recibe como parámetros estado inicial, final, edad y sexo
# Toma en cuenta que los individuos comienzan en estado 0

seguros<-function(inicial, final,edad,sexo){
  
  #Elije en que tabla trabajar
  if (sexo=="H"){
    Matrix=matriz_hombres
  }else{if(sexo=="M"){
    Matrix=matriz_mujeres
  }}
  
  
  valor<-c(rep(0,101))
  #incializa la sumatoria tomando en cuenta los casos de i=j con k=0
  if (inicial==final) {
    valor[1]=v^0*1
  }else{
    valor[1]=0
  }
  
  #caso k igual a 1
  valor[2]=v^1*Matrix[inicial,final,edad-19,1]
  
  #sumatoria de los k hasta los 100 (edad Maxima) 
  for (k in 2:100) {
    #sumatoria de los estados 
    for(i in 1:6){
      #evitando el estado final
      if(i!=final){
       valor[k+1]=valor[k+1]+v^k* Matrix[inicial,i,edad-19,k-1]*Matrix[i,final,edad-19,1]
      }
    }
  }
  

  suma=sum(valor)
  return(suma)
}


#Calculo de las primas por sexo y edad según principio de equivalencia
Primas<-function(edad, sexo){
 
   #Beneficios
  SA_6 = 150000
  Anua_5 = 30000
  
  #gastos
  Gasto_inicial= 200
  Gasto_mantenimiento= 0.05 #de las primas
  Gasto_Terminacion = 500
  
  # Egresos
  A = (SA_6+Gasto_Terminacion)*seguros(1,6,edad,sexo)+ (Anua_5)*Anualidades(1,5,edad,sexo)+Gasto_inicial
  #Ingresos
  B = (1-Gasto_mantenimiento)*(Anualidades(1,1,edad,sexo)+Anualidades(1,2,edad,sexo)+0.6*Anualidades(1,3,edad,sexo))

  return(A/B)
}

#Creacion de los vectores de primas
Primas_hombres <- c()
Primas_mujeres <- c()

for (i in 20:99){
  Primas_hombres[i-19] <- Primas(i, sexo="H")
  Primas_mujeres[i-19] <- Primas(i, sexo="M")
}



# Graficación de las primas por sexo
 
 data = data.frame(Edad = 20:99, Ph = Primas_hombres, Pm = Primas_mujeres)
 
 
graf_primas <- ggplot(data, mapping= aes(x = Edad)) +
   geom_line(aes(y = Ph,color = "Hombres"), size=1.1) +
   geom_line(aes(y = Pm, color = "Mujeres"), size=1.1) +
   labs(x="Edad", y="Monto", color="Sexo")+
   scale_color_manual(values = c("Hombres" = "dodgerblue1", "Mujeres" = "hotpink2"), labels=c("Hombres", "Mujeres") )+
   theme_cowplot(12)
 
 graf_primas
 
 base <- datosFinal_v2 %>% filter(Edades>=20 & Edades<=99)
 

#Ponderación de las primas

prima_ponderadaH<-sum((Primas_hombres*base$Hombres))/sum(base$Hombres)

prima_ponderadaM<-sum(Primas_mujeres*base$Mujeres)/sum(base$Mujeres)



