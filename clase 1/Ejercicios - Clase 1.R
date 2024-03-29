####Ejercicios Clase 1 ####

##Ejercicio 1##

funcion_ejercicio_1 <- function(matriz) {
  # Obtengo las dimensiones
  print(dim(matriz))
  # Chequeo si el primer elemento es par
  primer_elem <- matriz[1,1]
  if (primer_elem %% 2 == 0) {
    print('El primer elemento es par')
  }
  else{print('El primer elemento no es par')}
}

## Ejercicio 2##
 
# 1) Crear un dataframe con el dataset de R: state.x77
df <- state.x77

#2)a) ¿Cuál es la población total de Estados Unidos?
apply(X=df, MARGIN =2 , FUN=sum)
#b) ¿Cuál es la media de la expectativa de vida?
apply(X=df, MARGIN =2 , FUN=mean)
#c) ¿Cual es la mediana del ingreso en pesos argentinos?
apply(X=df, MARGIN =2 , FUN=function(x) median(x)*30.4)
# 3) Crear el dataset con las dos columnas
df_indice <- df[,c(3, 5)]
# Crear el indice
ilit_murd <- apply(X=df_indice, MARGIN =1 , FUN=sum)

#Para buscar los máximos y mínimos armo una máscara booleana
ilit_murd[ilit_murd==max(ilit_murd)]
ilit_murd[ilit_murd==min(ilit_murd)]

########### EJERCICIOS FINALES ##########################

rm(list=ls())

# - Crear un OBJETO llamado _OBJETO_ definido como el resultado de la suma: 5 + 6
OBJETO <- 5+6

# - Crear un VECTOR _VEC0_ que contenga los numeros 1, 3, 4.
VEC0 <- c(1,3,4)

# - Crear 3 vectores ( _VEC1_, _VEC2_, _VEC3_) que sean transformaciones del anterior.
VEC1 <- VEC0*2 
VEC2 <- VEC0^2 
VEC3 <- VEC0-2 

# - Crear 3 vectores con la misma cantidad de elementos que VEC0, pero con variables string (texto) ( _VEC4_, _VEC5_, _VEC6_),  
VEC4 <- c("NO","NO","SI") 
VEC5 <- c("PAGO","PAGO","LIBRE")
VEC6 <- c("SAS","SPSS","R")

# - Crear un dataframe _DFRAME_ como combinación de todos los vectores creados previamente
DFRAME <- data.frame(VEC0,VEC1,VEC2,VEC3,VEC4,VEC5,VEC6)

# - Crear una lista _LA_LISTA_ con el Objeto creado, Un vector cualquiera y el DATAFRAME
LALISTA <- list(OBJETO,VEC0,DFRAME) 

# - Para todos los valores del vector _Vec0_, imprimir mediante un loop el triple de dichos valores
for(i in VEC0){
  print(i*3)
}

# - Mediante un loop que itere sobre la variable VEC6 del dataframe _DFRAME_,
## imprimir un texto que combine el Valor de VEC6 y de VEC 0
for(i in unique(DFRAME$VEC6)){

A <- paste(DFRAME$VEC6[VEC6==i],DFRAME$VEC0[VEC6==i])
print(A)

}
# - Reescribir el VEC1 del DATAFRAME para que sus elementos sean:
## El Doble de VEC_0, cuando éste sea mayor a 2
## Iguales a VEC_0, para el resto de los casos 

DFRAME$VEC1 <- ifelse(DFRAME$VEC0>2,DFRAME$VEC0*2,DFRAME$VEC0)

# - Crear una función llamada _Hola_Mundo_ que imprima el texto "Hola mundo"
Hola_Mundo <- function(){
  print("Hola mundo")
}

# - Crear una función devuelva la sumatoria de los numeros enteros comprendidos entre 1 y un parametro x a definir
Sumatoria_enteros<- function(x){
  y <- 1:x 
  sum(y)
}

# - Levantar la base Individual del 1er trimestre de 2017, de la EPH

individual_t117 <- read.table("../Fuentes/usu_individual_t117.txt",
                              sep=";",
                              dec=",",
                              header = TRUE,
                              fill = TRUE)

# - Guardar la base como un archivo de extensión .RDS
saveRDS(individual_t117,"../Fuentes/individual_t117.RDS")

# - Volver a levantar la base, pero como .RDS y asignarla conel nombre _BaseRDS_ ¿tarda más o menos?
BaseRDS <- readRDS("../Fuentes/individual_t117.RDS")

# - Levantar del Excel llamado CANASTAS que se encuentra en la carpeta de Fuentes, la hoja "CBT".
## y Definirla como Hoja_CBT. Pueden usar la función _read.xlsx_ de la librería __xlsx__ 
## o la función read_excel de la libreria __readxl__
library(xlsx)
library(readxl)

Hoja_CBT_1 <- read.xlsx(file = "../Fuentes/CANASTAS.xls",sheetName = "CBT")
class(Hoja_CBT_1)

Hoja_CBT_2 <- read_excel(path = "../Fuentes/CANASTAS.xls",sheet = "CBT")
class(Hoja_CBT_2)
# - Levantar el mismo Excel, utilizando un Objeto que contenga el directorio del arhico a levantar.
##  chequear con ```dir.exist()``` que lo creamos bien (¿no funcionó? pista: /\\)
##  Levantar el mismo excel utilizando los nombres de directorio como objetos.

fuentes.dir <- "../Fuentes/" 
dir.exists(fuentes.dir)
Hoja_CBT <- read.xlsx(file = paste0(fuentes.dir,"CANASTAS.xls"),sheetName = "CBT",
                      encoding = "UTF-8")
