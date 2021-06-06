#Leemos el archivo con los permisos concedidos a la cuenta de drive

#install.packages("googledrive")
#library("googledrive")
#library(purrr)
# creamos el URL y obtenemos el acceso
#folder_url <- "https://drive.google.com/drive/u/0/folders/15wxhxZ80t8TJ3ZLL3btXrMBlUygNJM1l"
#folder <- drive_get(as_id(folder_url))

## identificamos los archivos en el drive y los descargamos
#csv_files <- drive_ls(folder, type = "csv")
#walk(csv_files$id, ~ drive_download(as_id(.x)))

#install.packages("caret")
library(caret)
#Leemos el archivo descargado
df<- read.csv("datos_adultos.csv")



#AJUSTE DE MODELO -----
#Modelo 1 ---- incluye la mayoria de las variables del DF
mod1<-glm(enfermedad_cardiaca~edad+sexo+presion_sistolica+presion_sistolica+
            colesterol+glucosa+fumador+alcoholico+actividad_fisica+peso+altura,family = binomial(),df)

summary(mod1)


#STEP Nos da un mejor modelo considerando un AIC menor (Criterio de información de Akaike)
step(mod1, test="LRT")
    

#creamos el mejor modelo obtenido con ayuda de  tep, el modelo contiene las variables
# Edad, presion sistolica, colesterol, glucosa, fumaodr, alcoholico, activiadad_fisia, peso y altura

mod2<-glm(enfermedad_cardiaca~edad+presion_sistolica+colesterol+glucosa+fumador+alcoholico+
            actividad_fisica+peso+altura,family=binomial(),df)

#Veamos que en efecto todas las variables en el modelo sean significativas.
summary(mod2)

