#Leemos el archivo con los permisos concedidos a la cuenta de drive

#install.packages("googledrive")
library("googledrive")
library(purrr)
# creamos el URL y obtenemos el acceso
folder_url <- "https://drive.google.com/drive/u/0/folders/15wxhxZ80t8TJ3ZLL3btXrMBlUygNJM1l"
folder <- drive_get(as_id(folder_url))

## identificamos los archivos en el drive y los descargamos
csv_files <- drive_ls(folder, type = "csv")
walk(csv_files$id, ~ drive_download(as_id(.x)))

#install.packages("caret")
library(caret)
#Leemos el archivo descargado
df<- read.csv("datos.csv")

#AJUSTE DE MODELO -----
#attach(df)
#str(df)
#Modelo 1 ---- incluye la mayoria de las variables del DF
mod1<-glm(enfermedad_cardiaca~edad+presion_sistolica+sexo+presion_sistolica+
            colesterol+glucosa+fumador+alcoholico+actividad_fisica+peso+altura+imc,family = binomial(),df)
summary(mod1)


#STEP Nos da un mejor modelo considerando un AIC menor (Criterio de información de Akaike)
step(mod1, test="LRT")

# De acuerdo con lo anterior creamos el modelo "final" con las variables significativas para poder replicarlo
mod2<-glm(enfermedad_cardiaca~edad+presion_sistolica+colesterol+glucosa+fumador+alcoholico+
            actividad_fisica+peso+altura,family=binomial(),df)
summary(mod2)


