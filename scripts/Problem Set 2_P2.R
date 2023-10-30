#Problem Set 2
# Segunda Parte: En este script se hace mas pruebas para el modelo OLS y se realizan Tecnicas de Ensablaje.

# integrantes
  # Celin Hernández: 202210067  
  # Merit Tejeda: 202210104
# instalación  de paquetes

#install.packages("caret")
#install.packages("rpart.plot")
#install.packages("ipred")
#install.packages("readxl")
#install.packages("readr")
#install.packages("pacman")
#install.packages("osmdata")
#install.packages("leaflet")
#install.packages("dplyr")
#install.packages("rgeos")
#install.packages("openxlsx")
#install.packages("ggplot2")
#install.packages("writexl")
#install.packages("geosphere")
#install.packages("ggmap")
#install.packages("geopy")
#install.packages("stargazer")
#install.packages("zoo")
#install.packages("gridExtra")
#install.packages("tidyverse")
#install.packages("rvest")
#install.packages("sf")
#install.packages("glmnet")
#install.packages("broom")

library(rsample)
library(yardstick)
library(osmdata)
library(leaflet)
library(ggplot2)
library(openxlsx)
library(pacman)
library(readxl)
library(dplyr)
library(writexl)
library(geosphere)
library(ggmap)
library(stargazer)
library(gridExtra)
library(zoo)
library(tidyverse)
library(rvest)
library(sf)
library(readr)
library(glmnet)
library(broom)
library(tidymodels)
library(yardstick)
library(caret)
library(ipred)
library(caret)
library(rpart.plot)


# Cargar las librerías listadas e instalarlas en caso de ser necesario
p_load(tidyverse, # Manipular dataframes
       rio, # Import data easily
       plotly, # Gráficos 
       leaflet, # Mapas interactivos
       rgeos, # Calcular centroides de un poligono
       tmaptools, # geocode_OSM()
       sf, # Leer/escribir/manipular datos espaciales
       osmdata, # Get OSM's data 
       tidymodels,
       dplyr,
       caret) #para modelos de ML

submission_template <- read.xlsx("https://github.com/chernan77/Data_Taller2/raw/main/submission_template.xlsx")
train <- read.xlsx("https://github.com/chernan77/Data_Taller2/raw/main/train.xlsx")
test <- read.xlsx("https://github.com/chernan77/Data_Taller2/raw/main/test.xlsx")
estrato <- read.xlsx("https://github.com/chernan77/Data_Taller2/raw/main/estrato.xlsx")

library(readxl)

# Cargar las librerías listadas e instalarlas en caso de ser necesario
p_load(tidyverse, # Manipular dataframes
       rio, # Import data easily
       plotly, # Gráficos 
       leaflet, # Mapas interactivos
       rgeos, # Calcular centroides de un poligono
       tmaptools, # geocode_OSM()
       sf, # Leer/escribir/manipular datos espaciales
       osmdata, # Get OSM's data 
       tidymodels,
       dplyr) #para modelos de ML


# Define the URL of the Excel file
excel_urls <- c(
  "https://github.com/chernan77/Data_Taller2/raw/main/Tabla_train_casas.xlsx",
  "https://github.com/chernan77/Data_Taller2/raw/main/Tabla_train_apart.xlsx",
  "https://github.com/chernan77/Data_Taller2/raw/main/Tabla_test_casas.xlsx",
  "https://github.com/chernan77/Data_Taller2/raw/main/Tabla_test_apart.xlsx"
)

# Create empty data frames to store the data from each Excel file
train_casas1 <- data.frame()
train_apart1 <- data.frame()
test_casas1 <- data.frame()
test_apart1 <- data.frame()
# Download and read each Excel file
for (url in excel_urls) {
  temp_file <- tempfile(fileext = ".xlsx")
  download.file(url, temp_file, mode = "wb")
  
  if (grepl("Tabla_train_casas", url)) {
    train_casas1 <- read_excel(temp_file)
  } else if (grepl("Tabla_train_apart", url)) {
    train_apart1 <- read_excel(temp_file)
  } else if (grepl("Tabla_test_casas", url)) {
    test_casas1 <- read_excel(temp_file)
  } else if (grepl("Tabla_test_apart", url)) {
    test_apart1 <- read_excel(temp_file)
  }
  
  # Clean up by removing the temporary file
  file.remove(temp_file)
}
combined_train <- rbind(train_apart1, train_casas1)
combined_test <- rbind(test_apart1, test_casas1)

### A continuación se hacen transformaciones en todas lso dataframe utilizadas para estos modelos
### --------------------------------------Combined_train --------------------------------#

combined_train$Habitaciones2 <- combined_train$Habitaciones^2
combined_train$M2_por_Habitacion_Garaje <- combined_train$M2_por_Habitación * combined_train$Garaje
combined_train$M2_por_Habitacion <- combined_train$M2_por_Habitación 
combined_train$Sala_BBQ_terraza <- combined_train$Sala_BBQ * combined_train$Terraza
combined_train$year <- as.character(combined_train$year)
combined_train$month <- as.character(combined_train$month)
combined_train$Fecha <- as.Date(paste0(combined_train$year, "-", combined_train$month, "-01"))
combined_train$Fecha <- as.Date(combined_train$Fecha)
combined_train$Año <- as.numeric(combined_train$year)

combined_test$Habitaciones2 <- combined_test$Habitaciones^2
combined_test$M2_por_Habitacion_Garaje <- combined_test$M2_por_Habitacion * combined_test$Garaje
combined_test$Sala_BBQ_terraza <- combined_test$Sala_BBQ * combined_test$Terraza
combined_test$year <- as.character(combined_test$year)
combined_test$month <- as.character(combined_test$month)
combined_test$Fecha <- as.Date(paste0(combined_test$year, "-", combined_test$month, "-01"))
combined_test$Fecha <- as.Date(combined_test$Fecha)
combined_test$Año <- as.numeric(combined_test$year)

# -------------------------------CREACION DE OTRAS VARIABLES-------------------------- # 
train_casas1$M2_por_Habitacion<- train_casas1$Area/train_casas1$Habitaciones
train_casas1$Habitaciones2 <- train_casas1$Habitaciones^2
train_casas1$M2_por_Habitacion_Garaje <- train_casas1$M2_por_Habitacion * train_casas1$Garaje
train_casas1$Sala_BBQ_terraza <- train_casas1$Sala_BBQ * train_casas1$Terraza
train_casas1$year <- as.character(train_casas1$year)
train_casas1$month <- as.character(train_casas1$month)
train_casas1$Fecha <- as.Date(paste0(train_casas1$year, "-", train_casas1$month, "-01"))
train_casas1$Fecha <- as.Date(train_casas1$Fecha)
train_casas1$Año <- as.numeric(train_casas1$year)

# -------------------------------CREACION DE OTRAS VARIABLES-------------------------- # 
train_apart1$M2_por_Habitacion<- train_apart1$Area/train_apart1$Habitaciones
train_apart1$Habitaciones2<- train_apart1$Habitaciones^2
train_apart1$M2_por_Habitacion_Garaje <- train_apart1$M2_por_Habitacion * train_apart1$Garaje
train_apart1$Sala_BBQ_terraza <- train_apart1$Sala_BBQ * train_apart1$Terraza
train_apart1$year <- as.character(train_apart1$year)
train_apart1$month <- as.character(train_apart1$month)
train_apart1$Fecha <- as.Date(paste0(train_apart1$year, "-", train_apart1$month, "-01"))
train_apart1$Fecha <- as.Date(train_apart1$Fecha)
train_apart1$Año <- as.numeric(train_apart1$year)


# -------------------------------CREACION DE OTRAS VARIABLES CASAS-------------------------# 

test_casas1$Habitaciones2 <- test_casas1$Habitaciones^2
test_casas1$M2_por_Habitacion_Garaje <- test_casas1$M2_por_Habitacion * test_casas1$Garaje
test_casas1$Sala_BBQ_terraza <- test_casas1$Sala_BBQ * test_casas1$Terraza
test_casas1$year <- as.character(test_casas1$year)
test_casas1$month <- as.character(test_casas1$month)
test_casas1$Fecha <- as.Date(paste0(test_casas1$year, "-", test_casas1$month, "-01"))
test_casas1$Fecha <- as.Date(test_casas1$Fecha)
test_casas1$Año <- as.numeric(test_casas1$year)

#test_casas1_1 <- "C:/Output R/Taller 2/Taller_2/tabla_train_apart.xlsx"  
#write_xlsx(test_casas1, tabla_train_apart)

# -------------------------------CREACION DE OTRAS VARIABLES CASAS-------------------------# 

test_apart1$Habitaciones2 <- test_apart1$Habitaciones^2
test_apart1$M2_por_Habitacion_Garaje <- test_apart1$M2_por_Habitacion * test_apart1$Garaje
test_apart1$Sala_BBQ_terraza <- test_apart1$Sala_BBQ * test_apart1$Terraza
test_apart1$year <- as.character(test_apart1$year)
test_apart1$month <- as.character(test_apart1$month)
test_apart1$Fecha <- as.Date(paste0(test_apart1$year, "-", test_apart1$month, "-01"))
test_apart1$Fecha <- as.Date(test_apart1$Fecha)
test_apart1$Año<- as.numeric(test_apart1$year)


########### Crear la Base de Datos de Usaquen
# Segmenta la base de datos en localidad de Usaquén (entrenamiento) y resto de localidades (prueba)
localidad_entrenamiento <- "Usaquén"  # Reemplaza con el nombre de tu localidad de interés
test_data <- combined_train[combined_train$localidad != localidad_entrenamiento, ]
localidades_a_excluir <- c("Fontibón", "Los Martires")
test_data <- test_data[!(test_data$localidad %in% localidades_a_excluir), ]
train_data1 <- subset(combined_train, localidad == localidad_entrenamiento)
test_data1 <- test_data[!(test_data$localidad %in% localidades_a_excluir), ]

### Se atransforman variables para esta una base de datos 

train_data1$M2_por_Habitacion<- train_data1$Area/train_data1$Habitaciones
train_data1$Habitaciones2 <- train_data1$Habitaciones^2
train_data1$M2_por_Habitacion_Garaje <- train_data1$M2_por_Habitacion * train_data1$Garaje
train_data1$Sala_BBQ_terraza <- train_data1$Sala_BBQ * train_data1$Terraza
train_data1$year <- as.character(train_data1$year)
train_data1$month <- as.character(train_data1$month)
train_data1$Fecha <- as.Date(paste0(train_data1$year, "-", train_data1$month, "-01"))
train_data1$Fecha <- as.Date(train_data1$Fecha)
train_data1$lPrecio<- log(train_data1$Precio)
train_data1$Año <- as.numeric(train_data1$year)

test_data1$M2_por_Habitacion<- test_data1$Area/test_data1$Habitaciones
test_data1$Habitaciones2 <- test_data1$Habitaciones^2
test_data1$M2_por_Habitacion_Garaje <- test_data1$M2_por_Habitacion * test_data1$Garaje
test_data1$Sala_BBQ_terraza <- test_data1$Sala_BBQ * test_data1$Terraza
test_data1$year <- as.character(test_data1$year)
test_data1$month <- as.character(test_data1$month)
test_data1$Fecha <- as.Date(paste0(test_data1$year, "-", test_data1$month, "-01"))
test_data1$Fecha <- as.Date(test_data1$Fecha)
test_data1$lPrecio<- log(test_data1$Precio)
test_data1$Año <-as.numeric(test_data1$year)

##### Se segmenta la Base de Datos para el Estrato 4
train_apart4 <- train_apart1[train_apart1$Estrato == 4, c("property_id","title", "Fecha", "localidad","Precio", "lPrecio",
                                                          "Precio_M2", "Habitaciones","Habitaciones2", "Baños", "Area","M2_por_Habitacion", "lat", "lon", "Terraza",
                                                          "Garaje", "Sala_BBQ","Piscina","Gimnasio", "Chimenea","Seguridad",
                                                          "Dist_Parques", "Dist_Transp_Publico", "Dist_Establecimientos",
                                                          "Dist_C_Comerc", "Dist_Centros_Educ", "Dist_Restaurantes", "Dist_Bancos", "Sala_BBQ_terraza","Año")]
train_casas4 <- train_casas1[train_casas1$Estrato == 4, c("property_id","title", "Fecha", "localidad","Precio", "lPrecio",
                                                          "Precio_M2", "Habitaciones","Habitaciones2", "Baños", "Area","M2_por_Habitacion", "lat", "lon", "Terraza",
                                                          "Garaje", "Sala_BBQ","Piscina","Gimnasio", "Chimenea","Seguridad",
                                                          "Dist_Parques", "Dist_Transp_Publico", "Dist_Establecimientos",
                                                          "Dist_C_Comerc", "Dist_Centros_Educ", "Dist_Restaurantes", "Dist_Bancos", "Sala_BBQ_terraza", "Año")]
## Se une las base de datos para casas y apartamentos del estarto 4
combined_train4 <- rbind(train_apart4, train_casas4)
combined_data <- rbind(train_apart1, train_casas1)
filtered_data <- subset(combined_data, localidad != "Usaquén")

#----------------------------------------------------RAMDOM FOREST----------------------------------------------------------#
#-----------------------------------------------------Estrato 4-------------------------------------------------------------#

# se divide el conjunto de datos en entrenamiento y prueba para el estrato 4
set.seed(123)

training_indices4 <- initial_split(train_apart4, prop = 0.7)
train_data4a <- training_indices4 %>% training()
test_data4a <- training_indices4 %>% testing()
training_indices4c <- initial_split(train_casas4, prop = 0.7)
train_data4c <- training_indices4c %>% training()
test_data4c <- training_indices4c %>% testing()
combined_test4 <- rbind(test_data4c, test_data4a)

fitControl<-trainControl(method ="cv",
                         number=5)

# Ajusta el modelo en el conjunto de entrenamiento
model4a <- train(
  lPrecio ~ Area+ Habitaciones + Habitaciones2 + lat+lon + Baños+ Año + M2_por_Habitacion + Terraza + Garaje + Sala_BBQ + Gimnasio + Sala_BBQ_terraza + Chimenea + Seguridad + Dist_Parques + 
    Dist_Transp_Publico + Dist_Establecimientos + Dist_C_Comerc + Dist_Centros_Educ + Dist_Restaurantes + Dist_Bancos, 
  data = train_data4a,
  method = "ranger",
  trControl = fitControl,
  tuneGrid = expand.grid(
    mtry = c(1, 2, 3,4,5,6),
    splitrule = "variance",
    min.node.size = c(5, 10, 15,20)),
  importance = "impurity"  
)


# Calcular la importancia de las variables para el modelo 4a
importance_scores_model4a <- model4a$finalModel$variable.importance
importance_scores_model4a
# Crear un data frame con las variables y su importancia
importance_data_model4a <- data.frame(
  Variable = names(importance_scores_model4a),
  Importance = importance_scores_model4a
)

# Crear el gráfico de barras
plot_model4a <- ggplot(data = importance_data_model4a, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Apartamentos", x = "Variable", y = "Importancia") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# Realiza predicciones en el conjunto de prueba
test_data4a$predictions <-predict(model4a, newdata = test_data4a)

# Calcular el MAE de manera manual
mae_value1 <- mean(abs(exp(test_data4a$predictions) - exp(test_data4a$lPrecio)), na.rm = TRUE)

# Imprimir el valor del MAE
cat("MAE en datos de prueba:", mae_value1, "\n")

model4c <- train(
  lPrecio ~ Area+ Habitaciones + Habitaciones2 + Baños + lat + lon+ Año+ M2_por_Habitacion + Terraza + Garaje + Sala_BBQ + Gimnasio + Sala_BBQ_terraza + Chimenea + Seguridad + Dist_Parques + 
    Dist_Transp_Publico + Dist_Establecimientos + Dist_C_Comerc + Dist_Centros_Educ + Dist_Restaurantes + Dist_Bancos, 
  data = train_data4c,
  method = "ranger",
  trControl = fitControl,
  tuneGrid = expand.grid(
    mtry = c(1, 2, 3,4,5,6),
    splitrule = "variance",
    min.node.size = c(5, 10, 15,20)),
  importance = "impurity"  
)
# Realiza predicciones en el conjunto de prueba
test_data4c$predictions <-predict(model4c, newdata = test_data4c)
mae_value2 <- mean(abs(exp(test_data4c$predictions) - exp(test_data4c$lPrecio)), na.rm = TRUE)
test_datat<-rbind(test_data4a, test_data4c)
mae_valuetotal1<- mean(abs(exp(test_datat$predictions) - exp(test_datat$lPrecio)), na.rm = TRUE)
cat("MAE en datos de prueba:", mae_valuetotal1, "\n")

importance_scores_model4c <- model4c$finalModel$variable.importance
importance_scores_model4c

# Crear un data frame con las variables y su importancia
importance_data_model4c <- data.frame(
  Variable = names(importance_scores_model4c),
  Importance = importance_scores_model4c
)

# Crear el gráfico de barras
plot_model4c <- ggplot(data = importance_data_model4c, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "lightcoral") +
  labs(title = "Casas", x = "Variable", y = "Importancia") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Combinar los gráficos
combined_plot <- grid.arrange(plot_model4a, plot_model4c, ncol = 2)

# Mostrar el gráfico combinado
print(combined_plot)

#--------------------------------------Muestra total dividida----------------------------------------------#

training_indices1a <- initial_split(train_apart1, prop = 0.7)
train_data1a <- training_indices1a %>% training()
test_data1a <- training_indices1a %>% testing()
training_indices1c <- initial_split(train_casas1, prop = 0.7)
train_data1c <- training_indices1c %>% training()
test_data1c <- training_indices1c %>% testing()
combined_test1 <- rbind(test_data1c, test_data1a)

# Ajusta el modelo en el conjunto de entrenamiento
arbola <- train(
  lPrecio ~ Area+ Habitaciones + Habitaciones2 + lat+lon +Baños + Año +   M2_por_Habitacion + Terraza + Garaje + Sala_BBQ + Gimnasio + Sala_BBQ_terraza + Chimenea + Seguridad + Dist_Parques + 
    Dist_Transp_Publico + Dist_Establecimientos + Dist_C_Comerc + Dist_Centros_Educ + Dist_Restaurantes + Dist_Bancos, 
  data = train_data1a,
  method = "ranger",
  trControl = fitControl,
  tuneGrid = expand.grid(
    mtry = c(1, 2, 3,4,5,6),
    splitrule = "variance",
    min.node.size = c(5, 10, 15,20)),
  importance = "impurity"  
)

arbolc <- train(
  lPrecio ~ Area+Habitaciones + Habitaciones2 + Baños + lat + lon + Año +  M2_por_Habitacion + Terraza + Garaje + Sala_BBQ + Gimnasio + Sala_BBQ_terraza + Chimenea + Seguridad + Dist_Parques + 
    Dist_Transp_Publico + Dist_Establecimientos + Dist_C_Comerc + Dist_Centros_Educ + Dist_Restaurantes + Dist_Bancos, 
  data = train_data1c,
  method = "ranger",
  trControl = fitControl,
  tuneGrid = expand.grid(
    mtry = c(1, 2, 3,4,5,6),
    splitrule = "variance",
    min.node.size = c(5, 10, 15,20)),
  importance = "impurity"  
)

# Realiza predicciones en el conjunto de prueba
test_data1c$predictions1 <-predict(arbolc, newdata = test_data1c)
mae_valuec <- mean(abs(exp(test_data1c$predictions1) - exp(test_data1c$lPrecio)), na.rm = TRUE)
test_data1a$predictions1 <-predict(arbola, newdata = test_data1a)
mae_valuep <- mean(abs(exp(test_data1a$predictions1) - exp(test_data1a$lPrecio)), na.rm = TRUE)
test_dataarbol<-rbind(test_data1a, test_data1a)
mae_valuetotal2<- mean(abs(exp(test_dataarbol$predictions1) - exp(test_dataarbol$lPrecio)), na.rm = TRUE)
cat("MAE en datos de prueba:", mae_valuetotal2, "\n")

#Calcular la importancia de las variables:
importance_scores_arbola <- arbola$finalModel$variable.importance
importance_scores_arbolc <- arbolc$finalModel$variable.importance
importance_scores_arbola
importance_scores_arbolc
# Crear un data frame para cada modelo con las variables y su importancia:
importance_data_arbola <- data.frame(
  Variable = names(importance_scores_arbola),
  Importance = importance_scores_arbola
)

importance_data_arbolc <- data.frame(
  Variable = names(importance_scores_arbolc),
  Importance = importance_scores_arbolc
)
# Crear gráficos de barras para visualizar la importancia de las variables:

library(ggplot2)

# Gráfico para arbola
plot_arbola <- ggplot(data = importance_data_arbola, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Apartamentos", x = "Variable", y = "Importancia") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Gráfico para arbolc
plot_arbolc <- ggplot(data = importance_data_arbolc, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "lightcoral") +
  labs(title = "Casas", x = "Variable", y = "Importancia") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Muestra los gráficos
# Combinar los gráficos
combined_plot <- grid.arrange(plot_arbola, plot_arbolc, ncol = 2)

# Mostrar el gráfico combinado
print(combined_plot)
#------------------------------------------------Muestra total sin división--------------------------------------------
set.seed(123)

training_indicest <- initial_split(combined_train, prop = 0.7)
train_datat <- training_indicest %>% training()
test_datat <- training_indicest %>% testing()


fitControl<-trainControl(method ="cv",
                         number=5)

# Ajusta el modelo en el conjunto de entrenamiento
modelt <- train(
  lPrecio ~ Area+  Habitaciones + Habitaciones2 + lat+lon +Baños + M2_por_Habitación + Terraza + Garaje + Sala_BBQ + Gimnasio + Sala_BBQ_terraza + Chimenea + Seguridad + Dist_Parques + 
    Dist_Transp_Publico + Dist_Establecimientos + Dist_C_Comerc + Dist_Centros_Educ + Dist_Restaurantes + Dist_Bancos, 
  data = train_datat,
  method = "ranger",
  trControl = fitControl,
  tuneGrid = expand.grid(
    mtry = c(1, 2, 3,4,5,6),
    splitrule = "variance",
    min.node.size = c(5, 10, 15,20)
  ),
  importance = "impurity"  
)

# Realiza predicciones en el conjunto de prueba
test_datat$predictionst <-predict(modelt, newdata = test_datat)

# Calcular el MAE de manera manual
mae_valuet3 <- mean(abs(exp(test_datat$predictionst) - exp(test_datat$lPrecio)), na.rm = TRUE)

# Imprimir el valor del MAE
cat("MAE en datos de prueba:", mae_valuet3, "\n")
#MAE en datos de prueba: 100235271


#------------------------------------------------USAQUEN --------------------------------------------


# Ajusta el modelo en el conjunto de entrenamiento
modelusaquen <- train(
  lPrecio ~  Area + Habitaciones + Habitaciones2 + lat+lon +Año+Baños + M2_por_Habitación + Terraza + Garaje + Sala_BBQ + Gimnasio + Sala_BBQ_terraza + Chimenea + Seguridad + Dist_Parques + 
    Dist_Transp_Publico + Dist_Establecimientos + Dist_C_Comerc + Dist_Centros_Educ + Dist_Restaurantes + Dist_Bancos, 
  data = train_data1,
  method = "ranger",
  trControl = fitControl,
  tuneGrid = expand.grid(
    mtry = c(1, 2, 3,4,5,6),
    splitrule = "variance",
    min.node.size = c(5, 10, 15,20)),
  importance = "impurity"  
)
combined_test4$M2_por_Habitación<- combined_test4$M2_por_Habitacion 
# Realiza predicciones en el conjunto de prueba
combined_test4$modelusaquen <-predict(modelusaquen, newdata = combined_test4)

# Calcular el MAE de manera manual
mae_valuet4 <- mean(abs(exp(combined_test4$modelusaquen) - exp(combined_test4$lPrecio)), na.rm = TRUE)

# Imprimir el valor del MAE
cat("MAE en datos de prueba:", mae_valuet4, "\n")

#install.packages("knitr")
resultados_mae <- data.frame(
  Conjunto = c("Conjunto 1", "Conjunto 2", "Conjunto 3", "Conjunto 4"),
  MAE = c(mae_valuetotal1, mae_valuetotal2, mae_valuet3, mae_valuet4)
)

library(knitr)

# Imprime la tabla
kable(resultados_mae, format = "markdown")
#### Calculo de la Importancia 
# Calcular la importancia de las variables para el modelo "modelt"
# Carga el paquete knitr

importance_scores_modelt <- modelt$finalModel$variable.importance

# Calcular la importancia de las variables para el modelo "modelusaquen"
importance_scores_modelusaquen <- modelusaquen$finalModel$variable.importance

# Crear un data frame para cada modelo
importance_scores_modelt <- data.frame(
  Variable = names(importance_scores_modelt),
  Importance = importance_scores_modelt
)

importance_scores_modelusaquen <- data.frame(
  Variable = names(importance_scores_modelusaquen),
  Importance = importance_scores_modelusaquen
)


# Gráfico de importancia para "modelt"
plot_modelt <- ggplot(data = importance_scores_modelt, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Muestra Total", x = "Variable", y = "Importancia") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Gráfico de importancia para "modelusaquen"
plot_modelusaquen <- ggplot(data = importance_scores_modelusaquen, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "lightcoral") +
  labs(title = "Usaquen", x = "Variable", y = "Importancia") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Combinar los gráficos de importancia de variables
combined_plot <- grid.arrange(plot_modelt, plot_modelusaquen, ncol = 2)

# Mostrar el gráfico combinado
print(combined_plot)



#-------------------------------------ALGORITMO XGBOOSTING_______________________________________________________________
#install.packages("xgboost")
library(xgboost)


# Lista de nombres de variables que deseas incluir en el modelo
variables_incluidas <- c("Habitaciones","Habitaciones2", "Baños", "Area","M2_por_Habitacion", "lat", "lon", "Terraza",
                         "Garaje", "Sala_BBQ","Piscina","Gimnasio", "Chimenea","Seguridad",
                         "Dist_Parques", "Dist_Transp_Publico", "Dist_Establecimientos",
                         "Dist_C_Comerc", "Dist_Centros_Educ", "Dist_Restaurantes", "Dist_Bancos", "Sala_BBQ_terraza","Año" )

#####################################################Estrato 4#######################################################################
# Conjunto de datos de entrenamiento con variables seleccionadas
train_data4a_subset <- train_data4a[, c("lPrecio", variables_incluidas)]
# Conjunto de datos de prueba con variables seleccionadas
test_data4a_subset <- test_data4a[, c("lPrecio", variables_incluidas)]
#--------------------------------------------------Apartamentos---------------------------------------------------------------------#
set.seed(123)
# Ajusta el modelo en el conjunto de entrenamiento con variables seleccionadas
model_xgboost_subseta <- xgboost(
  data = as.matrix(train_data4a_subset[, -1]),  # Excluye la columna de lPrecio
  label = train_data4a_subset$lPrecio,
  booster = "gbtree",
  nrounds = 200,
  max_depth = 14,
  eta = 0.1,
  objective = "reg:linear",
  eval_metric = "mae"
)

# Realiza predicciones en el conjunto de prueba con variables seleccionadas
predictions_xgboost_subseta <- predict(model_xgboost_subseta, as.matrix(test_data4a_subset[, -1]))

# Calcular el MAE de manera manual con variables seleccionadas
mae_value_xgboost_subseta <- mean(abs(exp(predictions_xgboost_subseta) - exp(test_data4a_subset$lPrecio)), na.rm = TRUE)


mae_value_xgboost_subseta

############################# CASAS 
# Conjunto de datos de entrenamiento con variables seleccionadas
train_data4c_subset <- train_data4c[, c("lPrecio", variables_incluidas)]
# Conjunto de datos de prueba con variables seleccionadas
test_data4c_subset <- test_data4c[, c("lPrecio", variables_incluidas)]


# Ajusta el modelo en el conjunto de entrenamiento con variables seleccionadas
model_xgboost_subsetc <- xgboost(
  data = as.matrix(train_data4c_subset[, -1]),  # Excluye la columna de lPrecio
  label = train_data4c_subset$lPrecio,
  booster = "gbtree",
  nrounds = 200,
  max_depth = 14,
  eta = 0.1,
  objective = "reg:linear",
  eval_metric = "mae"
)

# Realiza predicciones en el conjunto de prueba con variables seleccionadas
predictions_xgboost_subsetc <- predict(model_xgboost_subsetc, as.matrix(test_data4c_subset[, -1]))

# Calcular el MAE de manera manual con variables seleccionadas
mae_value_xgboost_subsetc <- mean(abs(exp(predictions_xgboost_subsetc) - exp(test_data4c_subset$lPrecio)), na.rm = TRUE)

mae_value_xgboost_subsetc


# Realiza predicciones en el conjunto de prueba para casas
test_data4a$predictions4 <- predict(model_xgboost_subseta, newdata = as.matrix(test_data4a_subset[, -1]))

# Calcular el MAE de manera manual para casas
mae_value4a <- mean(abs(exp(test_data4a$predictions4) - exp(test_data4a$lPrecio)), na.rm = TRUE)

# Realiza predicciones en el conjunto de prueba para apartamentos
test_data4c$predictions4 <- predict(model_xgboost_subsetc, newdata = as.matrix(test_data4c_subset[, -1]))

# Calcular el MAE de manera manual para apartamentos
mae_value4c <- mean(abs(exp(test_data4c$predictions4) - exp(test_data4c$lPrecio)), na.rm = TRUE)

# Combinar los resultados de casas y apartamentos
test_data_combined <- rbind(test_data4c, test_data4a)
mae_value_total <- mean(abs(exp(test_data_combined$predictions4) - exp(test_data_combined$lPrecio)), na.rm = TRUE)

cat("MAE en datos de prueba para casas:", mae_value4c, "\n")
cat("MAE en datos de prueba para apartamentos:", mae_value4a, "\n")
cat("MAE total en datos de prueba:", mae_value_total, "\n")

#----------------------------------------------Muestra total dividida-----------------------------------------------------------------#

# Conjunto de datos de entrenamiento con variables seleccionadas
train_data1a_subset <- train_data1a[, c("lPrecio", variables_incluidas)]

# Conjunto de datos de prueba con variables seleccionadas
test_data1a_subset <- test_data1a [, c("lPrecio", variables_incluidas)]

set.seed(123)
# Ajusta el modelo en el conjunto de entrenamiento con variables seleccionadas
model_xgboost_subsetta <- xgboost(
  data = as.matrix(train_data1a_subset[, -1]),  # Excluye la columna de lPrecio
  label = train_data1a_subset$lPrecio,
  booster = "gbtree",
  nrounds = 200,
  max_depth = 14,
  eta = 0.1,
  objective = "reg:linear",
  eval_metric = "mae"
)

# Realiza predicciones en el conjunto de prueba con variables seleccionadas
predictions_xgboost_subsetta <- predict(model_xgboost_subsetta, as.matrix(test_data1a_subset [, -1]))

# Calcular el MAE de manera manual con variables seleccionadas
mae_value_xgboost_subsetta <- mean(abs(exp(predictions_xgboost_subseta) - exp(test_data1a_subset $lPrecio)), na.rm = TRUE)


mae_value_xgboost_subsetta

#-----------------------------------------------------CASAS-----------------------------------------------------------

train_data1c_subset <- train_data1c[, c("lPrecio", variables_incluidas)]
test_casas1_subset <- test_data1c[, c("lPrecio", variables_incluidas)]

# Ajusta el modelo en el conjunto de entrenamiento con variables seleccionadas
model_xgboost_subsettc <- xgboost(
  data = as.matrix(train_data1c_subset[, -1]),  # Excluye la columna de lPrecio
  label = train_data1c_subset$lPrecio,
  booster = "gbtree",
  nrounds = 200,
  max_depth = 14,
  eta = 0.1,
  objective = "reg:linear",
  eval_metric = "mae"
)

# Realiza predicciones en el conjunto de prueba con variables seleccionadas
predictions_xgboost_subsettc <- predict(model_xgboost_subsettc, as.matrix(test_casas1_subset[, -1]))

# Calcular el MAE de manera manual con variables seleccionadas
mae_value_xgboost_subsettc <- mean(abs(exp(predictions_xgboost_subsettc) - exp(test_casas1_subset$lPrecio)), na.rm = TRUE)

mae_value_xgboost_subsettc


# Realiza predicciones en el conjunto de prueba para casas
test_data1a$predictionst4 <- predict(model_xgboost_subsetta, newdata = as.matrix(test_data1a_subset[, -1]))

# Calcular el MAE de manera manual para casas
mae_valuet1a <- mean(abs(exp(test_data1a$predictionst4) - exp(test_data1a$lPrecio)), na.rm = TRUE)

# Realiza predicciones en el conjunto de prueba para apartamentos
test_data1c$predictionst4 <- predict(model_xgboost_subsettc, newdata = as.matrix(test_casas1_subset[, -1]))

# Calcular el MAE de manera manual para apartamentos
mae_valuet1c <- mean(abs(exp(test_data1c$predictionst4) - exp(test_data1c$lPrecio)), na.rm = TRUE)

# Combinar los resultados de casas y apartamentos
test_data_combinedt <- rbind(test_data1c, test_data1a)
mae_value_total1t <- mean(abs(exp(test_data_combinedt$predictionst4) - exp(test_data_combinedt$lPrecio)), na.rm = TRUE)

cat("MAE en datos de prueba para casas:", mae_valuet1c, "\n")
cat("MAE en datos de prueba para apartamentos:", mae_valuet1a, "\n")
cat("MAE total en datos de prueba:", mae_value_total1t, "\n")

#---------------------------------------Muestra total sin división--------------------------------------------------------------------


training_indicest <- initial_split(combined_train, prop = 0.7)
train_datat <- training_indicest %>% training()
test_datat <- training_indicest %>% testing()

train_datat_subset <- train_datat[, c("lPrecio", variables_incluidas)]
test_casast_subset <- test_datat[, c("lPrecio", variables_incluidas)]


# Ajusta el modelo en el conjunto de entrenamiento con variables seleccionadas
model_xgboostsindiv <- xgboost(
  data = as.matrix(train_datat_subset[, -1]),  # Excluye la columna de lPrecio
  label = train_datat_subset$lPrecio,
  booster = "gbtree",
  nrounds = 200,
  max_depth = 14,
  eta = 0.1,
  objective = "reg:linear",
  eval_metric = "mae"
)

# Realiza predicciones en el conjunto de prueba con variables seleccionadas
predictions_xgboost_sindiv <- predict(model_xgboostsindiv, as.matrix(test_casast_subset[, -1]))

# Calcular el MAE de manera manual con variables seleccionadas
mae_value_xgboost_sindiv  <- mean(abs(exp(predictions_xgboost_sindiv ) - exp(test_casast_subset$lPrecio)), na.rm = TRUE)

mae_value_xgboost_sindiv 


#--------------------------------------------------------------Muestra usaquen -----------------------------------------------------#


train_datat1_subset <- train_data1[, c("lPrecio", variables_incluidas)]
test_casast1_subset <- combined_test4[, c("lPrecio", variables_incluidas)]

# Ajusta el modelo en el conjunto de entrenamiento con variables seleccionadas
model_xgboostsindiv1 <- xgboost(
  data = as.matrix(train_datat1_subset[, -1]),  # Excluye la columna de lPrecio
  label = train_datat1_subset$lPrecio,
  booster = "gbtree",
  nrounds = 100,
  max_depth = 14,
  eta = 0.1,
  objective = "reg:linear",
  eval_metric = "mae"
)

# Realiza predicciones en el conjunto de prueba con variables seleccionadas
predictions_xgboost_sindiv1 <- predict(model_xgboostsindiv1, as.matrix(test_casast1_subset[, -1]))

# Calcular el MAE de manera manual con variables seleccionadas
mae_value_xgboost_sindiv1  <- mean(abs(exp(predictions_xgboost_sindiv1 ) - exp(test_casast1_subset$lPrecio)), na.rm = TRUE)

mae_value_xgboost_sindiv1
library(knitr)

# Crear un dataframe con los resultados de MAE
resultados_mae <- data.frame(
  Modelo = c("Modelo XGBoost Estrato 4", "Modelo XGBoost Muestra Total Dividida", "Modelo XGBoost Muestra Total Sin División", "Modelo XGBoost Usaquén"),
  MAE = c(mae_value_total, mae_value_total1t,mae_value_xgboost_sindiv, mae_value_xgboost_sindiv1)
)

# Imprimir la tabla
kable(resultados_mae, format = "markdown")

####################################ALGORITMO XGBOOSTING#################################################################
#install.packages("xgboost")
library(xgboost)


# Lista de nombres de variables que deseas incluir en el modelo
variables_incluidas <- c("Habitaciones","Habitaciones2", "Baños", "Area","M2_por_Habitacion", "lat", "lon", "Terraza",
                         "Garaje", "Sala_BBQ","Piscina","Gimnasio", "Chimenea","Seguridad",
                         "Dist_Parques", "Dist_Transp_Publico", "Dist_Establecimientos",
                         "Dist_C_Comerc", "Dist_Centros_Educ", "Dist_Restaurantes", "Dist_Bancos", "Sala_BBQ_terraza","Año" )

#####################################################Estrato 4######################################################
# Conjunto de datos de entrenamiento con variables seleccionadas
train_data4a_subset <- train_data4a[, c("lPrecio", variables_incluidas)]
# Conjunto de datos de prueba con variables seleccionadas
test_data4a_subset <- test_data4a[, c("lPrecio", variables_incluidas)]

set.seed(123)
# Ajusta el modelo en el conjunto de entrenamiento con variables seleccionadas
model_xgboost_subseta <- xgboost(
  data = as.matrix(train_data4a_subset[, -1]),  # Excluye la columna de lPrecio
  label = train_data4a_subset$lPrecio,
  booster = "gbtree",
  nrounds = 200,
  max_depth = 14,
  eta = 0.1,
  objective = "reg:linear",
  eval_metric = "mae"
)

# Realiza predicciones en el conjunto de prueba con variables seleccionadas
predictions_xgboost_subseta <- predict(model_xgboost_subseta, as.matrix(test_data4a_subset[, -1]))

# Calcular el MAE de manera manual con variables seleccionadas
mae_value_xgboost_subseta <- mean(abs(exp(predictions_xgboost_subseta) - exp(test_data4a_subset$lPrecio)), na.rm = TRUE)


mae_value_xgboost_subseta

#-------------------------------------------------------------- CASAS--------------------------------------------------------------# 
# Conjunto de datos de entrenamiento con variables seleccionadas
train_data4c_subset <- train_data4c[, c("lPrecio", variables_incluidas)]
# Conjunto de datos de prueba con variables seleccionadas
test_data4c_subset <- test_data4c[, c("lPrecio", variables_incluidas)]


# Ajusta el modelo en el conjunto de entrenamiento con variables seleccionadas
model_xgboost_subsetc <- xgboost(
  data = as.matrix(train_data4c_subset[, -1]),  # Excluye la columna de lPrecio
  label = train_data4c_subset$lPrecio,
  booster = "gbtree",
  nrounds = 200,
  max_depth = 14,
  eta = 0.1,
  objective = "reg:linear",
  eval_metric = "mae"
)

# Realiza predicciones en el conjunto de prueba con variables seleccionadas
predictions_xgboost_subsetc <- predict(model_xgboost_subsetc, as.matrix(test_data4c_subset[, -1]))

# Calcular el MAE de manera manual con variables seleccionadas
mae_value_xgboost_subsetc <- mean(abs(exp(predictions_xgboost_subsetc) - exp(test_data4c_subset$lPrecio)), na.rm = TRUE)

mae_value_xgboost_subsetc


# Realiza predicciones en el conjunto de prueba para casas
test_data4a$predictions4 <- predict(model_xgboost_subseta, newdata = as.matrix(test_data4a_subset[, -1]))

# Calcular el MAE de manera manual para casas
mae_value4a <- mean(abs(exp(test_data4a$predictions4) - exp(test_data4a$lPrecio)), na.rm = TRUE)

# Realiza predicciones en el conjunto de prueba para apartamentos
test_data4c$predictions4 <- predict(model_xgboost_subsetc, newdata = as.matrix(test_data4c_subset[, -1]))

# Calcular el MAE de manera manual para apartamentos
mae_value4c <- mean(abs(exp(test_data4c$predictions4) - exp(test_data4c$lPrecio)), na.rm = TRUE)

# Combinar los resultados de casas y apartamentos
test_data_combined <- rbind(test_data4c, test_data4a)
mae_value_total <- mean(abs(exp(test_data_combined$predictions4) - exp(test_data_combined$lPrecio)), na.rm = TRUE)

cat("MAE en datos de prueba para casas:", mae_value4c, "\n")
cat("MAE en datos de prueba para apartamentos:", mae_value4a, "\n")
cat("MAE total en datos de prueba:", mae_value_total, "\n")

#-----------------------------------------------Muestra total dividida------------------------------------------------------------#

# Conjunto de datos de entrenamiento con variables seleccionadas
train_data1a_subset <- train_data1a[, c("lPrecio", variables_incluidas)]

# Conjunto de datos de prueba con variables seleccionadas
test_data1a_subset <- test_data1a [, c("lPrecio", variables_incluidas)]

set.seed(123)
# Ajusta el modelo en el conjunto de entrenamiento con variables seleccionadas
model_xgboost_subsetta <- xgboost(
  data = as.matrix(train_data1a_subset[, -1]),  # Excluye la columna de lPrecio
  label = train_data1a_subset$lPrecio,
  booster = "gbtree",
  nrounds = 200,
  max_depth = 14,
  eta = 0.1,
  objective = "reg:linear",
  eval_metric = "mae"
)

# Realiza predicciones en el conjunto de prueba con variables seleccionadas
predictions_xgboost_subsetta <- predict(model_xgboost_subsetta, as.matrix(test_data1a_subset [, -1]))

# Calcular el MAE de manera manual con variables seleccionadas
mae_value_xgboost_subsetta <- mean(abs(exp(predictions_xgboost_subseta) - exp(test_data1a_subset $lPrecio)), na.rm = TRUE)


mae_value_xgboost_subsetta

#-----------------------------------------------------CASAS-----------------------------------------------------------

train_data1c_subset <- train_data1c[, c("lPrecio", variables_incluidas)]
test_casas1_subset <- test_data1c[, c("lPrecio", variables_incluidas)]

# Ajusta el modelo en el conjunto de entrenamiento con variables seleccionadas
model_xgboost_subsettc <- xgboost(
  data = as.matrix(train_data1c_subset[, -1]),  # Excluye la columna de lPrecio
  label = train_data1c_subset$lPrecio,
  booster = "gbtree",
  nrounds = 200,
  max_depth = 14,
  eta = 0.1,
  objective = "reg:linear",
  eval_metric = "mae"
)

# Realiza predicciones en el conjunto de prueba con variables seleccionadas
predictions_xgboost_subsettc <- predict(model_xgboost_subsettc, as.matrix(test_casas1_subset[, -1]))

# Calcular el MAE de manera manual con variables seleccionadas
mae_value_xgboost_subsettc <- mean(abs(exp(predictions_xgboost_subsettc) - exp(test_casas1_subset$lPrecio)), na.rm = TRUE)

mae_value_xgboost_subsettc


# Realiza predicciones en el conjunto de prueba para casas
test_data1a$predictionst4 <- predict(model_xgboost_subsetta, newdata = as.matrix(test_data1a_subset[, -1]))

# Calcular el MAE de manera manual para casas
mae_valuet1a <- mean(abs(exp(test_data1a$predictionst4) - exp(test_data1a$lPrecio)), na.rm = TRUE)

# Realiza predicciones en el conjunto de prueba para apartamentos
test_data1c$predictionst4 <- predict(model_xgboost_subsettc, newdata = as.matrix(test_casas1_subset[, -1]))

# Calcular el MAE de manera manual para apartamentos
mae_valuet1c <- mean(abs(exp(test_data1c$predictionst4) - exp(test_data1c$lPrecio)), na.rm = TRUE)

# Combinar los resultados de casas y apartamentos
test_data_combinedt <- rbind(test_data1c, test_data1a)
mae_value_total1t <- mean(abs(exp(test_data_combinedt$predictionst4) - exp(test_data_combinedt$lPrecio)), na.rm = TRUE)

cat("MAE en datos de prueba para casas:", mae_valuet1c, "\n")
cat("MAE en datos de prueba para apartamentos:", mae_valuet1a, "\n")
cat("MAE total en datos de prueba:", mae_value_total1t, "\n")

#----------------------------------------Muestra total sin división----------------------------------------------------------------#


training_indicest <- initial_split(combined_train, prop = 0.7)
train_datat <- training_indicest %>% training()
test_datat <- training_indicest %>% testing()

train_datat_subset <- train_datat[, c("lPrecio", variables_incluidas)]
test_casast_subset <- test_datat[, c("lPrecio", variables_incluidas)]


# Ajusta el modelo en el conjunto de entrenamiento con variables seleccionadas
model_xgboostsindiv <- xgboost(
  data = as.matrix(train_datat_subset[, -1]),  # Excluye la columna de lPrecio
  label = train_datat_subset$lPrecio,
  booster = "gbtree",
  nrounds = 200,
  max_depth = 14,
  eta = 0.1,
  objective = "reg:linear",
  eval_metric = "mae"
)


#---------------------------------------------------------Muestra usaquen -----------------------------------------------------------#


train_datat1_subset <- train_data1[, c("lPrecio", variables_incluidas)]
test_casast1_subset <- test_data1[, c("lPrecio", variables_incluidas)]

# Ajusta el modelo en el conjunto de entrenamiento con variables seleccionadas
model_xgboostsindiv1 <- xgboost(
  data = as.matrix(train_datat1_subset[, -1]),  # Excluye la columna de lPrecio
  label = train_datat1_subset$lPrecio,
  booster = "gbtree",
  nrounds = 200,
  max_depth = 14,
  eta = 0.1,
  objective = "reg:linear",
  eval_metric = "mae"
)

# Realiza predicciones en el conjunto de prueba con variables seleccionadas
predictions_xgboost_sindiv1 <- predict(model_xgboostsindiv1, as.matrix(test_casast1_subset[, -1]))

# Calcular el MAE de manera manual con variables seleccionadas
mae_value_xgboost_sindiv1  <- mean(abs(exp(predictions_xgboost_sindiv1 ) - exp(test_casast1_subset$lPrecio)), na.rm = TRUE)

mae_value_xgboost_sindiv1
library(knitr)

# Crear un dataframe con los resultados de MAE
resultados_mae <- data.frame(
  Modelo = c("Modelo XGBoost Estrato 4", "Modelo XGBoost Muestra Total Dividida", "Modelo XGBoost Muestra Total Sin División", "Modelo XGBoost Usaquén"),
  MAE = c(mae_value_total, mae_value_total1t,mae_value_xgboost_sindiv, mae_value_xgboost_sindiv1)
)

# Imprimir la tabla
kable(resultados_mae, format = "markdown")


#----------------------------------------------------------GRADIENT DESCENT---------------------------------------------------------#
#--------------------------------------------------------- ESTRATO 4---------------------------------------------------------------
# Conjunto de datos de entrenamiento con variables seleccionadas
train_data4a_subset <- train_data4a[, c("lPrecio", variables_incluidas)]
# Conjunto de datos de prueba con variables seleccionadas
test_data4a_subset <- test_data4a[, c("lPrecio", variables_incluidas)]
# Instala y carga la biblioteca randomForest si no está instalada
if (!require(randomForest)) {
  install.packages("randomForest")
  library(randomForest)
}

# Ajusta el modelo Random Forest en el conjunto de entrenamiento
model_rfa <- randomForest(
  lPrecio ~ .,  # Variables predictoras
  data = train_data4a_subset,  # Conjunto de entrenamiento
  ntree = 500,  # Número de árboles en el bosque
  mtry = sqrt(ncol(train_data4a_subset) - 1),  # Número de variables a considerar en cada división
  importance = TRUE  # Calcular la importancia de las variables
)

# Realiza predicciones en el conjunto de prueba
predictions_rfa <- predict(model_rfa, newdata = test_data4a_subset)

# Calcular el MAE en el conjunto de prueba
mae_value_rfa <- mean(abs(exp(predictions_rfa) - exp(test_data4a_subset$lPrecio)))

cat("MAE con Random Forest para Subset A:", mae_value_rfa, "\n")

# Puedes repetir el proceso para otras localidades si es necesario
# Ajusta el modelo Random Forest en el conjunto de entrenamiento
model_rfc <- randomForest(
  lPrecio ~ .,  # Variables predictoras
  data = train_data4c_subset,  # Conjunto de entrenamiento
  ntree = 500,  # Número de árboles en el bosque
  mtry = sqrt(ncol(train_data4c_subset) - 1),  # Número de variables a considerar en cada división
  importance = TRUE  # Calcular la importancia de las variables
)

# Realiza predicciones en el conjunto de prueba
predictions_rfc <- predict(model_rfc, newdata = test_data4c_subset)

# Calcular el MAE en el conjunto de prueba
mae_value_rfc <- mean(abs(exp(predictions_rfc) - exp(test_data4c_subset$lPrecio)))

cat("MAE con Random Forest para Subset A:", mae_value_rfc, "\n")

# Realiza predicciones en el conjunto de prueba para casas
test_data4a$predictions5 <- predict(model_rfa , newdata = as.matrix(test_data4a_subset[, -1]))

# Calcular el MAE de manera manual para casas
mae_value5a<- mean(abs(exp(test_data4a$predictions5) - exp(test_data4a$lPrecio)), na.rm = TRUE)

# Realiza predicciones en el conjunto de prueba para apartamentos
test_data4c$predictions5 <- predict(model_rfc , newdata = as.matrix(test_data4c_subset[, -1]))

# Calcular el MAE de manera manual para apartamentos
mae_value5c <- mean(abs(exp(test_data4c$predictions5) - exp(test_data4c$lPrecio)), na.rm = TRUE)

# Combinar los resultados de casas y apartamentos
test_data_combined <- rbind(test_data4c, test_data4a)
mae_value_total5 <- mean(abs(exp(test_data_combined$predictions5) - exp(test_data_combined$lPrecio)), na.rm = TRUE)

cat("MAE en datos de prueba para casas:", mae_value5c, "\n")
cat("MAE en datos de prueba para apartamentos:", mae_value5a, "\n")
cat("MAE total en datos de prueba:", mae_value_total5, "\n")
# Calcular la importancia de las variables


#------------------------------------------------------Muestra total dividida------------------------------------------------------------------

# Conjunto de datos de entrenamiento con variables seleccionadas
train_data1a_subset <- train_data1a[, c("lPrecio", variables_incluidas)]

# Conjunto de datos de prueba con variables seleccionadas
test_data1a_subset <- test_data1a [, c("lPrecio", variables_incluidas)]


# Ajusta el modelo Random Forest en el conjunto de entrenamiento
model_rfatd <- randomForest(
  lPrecio ~ .,  # Variables predictoras
  data = train_data1a_subset,  # Conjunto de entrenamiento
  ntree = 500,  # Número de árboles en el bosque
  mtry = sqrt(ncol(train_data1a_subset) - 1),  # Número de variables a considerar en cada división
  importance = TRUE  # Calcular la importancia de las variables
)

# Realiza predicciones en el conjunto de prueba
predictions_rfatd <- predict(model_rfatd, newdata = test_data1a_subset)

# Calcular el MAE en el conjunto de prueba
mae_value_rfatd <- mean(abs(exp(predictions_rfatd) - exp(test_data1a_subset$lPrecio)))

cat("MAE con Random Forest para Subset A:", mae_value_rfatd, "\n")
train_data1c_subset <- train_data1c[, c("lPrecio", variables_incluidas)]
test_casas1_subset <- test_data1c[, c("lPrecio", variables_incluidas)]
# Puedes repetir el proceso para otras localidades si es necesario
# Ajusta el modelo Random Forest en el conjunto de entrenamiento
model_rfctd <- randomForest(
  lPrecio ~ .,  # Variables predictoras
  data = train_data1c_subset,  # Conjunto de entrenamiento
  ntree = 500,  # Número de árboles en el bosque
  mtry = sqrt(ncol(train_data1c_subset) - 1),  # Número de variables a considerar en cada división
  importance = TRUE  # Calcular la importancia de las variables
)

# Realiza predicciones en el conjunto de prueba
predictions_rfctd <- predict(model_rfc, newdata = test_casas1_subset)

# Calcular el MAE en el conjunto de prueba
mae_value_rfctd <- mean(abs(exp(predictions_rfctd) - exp(test_casas1_subset$lPrecio)))

cat("MAE con Random Forest para Subset A:", mae_value_rfc, "\n")

# Realiza predicciones en el conjunto de prueba para casas
test_data1a$predictions5td <- predict(model_rfa , newdata = as.matrix(test_data1a_subset[, -1]))

# Calcular el MAE de manera manual para casas
mae_value5atd<- mean(abs(exp(test_data1a$predictions5td) - exp(test_data1a$lPrecio)), na.rm = TRUE)

# Realiza predicciones en el conjunto de prueba para apartamentos
test_data1c$predictions5td <- predict(model_rfctd , newdata = as.matrix(test_casas1_subset[, -1]))

# Calcular el MAE de manera manual para apartamentos
mae_value5ctd <- mean(abs(exp(test_data1c$predictions5td) - exp(test_data1c$lPrecio)), na.rm = TRUE)

# Combinar los resultados de casas y apartamentos
test_data_combinedtd <- rbind(test_data1c, test_data1a)
mae_value_total5td <- mean(abs(exp(test_data_combinedtd$predictions5td) - exp(test_data_combinedtd$lPrecio)), na.rm = TRUE)

cat("MAE en datos de prueba para casas:", mae_value5c, "\n")
cat("MAE en datos de prueba para apartamentos:", mae_value5a, "\n")
cat("MAE total en datos de prueba:", mae_value_total5, "\n")

##Importancia

# Crear un gráfico de importancia de variables
varImpPlot(model_rfatd)
varImpPlot(model_rfctd)

#-------------------------------------------------- SIN DIVISION---------------------------------------------------------------------#

training_indicest <- initial_split(combined_train, prop = 0.7)
train_datat <- training_indicest %>% training()
test_datat <- training_indicest %>% testing()

train_datat_subset <- train_datat[, c("lPrecio", variables_incluidas)]
test_casast_subset <- test_datat[, c("lPrecio", variables_incluidas)]


# Ajusta el modelo Random Forest en el conjunto de entrenamiento
model_rfsd <- randomForest(
  lPrecio ~ .,  # Variables predictoras
  data = train_datat_subset,  # Conjunto de entrenamiento
  ntree = 500,  # Número de árboles en el bosque
  mtry = sqrt(ncol(train_datat_subset) - 1),  # Número de variables a considerar en cada división
  importance = TRUE  # Calcular la importancia de las variables
)

# Realiza predicciones en el conjunto de prueba
predictions_rfsd <- predict(model_rfsd, newdata =test_casast_subset)

# Calcular el MAE en el conjunto de prueba
mae_value_rfsd <- mean(abs(exp(predictions_rfsd) - exp(test_casast_subset$lPrecio)))

cat("MAE con Random Forest para Subset A:", mae_value_rfsd, "\n")

# Crear un gráfico de importancia de variables
varImpPlot(model_rfsd )
#----------------------------------------------------------Muestra usaquen ---------------------------------------------------------#


train_datat1_subset <- train_data1[, c("lPrecio", variables_incluidas)]
test_casast1_subset <- test_data1[, c("lPrecio", variables_incluidas)]


# Ajusta el modelo Random Forest en el conjunto de entrenamiento
model_rfusaquen <- randomForest(
  lPrecio ~ .,  # Variables predictoras
  data = train_datat1_subset,  # Conjunto de entrenamiento
  ntree = 500,  # Número de árboles en el bosque
  mtry = sqrt(ncol(train_datat1_subset) - 1),  # Número de variables a considerar en cada división
  importance = TRUE  # Calcular la importancia de las variables
)

# Realiza predicciones en el conjunto de prueba
predictions_rfusaquen <- predict(model_rfusaquen, newdata =test_casast_subset)

# Calcular el MAE en el conjunto de prueba
mae_value_usaquen <- mean(abs(exp(predictions_rfusaquen) - exp(test_casast1_subset$lPrecio)))

cat("MAE con Random Forest para Subset A:", mae_value_usaquen , "\n")
library(knitr)

# Crear un dataframe con los resultados de MAE
resultados_mae <- data.frame(
  Modelo = c("Modelo Estrato 4", "Modelo Muestra Total Dividida", "Modelo  Muestra Total Sin División", "Modelo  Usaquén"),
  MAE = c(mae_value_total5,mae_value_total5td,mae_value_rfsd, mae_value_usaquen)
)

# Imprimir la tabla
kable(resultados_mae, format = "markdown")
varImpPlot(model_rfusaquen)

                  
#---------------------------Modelos de Regresión Lineal Segunda Prueba--------------------------------------------------#
                  
## Calculo el mae utilizando como conjunto de entrenamiento Usaquen y conjunto de prueba el resto del
#----------------------------------------------------------MUESTRA USAQUEN----------------------------------------------------------                

# Crear una lista de recetas
recetas <- list(
  recipe(lPrecio ~ Estrato+lat +lon+Año, data = train_data1),
  recipe(lPrecio ~ Estrato +lat +lon+Año+ Habitaciones + Habitaciones2, data = train_data1),
  recipe(lPrecio ~ Estrato +lat +lon+Año+ Habitaciones + Habitaciones2 + M2_por_Habitación, data = train_data1),
  recipe(lPrecio ~ Estrato +lat +lon+Año+ Habitaciones + Habitaciones2 + M2_por_Habitación + Terraza, data = train_data1),
  recipe(lPrecio ~ Estrato +lat +lon+Año+ Habitaciones + Habitaciones2 + M2_por_Habitación + Terraza + Garaje, data = train_data1),
  recipe(lPrecio ~ Estrato +lat +lon+Año+ Habitaciones + Habitaciones2 + M2_por_Habitación + Terraza + Garaje + Sala_BBQ, data = train_data1),
  recipe(lPrecio ~ Estrato +lat +lon+Año+ Habitaciones + Habitaciones2 + M2_por_Habitación + Terraza + Garaje + Sala_BBQ + Sala_BBQ_terraza, data = train_data1),
  recipe(lPrecio ~ Estrato +lat +lon+Año+ Habitaciones + Habitaciones2 + M2_por_Habitación + Terraza + Garaje + Sala_BBQ + Sala_BBQ_terraza + Gimnasio, data = train_data1),
  recipe(lPrecio ~ Estrato +lat +lon+Año+ Habitaciones + Habitaciones2 + M2_por_Habitación + Terraza + Garaje + Sala_BBQ + Gimnasio, data = train_data1),
  recipe(lPrecio ~ Estrato +lat +lon+Año+ Habitaciones + Habitaciones2 + M2_por_Habitación + Terraza + Garaje + Sala_BBQ_terraza + Gimnasio, data = train_data1),
  recipe(lPrecio ~ Estrato +lat +lon+Año+ Habitaciones + Habitaciones2 + M2_por_Habitación + Terraza + Garaje + Sala_BBQ_terraza + Sala_BBQ + Gimnasio, data = train_data1),
  recipe(lPrecio ~ Estrato +lat +lon+Año+ Habitaciones + Habitaciones2 + M2_por_Habitación + Terraza + Garaje + Sala_BBQ_terraza + Sala_BBQ + Gimnasio + Chimenea, data = train_data1),
  recipe(lPrecio ~ Estrato +lat +lon+Año+ Habitaciones + Habitaciones2 + M2_por_Habitación + Terraza + Garaje + Sala_BBQ_terraza + Sala_BBQ + Gimnasio + Chimenea + Seguridad, data = train_data1),
  recipe(lPrecio ~ Estrato +lat +lon+Año+ Habitaciones + Habitaciones2 + M2_por_Habitación + Terraza + Garaje + Sala_BBQ_terraza + Sala_BBQ + Gimnasio + Chimenea + Seguridad + Dist_Parques, data = train_data1),
  recipe(lPrecio ~ Estrato +lat +lon+Año+ Habitaciones + Habitaciones2 + M2_por_Habitación + Terraza + Garaje + Sala_BBQ_terraza + Sala_BBQ + Gimnasio + Chimenea + Seguridad + Dist_Parques + Dist_Transp_Publico, data = train_data1),
  recipe(lPrecio ~ Estrato +lat +lon+Año+ Habitaciones + Habitaciones2 + M2_por_Habitación + Terraza + Garaje + Sala_BBQ_terraza + Sala_BBQ + Gimnasio + Chimenea + Seguridad + Dist_Parques + Dist_Transp_Publico + Dist_Establecimientos, data = train_data1),
  recipe(lPrecio ~ Estrato +lat +lon+Año+ Habitaciones + Habitaciones2 + M2_por_Habitación + Terraza + Garaje + Sala_BBQ_terraza + Sala_BBQ + Gimnasio + Chimenea + Seguridad + Dist_Parques + Dist_Transp_Publico + Dist_Establecimientos + Dist_C_Comerc, data = train_data1),
  recipe(lPrecio ~ Estrato +lat +lon+Año+ Habitaciones + Habitaciones2 + M2_por_Habitación + Terraza + Garaje + Sala_BBQ_terraza + Sala_BBQ + Gimnasio + Chimenea + Seguridad + Dist_Parques + Dist_Transp_Publico + Dist_Establecimientos + Dist_C_Comerc + Dist_Centros_Educ, data = train_data1),
  recipe(lPrecio ~ Estrato +lat  +lon +Año+ Habitaciones + Habitaciones2 + M2_por_Habitación + Terraza + Garaje + Sala_BBQ_terraza + Sala_BBQ + Gimnasio + Chimenea + Seguridad + Dist_Parques + Dist_Transp_Publico + Dist_Establecimientos + Dist_C_Comerc + Dist_Centros_Educ + Dist_Restaurantes + Dist_Bancos, data = train_data1)
)

# Crear un contenedor para los resultados
results <- list()
# Iterar a través de las recetas y ajustar los modelos
for (i in seq_along(recetas)) {
  receta <- recetas[[i]]
  
  # Ajustar el modelo
  lm_mod <- linear_reg() %>%
    set_engine("lm") %>%
    set_mode("regression")
  
  modelo <- workflow() %>%
    add_recipe(receta) %>%
    add_model(lm_mod) %>%
    fit(data = train_data1)
  
  # Realizar predicciones en el conjunto de prueba
  predicciones <- predict(modelo, new_data = filtered_data) %>%
    bind_cols(filtered_data)
  
  predicciones <- predicciones %>% mutate(.pred = exp(.pred))
  
  # Calcular el MAE
  mae <- yardstick::mae(data = predicciones, truth = Precio, estimate = .pred)
  
  results[[i]] <- mae
}

# Mostrar los resultados
results

# Dividir los datos en entrenamiento y prueba
set.seed(123)
#--------------------------------------------------------MUESTRA TOTAL-----------------------------------------------------------#
# Dividir los datos en entrenamiento y prueba
training_indices <- initial_split(combined_train, prop = 0.7)
training_data <- training_indices %>% training()
testing_data <- training_indices %>% testing()
# Crear una lista de recetas
recetas2 <- list(
  recipe(lPrecio ~ Estrato+lat+lon+año, data = training_data),
  recipe(lPrecio ~ Estrato +lat+lon+año+ Habitaciones + Habitaciones2, data = training_data),
  recipe(lPrecio ~ Estrato +lat+lon+año+ Habitaciones + Habitaciones2 + M2_por_Habitación, data = training_data),
  recipe(lPrecio ~ Estrato +lat+lon+año+ Habitaciones + Habitaciones2 + M2_por_Habitación + Terraza, data = training_data),
  recipe(lPrecio ~ Estrato +lat+lon+año+ Habitaciones + Habitaciones2 + M2_por_Habitación + Terraza + Garaje, data = training_data),
  recipe(lPrecio ~ Estrato +lat+lon+año+ Habitaciones + Habitaciones2 + M2_por_Habitación + Terraza + Garaje + Sala_BBQ, data = training_data),
  recipe(lPrecio ~ Estrato +lat+lon+año+ Habitaciones + Habitaciones2 + M2_por_Habitación + Terraza + Garaje + Sala_BBQ + Sala_BBQ_terraza, data = training_data),
  recipe(lPrecio ~ Estrato +lat+lon+año+ Habitaciones + Habitaciones2 + M2_por_Habitación + Terraza + Garaje + Sala_BBQ + Sala_BBQ_terraza + Gimnasio, data = training_data),
  recipe(lPrecio ~ Estrato +lat+lon+año+ Habitaciones + Habitaciones2 + M2_por_Habitación + Terraza + Garaje + Sala_BBQ + Gimnasio, data = training_data),
  recipe(lPrecio ~ Estrato +lat+lon+año+ Habitaciones + Habitaciones2 + M2_por_Habitación + Terraza + Garaje + Sala_BBQ_terraza + Gimnasio, data = training_data),
  recipe(lPrecio ~ Estrato +lat+lon+año+ Habitaciones + Habitaciones2 + M2_por_Habitación + Terraza + Garaje + Sala_BBQ_terraza + Sala_BBQ + Gimnasio, data = training_data),
  recipe(lPrecio ~ Estrato +lat+lon+año+ Habitaciones + Habitaciones2 + M2_por_Habitación + Terraza + Garaje + Sala_BBQ_terraza + Sala_BBQ + Gimnasio + Chimenea, data = training_data),
  recipe(lPrecio ~ Estrato +lat+lon+año+ Habitaciones + Habitaciones2 + M2_por_Habitación + Terraza + Garaje + Sala_BBQ_terraza + Sala_BBQ + Gimnasio + Chimenea + Seguridad, data = training_data),
  recipe(lPrecio ~ Estrato +lat+lon+año+ Habitaciones + Habitaciones2 + M2_por_Habitación + Terraza + Garaje + Sala_BBQ_terraza + Sala_BBQ + Gimnasio + Chimenea + Seguridad + Dist_Parques, data = training_data),
  recipe(lPrecio ~ Estrato +lat+lon+año+ Habitaciones + Habitaciones2 + M2_por_Habitación + Terraza + Garaje + Sala_BBQ_terraza + Sala_BBQ + Gimnasio + Chimenea + Seguridad + Dist_Parques + Dist_Transp_Publico, data = training_data),
  recipe(lPrecio ~ Estrato +lat+lon+año+ Habitaciones + Habitaciones2 + M2_por_Habitación + Terraza + Garaje + Sala_BBQ_terraza + Sala_BBQ + Gimnasio + Chimenea + Seguridad + Dist_Parques + Dist_Transp_Publico + Dist_Establecimientos, data = training_data),
  recipe(lPrecio ~ Estrato +lat+lon+año+ Habitaciones + Habitaciones2 + M2_por_Habitación + Terraza + Garaje + Sala_BBQ_terraza + Sala_BBQ + Gimnasio + Chimenea + Seguridad + Dist_Parques + Dist_Transp_Publico + Dist_Establecimientos + Dist_C_Comerc, data = training_data),
  recipe(lPrecio ~ Estrato +lat+lon+año+ Habitaciones + Habitaciones2 + M2_por_Habitación + Terraza + Garaje + Sala_BBQ_terraza + Sala_BBQ + Gimnasio + Chimenea + Seguridad + Dist_Parques + Dist_Transp_Publico + Dist_Establecimientos + Dist_C_Comerc + Dist_Centros_Educ, data = training_data),
  recipe(lPrecio ~ Estrato +lat+lon+año+ Habitaciones + Habitaciones2 + M2_por_Habitación + Terraza + Garaje + Sala_BBQ_terraza + Sala_BBQ + Gimnasio + Chimenea + Seguridad + Dist_Parques + Dist_Transp_Publico + Dist_Establecimientos + Dist_C_Comerc + Dist_Centros_Educ + Dist_Restaurantes + Dist_Bancos+lat+lon, data = training_data))

# Crear un contenedor para los resultados
results <- list()
# Iterar a través de las recetas y ajustar los modelos
for (i in seq_along(recetas2)) {
  receta <- recetas[[i]]
  
  # Ajustar el modelo
  lm_mod <- linear_reg() %>%
    set_engine("lm") %>%
    set_mode("regression")
  
  modelo <- workflow() %>%
    add_recipe(receta) %>%
    add_model(lm_mod) %>%
    fit(data = training_data)
  
  # Realizar predicciones en el conjunto de prueba
  predicciones <- predict(modelo, new_data = testing_data) %>%
    bind_cols(testing_data)
  predicciones <- predicciones %>% mutate(.pred = exp(.pred))
  
  # Calcular el MAE
  mae <- yardstick::mae(data = predicciones, truth = lPrecio, estimate = .pred)
  
  results[[i]] <- mae
}


# Mostrar los resultados
results


###### Divisón por Estrato 4
training_indices4 <- initial_split(train_apart4, prop = 0.7)
train_data4a <- training_indices4 %>% training()
test_data4a <- training_indices4 %>% testing()
training_indices4c <- initial_split(train_casas4, prop = 0.7)
train_data4c <- training_indices4c %>% training()
test_data4c <- training_indices4c %>% testing()
combined_test4 <- rbind(test_data4c, test_data4a)

casas_4<- list(
  recipe(lPrecio ~ lat+lon+Año, data = train_data4c),
  recipe(lPrecio ~lat+lon+Año+ Habitaciones + Habitaciones2, data = train_data4c),
  recipe(lPrecio ~ lat+lon+Año+ Habitaciones + Habitaciones2 + M2_por_Habitacion, data = train_data4c),
  recipe(lPrecio ~ lat+lon+Año+ Habitaciones + Habitaciones2 + M2_por_Habitacion + Terraza, data = train_data4c),
  recipe(lPrecio ~ lat+lon+Año+ Habitaciones + Habitaciones2 + M2_por_Habitacion + Terraza + Garaje, data = train_data4c),
  recipe(lPrecio ~ lat+lon+Año+ Habitaciones + Habitaciones2 + M2_por_Habitacion + Terraza + Garaje + Sala_BBQ, data = train_data4c),
  recipe(lPrecio ~ lat+lon+Año+ Habitaciones + Habitaciones2 + M2_por_Habitacion + Terraza + Garaje + Sala_BBQ + Sala_BBQ_terraza, data = train_data4c),
  recipe(lPrecio ~ lat+lon+Año+ Habitaciones + Habitaciones2 + M2_por_Habitacion + Terraza + Garaje + Sala_BBQ + Sala_BBQ_terraza + Gimnasio, data = train_data4c),
  recipe(lPrecio ~ lat+lon+Año+ Habitaciones + Habitaciones2 + M2_por_Habitacion + Terraza + Garaje + Sala_BBQ + Gimnasio, data = train_data4c),
  recipe(lPrecio ~ lat+lon+Año+ Habitaciones + Habitaciones2 + M2_por_Habitacion + Terraza + Garaje + Sala_BBQ_terraza + Gimnasio, data = train_data4c),
  recipe(lPrecio ~ lat+lon+Año+ Habitaciones + Habitaciones2 + M2_por_Habitacion + Terraza + Garaje + Sala_BBQ_terraza + Sala_BBQ + Gimnasio, data = train_data4c),
  recipe(lPrecio ~ lat+lon+Año+ Habitaciones + Habitaciones2 + M2_por_Habitacion + Terraza + Garaje + Sala_BBQ_terraza + Sala_BBQ + Gimnasio + Chimenea, data = train_data4a),
  recipe(lPrecio ~ lat+lon+Año+ Habitaciones + Habitaciones2 + M2_por_Habitacion + Terraza + Garaje + Sala_BBQ_terraza + Sala_BBQ + Gimnasio + Chimenea + Seguridad, data = train_data4c),
  recipe(lPrecio ~ lat+lon+Año+ Habitaciones + Habitaciones2 + M2_por_Habitacion + Terraza + Garaje + Sala_BBQ_terraza + Sala_BBQ + Gimnasio + Chimenea + Seguridad + Dist_Parques, data = train_data4c),
  recipe(lPrecio ~ lat+lon+Año+ Habitaciones + Habitaciones2 + M2_por_Habitacion + Terraza + Garaje + Sala_BBQ_terraza + Sala_BBQ + Gimnasio + Chimenea + Seguridad + Dist_Parques + Dist_Transp_Publico, data = train_data4c),
  recipe(lPrecio ~ lat+lon+Año+ Habitaciones + Habitaciones2 + M2_por_Habitacion + Terraza + Garaje + Sala_BBQ_terraza + Sala_BBQ + Gimnasio + Chimenea + Seguridad + Dist_Parques + Dist_Transp_Publico + Dist_Establecimientos, data = train_data4c),
  recipe(lPrecio ~ lat+lon+Año+ Habitaciones + Habitaciones2 + M2_por_Habitacion + Terraza + Garaje + Sala_BBQ_terraza + Sala_BBQ + Gimnasio + Chimenea + Seguridad + Dist_Parques + Dist_Transp_Publico + Dist_Establecimientos + Dist_C_Comerc, data = train_data4c),
  recipe(lPrecio ~ lat+lon+Año+ Habitaciones + Habitaciones2 + M2_por_Habitacion + Terraza + Garaje + Sala_BBQ_terraza + Sala_BBQ + Gimnasio + Chimenea + Seguridad + Dist_Parques + Dist_Transp_Publico + Dist_Establecimientos + Dist_C_Comerc + Dist_Centros_Educ, data = train_data4c),
  recipe(lPrecio ~ lat+lon+Año+ Habitaciones + Habitaciones2 + M2_por_Habitacion + Terraza + Garaje + Sala_BBQ_terraza + Sala_BBQ + Gimnasio + Chimenea + Seguridad + Dist_Parques + Dist_Transp_Publico + Dist_Establecimientos + Dist_C_Comerc + Dist_Centros_Educ + Dist_Restaurantes + Dist_Bancos+lat+lon, data = train_data4c))

# Crear un contenedor para los resultados
resultscasas <- list()
# Iterar a través de las recetas y ajustar los modelos
for (i in seq_along(casas_4)) {
  receta <- casas_4[[i]]
  
  # Ajustar el modelo
  lm_mod <- linear_reg() %>%
    set_engine("lm") %>%
    set_mode("regression")
  
  modelo <- workflow() %>%
    add_recipe(receta) %>%
    add_model(lm_mod) %>%
    fit(data = train_data4c)
  
  # Realizar predicciones en el conjunto de prueba
  predicciones <- predict(modelo, new_data = test_data4c) %>%
    bind_cols(test_data4c)
  predicciones <- predicciones %>% mutate(.pred = exp(.pred))
  
  # Calcular el MAE
  mae <- yardstick::mae(data = predicciones, truth = lPrecio, estimate = .pred)
  
  resultscasas[[i]] <- mae
}
resultscasas

Apartamento_4<- list(
  recipe(lPrecio ~ lat+lon+Año, data = train_data4a),
  recipe(lPrecio ~lat+lon+Año+ Habitaciones + Habitaciones2, data = train_data4a),
  recipe(lPrecio ~ lat+lon+Año+ Habitaciones + Habitaciones2 + M2_por_Habitacion, data = train_data4a),
  recipe(lPrecio ~ lat+lon+Año+ Habitaciones + Habitaciones2 + M2_por_Habitacion + Terraza, data = train_data4a),
  recipe(lPrecio ~ lat+lon+Año+ Habitaciones + Habitaciones2 + M2_por_Habitacion + Terraza + Garaje, data = train_data4a),
  recipe(lPrecio ~ lat+lon+Año+ Habitaciones + Habitaciones2 + M2_por_Habitacion + Terraza + Garaje + Sala_BBQ, data = train_data4a),
  recipe(lPrecio ~ lat+lon+Año+ Habitaciones + Habitaciones2 + M2_por_Habitacion + Terraza + Garaje + Sala_BBQ + Sala_BBQ_terraza, data = train_data4a),
  recipe(lPrecio ~ lat+lon+Año+ Habitaciones + Habitaciones2 + M2_por_Habitacion + Terraza + Garaje + Sala_BBQ + Sala_BBQ_terraza + Gimnasio, data = train_data4a),
  recipe(lPrecio ~ lat+lon+Año+ Habitaciones + Habitaciones2 + M2_por_Habitacion + Terraza + Garaje + Sala_BBQ + Gimnasio, data = train_data4a),
  recipe(lPrecio ~ lat+lon+Año+ Habitaciones + Habitaciones2 + M2_por_Habitacion + Terraza + Garaje + Sala_BBQ_terraza + Gimnasio, data = train_data4a),
  recipe(lPrecio ~ lat+lon+Año+ Habitaciones + Habitaciones2 + M2_por_Habitacion + Terraza + Garaje + Sala_BBQ_terraza + Sala_BBQ + Gimnasio, data = train_data4a),
  recipe(lPrecio ~ lat+lon+Año+ Habitaciones + Habitaciones2 + M2_por_Habitacion + Terraza + Garaje + Sala_BBQ_terraza + Sala_BBQ + Gimnasio + Chimenea, data = train_data4a),
  recipe(lPrecio ~ lat+lon+Año+ Habitaciones + Habitaciones2 + M2_por_Habitacion + Terraza + Garaje + Sala_BBQ_terraza + Sala_BBQ + Gimnasio + Chimenea + Seguridad, data = train_data4a),
  recipe(lPrecio ~ lat+lon+Año+ Habitaciones + Habitaciones2 + M2_por_Habitacion + Terraza + Garaje + Sala_BBQ_terraza + Sala_BBQ + Gimnasio + Chimenea + Seguridad + Dist_Parques, data = train_data4a),
  recipe(lPrecio ~ lat+lon+Año+ Habitaciones + Habitaciones2 + M2_por_Habitacion + Terraza + Garaje + Sala_BBQ_terraza + Sala_BBQ + Gimnasio + Chimenea + Seguridad + Dist_Parques + Dist_Transp_Publico, data = train_data4a),
  recipe(lPrecio ~ lat+lon+Año+ Habitaciones + Habitaciones2 + M2_por_Habitacion + Terraza + Garaje + Sala_BBQ_terraza + Sala_BBQ + Gimnasio + Chimenea + Seguridad + Dist_Parques + Dist_Transp_Publico + Dist_Establecimientos, data = train_data4a),
  recipe(lPrecio ~ lat+lon+Año+ Habitaciones + Habitaciones2 + M2_por_Habitacion + Terraza + Garaje + Sala_BBQ_terraza + Sala_BBQ + Gimnasio + Chimenea + Seguridad + Dist_Parques + Dist_Transp_Publico + Dist_Establecimientos + Dist_C_Comerc, data = train_data4a),
  recipe(lPrecio ~ lat+lon+Año+ Habitaciones + Habitaciones2 + M2_por_Habitacion + Terraza + Garaje + Sala_BBQ_terraza + Sala_BBQ + Gimnasio + Chimenea + Seguridad + Dist_Parques + Dist_Transp_Publico + Dist_Establecimientos + Dist_C_Comerc + Dist_Centros_Educ, data = train_data4a),
  recipe(lPrecio ~ lat+lon+Año+ Habitaciones + Habitaciones2 + M2_por_Habitacion + Terraza + Garaje + Sala_BBQ_terraza + Sala_BBQ + Gimnasio + Chimenea + Seguridad + Dist_Parques + Dist_Transp_Publico + Dist_Establecimientos + Dist_C_Comerc + Dist_Centros_Educ + Dist_Restaurantes + Dist_Bancos+lat+lon, data = train_data4a))

# Crear un contenedor para los resultados
resultsapart <- list()
# Iterar a través de las recetas y ajustar los modelos
for (i in seq_along(Apartamento_4)) {
  receta <- Apartamento_4[[i]]
  
  # Ajustar el modelo
  lm_mod <- linear_reg() %>%
    set_engine("lm") %>%
    set_mode("regression")
  
  modelo <- workflow() %>%
    add_recipe(receta) %>%
    add_model(lm_mod) %>%
    fit(data = train_data4a)
  
  # Realizar predicciones en el conjunto de prueba
  predicciones <- predict(modelo, new_data = test_data4a) %>%
    bind_cols(test_data4a)
  predicciones <- predicciones %>% mutate(.pred = exp(.pred))
  
  # Calcular el MAE
  mae <- yardstick::mae(data = predicciones, truth = lPrecio, estimate = .pred)
  
  resultsapart[[i]] <- mae
}
resultsapart


######## División por apartamentos y Casas


training_indices1a <- initial_split(train_apart1, prop = 0.7)
train_data1a <- training_indices1a %>% training()
test_data1a <- training_indices1a %>% testing()
training_indices1c <- initial_split(train_casas1, prop = 0.7)
train_data1c <- training_indices1c %>% training()
test_data1c <- training_indices1c %>% testing()
combined_test1 <- rbind(test_data1c, test_data1a)



casas_1<- list(
  recipe(lPrecio ~ lat+lon+Año, data = train_data1c),
  recipe(lPrecio ~lat+lon+Año+ Habitaciones + Habitaciones2, data = train_data1c),
  recipe(lPrecio ~ lat+lon+Año+ Habitaciones + Habitaciones2 + M2_por_Habitacion, data = train_data1c),
  recipe(lPrecio ~ lat+lon+Año+ Habitaciones + Habitaciones2 + M2_por_Habitacion + Terraza, data = train_data1c),
  recipe(lPrecio ~ lat+lon+Año+ Habitaciones + Habitaciones2 + M2_por_Habitacion + Terraza + Garaje, data = train_data1c),
  recipe(lPrecio ~ lat+lon+Año+ Habitaciones + Habitaciones2 + M2_por_Habitacion + Terraza + Garaje + Sala_BBQ, data = train_data1c),
  recipe(lPrecio ~ lat+lon+Año+ Habitaciones + Habitaciones2 + M2_por_Habitacion + Terraza + Garaje + Sala_BBQ + Sala_BBQ_terraza, data = train_data1c),
  recipe(lPrecio ~ lat+lon+Año+ Habitaciones + Habitaciones2 + M2_por_Habitacion + Terraza + Garaje + Sala_BBQ + Sala_BBQ_terraza + Gimnasio, data = train_data1c),
  recipe(lPrecio ~ lat+lon+Año+ Habitaciones + Habitaciones2 + M2_por_Habitacion + Terraza + Garaje + Sala_BBQ + Gimnasio, data = train_data1c),
  recipe(lPrecio ~ lat+lon+Año+ Habitaciones + Habitaciones2 + M2_por_Habitacion + Terraza + Garaje + Sala_BBQ_terraza + Gimnasio, data = train_data1c),
  recipe(lPrecio ~ lat+lon+Año+ Habitaciones + Habitaciones2 + M2_por_Habitacion + Terraza + Garaje + Sala_BBQ_terraza + Sala_BBQ + Gimnasio, data = train_data1c),
  recipe(lPrecio ~ lat+lon+Año+ Habitaciones + Habitaciones2 + M2_por_Habitacion + Terraza + Garaje + Sala_BBQ_terraza + Sala_BBQ + Gimnasio + Chimenea, data = train_data1a),
  recipe(lPrecio ~ lat+lon+Año+ Habitaciones + Habitaciones2 + M2_por_Habitacion + Terraza + Garaje + Sala_BBQ_terraza + Sala_BBQ + Gimnasio + Chimenea + Seguridad, data = train_data1c),
  recipe(lPrecio ~ lat+lon+Año+ Habitaciones + Habitaciones2 + M2_por_Habitacion + Terraza + Garaje + Sala_BBQ_terraza + Sala_BBQ + Gimnasio + Chimenea + Seguridad + Dist_Parques, data = train_data1c),
  recipe(lPrecio ~ lat+lon+Año+ Habitaciones + Habitaciones2 + M2_por_Habitacion + Terraza + Garaje + Sala_BBQ_terraza + Sala_BBQ + Gimnasio + Chimenea + Seguridad + Dist_Parques + Dist_Transp_Publico, data = train_data1c),
  recipe(lPrecio ~ lat+lon+Año+ Habitaciones + Habitaciones2 + M2_por_Habitacion + Terraza + Garaje + Sala_BBQ_terraza + Sala_BBQ + Gimnasio + Chimenea + Seguridad + Dist_Parques + Dist_Transp_Publico + Dist_Establecimientos, data = train_data1c),
  recipe(lPrecio ~ lat+lon+Año+ Habitaciones + Habitaciones2 + M2_por_Habitacion + Terraza + Garaje + Sala_BBQ_terraza + Sala_BBQ + Gimnasio + Chimenea + Seguridad + Dist_Parques + Dist_Transp_Publico + Dist_Establecimientos + Dist_C_Comerc, data = train_data1c),
  recipe(lPrecio ~ lat+lon+Año+ Habitaciones + Habitaciones2 + M2_por_Habitacion + Terraza + Garaje + Sala_BBQ_terraza + Sala_BBQ + Gimnasio + Chimenea + Seguridad + Dist_Parques + Dist_Transp_Publico + Dist_Establecimientos + Dist_C_Comerc + Dist_Centros_Educ, data = train_data1c),
  recipe(lPrecio ~ lat+lon+Año+ Habitaciones + Habitaciones2 + M2_por_Habitacion + Terraza + Garaje + Sala_BBQ_terraza + Sala_BBQ + Gimnasio + Chimenea + Seguridad + Dist_Parques + Dist_Transp_Publico + Dist_Establecimientos + Dist_C_Comerc + Dist_Centros_Educ + Dist_Restaurantes + Dist_Bancos+lat+lon, data = train_data1c))

# Crear un contenedor para los resultados
resultscasas <- list()
# Iterar a través de las recetas y ajustar los modelos
for (i in seq_along(casas_1)) {
  receta <- casas_1[[i]]
  
  # Ajustar el modelo
  lm_mod <- linear_reg() %>%
    set_engine("lm") %>%
    set_mode("regression")
  
  modelo <- workflow() %>%
    add_recipe(receta) %>%
    add_model(lm_mod) %>%
    fit(data = train_data1c)
  
  # Realizar predicciones en el conjunto de prueba
  predicciones <- predict(modelo, new_data = test_data1c) %>%
    bind_cols(test_data1c)
  predicciones <- predicciones %>% mutate(.pred = exp(.pred))
  
  # Calcular el MAE
  mae <- yardstick::mae(data = predicciones, truth = lPrecio, estimate = .pred)
  
  resultscasas[[i]] <- mae
}
resultscasas

Apartamento_1<- list(
  recipe(lPrecio ~ lat+lon+Año, data = train_data1a),
  recipe(lPrecio ~lat+lon+Año+ Habitaciones + Habitaciones2, data = train_data1a),
  recipe(lPrecio ~ lat+lon+Año+ Habitaciones + Habitaciones2 + M2_por_Habitacion, data = train_data1a),
  recipe(lPrecio ~ lat+lon+Año+ Habitaciones + Habitaciones2 + M2_por_Habitacion + Terraza, data = train_data1a),
  recipe(lPrecio ~ lat+lon+Año+ Habitaciones + Habitaciones2 + M2_por_Habitacion + Terraza + Garaje, data = train_data1a),
  recipe(lPrecio ~ lat+lon+Año+ Habitaciones + Habitaciones2 + M2_por_Habitacion + Terraza + Garaje + Sala_BBQ, data = train_data1a),
  recipe(lPrecio ~ lat+lon+Año+ Habitaciones + Habitaciones2 + M2_por_Habitacion + Terraza + Garaje + Sala_BBQ + Sala_BBQ_terraza, data = train_data1a),
  recipe(lPrecio ~ lat+lon+Año+ Habitaciones + Habitaciones2 + M2_por_Habitacion + Terraza + Garaje + Sala_BBQ + Sala_BBQ_terraza + Gimnasio, data = train_data1a),
  recipe(lPrecio ~ lat+lon+Año+ Habitaciones + Habitaciones2 + M2_por_Habitacion + Terraza + Garaje + Sala_BBQ + Gimnasio, data = train_data1a),
  recipe(lPrecio ~ lat+lon+Año+ Habitaciones + Habitaciones2 + M2_por_Habitacion + Terraza + Garaje + Sala_BBQ_terraza + Gimnasio, data = train_data1a),
  recipe(lPrecio ~ lat+lon+Año+ Habitaciones + Habitaciones2 + M2_por_Habitacion + Terraza + Garaje + Sala_BBQ_terraza + Sala_BBQ + Gimnasio, data = train_data1a),
  recipe(lPrecio ~ lat+lon+Año+ Habitaciones + Habitaciones2 + M2_por_Habitacion + Terraza + Garaje + Sala_BBQ_terraza + Sala_BBQ + Gimnasio + Chimenea, data = train_data1a),
  recipe(lPrecio ~ lat+lon+Año+ Habitaciones + Habitaciones2 + M2_por_Habitacion + Terraza + Garaje + Sala_BBQ_terraza + Sala_BBQ + Gimnasio + Chimenea + Seguridad, data = train_data1a),
  recipe(lPrecio ~ lat+lon+Año+ Habitaciones + Habitaciones2 + M2_por_Habitacion + Terraza + Garaje + Sala_BBQ_terraza + Sala_BBQ + Gimnasio + Chimenea + Seguridad + Dist_Parques, data = train_data1a),
  recipe(lPrecio ~ lat+lon+Año+ Habitaciones + Habitaciones2 + M2_por_Habitacion + Terraza + Garaje + Sala_BBQ_terraza + Sala_BBQ + Gimnasio + Chimenea + Seguridad + Dist_Parques + Dist_Transp_Publico, data = train_data1a),
  recipe(lPrecio ~ lat+lon+Año+ Habitaciones + Habitaciones2 + M2_por_Habitacion + Terraza + Garaje + Sala_BBQ_terraza + Sala_BBQ + Gimnasio + Chimenea + Seguridad + Dist_Parques + Dist_Transp_Publico + Dist_Establecimientos, data = train_data1a),
  recipe(lPrecio ~ lat+lon+Año+ Habitaciones + Habitaciones2 + M2_por_Habitacion + Terraza + Garaje + Sala_BBQ_terraza + Sala_BBQ + Gimnasio + Chimenea + Seguridad + Dist_Parques + Dist_Transp_Publico + Dist_Establecimientos + Dist_C_Comerc, data = train_data1a),
  recipe(lPrecio ~ lat+lon+Año+ Habitaciones + Habitaciones2 + M2_por_Habitacion + Terraza + Garaje + Sala_BBQ_terraza + Sala_BBQ + Gimnasio + Chimenea + Seguridad + Dist_Parques + Dist_Transp_Publico + Dist_Establecimientos + Dist_C_Comerc + Dist_Centros_Educ, data = train_data1a),
  recipe(lPrecio ~ lat+lon+Año+ Habitaciones + Habitaciones2 + M2_por_Habitacion + Terraza + Garaje + Sala_BBQ_terraza + Sala_BBQ + Gimnasio + Chimenea + Seguridad + Dist_Parques + Dist_Transp_Publico + Dist_Establecimientos + Dist_C_Comerc + Dist_Centros_Educ + Dist_Restaurantes + Dist_Bancos+lat+lon, data = train_data1a))

# Crear un contenedor para los resultados
resultsapart <- list()
# Iterar a través de las recetas y ajustar los modelos
for (i in seq_along(Apartamento_1)) {
  receta <- Apartamento_1[[i]]
  
  # Ajustar el modelo
  lm_mod <- linear_reg() %>%
    set_engine("lm") %>%
    set_mode("regression")
  
  modelo <- workflow() %>%
    add_recipe(receta) %>%
    add_model(lm_mod) %>%
    fit(data = train_data1a)
  
  # Realizar predicciones en el conjunto de prueba
  predicciones <- predict(modelo, new_data = test_data1a) %>%
    bind_cols(test_data1a)
  predicciones <- predicciones %>% mutate(.pred = exp(.pred))
  
  # Calcular el MAE
  mae <- yardstick::mae(data = predicciones, truth = lPrecio, estimate = .pred)
  
  resultsapart[[i]] <- mae
}
resultsapart

###############-------------------------------FIN-----------------------------------------------------------------------------########