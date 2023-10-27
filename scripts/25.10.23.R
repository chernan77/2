#Problem Set 2
#Big Data y Machine Learning para Economía Aplicada

# Celin Hernández: 202210067
# Merit Tejeda: 202210104
# Estefanía Laborde: 201533743

install.packages("readxl")
install.packages("readr")
install.packages("pacman")
install.packages("osmdata")
install.packages("leaflet")
install.packages("dplyr")
install.packages("rgeos")
install.packages("openxlsx")
install.packages("ggplot2")
install.packages("writexl")
install.packages("geosphere")
install.packages("ggmap")
install.packages("geopy")
install.packages("stargazer")
install.packages("zoo")
install.packages("gridExtra")
install.packages("tidyverse")
install.packages("rvest")
install.packages("sf")
install.packages("glmnet")
install.packages("broom")
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

### --------------------------------------Combined_train --------------------------------#

combined_train$Habitaciones2 <- combined_train$Habitaciones^2
combined_train$M2_por_Habitacion_Garaje <- combined_train$M2_por_Habitación * combined_train$Garaje
combined_train$M2_por_Habitacion <- combined_train$M2_por_Habitación 
combined_train$Sala_BBQ_terraza <- combined_train$Sala_BBQ * combined_train$Terraza
combined_train$year <- as.character(combined_train$year)
combined_train$month <- as.character(combined_train$month)
combined_train$Fecha <- as.Date(paste0(combined_train$year, "-", combined_train$month, "-01"))
combined_train$Fecha <- as.Date(combined_train$Fecha)

combined_test$Habitaciones2 <- combined_test$Habitaciones^2
combined_test$M2_por_Habitacion_Garaje <- combined_test$M2_por_Habitacion * combined_test$Garaje
combined_test$Sala_BBQ_terraza <- combined_test$Sala_BBQ * combined_test$Terraza
combined_test$year <- as.character(combined_test$year)
combined_test$month <- as.character(combined_test$month)
combined_test$Fecha <- as.Date(paste0(combined_test$year, "-", combined_test$month, "-01"))
combined_test$Fecha <- as.Date(combined_test$Fecha)

# -------------------------------CREACION DE OTRAS VARIABLES-------------------------- # 
train_casas1$M2_por_Habitacion<- train_casas1$Area/train_casas1$Habitaciones
train_casas1$Habitaciones2 <- train_casas1$Habitaciones^2
train_casas1$M2_por_Habitacion_Garaje <- train_casas1$M2_por_Habitacion * train_casas1$Garaje
train_casas1$Sala_BBQ_terraza <- train_casas1$Sala_BBQ * train_casas1$Terraza
train_casas1$year <- as.character(train_casas1$year)
train_casas1$month <- as.character(train_casas1$month)
train_casas1$Fecha <- as.Date(paste0(train_casas1$year, "-", train_casas1$month, "-01"))
train_casas1$Fecha <- as.Date(train_casas1$Fecha)

# -------------------------------CREACION DE OTRAS VARIABLES-------------------------- # 
train_apart1$M2_por_Habitacion<- train_apart1$Area/train_apart1$Habitaciones
train_apart1$Habitaciones2<- train_apart1$Habitaciones^2
train_apart1$M2_por_Habitacion_Garaje <- train_apart1$M2_por_Habitacion * train_apart1$Garaje
train_apart1$Sala_BBQ_terraza <- train_apart1$Sala_BBQ * train_apart1$Terraza
train_apart1$year <- as.character(train_apart1$year)
train_apart1$month <- as.character(train_apart1$month)
train_apart1$Fecha <- as.Date(paste0(train_apart1$year, "-", train_apart1$month, "-01"))
train_apart1$Fecha <- as.Date(train_apart1$Fecha)
# ------------------------------PRONOSTICOS FUERA DE MUESTRA-------------------------------# 

# -------------------------------CREACION DE OTRAS VARIABLES CASAS-------------------------# 

test_casas1$Habitaciones2 <- test_casas1$Habitaciones^2
test_casas1$M2_por_Habitacion_Garaje <- test_casas1$M2_por_Habitacion * test_casas1$Garaje
test_casas1$Sala_BBQ_terraza <- test_casas1$Sala_BBQ * test_casas1$Terraza
test_casas1$year <- as.character(test_casas1$year)
test_casas1$month <- as.character(test_casas1$month)
test_casas1$Fecha <- as.Date(paste0(test_casas1$year, "-", test_casas1$month, "-01"))
test_casas1$Fecha <- as.Date(test_casas1$Fecha)

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

#install.packages("caret")
library(caret)
#install.packages("ipred")
library(ipred)
library(caret)
#install.packages("rpart.plot")
library(rpart.plot)
#######################################Empecemos con un modelo de regresión lineal###################################

# Segmenta la base de datos en localidad de Usaquén (entrenamiento) y resto de localidades (prueba)
localidad_entrenamiento <- "Usaquén"  # Reemplaza con el nombre de tu localidad de interés
test_data <- combined_train[combined_train$localidad != localidad_entrenamiento, ]
localidades_a_excluir <- c("Fontibón", "Los Martires")
test_data <- test_data[!(test_data$localidad %in% localidades_a_excluir), ]
train_data1 <- subset(combined_train, localidad == localidad_entrenamiento)
test_data1 <- test_data[!(test_data$localidad %in% localidades_a_excluir), ]

# Verifica la cantidad de datos en cada conjunto
nrow(train_data1)  # Número de filas en el conjunto de entrenamiento
nrow(test_data1)   # Número de filas en el conjunto de prueba

train_data1$M2_por_Habitacion<- train_data1$Area/train_data1$Habitaciones
train_data1$Habitaciones2 <- train_data1$Habitaciones^2
train_data1$M2_por_Habitacion_Garaje <- train_data1$M2_por_Habitacion * train_data1$Garaje
train_data1$Sala_BBQ_terraza <- train_data1$Sala_BBQ * train_data1$Terraza
train_data1$year <- as.character(train_data1$year)
train_data1$month <- as.character(train_data1$month)
train_data1$Fecha <- as.Date(paste0(train_data1$year, "-", train_data1$month, "-01"))
train_data1$Fecha <- as.Date(train_data1$Fecha)
train_data1$lPrecio<- log(train_data1$Precio)

test_data1$M2_por_Habitacion<- test_data1$Area/test_data1$Habitaciones
test_data1$Habitaciones2 <- test_data1$Habitaciones^2
test_data1$M2_por_Habitacion_Garaje <- test_data1$M2_por_Habitacion * test_data1$Garaje
test_data1$Sala_BBQ_terraza <- test_data1$Sala_BBQ * test_data1$Terraza
test_data1$year <- as.character(test_data1$year)
test_data1$month <- as.character(test_data1$month)
test_data1$Fecha <- as.Date(paste0(test_data1$year, "-", test_data1$month, "-01"))
test_data1$Fecha <- as.Date(test_data1$Fecha)
test_data1$lPrecio<- log(test_data1$Precio)


train_apart4 <- train_apart1[train_apart1$Estrato == 4, c("property_id","title", "Fecha", "localidad","Precio", "lPrecio",
                                                            "Precio_M2", "Habitaciones","Habitaciones2", "Baños", "Area","M2_por_Habitacion", "lat", "lon", "Terraza",
                                                            "Garaje", "Sala_BBQ","Piscina","Gimnasio", "Chimenea","Seguridad",
                                                            "Dist_Parques", "Dist_Transp_Publico", "Dist_Establecimientos",
                                                            "Dist_C_Comerc", "Dist_Centros_Educ", "Dist_Restaurantes", "Dist_Bancos", "Sala_BBQ_terraza")]
train_casas4 <- train_casas1[train_casas1$Estrato == 4, c("property_id","title", "Fecha", "localidad","Precio", "lPrecio",
                                                            "Precio_M2", "Habitaciones","Habitaciones2", "Baños", "Area","M2_por_Habitacion", "lat", "lon", "Terraza",
                                                            "Garaje", "Sala_BBQ","Piscina","Gimnasio", "Chimenea","Seguridad",
                                                            "Dist_Parques", "Dist_Transp_Publico", "Dist_Establecimientos",
                                                            "Dist_C_Comerc", "Dist_Centros_Educ", "Dist_Restaurantes", "Dist_Bancos", "Sala_BBQ_terraza")]



#------------------------------------------------------------------- Modelo OLS-----------------------------------------------####


#############################---------------------------------------------------------------------------------------############
library(tidymodels)
library(yardstick)

# Crear una lista de recetas
recetas <- list(
  recipe(lPrecio ~ Estrato, data = train_data1),
  recipe(lPrecio ~ Estrato + Habitaciones + Habitaciones2, data = train_data1),
  recipe(lPrecio ~ Estrato + Habitaciones + Habitaciones2 + M2_por_Habitación, data = train_data1),
  recipe(lPrecio ~ Estrato + Habitaciones + Habitaciones2 + M2_por_Habitación + Terraza, data = train_data1),
  recipe(lPrecio ~ Estrato + Habitaciones + Habitaciones2 + M2_por_Habitación + Terraza + Garaje, data = train_data1),
  recipe(lPrecio ~ Estrato + Habitaciones + Habitaciones2 + M2_por_Habitación + Terraza + Garaje + Sala_BBQ, data = train_data1),
  recipe(lPrecio ~ Estrato + Habitaciones + Habitaciones2 + M2_por_Habitación + Terraza + Garaje + Sala_BBQ + Sala_BBQ_terraza, data = train_data1),
  recipe(lPrecio ~ Estrato + Habitaciones + Habitaciones2 + M2_por_Habitación + Terraza + Garaje + Sala_BBQ + Sala_BBQ_terraza + Gimnasio, data = train_data1),
  recipe(lPrecio ~ Estrato + Habitaciones + Habitaciones2 + M2_por_Habitación + Terraza + Garaje + Sala_BBQ + Gimnasio, data = train_data1),
  recipe(lPrecio ~ Estrato + Habitaciones + Habitaciones2 + M2_por_Habitación + Terraza + Garaje + Sala_BBQ_terraza + Gimnasio, data = train_data1),
  recipe(lPrecio ~ Estrato + Habitaciones + Habitaciones2 + M2_por_Habitación + Terraza + Garaje + Sala_BBQ_terraza + Sala_BBQ + Gimnasio, data = train_data1),
  recipe(lPrecio ~ Estrato + Habitaciones + Habitaciones2 + M2_por_Habitación + Terraza + Garaje + Sala_BBQ_terraza + Sala_BBQ + Gimnasio + Chimenea, data = train_data1),
  recipe(lPrecio ~ Estrato + Habitaciones + Habitaciones2 + M2_por_Habitación + Terraza + Garaje + Sala_BBQ_terraza + Sala_BBQ + Gimnasio + Chimenea + Seguridad, data = train_data1),
  recipe(lPrecio ~ Estrato + Habitaciones + Habitaciones2 + M2_por_Habitación + Terraza + Garaje + Sala_BBQ_terraza + Sala_BBQ + Gimnasio + Chimenea + Seguridad + Dist_Parques, data = train_data1),
  recipe(lPrecio ~ Estrato + Habitaciones + Habitaciones2 + M2_por_Habitación + Terraza + Garaje + Sala_BBQ_terraza + Sala_BBQ + Gimnasio + Chimenea + Seguridad + Dist_Parques + Dist_Transp_Publico, data = train_data1),
  recipe(lPrecio ~ Estrato + Habitaciones + Habitaciones2 + M2_por_Habitación + Terraza + Garaje + Sala_BBQ_terraza + Sala_BBQ + Gimnasio + Chimenea + Seguridad + Dist_Parques + Dist_Transp_Publico + Dist_Establecimientos, data = train_data1),
  recipe(lPrecio ~ Estrato + Habitaciones + Habitaciones2 + M2_por_Habitación + Terraza + Garaje + Sala_BBQ_terraza + Sala_BBQ + Gimnasio + Chimenea + Seguridad + Dist_Parques + Dist_Transp_Publico + Dist_Establecimientos + Dist_C_Comerc, data = train_data1),
  recipe(lPrecio ~ Estrato + Habitaciones + Habitaciones2 + M2_por_Habitación + Terraza + Garaje + Sala_BBQ_terraza + Sala_BBQ + Gimnasio + Chimenea + Seguridad + Dist_Parques + Dist_Transp_Publico + Dist_Establecimientos + Dist_C_Comerc + Dist_Centros_Educ, data = train_data1),
  recipe(lPrecio ~ Estrato + Habitaciones + Habitaciones2 + M2_por_Habitación + Terraza + Garaje + Sala_BBQ_terraza + Sala_BBQ + Gimnasio + Chimenea + Seguridad + Dist_Parques + Dist_Transp_Publico + Dist_Establecimientos + Dist_C_Comerc + Dist_Centros_Educ + Dist_Restaurantes + Dist_Bancos, data = train_data1)
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
    fit(data = train_data)
  
  # Realizar predicciones en el conjunto de prueba
  predicciones <- predict(modelo, new_data = test_data1) %>%
    bind_cols(test_data)
  
  predicciones <- predicciones %>% mutate(.pred = exp(.pred))
  
  # Calcular el MAE
  mae <- yardstick::mae(data = predicciones, truth = Precio, estimate = .pred)
  
  results[[i]] <- mae
}

# Mostrar los resultados
results

# Dividir los datos en entrenamiento y prueba
set.seed(123)

# Dividir los datos en entrenamiento y prueba
training_indices <- initial_split(combined_train, prop = 0.7)
training_data <- training_indices %>% training()
testing_data <- training_indices %>% testing()
# Crear una lista de recetas
recetas2 <- list(
  recipe(lPrecio ~ Estrato, data = training_data),
  recipe(lPrecio ~ Estrato + Habitaciones + Habitaciones2, data = training_data),
  recipe(lPrecio ~ Estrato + Habitaciones + Habitaciones2 + M2_por_Habitación, data = training_data),
  recipe(lPrecio ~ Estrato + Habitaciones + Habitaciones2 + M2_por_Habitación + Terraza, data = training_data),
  recipe(lPrecio ~ Estrato + Habitaciones + Habitaciones2 + M2_por_Habitación + Terraza + Garaje, data = training_data),
  recipe(lPrecio ~ Estrato + Habitaciones + Habitaciones2 + M2_por_Habitación + Terraza + Garaje + Sala_BBQ, data = training_data),
  recipe(lPrecio ~ Estrato + Habitaciones + Habitaciones2 + M2_por_Habitación + Terraza + Garaje + Sala_BBQ + Sala_BBQ_terraza, data = training_data),
  recipe(lPrecio ~ Estrato + Habitaciones + Habitaciones2 + M2_por_Habitación + Terraza + Garaje + Sala_BBQ + Sala_BBQ_terraza + Gimnasio, data = training_data),
  recipe(lPrecio ~ Estrato + Habitaciones + Habitaciones2 + M2_por_Habitación + Terraza + Garaje + Sala_BBQ + Gimnasio, data = training_data),
  recipe(lPrecio ~ Estrato + Habitaciones + Habitaciones2 + M2_por_Habitación + Terraza + Garaje + Sala_BBQ_terraza + Gimnasio, data = training_data),
  recipe(lPrecio ~ Estrato + Habitaciones + Habitaciones2 + M2_por_Habitación + Terraza + Garaje + Sala_BBQ_terraza + Sala_BBQ + Gimnasio, data = training_data),
  recipe(lPrecio ~ Estrato + Habitaciones + Habitaciones2 + M2_por_Habitación + Terraza + Garaje + Sala_BBQ_terraza + Sala_BBQ + Gimnasio + Chimenea, data = training_data),
  recipe(lPrecio ~ Estrato + Habitaciones + Habitaciones2 + M2_por_Habitación + Terraza + Garaje + Sala_BBQ_terraza + Sala_BBQ + Gimnasio + Chimenea + Seguridad, data = training_data),
  recipe(lPrecio ~ Estrato + Habitaciones + Habitaciones2 + M2_por_Habitación + Terraza + Garaje + Sala_BBQ_terraza + Sala_BBQ + Gimnasio + Chimenea + Seguridad + Dist_Parques, data = training_data),
  recipe(lPrecio ~ Estrato + Habitaciones + Habitaciones2 + M2_por_Habitación + Terraza + Garaje + Sala_BBQ_terraza + Sala_BBQ + Gimnasio + Chimenea + Seguridad + Dist_Parques + Dist_Transp_Publico, data = training_data),
  recipe(lPrecio ~ Estrato + Habitaciones + Habitaciones2 + M2_por_Habitación + Terraza + Garaje + Sala_BBQ_terraza + Sala_BBQ + Gimnasio + Chimenea + Seguridad + Dist_Parques + Dist_Transp_Publico + Dist_Establecimientos, data = training_data),
  recipe(lPrecio ~ Estrato + Habitaciones + Habitaciones2 + M2_por_Habitación + Terraza + Garaje + Sala_BBQ_terraza + Sala_BBQ + Gimnasio + Chimenea + Seguridad + Dist_Parques + Dist_Transp_Publico + Dist_Establecimientos + Dist_C_Comerc, data = training_data),
  recipe(lPrecio ~ Estrato + Habitaciones + Habitaciones2 + M2_por_Habitación + Terraza + Garaje + Sala_BBQ_terraza + Sala_BBQ + Gimnasio + Chimenea + Seguridad + Dist_Parques + Dist_Transp_Publico + Dist_Establecimientos + Dist_C_Comerc + Dist_Centros_Educ, data = training_data),
  recipe(lPrecio ~ Estrato + Habitaciones + Habitaciones2 + M2_por_Habitación + Terraza + Garaje + Sala_BBQ_terraza + Sala_BBQ + Gimnasio + Chimenea + Seguridad + Dist_Parques + Dist_Transp_Publico + Dist_Establecimientos + Dist_C_Comerc + Dist_Centros_Educ + Dist_Restaurantes + Dist_Bancos, data = training_data))

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
###############------------------------------------------------------------------------#########################################

recp1 <- recipe(lPrecio ~ Estrato, data = combined_train)
recp2 <- recipe(lPrecio ~ Estrato + Habitaciones + Habitaciones2 , data = combined_train)
recp3 <- recipe(lPrecio ~ Estrato + Habitaciones + Habitaciones2 + M2_por_Habitación, data = combined_train)
recp4 <- recipe(lPrecio ~ Estrato + Habitaciones + Habitaciones2 + M2_por_Habitación + Terraza, data = combined_train)
recp5 <- recipe(lPrecio ~ Estrato + Habitaciones + Habitaciones2 + M2_por_Habitación + Terraza + Garaje, data = combined_train)
recp6 <- recipe(lPrecio ~ Estrato + Habitaciones + Habitaciones2 + M2_por_Habitación + Terraza + Garaje + Sala_BBQ, data = combined_train)
recp7 <- recipe(lPrecio ~ Estrato + Habitaciones + Habitaciones2 + M2_por_Habitación + Terraza + Garaje + Sala_BBQ +Sala_BBQ_terraza , data = combined_train)
recp8 <- recipe(lPrecio ~ Estrato + Habitaciones + Habitaciones2 + M2_por_Habitación + Terraza + Garaje + Sala_BBQ + Sala_BBQ_terraza+ Gimnasio, data = combined_train)
recp9 <- recipe(lPrecio ~ Estrato + Habitaciones + Habitaciones2 + M2_por_Habitación + Terraza + Garaje + Sala_BBQ+ Gimnasio, data = combined_train)
recp10 <- recipe(lPrecio ~ Estrato + Habitaciones + Habitaciones2 + M2_por_Habitación + Terraza + Garaje + Sala_BBQ_terraza+ Gimnasio, data = combined_train)
recp11 <- recipe(lPrecio ~ Estrato + Habitaciones + Habitaciones2 + M2_por_Habitación + Terraza + Garaje + Sala_BBQ_terraza+ Sala_BBQ + Gimnasio, data = combined_train)
recp12 <- recipe(lPrecio ~ Estrato + Habitaciones + Habitaciones2 + M2_por_Habitación + Terraza + Garaje + Sala_BBQ_terraza+ Sala_BBQ + Gimnasio+ Chimenea , data = combined_train)
recp12 <- recipe(lPrecio ~ Estrato + Habitaciones + Habitaciones2 + M2_por_Habitación + Terraza + Garaje + Sala_BBQ_terraza+ Sala_BBQ + Gimnasio+ Chimenea + Seguridad , data = combined_train)
recp13 <- recipe(lPrecio ~ Estrato + Habitaciones + Habitaciones2 + M2_por_Habitación + Terraza + Garaje + Sala_BBQ_terraza+ Sala_BBQ + Gimnasio+ Chimenea + Seguridad + Dist_Parques , data = combined_train)
recp13 <- recipe(lPrecio ~ Estrato + Habitaciones + Habitaciones2 + M2_por_Habitación + Terraza + Garaje + Sala_BBQ_terraza+ Sala_BBQ + Gimnasio+ Chimenea + Seguridad + Dist_Parques +Dist_Transp_Publico , data = combined_train)
recp14 <- recipe(lPrecio ~ Estrato + Habitaciones + Habitaciones2 + M2_por_Habitación + Terraza + Garaje + Sala_BBQ_terraza+ Sala_BBQ + Gimnasio+ Chimenea + Seguridad + Dist_Parques+Dist_Transp_Publico + Dist_Establecimientos
                 , data = combined_train)
recp15 <- recipe(lPrecio ~ Estrato + Habitaciones + Habitaciones2 + M2_por_Habitación + Terraza + Garaje + Sala_BBQ_terraza+ Sala_BBQ + Gimnasio+ Chimenea + Seguridad + Dist_Parques+Dist_Transp_Publico + Dist_Establecimientos
                 , data = combined_train)
recp16 <- recipe(lPrecio ~ Estrato + Habitaciones + Habitaciones2 + M2_por_Habitación + Terraza + Garaje + Sala_BBQ_terraza+ Sala_BBQ + Gimnasio+ Chimenea + Seguridad + Dist_Parques+Dist_Transp_Publico + Dist_Establecimientos
                 + Dist_C_Comerc, data = combined_train)
recp17 <- recipe(lPrecio ~ Estrato + Habitaciones + Habitaciones2 + M2_por_Habitación + Terraza + Garaje + Sala_BBQ_terraza+ Sala_BBQ + Gimnasio+ Chimenea + Seguridad + Dist_Parques+Dist_Transp_Publico + Dist_Establecimientos
                 + Dist_C_Comerc+ Dist_Centros_Educ, data = combined_train)
recp18 <- recipe(lPrecio ~ Estrato + Habitaciones + Habitaciones2 + M2_por_Habitación + Terraza + Garaje + Sala_BBQ_terraza+ Sala_BBQ + Gimnasio+ Chimenea + Seguridad + Dist_Parques+Dist_Transp_Publico + Dist_Establecimientos
                 + Dist_C_Comerc+ Dist_Centros_Educ + Dist_Restaurantes, data = combined_train)
recp19 <- recipe(lPrecio ~ Estrato + Habitaciones + Habitaciones2 + M2_por_Habitación + Terraza + Garaje + Sala_BBQ_terraza+ Sala_BBQ + Gimnasio+ Chimenea + Seguridad + Dist_Parques+Dist_Transp_Publico + Dist_Establecimientos
                 + Dist_C_Comerc+ Dist_Centros_Educ + Dist_Restaurantes+ Dist_Bancos, data = combined_train)%>%
  step_dummy(all_nominal_predictors())  #Convertir todas las variables categóricas a dummies 
lm_mod <- linear_reg() 

# Crear el flujo de trabajo
wf1 <- workflow() %>%
  add_recipe(recp1) %>%
  add_model(lm_mod)

# Crear el flujo de trabajo
wf2 <- workflow() %>%
  add_recipe(recp1) %>%
  add_model(lm_mod)

# Crear el flujo de trabajo
wf3 <- workflow() %>%
  add_recipe(recp3) %>%
  add_model(lm_mod)

# Crear el flujo de trabajo
wf4 <- workflow() %>%
  add_recipe(recp4) %>%
  add_model(lm_mod)

# Crear el flujo de trabajo
wf5 <- workflow() %>%
  add_recipe(recp5) %>%
  add_model(lm_mod)

# Crear el flujo de trabajo
wf6 <- workflow() %>%
  add_recipe(recp6) %>%
  add_model(lm_mod)

# Crear el flujo de trabajo
wf7 <- workflow() %>%
  add_recipe(recp7) %>%
  add_model(lm_mod)

# Crear el flujo de trabajo
wf8 <- workflow() %>%
  add_recipe(recp8) %>%
  add_model(lm_mod)

# Crear el flujo de trabajo
wf9 <- workflow() %>%
  add_recipe(recp9) %>%
  add_model(lm_mod)

# Crear el flujo de trabajo
wf10 <- workflow() %>%
  add_recipe(recp10) %>%
  add_model(lm_mod)

# Crear el flujo de trabajo
wf11 <- workflow() %>%
  add_recipe(recp11) %>%
  add_model(lm_mod)

# Crear el flujo de trabajo
wf12 <- workflow() %>%
  add_recipe(recp12) %>%
  add_model(lm_mod)


# Crear el flujo de trabajo
wf13 <- workflow() %>%
  add_recipe(recp13) %>%
  add_model(lm_mod)

# Crear el flujo de trabajo
wf14 <- workflow() %>%
  add_recipe(recp14) %>%
  add_model(lm_mod)

# Crear el flujo de trabajo
wf15 <- workflow() %>%
  add_recipe(recp15) %>%
  add_model(lm_mod)

# Crear el flujo de trabajo
wf16 <- workflow() %>%
  add_recipe(recp16) %>%
  add_model(lm_mod)
# Crear el flujo de trabajo
wf17 <- workflow() %>%
  add_recipe(recp17) %>%
  add_model(lm_mod)
# Crear el flujo de trabajo
wf18 <- workflow() %>%
  add_recipe(recp17) %>%
  add_model(lm_mod)
# Crear el flujo de trabajo
wf19 <- workflow() %>%
  add_recipe(recp17) %>%
  add_model(lm_mod)

################################ERROR CUADRATICO MEDIO##########################################################
folds <- vfold_cv(combined_train, v = 5)

# Ajusta el modelo en cada fold
fit_res2.1 <- fit_resamples(
  wf1,
  resamples = folds,
  metrics = metric_set(rmse)
)

# Obtener el RMSE para cada fold
individual_rmse2.1 <- fit_res2.1 %>% 
  unnest(.metrics) %>% 
  select(id, .metric, .estimate)

individual_rmse2.1
mean(individual_rmse2.1$.estimate)
individual_rmse2.1
# 2##########################Ajusta el modelo en cada fold
fit_res2.2 <- fit_resamples(
  wf2,
  resamples = folds,
  metrics = metric_set(rmse)
)

# Obtener el RMSE para cada fold
individual_rmse2.2 <- fit_res2.2 %>% 
  unnest(.metrics) %>% 
  select(id, .metric, .estimate)
mean(individual_rmse2.2$.estimate)
individual_rmse2.2
#####3########################
fit_res2.3 <- fit_resamples(
  wf3,
  resamples = folds,
  metrics = metric_set(rmse)
)

# Obtener el RMSE para cada fold
individual_rmse2.3 <- fit_res2.3 %>% 
  unnest(.metrics) %>% 
  select(id, .metric, .estimate)
mean(individual_rmse2.3$.estimate)
individual_rmse2.3
#44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444
fit_res2.4 <- fit_resamples(
  wf4,
  resamples = folds,
  metrics = metric_set(rmse)
)

# Obtener el RMSE para cada fold
individual_rmse2.4 <- fit_res2.4 %>% 
  unnest(.metrics) %>% 
  select(id, .metric, .estimate)
mean(individual_rmse2.4$.estimate)
individual_rmse2.4
#55555555555555555555555555555555555555555555555555555555555555555555555555555555
fit_res2.5 <- fit_resamples(
  wf5,
  resamples = folds,
  metrics = metric_set(rmse)
)

# Obtener el RMSE para cada fold
individual_rmse2.5 <- fit_res2.5 %>% 
  unnest(.metrics) %>% 
  select(id, .metric, .estimate)
mean(individual_rmse2.5$.estimate)
individual_rmse2.5
#6666666666666666666666666666666666666666666666666666666666666666666666666666666666666
fit_res2.6 <- fit_resamples(
  wf6,
  resamples = folds,
  metrics = metric_set(rmse)
)

# Obtener el RMSE para cada fold
individual_rmse2.6 <- fit_res2.6 %>% 
  unnest(.metrics) %>% 
  select(id, .metric, .estimate)
mean(individual_rmse2.6$.estimate)
individual_rmse2.6
#77777777777777777777777777777777777777777777777777777777777777777777777777777777777777777
fit_res2.7 <- fit_resamples(
  wf7,
  resamples = folds,
  metrics = metric_set(rmse)
)

# Obtener el RMSE para cada fold
individual_rmse2.7 <- fit_res2.7 %>% 
  unnest(.metrics) %>% 
  select(id, .metric, .estimate)
mean(individual_rmse2.7$.estimate)
individual_rmse2.7
#88888888888888888888888888888888888888888888888888888888888888888888888888888888888888
fit_res2.8 <- fit_resamples(
  wf8,
  resamples = folds,
  metrics = metric_set(rmse)
)

# Obtener el RMSE para cada fold
individual_rmse2.8 <- fit_res2.8 %>% 
  unnest(.metrics) %>% 
  select(id, .metric, .estimate)
mean(individual_rmse2.8$.estimate)
individual_rmse2.8
#999999999999999999999999999999999999999999999999999999999999999999999999999999999999999
fit_res2.9 <- fit_resamples(
  wf9,
  resamples = folds,
  metrics = metric_set(rmse)
)

# Obtener el RMSE para cada fold
individual_rmse2.9 <- fit_res2.9 %>% 
  unnest(.metrics) %>% 
  select(id, .metric, .estimate)
mean(individual_rmse2.9$.estimate)
individual_rmse2.9
#100000000000000000000000000000000000000000000000000000000000000000000000
fit_res2.10 <- fit_resamples(
  wf10,
  resamples = folds,
  metrics = metric_set(rmse)
)

# Obtener el RMSE para cada fold
individual_rmse2.10 <- fit_res2.10 %>% 
  unnest(.metrics) %>% 
  select(id, .metric, .estimate)
mean(individual_rmse2.10$.estimate)
individual_rmse2.10
#11111111111111111111111111111111111111111111111111111111111111111111111111
fit_res2.11 <- fit_resamples(
  wf11,
  resamples = folds,
  metrics = metric_set(rmse)
)

# Obtener el RMSE para cada fold
individual_rmse2.11 <- fit_res2.11 %>% 
  unnest(.metrics) %>% 
  select(id, .metric, .estimate)
mean(individual_rmse2.11$.estimate)
individual_rmse2.11 
#1222222222222222222222222222222222222222222222222222222222222222222222
fit_res2.12 <- fit_resamples(
  wf12,
  resamples = folds,
  metrics = metric_set(rmse)
)

# Obtener el RMSE para cada fold
individual_rmse2.12 <- fit_res2.12 %>% 
  unnest(.metrics) %>% 
  select(id, .metric, .estimate)
mean(individual_rmse2.12$.estimate)
individual_rmse2.12 
#199999999999999999999999999999999999999999999999999999999999999
fit_res2.19 <- fit_resamples(
  wf19,
  resamples = folds,
  metrics = metric_set(rmse)
)

# Obtener el RMSE para cada fold
individual_rmse2.19 <- fit_res2.19 %>% 
  unnest(.metrics) %>% 
  select(id, .metric, .estimate)
mean(individual_rmse2.19$.estimate)
individual_rmse2.19
#1444444444444444444444444444444444444444444444444444444444444444444
fit_res2.14 <- fit_resamples(
  wf14,
  resamples = folds,
  metrics = metric_set(rmse)
)

# Obtener el RMSE para cada fold
individual_rmse2.14 <- fit_res2.14 %>% 
  unnest(.metrics) %>% 
  select(id, .metric, .estimate)
mean(individual_rmse2.14$.estimate)
individual_rmse2.14
#1333333333333333333333333333333333333333333333333333333333333333333
fit_res2.13 <- fit_resamples(
  wf13,
  resamples = folds,
  metrics = metric_set(rmse)
)

# Obtener el RMSE para cada fold
individual_rmse2.13 <- fit_res2.13 %>% 
  unnest(.metrics) %>% 
  select(id, .metric, .estimate)
mean(individual_rmse2.13$.estimate)
individual_rmse2.13
#15555555555555555555555555555555555555555555555555555555555555555555
fit_res2.15 <- fit_resamples(
  wf15,
  resamples = folds,
  metrics = metric_set(rmse)
)

# Obtener el RMSE para cada fold
individual_rmse2.15 <- fit_res2.15 %>% 
  unnest(.metrics) %>% 
  select(id, .metric, .estimate)
mean(individual_rmse2.15$.estimate)
individual_rmse2.15
#1666666666666666666666666666666666666666666666666666666666666666666
fit_res2.16 <- fit_resamples(
  wf16,
  resamples = folds,
  metrics = metric_set(rmse)
)

# Obtener el RMSE para cada fold
individual_rmse2.16 <- fit_res2.16 %>% 
  unnest(.metrics) %>% 
  select(id, .metric, .estimate)
mean(individual_rmse2.16$.estimate)
individual_rmse2.16
#17777777777777777777777777777777777777777777777777777777777777777777
fit_res2.17 <- fit_resamples(
  wf17,
  resamples = folds,
  metrics = metric_set(rmse)
)

# Obtener el RMSE para cada fold
individual_rmse2.17 <- fit_res2.17 %>% 
  unnest(.metrics) %>% 
  select(id, .metric, .estimate)
mean(individual_rmse2.17$.estimate)
individual_rmse2.17
#1888888888888888888888888888888888888888888888888888888888888888888888888888888
fit_res2.18 <- fit_resamples(
  wf18,
  resamples = folds,
  metrics = metric_set(rmse)
)

# Obtener el RMSE para cada fold
individual_rmse2.18 <- fit_res2.18 %>% 
  unnest(.metrics) %>% 
  select(id, .metric, .estimate)
mean(individual_rmse2.18$.estimate)
individual_rmse2.18

##################################################################Lasooo y RIDGE
training_indices <- initial_split(combined_train, prop = 0.7)
training_data <- training_indices %>% training()
testing_data <- training_indices %>% testing()
train_aE4_fold <- vfold_cv(training_data, v = 5)

ridge_recipe <-
  recipe(formula = lPrecio ~ Habitaciones + Habitaciones2 + Baños + M2_por_Habitación + Terraza + Garaje + Sala_BBQ + Piscina + Gimnasio + Chimenea + Seguridad + Dist_Parques + Dist_Transp_Publico + Dist_Establecimientos
         + Dist_C_Comerc + Dist_Centros_Educ + Dist_Restaurantes + Dist_Bancos, data = training_data) %>%
  step_interact(terms = ~ M2_por_Habitación:Terraza + M2_por_Habitación:Garaje) %>%
  step_novel(all_nominal_predictors()) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_predictors()) %>%
  step_normalize(Habitaciones, Habitaciones2, Baños, M2_por_Habitación)

ridge_spec <- linear_reg(penalty = tune(), mixture = 0) %>%
  set_mode("regression") %>%
  set_engine("glmnet")


ridge_workflow <- workflow() %>%
  add_recipe(ridge_recipe) %>%
  add_model(ridge_spec)

# Definir una cuadrícula de valores de penalización utilizando 'grid_regular'
penalty_grid <- grid_regular(penalty(range = c(-4, 4)), levels = 100)
penalty_grid

# Realizar la búsqueda de hiperparámetros utilizando 'tune_grid'
tune_res <- tune_grid(
  ridge_workflow,
  resamples = train_aE4_fold,
  grid = penalty_grid,
  metrics = metric_set(rmse)
)

# gráfico de los resultados de hiperparámetros
autoplot(tune_res)
collect_metrics(tune_res)

best_penalty <- select_best(tune_res, metric = "rmse")
best_penalty

ridge_final <- finalize_workflow(ridge_workflow, best_penalty)
Ridge_a <- fit(ridge_final, data =training_data)
testing_data$Predict_rgd_a <- predict(Ridge_a, new_data = testing_data)

# Convertir la lista en un vector numérico
testing_data$Predict_rgd_a <- unlist(testing_data$Predict_rgd_a)

# Verificar y convertir los datos al formato correcto
testing_data$Predict_rgd_a <- as.numeric(testing_data$Predict_rgd_a)
testing_data$lPrecio <- as.numeric(testing_data$lPrecio)

# Calcular el MAE de manera manual
mae_value <- mean(abs(exp(testing_data$Predict_rgd_a) - exp(testing_data$lPrecio)), na.rm = TRUE)

# Imprimir el valor del MAE
cat("MAE en datos de prueba:", mae_value, "\n")
#######################################################TRAIN DATA-------------------------------------------------------------
ridge_recipe <-
  recipe(formula = lPrecio ~ Habitaciones + Habitaciones2 + Baños + M2_por_Habitación + Terraza + Garaje + Sala_BBQ + Piscina + Gimnasio + Chimenea + Seguridad + Dist_Parques + Dist_Transp_Publico + Dist_Establecimientos
         + Dist_C_Comerc + Dist_Centros_Educ + Dist_Restaurantes + Dist_Bancos, data = train_data1) %>%
  step_interact(terms = ~ M2_por_Habitación:Terraza + M2_por_Habitación:Garaje) %>%
  step_novel(all_nominal_predictors()) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_predictors()) %>%
  step_normalize(Habitaciones, Habitaciones2, Baños, M2_por_Habitación)

ridge_spec <- linear_reg(penalty = tune(), mixture = 0) %>%
  set_mode("regression") %>%
  set_engine("glmnet")


ridge_workflow <- workflow() %>%
  add_recipe(ridge_recipe) %>%
  add_model(ridge_spec)

# Definir una cuadrícula de valores de penalización utilizando 'grid_regular'
penalty_grid <- grid_regular(penalty(range = c(-4, 4)), levels = 100)
penalty_grid

# Realizar la búsqueda de hiperparámetros utilizando 'tune_grid'
tune_res <- tune_grid(
  ridge_workflow,
  resamples = train_aE4_fold,
  grid = penalty_grid,
  metrics = metric_set(rmse)
)

# gráfico de los resultados de hiperparámetros
autoplot(tune_res)
collect_metrics(tune_res)

best_penalty <- select_best(tune_res, metric = "rmse")
best_penalty

ridge_final <- finalize_workflow(ridge_workflow, best_penalty)
Ridge_a <- fit(ridge_final, data =train_data)
test_data1$Predict_rgd_a <- predict(Ridge_a, new_data = test_data1)

# Convertir la lista en un vector numérico
test_data1$Predict_rgd_a <- unlist(test_data1$Predict_rgd_a)

# Verificar y convertir los datos al formato correcto
test_data1$Predict_rgd_a <- as.numeric(test_data1$Predict_rgd_a)
test_data1$lPrecio <- as.numeric(test_data1$lPrecio)

# Calcular el MAE de manera manual
mae_value <- mean(abs(exp(test_data1$Predict_rgd_a) - exp(test_data1$lPrecio)), na.rm = TRUE)

# Imprimir el valor del MAE
cat("MAE en datos de prueba:", mae_value, "\n")


####################################### ARBOLES ###############################################################################
set.seed(123)
indice_entrenamiento <- sample(1:nrow(combined_train), 0.7 * nrow(combined_train))
datos_entrenamiento <- combined_train[indice_entrenamiento, ]
datos_prueba <- combined_train[-indice_entrenamiento, ]
k <- 5  # Número de pliegues
folds <- createFolds(datos_entrenamiento$lPrecio, k = k, list = TRUE, returnTrain = TRUE)
fitControl <- trainControl(method = "cv", number = 5)

# Inicializa un vector para almacenar los MAE de cada pliegue
mae_values <- numeric(k)

for (i in 1:k) {
  # Selecciona el i-ésimo pliegue como conjunto de prueba y los demás como entrenamiento
  train_indices <- unlist(folds[i])
  test_indices <- setdiff(1:nrow(datos_entrenamiento), train_indices)
  
  train_data <- datos_entrenamiento[train_indices, ]
  test_data <- datos_entrenamiento[test_indices, ]
  
  # Ajusta el modelo en el conjunto de entrenamiento actual
  model <- train(
    lPrecio ~ Estrato + Habitaciones + Habitaciones2 + Baños + M2_por_Habitación + Terraza + Garaje + Sala_BBQ + Gimnasio + Sala_BBQ_terraza + Chimenea + Seguridad + Dist_Parques + 
      Dist_Transp_Publico + Dist_Establecimientos + Dist_C_Comerc + Dist_Centros_Educ + Dist_Restaurantes + Dist_Bancos, 
    data = train_data,
    method = "ranger",
    trControl = fitControl,
    tuneGrid = expand.grid(
      mtry = c(1,2,3),
      splitrule = "variance",
      min.node.size = c(5,10,15))
  )
  
  # Realiza predicciones en el conjunto de prueba actual
  predictions <- predict(model, newdata = test_data)
  
  # Calcula el MAE para este pliegue
  mae_values[i] <- mean(abs(exp(predictions) - exp(test_data$lPrecio)))
}

# Calcula el MAE promedio de todos los pliegues
average_mae <- mean(mae_values)

# Muestra el MAE promedio
print(paste("MAE promedio en validación cruzada:", average_mae))

###########################################ARBOLES 2##########################################################
# Ajusta el modelo en el conjunto de entrenamiento
tree_ranger_grid <- train(
  lPrecio ~ Estrato + Habitaciones + Habitaciones2 + Baños + M2_por_Habitación + Terraza + Garaje + Sala_BBQ + Gimnasio + Sala_BBQ_terraza + Chimenea + Seguridad + Dist_Parques + 
    Dist_Transp_Publico + Dist_Establecimientos + Dist_C_Comerc + Dist_Centros_Educ + Dist_Restaurantes + Dist_Bancos, 
  data = train_data1,
  method = "ranger",
  trControl = fitControl,
  tuneGrid = expand.grid(
    mtry = c(1,2,3),
    splitrule = "variance",
    min.node.size = c(5,10,15))
)

# Realiza predicciones en el conjunto de prueba
predicciones_fuera_de_muestra <- predict(tree_ranger_grid, newdata = test_data1)

# Evalúa el rendimiento del modelo
mae <- mean(abs(exp(predicciones_fuera_de_muestra) - exp(test_data$lPrecio)))
print(paste("MAE en datos de prueba:", mae))
###########################################LASSO##########################################################
lasso_recipe <-
  recipe(formula =lPrecio ~ Habitaciones + Habitaciones2 + Baños + M2_por_Habitación + Terraza + Garaje + Sala_BBQ + Piscina + Gimnasio + Chimenea + Seguridad + Dist_Parques + Dist_Transp_Publico + Dist_Establecimientos
         + Dist_C_Comerc + Dist_Centros_Educ + Dist_Restaurantes + Dist_Bancos, data = train_data) %>%
  step_interact(terms = ~ M2_por_Habitación:Terraza + M2_por_Habitación:Garaje) %>%
  step_novel(all_nominal_predictors()) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_predictors()) %>%
  step_normalize(Habitaciones, Habitaciones2, Baños, M2_por_Habitación)

lasso_spec <-
  linear_reg(penalty = tune(), mixture = 1) %>%
  set_mode("regression") %>%
  set_engine("glmnet")

lasso_workflow <- workflow() %>%
  add_recipe(lasso_recipe) %>%
  add_model(lasso_spec)

# Generar una cuadrícula de valores de penalización
penalty_grid <- grid_regular(penalty(range = c(-2, 2)), levels = 50)

# Hiperparámetros utilizando tune_grid
tune_res_la <- tune_grid(lasso_workflow, resamples = folds, grid = penalty_grid, metrics = metric_set(rmse))
autoplot(tune_res_la)

best_penalty_la <- select_best(tune_res_la, metric = "rmse")
lasso_final_a <- finalize_workflow(lasso_workflow, best_penalty_la)
Lasso_a <- fit(lasso_final_a, data = train_data)

test_data$Predict_ls_a <- predict(Lasso_a, new_data = test_data)

# Verificar y convertir los datos al formato correcto
test_data$Predict_ls_a <- as.numeric(unlist(test_data$Predict_ls_a))
test_data$lPrecio <- as.numeric(test_data$lPrecio)

# Calcular el MAE
mae <- mean(abs(test_data$Predict_ls_a - test_data$lPrecio), na.rm = TRUE)

# Mostrar el resultado
cat("Mean Absolute Error (MAE):", mae, "\n")

###########################################LASSO##########################################################
set.seed(123)

train_fold <- vfold_cv(train_data, v = 5)

lasso_recipe <-
  recipe(formula =lPrecio ~ Habitaciones + Habitaciones2 + Baños + M2_por_Habitación + Terraza + Garaje + Sala_BBQ + Piscina + Gimnasio + Chimenea + Seguridad + Dist_Parques + Dist_Transp_Publico + Dist_Establecimientos
         + Dist_C_Comerc + Dist_Centros_Educ + Dist_Restaurantes + Dist_Bancos, data = train_data) %>%
  step_interact(terms = ~ M2_por_Habitación:Terraza + M2_por_Habitación:Garaje) %>%
  step_novel(all_nominal_predictors()) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_predictors()) %>%
  step_normalize(Habitaciones, Habitaciones2, Baños, M2_por_Habitación)

lasso_spec <-
  linear_reg(penalty = tune(), mixture = 1) %>%
  set_mode("regression") %>%
  set_engine("glmnet")

lasso_workflow <- workflow() %>%
  add_recipe(lasso_recipe) %>%
  add_model(lasso_spec)

# Generar una cuadrícula de valores de penalización
penalty_grid <- grid_regular(penalty(range = c(-2, 2)), levels = 50)

# Hiperparámetros utilizando tune_grid
tune_res_la <- tune_grid(lasso_workflow,resamples = train_fold, grid = penalty_grid,metrics = metric_set(rmse))
autoplot(tune_res_la)

best_penalty_la <- select_best(tune_res_la, metric = "rmse")
lasso_final_a <- finalize_workflow(lasso_workflow, best_penalty_la)
Lasso_a <- fit(lasso_final_a, data = train_data)
test_data$Predict_ls_a <- predict(Lasso_a, new_data = test_data)

# Convertir la lista en un vector numérico
test_data$Predict_ls_a <- unlist(test_data$Predict_ls_a)

# Verificar y convertir los datos al formato correcto
test_data$Predict_ls_a <- as.numeric(test_data$Predict_ls_a)
test_data$lPrecio <- as.numeric(test_data$lPrecio)

# Calcular el MAE de manera manual
mae_value <- mean(abs(exp(test_data$Predict_ls_a) - exp(test_data$lPrecio)), na.rm = TRUE)

# Imprimir el valor del MAE
cat("MAE en datos de prueba:", mae_value, "\n")

###########################################LASSO 2##########################################################
set.seed(123)

train_fold2 <- vfold_cv(training_data, v = 5)

lasso_recipe <-
  recipe(formula =lPrecio ~ Habitaciones + Habitaciones2 + Baños + M2_por_Habitación + Terraza + Garaje + Sala_BBQ + Piscina + Gimnasio + Chimenea + Seguridad + Dist_Parques + Dist_Transp_Publico + Dist_Establecimientos
         + Dist_C_Comerc + Dist_Centros_Educ + Dist_Restaurantes + Dist_Bancos, data = training_data) %>%
  step_interact(terms = ~ M2_por_Habitación:Terraza + M2_por_Habitación:Garaje) %>%
  step_novel(all_nominal_predictors()) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_predictors()) %>%
  step_normalize(Habitaciones, Habitaciones2, Baños, M2_por_Habitación)

lasso_spec <-
  linear_reg(penalty = tune(), mixture = 1) %>%
  set_mode("regression") %>%
  set_engine("glmnet")

lasso_workflow <- workflow() %>%
  add_recipe(lasso_recipe) %>%
  add_model(lasso_spec)

# Generar una cuadrícula de valores de penalización
penalty_grid <- grid_regular(penalty(range = c(-2, 2)), levels = 50)

# Hiperparámetros utilizando tune_grid
tune_res_la <- tune_grid(lasso_workflow,resamples = train_fold2, grid = penalty_grid,metrics = metric_set(rmse))
autoplot(tune_res_la)

best_penalty_la <- select_best(tune_res_la, metric = "rmse")
lasso_final_a <- finalize_workflow(lasso_workflow, best_penalty_la)
Lasso_a <- fit(lasso_final_a, data = training_data)
testing_data$Predict_ls_a <- predict(Lasso_a, new_data = testing_data)

# Convertir la lista en un vector numérico
testing_data$Predict_ls_a <- unlist(testing_data$Predict_ls_a)

# Verificar y convertir los datos al formato correcto
testing_data$Predict_ls_a <- as.numeric(testing_data$Predict_ls_a)
testing_data$lPrecio <- as.numeric(testing_data$lPrecio)

# Calcular el MAE de manera manual
mae_value2 <- mean(abs(exp(testing_data$Predict_ls_a) - exp(testing_data$lPrecio)), na.rm = TRUE)

# Imprimir el valor del MAE
cat("MAE en datos de prueba:", mae_value2, "\n")

###########################################################################################################################
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
model <- train(
  lPrecio ~  Habitaciones + Habitaciones2 + Baños + M2_por_Habitacion + Terraza + Garaje + Sala_BBQ + Gimnasio + Sala_BBQ_terraza + Chimenea + Seguridad + Dist_Parques + 
    Dist_Transp_Publico + Dist_Establecimientos + Dist_C_Comerc + Dist_Centros_Educ + Dist_Restaurantes + Dist_Bancos, 
  data = train_data4a,
  method = "ranger",
  trControl = fitControl,
  tuneGrid = expand.grid(
    mtry = c(1, 2, 3),
    splitrule = "variance",
    min.node.size = c(5, 10, 15)
  )
)

# Realiza predicciones en el conjunto de prueba
test_data4a$predictions <-predict(model, newdata = test_data4a)

# Calcular el MAE de manera manual
mae_value2 <- mean(abs(exp(test_data4a$predictions) - exp(test_data4a$lPrecio)), na.rm = TRUE)

# Imprimir el valor del MAE
cat("MAE en datos de prueba:", mae_value2, "\n")

modelc <- train(
  lPrecio ~ Habitaciones + Habitaciones2 + Baños + M2_por_Habitacion + Terraza + Garaje + Sala_BBQ + Gimnasio + Sala_BBQ_terraza + Chimenea + Seguridad + Dist_Parques + 
    Dist_Transp_Publico + Dist_Establecimientos + Dist_C_Comerc + Dist_Centros_Educ + Dist_Restaurantes + Dist_Bancos, 
  data = train_data4c,
  method = "ranger",
  trControl = fitControl,
  tuneGrid = expand.grid(
    mtry = c(1, 2, 3),
    splitrule = "variance",
    min.node.size = c(5, 10, 15)
  )
)
# Realiza predicciones en el conjunto de prueba
test_data4c$predictions <-predict(modelc, newdata = test_data4c)
mae_value2 <- mean(abs(exp(test_data4c$predictions) - exp(test_data4c$lPrecio)), na.rm = TRUE)
test_datat<-rbind(test_data4a, test_data4c)
mae_valuetotal<- mean(abs(exp(test_datat$predictions) - exp(test_datat$lPrecio)), na.rm = TRUE)
cat("MAE en datos de prueba:", mae_valuetotal, "\n")



set.seed(123)
tree_rpart2 <- train(
  lPrecio ~Habitaciones + Habitaciones2 + Baños + M2_por_Habitacion + Terraza + Garaje + Sala_BBQ + Gimnasio + Sala_BBQ_terraza + Chimenea + Seguridad + Dist_Parques + 
    Dist_Transp_Publico + Dist_Establecimientos + Dist_C_Comerc + Dist_Centros_Educ + Dist_Restaurantes + Dist_Bancos, 
  data = train_data4a,
  method = "rpart2",
  trControl = fitControl,
  tuneGrid = expand.grid(maxdepth = seq(1,30,1))
)
tree_rpart2


# Realiza predicciones en el conjunto de prueba
test_data4a$predictions1 <-predict(tree_rpart2, newdata = test_data4a)

# Calcular el MAE de manera manual
mae_value2 <- mean(abs(exp(test_data4a$predictions1) - exp(test_data4a$lPrecio)), na.rm = TRUE)

# Imprimir el valor del MAE
cat("MAE en datos de prueba:", mae_value2, "\n")

tree_rpart2c <- train(
  lPrecio ~ Habitaciones + Habitaciones2 + Baños + M2_por_Habitacion + Terraza + Garaje + Sala_BBQ + Gimnasio + Sala_BBQ_terraza + Chimenea + Seguridad + Dist_Parques + 
    Dist_Transp_Publico + Dist_Establecimientos + Dist_C_Comerc + Dist_Centros_Educ + Dist_Restaurantes + Dist_Bancos, 
  data = train_data4c,
  method = "rpart2",
  trControl = fitControl,
  tuneGrid = expand.grid(maxdepth = seq(1,30,1))
)
tree_rpart2c

# Realiza predicciones en el conjunto de prueba
test_data4c$predictions1 <-predict(tree_rpart2c, newdata = test_data4c)
mae_value2 <- mean(abs(exp(test_data4c$predictions1) - exp(test_data4c$lPrecio)), na.rm = TRUE)
test_datat1<-rbind(test_data4a, test_data4c)
mae_valuetotal1<- mean(abs(exp(test_datat1$predictions1) - exp(test_datat$lPrecio)), na.rm = TRUE)
cat("MAE en datos de prueba:", mae_valuetotal1, "\n")



set.seed(123)
tree_lenghta <- train(
  lPrecio ~  Habitaciones + Habitaciones2 + Baños + M2_por_Habitacion + Terraza + Garaje + Sala_BBQ + Gimnasio + Sala_BBQ_terraza + Chimenea + Seguridad + Dist_Parques + 
    Dist_Transp_Publico + Dist_Establecimientos + Dist_C_Comerc + Dist_Centros_Educ + Dist_Restaurantes + Dist_Bancos, 
  data = train_data4a,
  method = "rpart",
  metric="MAE",
  trControl = fitControl,
  tuneLength=100,

)

tree_lenghtc <- train(
  lPrecio ~  Habitaciones + Habitaciones2 + Baños + M2_por_Habitacion + Terraza + Garaje + Sala_BBQ + Gimnasio + Sala_BBQ_terraza + Chimenea + Seguridad + Dist_Parques + 
    Dist_Transp_Publico + Dist_Establecimientos + Dist_C_Comerc + Dist_Centros_Educ + Dist_Restaurantes + Dist_Bancos, 
  data = train_data4c,
  method = "rpart",
  metric="MAE",
  trControl = fitControl,
  tuneLength=100,
)


# Realiza predicciones en el conjunto de prueba
test_data4a$predictions2 <-predict(tree_lenghta, newdata = test_data4a)

# Calcular el MAE de manera manual
mae_value2 <- mean(abs(exp(test_data4a$predictions2) - exp(test_data4a$lPrecio)), na.rm = TRUE)

test_data4c$predictions2 <-predict(tree_lenghtc , newdata = test_data4c)
mae_value2 <- mean(abs(exp(test_data4c$predictions2) - exp(test_data4c$lPrecio)), na.rm = TRUE)
test_datat1<-rbind(test_data4a, test_data4c)
mae_valuetotal1<- mean(abs(exp(test_datat1$predictions2) - exp(test_datat$lPrecio)), na.rm = TRUE)
cat("MAE en datos de prueba:", mae_valuetotal1, "\n")


set.seed(123)
tree_rpart2_roba <- train(
  lPrecio ~ Habitaciones + Habitaciones2 + Baños + M2_por_Habitacion + Terraza + Garaje + Sala_BBQ + Gimnasio + Sala_BBQ_terraza + Chimenea + Seguridad + Dist_Parques + 
    Dist_Transp_Publico + Dist_Establecimientos + Dist_C_Comerc + Dist_Centros_Educ + Dist_Restaurantes + Dist_Bancos, 
  data = train_data4a[-c(1:20),],
  method = "rpart2",
  trControl = fitControl,
  tuneGrid = expand.grid(maxdepth = seq(1,30,1))
)

set.seed(123)
tree_rpart2_robc <- train(
  lPrecio ~ Habitaciones + Habitaciones2 + Baños + M2_por_Habitacion + Terraza + Garaje + Sala_BBQ + Gimnasio + Sala_BBQ_terraza + Chimenea + Seguridad + Dist_Parques + 
    Dist_Transp_Publico + Dist_Establecimientos + Dist_C_Comerc + Dist_Centros_Educ + Dist_Restaurantes + Dist_Bancos, 
  data = train_data4c[-c(1:20),],
  method = "rpart2",
  trControl = fitControl,
  tuneGrid = expand.grid(maxdepth = seq(1,30,1))
)

# Realiza predicciones en el conjunto de prueba
test_data4a$predictions3 <-predict(tree_rpart2_roba, newdata = test_data4a)

# Calcular el MAE de manera manual
mae_value2 <- mean(abs(exp(test_data4a$predictions2) - exp(test_data4a$lPrecio)), na.rm = TRUE)

test_data4c$predictions3 <-predict(tree_rpart2_robc , newdata = test_data4c)
mae_value2 <- mean(abs(exp(test_data4c$predictions3) - exp(test_data4c$lPrecio)), na.rm = TRUE)
test_datat1<-rbind(test_data4a, test_data4c)
mae_valuetotal1<- mean(abs(exp(test_datat1$predictions3) - exp(test_datat$lPrecio)), na.rm = TRUE)
cat("MAE en datos de prueba:", mae_valuetotal1, "\n")


#install.packages("xgboost")
library(xgboost)


# Lista de nombres de variables que deseas incluir en el modelo
variables_incluidas <- c("Habitaciones","Habitaciones2", "Baños", "Area","M2_por_Habitacion", "lat", "lon", "Terraza",
                         "Garaje", "Sala_BBQ","Piscina","Gimnasio", "Chimenea","Seguridad",
                         "Dist_Parques", "Dist_Transp_Publico", "Dist_Establecimientos",
                         "Dist_C_Comerc", "Dist_Centros_Educ", "Dist_Restaurantes", "Dist_Bancos", "Sala_BBQ_terraza")


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
  nrounds = 100,
  max_depth = 10,
  eta = 0.3,
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
  eta = 0.5,
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
mae_value4c <- mean(abs(exp(test_data4c$predictions4) - exp(test_data4c$lPrecio)), na.rm = TRUE)

# Realiza predicciones en el conjunto de prueba para apartamentos
test_data4c$predictions4 <- predict(model_xgboost_subsetc, newdata = as.matrix(test_data4c_subset[, -1]))

# Calcular el MAE de manera manual para apartamentos
mae_value4a <- mean(abs(exp(test_data4a$predictions4) - exp(test_data4a$lPrecio)), na.rm = TRUE)

# Combinar los resultados de casas y apartamentos
test_data_combined <- rbind(test_data4c, test_data4a)
mae_value_total <- mean(abs(exp(test_data_combined$predictions4) - exp(test_data_combined$lPrecio)), na.rm = TRUE)

cat("MAE en datos de prueba para casas:", mae_value4c, "\n")
cat("MAE en datos de prueba para apartamentos:", mae_value4a, "\n")
cat("MAE total en datos de prueba:", mae_value_total, "\n")


####-------------------------------apartamentos------------------------------####
set.seed(123)
# Ajusta el modelo en el conjunto de entrenamiento con variables seleccionadas
model_xgboost_subseta <- xgboost(
  data = as.matrix(train_apart4[, -1]),  # Excluye la columna de lPrecio
  label = train_apart4$lPrecio,
  booster = "gbtree",
  nrounds = 100,
  max_depth = 10,
  eta = 0.3,
  objective = "reg:linear",
  eval_metric = "mae"
)

# Realiza predicciones en el conjunto de prueba con variables seleccionadas
predictions_xgboost_subsetc <- predict(model_xgboost_subsetc, as.matrix(test_data4c_subset[, -1]))


####-------------------------------Casas------------------------------####
# Ajusta el modelo en el conjunto de entrenamiento con variables seleccionadas
model_xgboost_subsetc <- xgboost(
  data = as.matrix(train_casas4[, -1]),  # Excluye la columna de lPrecio
  label = train_casas4$lPrecio,
  booster = "gbtree",
  nrounds = 200,
  max_depth = 14,
  eta = 0.5,
  objective = "reg:linear",
  eval_metric = "mae"
)




##################################################################
arbol1c <- data.frame(test_casas1$property_id, exp(predict(model_xgboost_subsetc, newdata = test_casas1)))
colnames(arbol1) <- c("property_id", "price")
arbol1p <- data.frame(test_apart1$property_id, exp(predict(model_xgboost_subseta, newdata = test_apart1)))
colnames(Pred_apart_arb7) <- c("property_id", "price")
Pred_arbol_fm7<- rbind(arbol1c, arbol1p)



# Define las variables comunes
variables_comunes <- c("Habitaciones", "Habitaciones2", "Baños", "Area", "M2_por_Habitacion", "lat", "lon", "Terraza",
                       "Garaje", "Sala_BBQ", "Piscina", "Gimnasio", "Chimenea", "Seguridad", "Dist_Parques",
                       "Dist_Transp_Publico", "Dist_Establecimientos", "Dist_C_Comerc", "Dist_Centros_Educ", 
                       "Dist_Restaurantes", "Dist_Bancos", "Sala_BBQ_terraza")

# Crear un objeto xgb.DMatrix para los datos de prueba de casas con variables comunes
dtest_casas <- xgb.DMatrix(data = as.matrix(test_casas1[, c("lPrecio", variables_comunes)]))

# Realizar predicciones para casas
predictions_casas <- predict(model_xgboost_subsetc, newdata = dtest_casas)

# Luego, puedes combinar las predicciones con el ID de la propiedad
arbol1c <- data.frame(property_id = test_casas1$property_id, price = exp(predictions_casas))
