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
install.packages("stringr")
install.packages("zoo")
install.packages("gridExtra")
install.packages("tidyverse")
install.packages("rvest")
install.packages("sf")
install.packages("glmnet")
install.packages("broom")
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
library(stringr)

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

submission_template <- read.xlsx("https://github.com/chernan77/Data_Taller2/raw/main/submission_template.xlsx")
train <- read.xlsx("https://github.com/chernan77/Data_Taller2/raw/main/train.xlsx")
test <- read.xlsx("https://github.com/chernan77/Data_Taller2/raw/main/test.xlsx")
estrato <- read.xlsx("https://github.com/chernan77/Data_Taller2/raw/main/estrato.xlsx")

train %>%
  summarise_all(~sum(is.na(.))) %>% transpose()

train <- train %>%
  mutate(surface_total = ifelse(is.na(surface_total), surface_covered, surface_total))

train %>%
  summarise_all(~sum(is.na(.))) %>% transpose()

# Expresión regular para encontrar valores numéricos seguidos de patrones
patron <- "\\d+(\\.\\d+)?\\s*(mts|mts²|m²|m2|metros cuadrados|metro cuadrado)"

# Extraer valores numéricos y convertirlos a valores numéricos
train$metros_cuadrados <- sapply(str_extract_all(train$description, patron), function(x) {
  if (length(x) > 0) {
    numero <- as.numeric(gsub("[^0-9.]", "", x[1]))
    if (!is.na(numero) && nchar(numero) >= 4 && substr(numero, nchar(numero), nchar(numero)) == "2") {
      numero <- substr(numero, 1, nchar(numero) - 1)
    }
    numero
  } else {
    NA
  }
})

train <- train %>%
  mutate(surface_total = ifelse(is.na(surface_total), metros_cuadrados, surface_total))

train$rooms <- as.numeric(train$rooms)
train$bedrooms <- as.numeric(train$bedrooms)
train$bathrooms <- as.numeric(train$bathrooms)
train$surface_total <- as.numeric(train$surface_total)
train$surface_covered <- as.numeric(train$surface_covered)
train$nueva_surface <- as.numeric(train$nueva_surface)


train$bedrooms <- ifelse(train$bedrooms == 0, train$rooms, train$bedrooms)
mediana_bedrooms <- median(train$bedrooms, na.rm = TRUE)
train$bedrooms <- ifelse(is.na(train$bedrooms), mediana_bedrooms, train$bedrooms)

train <- train %>%
  mutate(metros_cuadrados_bedrooms = surface_total / bedrooms)
summary(train$metros_cuadrados_bedrooms)

train$metros_cuadrados_bedrooms <- as.numeric(train$metros_cuadrados_bedrooms)
train$metros_cuadrados <- as.numeric(train$metros_cuadrados)

##-------------------------------------Casas---------------------------------##
promedio_metros_casas <- train %>%
  filter(property_type == "Casa", bedrooms > 0) %>%
  group_by(bedrooms) %>%
  filter(metros_cuadrados_bedrooms <= quantile(metros_cuadrados_bedrooms, 0.95, na.rm = TRUE)) %>%
  summarize(promedio = mean(metros_cuadrados_bedrooms, na.rm = TRUE))

promedio_metros_casas

# Accede a la media directamente desde el marco de datos resultante
media_metros_cuadrados_bedrooms <- promedio_metros_casas$promedio

# Calculo de la Media y Varianza Casas
media_m2_bedrooms <- c(140, 77.4, 63.6, 55.3, 49.2, 41, 37.2, 28.6, 26.5, 30.5, 19.8)
media_m2_bedrooms <- c(77.4, 63.6, 55.3, 49.2, 41, 37.2, 28.6, 26.5, 30.5, 19.8)

media_m2_bedrooms_casas <- mean(media_m2_bedrooms)
desv_media_m2_bedrooms_casas <- sd(media_m2_bedrooms)
max_media_m2_bedrooms_casas = media_m2_bedrooms_casas+2*desv_media_m2_bedrooms_casas
min_media_m2_bedrooms_casas = media_m2_bedrooms_casas-2*desv_media_m2_bedrooms_casas

train$metros_cuadrados_bedrooms[train$property_type == "Casa" & train$metros_cuadrados_bedrooms < min_media_m2_bedrooms_casas] <- min_media_m2_bedrooms_casas
train$metros_cuadrados_bedrooms[train$property_type == "Casa" & train$metros_cuadrados_bedrooms > max_media_m2_bedrooms_casas] <- max_media_m2_bedrooms_casas

train$surface_total <- ifelse(train$property_type == "Casa" & (train$surface_total/train$bedrooms) < min_media_m2_bedrooms_casas,
                                train$metros_cuadrados_bedrooms * train$bedrooms,
                                train$surface_total)
train$surface_total <- ifelse(train$property_type == "Casa" & (train$surface_total/train$bedrooms)  > max_media_m2_bedrooms_casas,
                                train$metros_cuadrados_bedrooms * train$bedrooms,
                                train$surface_total)

promedio_metros_casas1 <- train %>%
  filter(property_type == "Casa", !is.na(metros_cuadrados_bedrooms)) %>%
  group_by(bedrooms) %>%
  summarize(promedio = mean(metros_cuadrados_bedrooms, na.rm = TRUE))


train <- train %>%
  left_join(promedio_metros_casas1, by = "bedrooms")

train$metros_cuadrados_bedrooms <- ifelse(is.na(train$metros_cuadrados_bedrooms) & train$property_type == "Casa",
                                          train$promedio, train$metros_cuadrados_bedrooms)

train <- train %>% select(-promedio)

##-------------------------------------Apartamentos---------------------------------##

promedio_metros_apart <- train %>%
  filter(property_type == "Apartamento", bedrooms > 0) %>%
  group_by(bedrooms) %>%
  filter(metros_cuadrados_bedrooms <= quantile(metros_cuadrados_bedrooms, 0.95, na.rm = TRUE)) %>%
  summarize(promedio = mean(metros_cuadrados_bedrooms, na.rm = TRUE))

promedio_metros_apart

# Accede a la media directamente desde el marco de datos resultante
media_metros_cuadrados_bedrooms <- promedio_metros_casas$promedio

# Calculo de la Media y Varianza Casas
media_m2_bedrooms_a <- c(200, 108, 86, 113, 537, 31, 34, 29.8, 19.6, 20.4)
media_m2_bedrooms_a <- c(108, 86, 113, 31, 34, 29.8, 19.6, 20.4)

media_m2_bedrooms_apart <- mean(media_m2_bedrooms_a)
desv_media_m2_bedrooms_apart <- sd(media_m2_bedrooms_a)
max_media_m2_bedrooms_apart = media_m2_bedrooms_apart+1*desv_media_m2_bedrooms_apart
min_media_m2_bedrooms_apart = media_m2_bedrooms_apart-1*desv_media_m2_bedrooms_apart

train$metros_cuadrados_bedrooms[train$property_type == "Apartamento" & train$metros_cuadrados_bedrooms < min_media_m2_bedrooms_apart] <- min_media_m2_bedrooms_apart
train$metros_cuadrados_bedrooms[train$property_type == "Apartamento" & train$metros_cuadrados_bedrooms > max_media_m2_bedrooms_apart] <- max_media_m2_bedrooms_apart

train$surface_total <- ifelse(train$property_type == "Apartamento" & (train$surface_total/train$bedrooms) < min_media_m2_bedrooms_apart,
                              train$metros_cuadrados_bedrooms * train$bedrooms,
                              train$surface_total)
train$surface_total <- ifelse(train$property_type == "Apartamento" & (train$surface_total/train$bedrooms)  > max_media_m2_bedrooms_apart,
                              train$metros_cuadrados_bedrooms * train$bedrooms,
                              train$surface_total)


promedio_metros_apart1 <- train %>%
  filter(property_type == "Apartamento", !is.na(metros_cuadrados_bedrooms)) %>%
  group_by(bedrooms) %>%
  summarize(promedio = mean(metros_cuadrados_bedrooms, na.rm = TRUE))


train <- train %>%
  left_join(promedio_metros_apart1, by = "bedrooms")

train$metros_cuadrados_bedrooms <- ifelse(is.na(train$metros_cuadrados_bedrooms) & train$property_type == "Apartamento",
                                          train$promedio, train$metros_cuadrados_bedrooms)

train <- train %>% select(-promedio)

train <- train %>%
  mutate(surface_total = ifelse(is.na(surface_total), metros_cuadrados_bedrooms*bedrooms, surface_total))

train$nueva_surface <- train$surface_total

ggplot(train, aes(x = nueva_surface)) +
  geom_histogram(bins = 30, fill = "blue", color = "black", alpha = 0.7) +
  scale_y_log10()


Tabla_train <- "C:/Output R/Taller 2/Taller_2/Tabla_train.xlsx"  
write_xlsx(train, Tabla_train)

