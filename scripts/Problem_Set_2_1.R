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
       dplyr) #para modelos de ML

submission_template <- read.xlsx("https://github.com/chernan77/Data_Taller2/raw/main/submission_template.xlsx")
train <- read.xlsx("https://github.com/chernan77/Data_Taller2/raw/main/train.xlsx")
test <- read.xlsx("https://github.com/chernan77/Data_Taller2/raw/main/test.xlsx")
estrato <- read.xlsx("https://github.com/chernan77/Data_Taller2/raw/main/estrato.xlsx")


train %>%
  summarise_all(~sum(is.na(.))) %>% transpose()

# -----------------------DUPERACIÓN DE DATOS------------------------------------# 

#Imputacion del precio
any(is.na(train$price))
train<- subset(train, price > 9)

#Encontremos la moda de número de habitaciones 3, cuartos y número de baños
train %>%
  count(rooms) %>% head() 

### Analisis descriptivo de rooms 3
train %>%
  count(rooms) %>% head() 
train %>%
  count(bedrooms)

### Analisis descriptivo de rooms bathrooms 2

train %>%
  count(bathrooms)

##### Imputar la moda para bedrooms y bathrooms
train <- train %>%
  mutate(rooms = replace_na(rooms, 3),
         bathrooms = replace_na(bathrooms, 2),)

train$bedrooms <- ifelse(train$bedrooms == 0, train$rooms, train$bedrooms)
mediana_bedrooms <- median(train$bedrooms, na.rm = TRUE)
train$bedrooms <- ifelse(is.na(train$bedrooms), mediana_bedrooms, train$bedrooms)

train <- train %>%
  mutate(surface_total = ifelse(is.na(surface_total), surface_covered, surface_total))

train %>%
  summarise_all(~sum(is.na(.))) %>% transpose()

# Primero normalizaremos todo el texto
train <- train %>%
  mutate(description = str_to_lower(description))
# Eliminacion de tildes
train <- train %>%
  mutate(description = iconv(description, from = "UTF-8", to = "ASCII//TRANSLIT"))
# Eliminamos caracteres especiales
train <- train %>%
  mutate(description = str_replace_all(description, "[^[:alnum:]]", " "))
# Eliminamos espacios extras
train <- train %>%
  mutate(description = str_trim(gsub("\\s+", " ", description)))

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

# Crear un gráfico de dispersión (scatter plot) para la correlación entre precio y metros cuadrados
ggplot(train, aes(x = nueva_surface, y = price)) +
  geom_point() +
  labs(x = "Metros Cuadrados", y = "Precio") +
  ggtitle("Correlación entre Precio y Metros Cuadrados")

windows(width = 10, height = 6) # Para Windows
# Crear un gráfico de dispersión (scatter plot) para la correlación entre precio y metros cuadrados
plot(train$nueva_surface, train$price, 
     xlab = "Metros Cuadrados",
     ylab = "Precio",
     main = "Correlación entre Precio y Metros Cuadrados")
# Crear un gráfico de dispersión con una línea de regresión
ggplot(train, aes(x = nueva_surface, y = price)) +
  geom_point() +               # Puntos de datos
  geom_smooth(method = "lm",   # Línea de regresión lineal
              se = FALSE,      # No mostrar intervalo de confianza
              color = "blue") + # Color de la línea
  labs(x = "Metros Cuadrados", y = "Precio") +
  ggtitle("Correlación entre Precio y Metros Cuadrados")


# Remover los puntos y corregir la ubicación del decimal utilizando expresiones regulares
train$lon <- as.character(train$lon)
train$lon <- gsub("\\.", "", train$lon)  # Eliminar puntos
train$lon <- sub("^(.{3})(.*)$", "\\1.\\2", train$lon)  # Corregir ubicación del decimal

# Convertir la columna lon a numérico decimal
train$lon <- as.numeric(train$lon)

# Redondear los valores
train$lon <- round(train$lon, digits = 9)

# Remover los puntos y corregir la ubicación del decimal utilizando expresiones regulares
# Crear una copia de la variable lat
train$lat_valid <- train$lat

# Remover los puntos y corregir la ubicación del decimal
train$lat <- as.character(train$lat)
train$lat <- gsub("\\.", "", train$lat)  # Eliminar puntos
train$lat <- sub("^(.{1})(.*)$", "\\1.\\2", train$lat)  # Corregir ubicación del decimal

# Convertir la columna lat a numérico decimal
train$lat <- as.numeric(train$lat)

# Redondear los valores
train$lat <- round(train$lat, digits = 9)

train <- train %>%
  mutate(garaje = as.numeric(grepl("garaje", description)),
         parqueadero = as.numeric(grepl("parqueadero", description)),
         total_parqueo = garaje + parqueadero)

# Sustituir el valor "2" por "1" en total_parqueo
train$total_parqueo <- ifelse(train$total_parqueo == 2, 1, train$total_parqueo)


#### Ahora veamos la distribución del precio de los immuebles
summary(train$price) %>%
  as.matrix() %>%
  as.data.frame() %>%
  mutate(V1 = scales::dollar(V1))

# Calculamos valor del metro cuadrado
train <- train %>%
  mutate(precio_por_mt2 = round(price / nueva_surface, 0))

summary(train$precio_por_mt2) %>%
  as.matrix() %>%
  as.data.frame() %>%
  mutate(V1 = scales::dollar(V1))

# Eliminamos las observaciones que no tienen información de latitud o longitud
# Identificar filas con valores no numéricos en lon
non_numeric_lon <- !is.na(train$lon) & !is.numeric(train$lon)
non_numeric_lat <- !is.na(train$lat) & !is.numeric(train$lat)

train$lon <- as.numeric(train$lon)
train$lat <- as.numeric(train$lat)

train <- train %>%
  filter(!is.na(lat) & !is.na(lon))
# Observamos la primera visualización
leaflet() %>%
  addTiles() %>%
  addCircles(lng = train$lon,
             lat = train$lat)

#-------------------------------VARIABLES DE DISTANCIAS-----------------------------#
######--------------------------------PARQUES----------------------------------######

# Definir la ubicación de interés (en este caso, Bogotá, Colombia)
localizacion <- "Bogotá, Colombia"

# Obtener los límites geográficos (BBOX) de la ubicación
bbox_bogota <- getbb(localizacion)
parques <- opq(bbox = getbb("Bogotá, Colombia")) %>%
  add_osm_feature(key = "leisure" , value = "park")

# Cambiar el formato para que sea un objeto sf (simple features)
parques_sf <- osmdata_sf(parques)
parques_geometria <- parques_sf$osm_polygons %>%
  select(osm_id, name)
centroides <- st_centroid(parques_geometria)

# Encontrar el centro del mapa
latitud_central <- mean(bbox_bogota["lat"])
longitud_central <- mean(bbox_bogota["lon"])

# Crear el mapa de Bogotá con los parques
leaflet() %>%
  addTiles() %>%
  setView(lng = longitud_central, lat = latitud_central, zoom = 12) %>%
  addPolygons(data = parques_geometria, col = "red", weight = 10,
              opacity = 0.8, popup = parques_geometria$name) %>%
  addCircles(lng = st_coordinates(centroides)[, "X"],
             lat = st_coordinates(centroides)[, "Y"],
             col = "darkblue", opacity = 0.5, radius = 1)

train1_sf <- st_as_sf(train, coords = c("lon", "lat"), crs = 4326)
centroides_sf <- st_as_sf(centroides, coords = c("x", "y"), crs = 4326)
distancias_parque <- st_distance(train1_sf, centroides_sf)
dist_min_parques <- apply(distancias_parque, 1, min)
train1_sf$distancias_parque <- dist_min_parques


## ------------------------------------------------------------------------------##
## ---------------------------------Crear Terraza--------------------------------##

train$tiene_terraza <- as.numeric(grepl("terraza|azotea", train$description, ignore.case = TRUE))
  
# Mostrar las primeras filas del dataframe con la nueva variable
head(train)
table(train$tiene_terraza)
casas_con_terrazas <- sum(train$tiene_terraza == 1)
casas_con_terrazas

## ---------------------------------Crear Piscina---------------------------------##
# Crear una variable binaria "piscina" basada en la descripción
train$Piscina <- as.numeric(grepl("piscina|picina", train$description, ignore.case = TRUE))

# Mostrar las primeras filas del dataframe con la nueva variable
head(train)
table(train$Piscina)
casas_con_piscina <- sum(train$Piscina == 1)
casas_con_piscina

## ---------------------------------Crear Gimnasio---------------------------------##

train$Gimnasio <- as.numeric(grepl("gimnasio|gym", train$description, ignore.case = TRUE))

# Mostrar las primeras filas del dataframe con la nueva variable
head(train)
table(train$Gimnasio)
casas_con_gimnasio <- sum(train$Gimnasio == 1)
casas_con_gimnasio

## ---------------------------------Crear Chiminea---------------------------------##

train$Chimenea <- as.numeric(grepl("chimenea|chiminea", train$description, ignore.case = TRUE))
head(train)
table(train$Chimenea)
casas_con_chimenea <- sum(train$Chimenea == 1)
casas_con_chimenea

## ---------------------------------Seguridad Privada---------------------------------##

train$Seguridad <- as.numeric(grepl("vigilancia|sistema de seguridad|seguridad privada|seguridad 24|seguridad las veinticuatro horas|seguridad las 24", train$description, ignore.case = TRUE))
head(train)
table(train$Seguridad)
casas_con_seguridad <- sum(train$Seguridad == 1)
casas_con_seguridad

## ---------------------------------Crear Sala BBQ---------------------------------##
train$tiene_bbq <- grepl("BBQ|barbacoa", train$description, ignore.case = TRUE)

# Convertir valores lógicos en 1 (Tiene BBQ) y 0 (No tiene BBQ)
train$tiene_bbq <- as.numeric(train$tiene_bbq)

# Verificar la nueva variable "tiene_bbq"
table(train$tiene_bbq)


# Definir los límites geográficos de Bogotá
limites_bogota <- getbb("Bogotá, Colombia")

# Filtrar observaciones dentro de los límites de Bogotá
train_filtrado_bogota <- train %>%
  filter(
    between(lon, limites_bogota[1, "min"], limites_bogota[1, "max"]) &
      between(lat, limites_bogota[2, "min"], limites_bogota[2, "max"])
  )
# Escalamos para que se pueda graficar
train <- train %>%
  mutate(precio_por_mt2_sc =( (precio_por_mt2 - min(precio_por_mt2)) / (max(precio_por_mt2) - min(precio_por_mt2))))

# Crear una nueva columna "color" basada en el tipo de propiedad
train <- train %>%
  mutate(color = case_when(property_type == "Apartamento" ~ "#2A9D8F",
                           property_type == "Casa" ~ "#F4A261"))

# Encontrar el centro del mapa
latitud_central <- mean(train$lat)
longitud_central <- mean(train$lon)

# Crear el mensaje en el popup con HTML
html <- paste0("<b>Precio:</b> ",
               scales::dollar(train$price),
               "<br> <b>Area:</b> ",
               as.integer(train$nueva_surface), " mt2",
               "<br> <b>Tipo de inmueble:</b> ",
               train$property_type,
               "<br> <b>Numero de alcobas:</b> ",
               as.integer(train$rooms),
               "<br> <b>Numero de baños:</b> ",
               as.integer(train$bathrooms),
               "<br> <b>Sector:</b> ",
               train$l4,
               "<br> <b>Barrio:</b> ",
               train$l5)

# Crear el mapa con Leaflet
leaflet() %>%
  addTiles() %>%
  setView(lng = longitud_central, lat = latitud_central, zoom = 12) %>%
  addCircles(lng = train$lon,
             lat = train$lat,
             col = train$color,
             fillOpacity = 1,
             opacity = 1,
             radius = train$precio_por_mt2_sc * 10,
             popup = html)

train$bedrooms <- ifelse(train$bedrooms == 0, train$rooms, train$bedrooms)
mediana_bedrooms <- median(train$bedrooms, na.rm = TRUE)
train$bedrooms <- ifelse(is.na(train$bedrooms), mediana_bedrooms, train$bedrooms)

#-------------------------------VARIABLES DE DISTANCIAS-----------------------------#
#-----------------------------------------------------------------------------------#
##-----------------------DISTANCIA INSTITUCIONES FINANCIERAS CASAS------------------#

# Definir la ubicación de interés (en este caso, Bogotá, Colombia)
ubicacion <- "Bogotá, Colombia"

# Obtener los límites geográficos (BBOX) de la ubicación
bbox_bogota <- getbb(ubicacion)
bbox_bogota

bancos <- opq(bbox = bbox_bogota) %>%
  add_osm_feature(key = "amenity", value = "bank")

bancos_sf <- osmdata_sf(bancos)
bancos_geometria <- bancos_sf$osm_polygons %>%
  select(osm_id, name)

centroides_bancos <- st_centroid(bancos_geometria)

# Encontrar el centro del mapa
latitud_central <- mean(bbox_bogota["lat"])
longitud_central <- mean(bbox_bogota["lon"])

train_sf <- st_as_sf(train, coords = c("lon", "lat"), crs = 4326)
centroides_bancos_sf <- st_as_sf(centroides_bancos, coords = c("x", "y"), crs = 4326)
distancias_bancos <- st_distance(train_sf, centroides_bancos_sf)
dist_min_bancos <- apply(distancias_bancos, 1, min)
train_sf$distancias_bancos <- dist_min_bancos


######-------------------------------CENTROS COMERCIALES---------------------------------######


# Ubicación de interés (en este caso, Bogotá, Colombia) y buscar centros comerciales
centros_comerciales <- opq(bbox = getbb("Bogotá, Colombia")) %>%
  add_osm_feature(key = "shop" , value = "mall")
centros_comerciales_sf <- osmdata_sf(centros_comerciales)

# De las features de centros comerciales, nos interesa su geometría y ubicación
centros_comerciales_geometria <- centros_comerciales_sf$osm_points %>%
  select(osm_id, name)
centroides_centros_comerciales <- st_centroid(centros_comerciales_geometria)
distancias_centros_comerciales <- st_distance(train_sf, centroides_centros_comerciales)
dist_min_centros_comerciales <- apply(distancias_centros_comerciales, 1, min)
train_sf$distancia_centros_comerciales <- dist_min_centros_comerciales


##-----------------------SUPERMERCADOS Y OTROS ESTABLECIMIENTOS -----------------------------##

# Definir la ubicación de interés (en este caso, Bogotá, Colombia) y buscar supermercados
establecimientos <- opq(bbox = getbb("Bogotá, Colombia")) %>%
  add_osm_feature(key = "shop", value = c("supermarket", "grocery", "convenience", "bakery", "corner shop"))

# Cambiar el formato para que sea un objeto sf (simple features)
establecimientos_sf <- osmdata_sf(establecimientos)
establecimientos_geometria <- establecimientos_sf$osm_points %>%
  select(osm_id, name)
centroides_establecimientos <- st_centroid(establecimientos_geometria)

# Crear una nueva columna en train_sf que contenga la distancia mínima a un supermercado
distancias_establecimientos <- st_distance(train_sf, centroides_establecimientos)
dist_min_establecimientos <- apply(distancias_establecimientos, 1, min)
train_sf$distancia_establecimientos <- dist_min_establecimientos


###---------------------------------TRANSMILENEO--------------------------------------###

# Definir la ubicación de interés (en este caso, Bogotá, Colombia) y buscar estaciones de TransMilenio
transmilenio_transporte <- opq(bbox = getbb("Bogotá, Colombia")) %>%
  add_osm_feature(key = "public_transport" , value = "station") %>%
  add_osm_feature(key = "network", value = "TransMilenio")

# Cambiar el formato para que sea un objeto sf (simple features)
transmilenio_transporte_sf <- osmdata_sf(transmilenio_transporte)
transmilenio_transporte_geometria <- transmilenio_transporte_sf$osm_points %>%
  select(osm_id, name)
centroides_transmilenio_transporte <- st_centroid(transmilenio_transporte_geometria)

# Crear una nueva columna en train_sf que contenga la distancia mínima a una estación de TransMilenio
distancias_transmilenio_transporte <- st_distance(train_sf, centroides_transmilenio_transporte)
dist_min_transmilenio_transporte <- apply(distancias_transmilenio_transporte, 1, min)
train_sf$distancia_transmilenio_transporte <- dist_min_transmilenio_transporte


###------------------------------CENTROS EDUCATIVOS-----------------------------------###

etiquetas_educativos <- c("school", "college", "university", "library", "kindergarten")

# Definir la ubicación de interés (en este caso, Bogotá, Colombia) y buscar universidades
centros_educativos <- opq(bbox = getbb("Bogotá, Colombia")) %>%
  add_osm_feature(key = "amenity", value = etiquetas_educativos)
centros_educativos_sf <- osmdata_sf(centros_educativos)

# De las features de universidades, nos interesa su geometría y ubicación
centros_educativos_geometria <- centros_educativos_sf$osm_points %>%
  select(osm_id, name)
centroides_centros_educativos <- st_centroid(centros_educativos_geometria)

# Crear una nueva columna en train_sf que contenga la distancia mínima a una universidad
distancias_centros_educativos <- st_distance(train_sf, centroides_centros_educativos)
dist_min_centros_educativos <- apply(distancias_centros_educativos, 1, min)
train_sf$distancia_centros_educativos <- dist_min_centros_educativos

# Transformar los datos a una proyección en metros (por ejemplo, UTM para Bogotá)
train_sf <- st_transform(train_sf, crs = st_crs(centroides_centros_educativos))
distancias_centros_educativos <- st_distance(train_sf, centroides_centros_educativos)
dist_min_centros_educativos <- apply(distancias_centros_educativos, 1, min)
train_sf$distancia_centros_educativos <- dist_min_centros_educativos


##---------------------- RESTAURANTES Y BARES -------------------------------##

# Definir la búsqueda de restaurantes y bares en un solo grupo
restaurantes_bares <- opq(bbox = getbb("Bogotá, Colombia")) %>%
  add_osm_feature(key = "amenity", value = c("restaurant", "bar"))

# Cambiar el formato para que sea un objeto sf (simple features)
restaurantes_bares_sf <- osmdata_sf(restaurantes_bares)
restaurantes_bares_geometria <- restaurantes_bares_sf$osm_points %>%
  select(osm_id)
centroides_restaurantes_bares <- st_centroid(restaurantes_bares_geometria)

# Crear nuevas columnas en train_sf que contengan la distancia mínima a un restaurante o bar
distancias_restaurantes_bares <- st_distance(train_sf, centroides_restaurantes_bares)
dist_min_restaurantes_bares <- apply(distancias_restaurantes_bares, 1, min)
train_sf$distancia_restaurantes_bares <- dist_min_restaurantes_bares

# Crear un mapa de leaflet
m <- leaflet() %>%
  addTiles() %>%
  setView(lng = mean(train$lon), lat = mean(train$lat), zoom = 12) %>%
  addCircles(lng = train$lon, lat = train$lat, radius = 100, color = "blue")

coordinates <- train %>% select(lon, lat)
num_clusters <- 18
clusters <- kmeans(coordinates, centers = num_clusters)
train$cluster <- as.factor(clusters$cluster)


distancias1 <- train_sf %>% select(32:37) %>% st_drop_geometry()
distanciasp <- train1_sf %>% select(24) %>% st_drop_geometry()
train <- train %>% bind_cols(distancias1,distanciasp)


##########################################################################################################################

coordenadas_santa_fe <- c(-74.060071, 4.611765)    # Coordenadas de Santa Fe
coordenadas_usaquen <- c(-74.033055, 4.736126)     # Coordenadas de Usaquén
coordenadas_teusaquillo <- c(-74.083913, 4.635771) # Coordenadas de Teusaquillo
coordenadas_candelaria <- c(-74.080661, 4.601209)  # Coordenadas de La Candelaria
coordenadas_engativa <- c(-74.100196, 4.704383)  # Coordenadas de Engativá
coordenadas_suba <- c(-74.082256, 4.752255)     # Coordenadas de Suba
coordenadas_bosa <- c(-74.195850, 4.620330)     # Coordenadas de Bosa
coordenadas_kennedy <- c(-74.149874, 4.619825)    # Coordenadas de Kennedy
coordenadas_fontibon <- c(-74.169382, 4.669940)   # Coordenadas de Fontibón
coordenadas_ciudad_bolivar <- c(-74.153087, 4.546036)  # Coordenadas de Ciudad Bolívar
coordenadas_antonio_narino <- c(-74.122283, 4.583774)  # Coordenadas de Antonio Nariño
coordenadas_usme <- c(-74.121821, 4.496946)  # Coordenadas de usme
coordenadas_barrios_unidos <- c(-74.084313, 4.669536)  # Coordenadas de Barrios Unidos
coordenadas_san_cristobal <- c(-74.072592, 4.5542)  # Coordenadas de San Cristobal
coordenadas_rafael_uribe <- c(-74.11279, 4.558455)  # Coordenadas de Rafael Uribe Uribe
coordenadas_los_martirez <- c(-74.096674, 4.608344)  # Coordenadas Los Martirez
coordenadas_tunjuelito <- c(-74.140652, 4.580954)  # Coordenadas Tunjuelito
coordenadas_puente_aranda <- c( -74.103571,  4.612189)  # Coordenadas Puente Aranda

# Define umbrales de distancia en grados para cada localidad
umbral_grados_santa_fe <- 0.05       # Umbral para Santa Fe (aproximadamente 2.2 km en latitud y longitud)
umbral_grados_usaquen <- 0.06        # Umbral para Usaquén (aproximadamente 6.6 km en latitud y longitud)
umbral_grados_teusaquillo <- 0.07    # Umbral para Teusaquillo (aproximadamente 2.2 km en latitud y longitud)
umbral_grados_candelaria <- 0.04     # Umbral para La Candelaria (aproximadamente 2.2 km en latitud y longitud)
umbral_grados_engativa <- 0.06      # Umbral para Engativá (aproximadamente 3.3 km en latitud y longitud)
umbral_grados_suba <- 0.06         # Umbral para Suba (aproximadamente 3.3 km en latitud y longitud)
umbral_grados_bosa <- 0.03          # Umbral para Bosa (aproximadamente 3.3 km en latitud y longitud)
umbral_grados_kennedy <- 0.02      # Aproximadamente 2.2 km en latitud y longitud
umbral_grados_fontibon <- 0.02     # Aproximadamente 2.2 km en latitud y longitud
umbral_grados_ciudad_bolivar <- 0.04   # Umbral para Ciudad Bolívar (aproximadamente 4.4 km en latitud y longitud)
umbral_grados_antonio_narino <- 0.04   # Umbral para Antonio Nariño (aproximadamente 2.2 km en latitud y longitud)
umbral_grados_usme <- 0.12  # Por ejemplo, aproximadamente 9.9 km en latitud y longitud
umbral_grados_barrios_unidos <- 0.05   # Umbral para Barrios Unidos (aproximadamente 3.3 km en latitud y longitud)
umbral_grados_san_cristobal <- 0.05   # Umbral para San Cristobal (aproximadamente 3.3 km en latitud y longitud)
umbral_grados_rafael_uribe <- 0.06   # Umbral para Rafael Uribe Uribe (aproximadamente 3.3 km en latitud y longitud)
umbral_grados_los_martirez <- 0.05   # Umbral para Los Martirez (aproximadamente 3.3 km en latitud y longitud)
umbral_grados_tunjuelito <- 0.02   # Umbral para Tunjuelito (aproximadamente 3.3 km en latitud y longitud)
umbral_grados_puente_aranda <- 0.06   # Umbral para Puente Aranda (aproximadamente 3.3 km en latitud y longitud)

# Calcula la distancia en grados entre cada punto y las coordenadas de cada localidad
train$distancia_santa_fe <- abs(train$lon - coordenadas_santa_fe[1]) + abs(train$lat - coordenadas_santa_fe[2])
train$distancia_usaquen <- abs(train$lon - coordenadas_usaquen[1]) + abs(train$lat - coordenadas_usaquen[2])
train$distancia_teusaquillo <- abs(train$lon - coordenadas_teusaquillo[1]) + abs(train$lat - coordenadas_teusaquillo[2])
train$distancia_candelaria <- abs(train$lon - coordenadas_candelaria[1]) + abs(train$lat - coordenadas_candelaria[2])
train$distancia_engativa <- abs(train$lon - coordenadas_engativa[1]) + abs(train$lat - coordenadas_engativa[2])
train$distancia_suba <- abs(train$lon - coordenadas_suba[1]) + abs(train$lat - coordenadas_suba[2])
train$distancia_bosa <- abs(train$lon - coordenadas_bosa[1]) + abs(train$lat - coordenadas_bosa[2])
train$distancia_kennedy <- abs(train$lon - coordenadas_kennedy[1]) + abs(train$lat - coordenadas_kennedy[2])
train$distancia_fontibon <- abs(train$lon - coordenadas_fontibon[1]) + abs(train$lat - coordenadas_fontibon[2])
train$distancia_ciudad_bolivar <- abs(train$lon - coordenadas_ciudad_bolivar[1]) + abs(train$lat - coordenadas_ciudad_bolivar[2])
train$distancia_antonio_narino <- abs(train$lon - coordenadas_antonio_narino[1]) + abs(train$lat - coordenadas_antonio_narino[2])
train$distancia_usme <- abs(train$lon - coordenadas_usme[1]) + abs(train$lat - coordenadas_usme[2])
train$distancia_barrios_unidos <- abs(train$lon - coordenadas_barrios_unidos[1]) + abs(train$lat - coordenadas_barrios_unidos[2])
train$distancia_san_cristobal <- abs(train$lon - coordenadas_san_cristobal[1]) + abs(train$lat - coordenadas_san_cristobal[2])
train$distancia_rafael_uribe <- abs(train$lon - coordenadas_rafael_uribe[1]) + abs(train$lat - coordenadas_rafael_uribe[2])
train$distancia_los_martirez <- abs(train$lon - coordenadas_los_martirez[1]) + abs(train$lat - coordenadas_los_martirez[2])
train$distancia_tunjuelito <- abs(train$lon - coordenadas_tunjuelito[1]) + abs(train$lat - coordenadas_tunjuelito[2])
train$distancia_puente_aranda <- abs(train$lon - coordenadas_puente_aranda[1]) + abs(train$lat - coordenadas_puente_aranda[2])

# Asigna cada punto a una localidad basado en las distancias y umbrales
train$localidad[train$distancia_santa_fe < umbral_grados_santa_fe] <- "Santa Fe"
train$localidad[train$distancia_usaquen < umbral_grados_usaquen] <- "Usaquén"
train$localidad[train$distancia_teusaquillo < umbral_grados_teusaquillo] <- "Teusaquillo"
train$localidad[train$distancia_candelaria < umbral_grados_candelaria] <- "La Candelaria"
train$localidad[train$distancia_engativa < umbral_grados_engativa] <- "Engativá"
train$localidad[train$distancia_suba < umbral_grados_suba] <- "Suba"
train$localidad[train$distancia_bosa < umbral_grados_bosa] <- "Bosa"
train$localidad[train$distancia_kennedy < umbral_grados_kennedy] <- "Kennedy"
train$localidad[train$distancia_fontibon < umbral_grados_fontibon] <- "Fontibón"
train$localidad[train$distancia_ciudad_bolivar < umbral_grados_ciudad_bolivar] <- "Ciudad Bolívar"
train$localidad[train$distancia_antonio_narino < umbral_grados_antonio_narino] <- "Antonio Nariño"
train$localidad[train$distancia_usme < umbral_grados_usme] <- "Usme"
train$localidad[train$distancia_barrios_unidos < umbral_grados_barrios_unidos] <- "Barrios Unidos"
train$localidad[train$distancia_san_cristobal < umbral_grados_san_cristobal] <- "San Cristobal"
train$localidad[train$distancia_rafael_uribe < umbral_grados_rafael_uribe] <- "Rafael Uribe Uribe"
train$localidad[train$distancia_los_martirez < umbral_grados_los_martirez] <- "Los Martirez"
train$localidad[train$distancia_tunjuelito < umbral_grados_tunjuelito] <- "Tunjuelito"
train$localidad[train$distancia_puente_aranda < umbral_grados_puente_aranda] <- "Puente Aranda"

# Elimina las columnas de distancia si ya no son necesarias
train <- train %>% select(-distancia_santa_fe, -distancia_usaquen, -distancia_teusaquillo, -distancia_candelaria,-distancia_engativa,-distancia_suba,-distancia_bosa, -distancia_kennedy, -distancia_fontibon, -distancia_ciudad_bolivar,-distancia_antonio_narino, -distancia_usme,-distancia_barrios_unidos,-distancia_san_cristobal,-distancia_rafael_uribe, -distancia_los_martirez, -distancia_tunjuelito, -distancia_puente_aranda)


ggplot(train, aes(x = localidad)) +
  geom_bar(fill = "blue") +
  labs(title = "Distribución de Localidades") +
  coord_flip()


distant <- data.frame(
  Dist_Centros_Comerciales = train$distancia_centros_comerciales,
  Dist_Establecimientos = train$distancia_establecimientos,  
  Dist_Transp_Publico = train$distancia_transmilenio_transporte,
  Dist_Centros_Educ = train$distancia_centros_educativos,
  Dist_Parques = train$distancias_parque,
  Dist_Rests = train$distancia_restaurantes_bares,
  Dist_Bancos = train$distancias_bancos)
distant

distant <- data.frame(localidad = train$localidad, distant) 

promedio_distancias <- distant %>%
  group_by(localidad) %>%
  summarize(
    Dist_Centros_Comerciales = mean(Dist_Centros_Comerciales, na.rm = TRUE),
    Dist_Establecimientos = mean(Dist_Establecimientos, na.rm = TRUE),
    Dist_Transp_Publico = mean(Dist_Transp_Publico, na.rm = TRUE),
    Dist_Centros_Educ = mean(Dist_Centros_Educ, na.rm = TRUE),
    Dist_Parques = mean(Dist_Parques, na.rm = TRUE),
    Dist_Rests = mean(Dist_Rests, na.rm = TRUE),
    Dist_Bancos = mean(Dist_Bancos, na.rm = TRUE)
  ) %>%
  ungroup() 

print(promedio_distancias)

otra_localidad <- sum(train$localidad == "Otra Localidad")
print(otra_localidad)

# Unir las bases de datos por la columna "localidad"
train <- merge(train, estrato, by = "localidad")


# -------------------- Estadísticas Descriptivas ---------------------------------#
# ------------------------------------------------------------------------------- #

# Precio Promedio para Casas y Apartamentos
lprice <- log(train$price)
train$lprice <- lprice

# Filtrar las observaciones de "Casas" y "Apartamentos"

casas <- subset(train, property_type == "Casa")
fecha <- as.yearmon(paste(casas$year, casas$month, "01", sep = "-"))
Precio_Casas <- aggregate(lprice ~ year + month, data = casas, FUN = mean)
colnames(Precio_Casas) <- c("Year", "Month", "Precio_Promedio_Casas")

graf_casas <- ggplot(Precio_Casas, aes(x = as.Date(paste(Year, Month, "01", sep = "-")), y = Precio_Promedio_Casas)) +
  geom_line(linewidth = 1.5, color = "blue") + 
  labs(
    title = "Evolución Precios Promedio de Casas",
    x = "Fecha",
    y = "Precio Promedio"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.line = element_line(color = "gray"),
    panel.background = element_rect(fill = "transparent", color = NA),
    panel.grid = element_line(color = "gray")
  )
graf_casas

apartamentos <- subset(train, property_type == "Apartamento")
Precio_Deptos <- aggregate(lprice ~ year + month, data = apartamentos, FUN = mean)
colnames(Precio_Deptos) <- c("Year", "Month", "Precio_Promedio_Apartamentos")

graf_deptos <- ggplot(Precio_Deptos, aes(x = as.Date(paste(Year, Month, "01", sep = "-")), y = Precio_Promedio_Apartamentos)) +
  geom_line(linewidth = 1.5, color = "red") +
  labs(
    title = "Evolución Precios Promedio Apartamentos",
    x = "Fecha",
    y = "Precio Promedio"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.line = element_line(color = "gray"),
    panel.background = element_rect(fill = "transparent", color = NA),
    panel.grid = element_line(color = "gray"))
graf_deptos

# Combinar los gráficos en una sola figura
grid.arrange(graf_casas, graf_deptos, ncol = 2)

# ------------------- Crear la tabla de frecuencias para casas ------------------------------------#

Mode <- function(x) {
  uniq_x <- unique(x)
  uniq_x[which.max(tabulate(match(x, uniq_x)))] 
}

Precio_Casas <- train %>%
  filter(property_type == "Casa") %>%
  mutate(Precios_Casas = cut(lprice, 
                             breaks = c(19.50, 19.65, 19.80, 19.95, 20.10, 20.25, 20.4, 20.55, 20.80, 20.95, 21.10, 21.25),
                             labels = c("19.50-19.65", "19.65-19.80", "19.80-19.95", "19.95-20.10", "20.10-20.25","20.25-20.4","20.4-20.55",
                                        "20.55-20.80", "20.80-20.95", "20.95-21.10", "21.10-21.25")))

# Tabla de frecuencias con la moda del estrato para casas
Rango_precios_casas <- Precio_Casas %>%
  group_by(Precios_Casas) %>%
  summarise(Estrato = Mode(estrato),
            Habit = round(mean(bedrooms)),
            Bath = round(mean(bathrooms,na.rm = TRUE)),
            Area = mean(nueva_surface),
            Observaciones = n(),
            Part_Porc = (Observaciones / 9071 * 100))
colnames(Rango_precios_casas) <- c("Rango de Precios", "Estrato","Habitaciones","Baños", "Area", "Observaciones","Part. %" )


# Crear la tabla de frecuencias para apartamentos
Precio_Aparts <- train %>%
  filter(property_type == "Apartamento") %>%
  mutate(Precios_Apart = cut(lprice, 
                             breaks = c(19.50, 19.65, 19.80, 19.95, 20.10, 20.25, 20.4, 20.55, 20.80, 20.95, 21.10, 21.25),
                             labels = c("19.50-19.65", "19.65-19.80", "19.80-19.95", "19.95-20.10", "20.10-20.25","20.25-20.4","20.4-20.55",
                                        "20.55-20.80", "20.80-20.95", "20.95-21.10", "21.10-21.25")))

# Tabla de frecuencias con la moda del estrato para apartamentos
Rango_precios_Apart <- Precio_Aparts  %>%
  group_by(Precios_Apart) %>%
  summarise(Estrato = Mode(estrato),
            Habit = round(mean(bedrooms)),
            Bath = round(mean(bathrooms,na.rm = TRUE)),
            Area = mean(nueva_surface),
            Observaciones = n(),
            Part_Porc = (Observaciones / 28093 * 100))
colnames(Rango_precios_Apart) <- c("Rango de Precios", "Estrato","Habitaciones","Baños", "Area", "Observaciones","Part. %" )


# ---------------------Rango de Precios y Distancias-----------------------------------------------------------------------

# Tabla de frecuencias con la moda del estrato para casas
Precios_Distancias_C <- Precio_Casas %>%
  group_by(Precios_Casas) %>%
  summarise(Parques = round(mean(distancias_parque)),
            CComer = round(mean(distancia_centros_comerciales)),
            Centros_Educ = round(mean(distancia_centros_educativos)),
            Tranp_Publico = round(mean(distancia_transmilenio_transporte)),
            Restaurantes = round(mean(distancia_restaurantes_bares)),
            Establecimientos = round(mean(distancia_establecimientos)),
            Bancos = round(mean(distancias_bancos)))
colnames(Precios_Distancias_C) <- c("Rango de Precios","Distancia Parques", "Distancia Centros Comerciales","Distancia Centros Educativos","Distancia Transp. Publico","Distancia Restaurantes","Distancia Establecimientos","Distancia Bancos")


# Tabla de frecuencias con la moda del estrato para casas
Precios_Distancias_D <- Precio_Aparts %>%
  group_by(Precios_Apart) %>%
  summarise(Parques = round(mean(distancias_parque)),
            CComer = round(mean(distancia_centros_comerciales)),
            Centros_Educ = round(mean(distancia_centros_educativos)),
            Transp_Publico = round(mean(distancia_transmilenio_transporte)),
            Restaurantes = round(mean(distancias_restaurantes_bares)),
            Establecimientos = round(mean(distancia_establecimientos)),
            Bancos = round(mean(distancias_bancos)))
colnames(Precios_Distancias_D) <- c("Rango de Precios","Distancia Parques", "Distancia Centros Comerciales","Distancia Centros Educativos","Distancia Transp. Publico","Distancia Restaurantes","Distancia Establecimientos","Distancia Bancos")


# ---------------------Precios promedios por Localidad-----------------------------------------------------------------------

# 1. Calcular el promedio casas de precios por localidad
Precio_Medio_Localidad_C <- aggregate(price ~ localidad,  data = subset(train, property_type == "Casa"), FUN = mean)
colnames(Precio_Medio_Localidad_C) <- c("Localidad", "Precio_Promedio")
Precio_Medio_Localidad_C$Precio_Promedio <- round(Precio_Medio_Localidad_C$Precio_Promedio, 2)

# Crear el gráfico de barras
gc <- ggplot(Precio_Medio_Localidad_C, aes(x = reorder(Localidad, -Precio_Promedio), y = Precio_Promedio)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(
    title = "Precio Promedio por localidad Casas",
    x = "Localidad",
    y = "Precio Promedio"
  ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5),
        plot.title = element_text(hjust = 0.5),
        axis.line = element_line(color = "gray"),
        panel.background = element_rect(fill = "transparent", color = NA),
        panel.grid = element_line(color = "gray"))
gc

# 2. Calcular el promedio apartamentos de precios por localidad
Precio_Medio_Localidad_D <- aggregate(price ~ localidad,  data = subset(train, property_type == "Apartamento"), FUN = mean)
colnames(Precio_Medio_Localidad_D) <- c("Localidad", "Precio_Promedio")
Precio_Medio_Localidad_D$Precio_Promedio <- round(Precio_Medio_Localidad_D$Precio_Promedio, 2)

# Crear el gráfico de barras
gd <- ggplot(Precio_Medio_Localidad_D, aes(x = reorder(Localidad, -Precio_Promedio), y = Precio_Promedio)) +
  geom_bar(stat = "identity", fill = "red") +
  labs(
    title = "Precio Promedio por localidad Apartamentos",
    x = "Localidad",
    y = "Precio Promedio"
  ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5),
        plot.title = element_text(hjust = 0.5),
        axis.line = element_line(color = "gray"),
        panel.background = element_rect(fill = "transparent", color = NA),
        panel.grid = element_line(color = "gray"))
gd

# Combinar los gráficos en una sola figura
grid.arrange(gc, gd, ncol = 2)


# ---------------------Precios promedios por M2 Localidad-----------------------------------------------------------------------

# 1. Calcular el promedio casas de precios por localidad
Precio_M2_Localidad_C <- aggregate(precio_por_mt2 ~ localidad,  data = subset(train, property_type == "Casa"), FUN = mean)
colnames(Precio_M2_Localidad_C) <- c("Localidad", "Precio_Promedio M2")
Precio_M2_Localidad_C$Precio_Promedio <- round(Precio_M2_Localidad_C$Precio_Promedio, 2)

# Crear el gráfico de barras
gcm <- ggplot(Precio_M2_Localidad_C, aes(x = reorder(Localidad, -Precio_Promedio), y = Precio_Promedio)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(
    title = "Precio Medio por M2 de Casas por localidad",
    x = "Localidad",
    y = "Precio Promedio"
  ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5),
        plot.title = element_text(hjust = 0.5),
        axis.line = element_line(color = "gray"),
        panel.background = element_rect(fill = "transparent", color = NA),
        panel.grid = element_line(color = "gray"))
gcm

# 2. Calcular el promedio apartamentos de precios por localidad
Precio_M2_Localidad_D <- aggregate(precio_por_mt2 ~ localidad,  data = subset(train, property_type == "Apartamento"), FUN = mean)
colnames(Precio_M2_Localidad_D) <- c("Localidad", "Precio_Promedio M2")
Precio_M2_Localidad_D$Precio_Promedio <- round(Precio_M2_Localidad_D$Precio_Promedio, 2)


gdm <- ggplot(Precio_M2_Localidad_D, aes(x = reorder(Localidad, -Precio_Promedio), y = Precio_Promedio)) +
  geom_bar(stat = "identity", fill = "red") +
  labs(
    title = "Precio Medio por M2 de Apartamentos por localidad",
    x = "Localidad",
    y = "Precio Promedio"
  ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5),
        plot.title = element_text(hjust = 0.5),
        axis.line = element_line(color = "gray"),
        panel.background = element_rect(fill = "transparent", color = NA),
        panel.grid = element_line(color = "gray"))
gdm

# Combinar los gráficos en una sola figura
grid.arrange(gcm, gdm, ncol = 2)


##----------------- Variables Seleccionadas --------------------------------##
##--------------------------------------------------------------------------##

# Renombrar las variables para una mayor comprensión de las variables que estamos trabajando
train <- train %>% rename(Precio=price) 
train <- train %>% rename(lPrecio=lprice) 
train <- train %>% rename(Precio_M2=precio_por_mt2) 
train <- train %>% rename(Habitaciones=bedrooms)
train <- train %>% rename(Baños=bathrooms) 
train <- train %>% rename(Area=surface_total)
train <- train %>% rename(M2_por_Habitación=metros_cuadrados_bedrooms)
train <- train %>% rename(Terraza=tiene_terraza) 
train <- train %>% rename(Sala_BBQ=tiene_bbq) 
train <- train %>% rename(Garaje=total_parqueo) 
train <- train %>% rename(Dist_Parques=distancias_parque) 
train <- train %>% rename(Dist_Transp_Publico=distancia_transmilenio_transporte) 
train <- train %>% rename(Dist_Establecimientos=distancia_establecimientos)
train <- train %>% rename(Dist_C_Comerc=distancia_centros_comerciales) 
train <- train %>% rename(Dist_Centros_Educ=distancia_centros_educativos)
train <- train %>% rename(Dist_Restaurantes=distancia_restaurantes_bares)
train <- train %>% rename(Dist_Bancos=distancias_bancos)
train <- train %>% rename(Estrato=estrato) 

mediana_area <- median(train$Area, na.rm = TRUE)
train$Area <- ifelse(is.na(train$Area), mediana_area, train$Area)


##-----------------------------CASAS----------------------------------------

train_casas <- train[train$property_type == "Casa", c("property_id","title", "month", "year", "localidad","Estrato", "Precio", "lPrecio", 
                                                      "Precio_M2", "Habitaciones", "Baños", "Area","M2_por_Habitación", "lat", "lon", "Terraza", 
                                                      "Garaje", "Sala_BBQ","Piscina","Gimnasio", "Chimenea","Seguridad",
                                                      "Dist_Parques", "Dist_Transp_Publico", "Dist_Establecimientos", 
                                                      "Dist_C_Comerc", "Dist_Centros_Educ", "Dist_Restaurantes","Dist_Bancos")]

Tabla_train_casas <- "C:/Output R/Taller 2/Taller_2/Tabla_train_casas.xlsx"  
write_xlsx(train_casas, Tabla_train_casas)

# Imputar los Valores para los Baños
data_b_b <- train_casas[complete.cases(train_casas[c("Habitaciones", "Baños")]), ]
media_b_b <- mean(data_b_b$Habitaciones / data_b_b$Baños)
train_casas$Baños[is.na(train_casas$Baños)] <- train_casas$Habitaciones[is.na(train_casas$Baños)] / media_b_b
train_casas$Baños <- round(train_casas$Baños) 

# Calcular el precio por metro cuadrado
train <- train %>%
  mutate(Precio_M2 = round(Precio / Area))

train <- train %>%
  mutate(M2_por_Habitación = round(Area / Habitaciones))

# Revisión de stadística descriptivas de variables para el Modelo
Tabla_Stat <- train_casas  %>% select(Precio, 
                                      Precio_M2, 
                                      Habitaciones, 
                                      Baños,
                                      Area,
                                      M2_por_Habitación,
                                      Dist_Parques,
                                      Dist_Transp_Publico,
                                      Dist_Establecimientos,
                                      Dist_C_Comerc,
                                      Dist_Centros_Educ,
                                      Dist_Restaurantes,
                                      Dist_Bancos,
                                      Estrato)

stargazer(data.frame(Tabla_Stat), header=FALSE, type='text',title="Estadisticas Variables Seleccionadas Casas")

##---------------------------------------Apartamentos ----------------------------------------##

train_apart <- train[train$property_type == "Apartamento", c("property_id","title", "month", "year", "localidad","Estrato", "Precio", "lPrecio", 
                                                             "Precio_M2", "Habitaciones", "Baños", "Area","M2_por_Habitación", "lat", "lon", "Terraza", 
                                                             "Garaje", "Sala_BBQ","Piscina","Gimnasio", "Chimenea","Seguridad",
                                                             "Dist_Parques", "Dist_Transp_Publico", "Dist_Establecimientos", 
                                                             "Dist_C_Comerc", "Dist_Centros_Educ", "Dist_Restaurantes","Dist_Bancos")]


Tabla_train_apart <- "C:/Output R/Taller 2/Taller_2/Tabla_train_apart.xlsx"  
write_xlsx(train_apart, Tabla_train_apart)

# Imputar los Valores para los Baños
data1_b_b <- train_apart[complete.cases(train_apart[c("Habitaciones", "Baños")]), ]
media1_b_b <- mean(data_b_b$Habitaciones / data_b_b$Baños)
train_apart$Baños[is.na(train_apart$Baños)] <- train_apart$Habitaciones[is.na(train_apart$Baños)] / media1_b_b
train_apart$Baños <- round(train_apart$Baños) 


# Revisión de stadística descriptivas de variables para el Modelo
Tabla_Stat_D <- train_apart  %>% select(Precio, 
                                        Precio_M2, 
                                        Habitaciones, 
                                        Baños,
                                        Area,
                                        M2_por_Habitación,
                                        Dist_Parques,
                                        Dist_Transp_Publico,
                                        Dist_Establecimientos,
                                        Dist_C_Comerc,
                                        Dist_Centros_Educ,
                                        Dist_Restaurantes,
                                        Dist_Bancos,
                                        Estrato)

stargazer(data.frame(Tabla_Stat_D), header=FALSE, type='text',title="Estadisticas Variables Seleccionadas Apartamentos")

#Tabla_train <- "C:/Output R/Taller 2/Taller_2/Tabla_1.xlsx"  
#write_xlsx(train, Tabla_train)

##############################################################################################################
#--------------------------------------------- CHAPINERO-----------------------------------------------------#
###################################################TEST########################################################

test %>%
  summarise_all(~sum(is.na(.))) %>% transpose()

test$surface_total[is.na(test$surface_total)] <- test$surface_covered[is.na(test$surface_total)]

#Encontremos la moda de número de habitaciones 3, cuartos y número de baños
test$bedrooms <- ifelse(test$bedrooms == 0, test$rooms, test$bedrooms)                                                    

### Analisis descriptivo de rooms 3
test %>%
  count(rooms) %>% head() 
test%>%
  count(bedrooms)

### Analisis descriptivo de rooms bathrooms 2

test %>%
  count(bathrooms)
library(dplyr)

##### Imputar la moda para bedrooms y bathrooms
test <- test %>%
  mutate(rooms = replace_na(rooms, 3),
         bedrooms = replace_na(bedrooms, 3),
         bathrooms = replace_na(bathrooms, 2),)
# Añadir las mismas transformaciones que aplicaste a "train" para la variable "description"
test <- test %>%
  mutate(description = str_to_lower(description),
         description = iconv(description, from = "UTF-8", to = "ASCII//TRANSLIT"),
         description = str_replace_all(description, "[^[:alnum:]]", " "),
         description = str_trim(gsub("\\s+", " ", description)))

# Añadir la misma lógica que usaste para extraer los metros cuadrados de la descripción
test$metros_cuadrados <- NA

for (i in 1:nrow(test)) {
  descripcion <- test$description[i]
  
  coincidencias <- gregexpr("\\d+(\\.\\d+)?\\s*(mts|mts²|m²|m2|metros cuadrados|metro cuadrado)", descripcion)
  todas_coincidencias <- regmatches(descripcion, coincidencias)[[1]]
  
  metros_cuadrados <- NA
  distancia_minima <- Inf
  
  for (coincidencia in todas_coincidencias) {
    distancia_a_metros <- min(gregexpr("metros", coincidencia)[[1]])
    
    if (distancia_a_metros < distancia_minima) {
      distancia_minima <- distancia_a_metros
      metros_cuadrados <- coincidencia
    }
  }
  
  if (!is.na(metros_cuadrados)) {
    valor_metros <- regmatches(metros_cuadrados, gregexpr("\\d+(\\.\\d+)?", metros_cuadrados))[[1]]
    test$metros_cuadrados[i] <- as.numeric(valor_metros)
  }
}

# Reemplazar los valores 0 con NA en la columna "metros_cuadrados"
test <- test %>%
  mutate(metros_cuadrados = ifelse(metros_cuadrados == 0, NA, metros_cuadrados))
######### remplazar metros cuadrados solo cuando surface sea na
# Crear una nueva variable "nueva_surface" que reemplace "surface" con "metros_cuadrados" cuando "surface" sea NA
test <- test %>%
  mutate(nueva_surface = ifelse(is.na(surface_total), metros_cuadrados, surface_total))
test %>%
  summarise_all(~sum(is.na(.))) %>% transpose()
# Obtener un resumen estadístico de la variable "nueva_surface"
resumen_nueva_surface <- summary(test$nueva_surface)

# Imprimir el resumen
print(resumen_nueva_surface)

frecuencias <- table(test$nueva_surface)
moda <- as.numeric(names(frecuencias)[which.max(frecuencias)])

### Analisis para nueva surface
test$rooms <- as.numeric(test$rooms)
test$bedrooms <- as.numeric(test$bedrooms)
test$bathrooms <- as.numeric(test$bathrooms)
test$surface_total <- as.numeric(test$surface_total)
test$surface_covered <- as.numeric(test$surface_covered)

test$bedrooms <- ifelse(test$bedrooms == 0, test$rooms, test$bedrooms)
mediana_bedrooms <- median(test$bedrooms, na.rm = TRUE)
test$bedrooms <- ifelse(is.na(test$bedrooms), mediana_bedrooms, test$bedrooms)

test <- test %>%
  mutate(metros_cuadrados_bedrooms = nueva_surface / bedrooms)
summary(test$metros_cuadrados_bedrooms)


# --Segun los datos finales de Train la media de m^2 por habitación es 45.81

test$metros_cuadrados <- ifelse(test$property_type == "Casa" & is.na(test$metros_cuadrados), 34.74 * test$bedrooms, test$metros_cuadrados)
test$metros_cuadrados <- ifelse(test$property_type == "Apartamento" & is.na(test$metros_cuadrados), 49.38 * test$bedrooms, test$metros_cuadrados)

test <- test %>%
  mutate(metros_cuadrados_por_bedrooms = metros_cuadrados / bedrooms)
summary(test$metros_cuadrados_por_bedrooms)

##-------------------------------------Casas---------------------------------##
promedio_metros_casas <- test %>%
  filter(property_type == "Casa", bedrooms > 0) %>%
  group_by(bedrooms) %>%
  summarize(promedio = mean(metros_cuadrados_por_bedrooms, na.rm = TRUE))
promedio_metros_casas


# Accede a la media directamente desde el marco de datos resultante
media_metros_cuadrados_por_bedrooms <- promedio_metros_casas$promedio


# Calculo de la Media y Varianza Casas
media_metros_cuadrados_por_bedrooms <- c(82.39600, 399.11630, 57.48632, 207.74821, 51.60242, 45.01286, 32.27844, 34.44500, 34.74000, 34.74000, 30.52857)
media_m2_casas <- c(82.39600, 57.48632, 51.60242, 45.01286, 32.27844, 34.44500, 34.74000, 34.74000, 30.52857)

media_metros2_bedrooms_casas <- mean(media_m2_casas)
desv_metros2_bedrooms_casas <- sd(media_m2_casas)
max_media_m2_casas = media_metros2_bedrooms_casas+3*desv_metros2_bedrooms_casas
min_media_m2_casas = media_metros2_bedrooms_casas-2*desv_metros2_bedrooms_casas
test$metros_cuadrados_por_bedrooms[test$property_type == "Casa" & test$metros_cuadrados_por_bedrooms < min_media_m2_casas] <- min_media_m2_casas
test$metros_cuadrados_por_bedrooms[test$property_type == "Casa" & test$metros_cuadrados_por_bedrooms > max_media_m2_casas] <- max_media_m2_casas


test$metros_cuadrados <- ifelse(test$property_type == "Casa" & (test$metros_cuadrados/test$bedrooms) < min_media_m2_casas,
                                test$metros_cuadrados_por_bedrooms * test$bedrooms,
                                test$metros_cuadrados)
test$metros_cuadrados <- ifelse(test$property_type == "Casa" & (test$metros_cuadrados/test$bedrooms)  > max_media_m2_casas,
                                test$metros_cuadrados_por_bedrooms * test$bedrooms,
                                test$metros_cuadrados)


##-------------------------------------Apartamentos---------------------------------##

##----- Calcula el promedio de metros cuadrados por habitación exlclyendo los extremos--

promedio_metros_apart <- test %>%
  filter(property_type == "Apartamento") %>%  # Filtrar solo propiedades de tipo "Casa"
  group_by(bedrooms) %>%
  mutate(
    lower_limit = quantile(metros_cuadrados_por_bedrooms, 0.05, na.rm = TRUE),
    upper_limit = quantile(metros_cuadrados_por_bedrooms, 0.95, na.rm = TRUE)
  ) %>%
  filter(metros_cuadrados_por_bedrooms >= lower_limit & metros_cuadrados_por_bedrooms <= upper_limit) %>%
  summarize(
    promedio = mean(metros_cuadrados_por_bedrooms, na.rm = TRUE)
  )
promedio_metros_apart

# Accede a la media directamente desde el marco de datos resultante
media_m2_bedrooms <- promedio_metros_apart$promedio
media_m2_bedrooms_d <- c(101, 53.9, 50.5, 50.6, 46.5, 49.38000, 49.38000, 49.38000, 746.00)
media_m2_bedrooms_d2 <- c(101, 53.9, 50.5, 50.6, 46.5, 49.38000, 49.38000, 49.38000)

# Calculo de la Media y Varianza Casas
desv_media_m2_bedrooms_d <- sd(media_m2_bedrooms_d2)
desv_media_m2_bedrooms_d
media_m2_bedrooms_d <- mean(media_m2_bedrooms_d2)
media_m2_bedrooms_d

max_media_m2_apart = media_m2_bedrooms_d+2*desv_media_m2_bedrooms_d
min_media_m2_apart = media_m2_bedrooms_d-2*desv_media_m2_bedrooms_d
test$metros_cuadrados_por_bedrooms[test$property_type == "Apartamento" & test$metros_cuadrados_por_bedrooms < min_media_m2_apart] <- min_media_m2_apart
test$metros_cuadrados_por_bedrooms[test$property_type == "Apartamento" & test$metros_cuadrados_por_bedrooms > max_media_m2_apart] <- max_media_m2_apart

test$metros_cuadrados <- ifelse(test$property_type == "Apartamento" & (test$metros_cuadrados/test$bedrooms) < min_media_m2_apart,
                                test$metros_cuadrados_por_bedrooms * test$bedrooms,
                                test$metros_cuadrados)
test$metros_cuadrados <- ifelse(test$property_type == "Apartamento" & (test$metros_cuadrados/test$bedrooms)  > max_media_m2_apart,
                                test$metros_cuadrados_por_bedrooms * test$bedrooms,
                                test$metros_cuadrados)

test$nueva_surface <- test$metros_cuadrados

# Calcular la media de nueva_surface por número de habitaciones
media_por_numero_habitaciones <- aggregate(nueva_surface ~ bedrooms, data = test, FUN = mean, na.rm = TRUE)
media_por_numero_habitaciones
summary(test$nueva_surface)

ggplot(test, aes(x = nueva_surface)) +
  geom_histogram(bins = 30, fill = "blue", color = "black", alpha = 0.7) +
  scale_y_log10()

# Reemplazar comas por nada y agregar un punto decimal
test$lat<- gsub(",", "", test$lat)  # Eliminar comas
test$lat <- sub("^(\\d{2})(\\d{3})(\\d{3})$", "\\1.\\2\\3", test$lat)  # Corregir ubicación del decimal

# Convertir la columna latitud a numérico
test$lat <- gsub(",", ".", test$lat)
test$lat<- as.character(test$lat)
test$lat<- gsub("\\.", "", test$lat)  # Eliminar puntos
test$lat<- sub("^(.{1})(.*)$", "\\1.\\2", test$lat)  # Corregir ubicación del decimal
head(test$lat)
test$lat<- as.numeric(test$lat)

# Reemplazar comas por nada y agregar un punto decimal
# Reemplazar las comas y corregir la ubicación del decimal en la columna "longitud"
test$lon <- gsub(",", ".", test$lon)
# Remover los puntos y corregir la ubicación del decimal en lon
test$lon <- as.character(test$lon)
test$lon <- gsub("\\.", "", test$lon)  # Eliminar puntos
test$lon<- sub("^(.{3})(.*)$", "\\1.\\2", test$lon)  # Corregir ubicación del decimal
head(test$lon)
test$lon <- gsub(",", ".", test$lon)
# Convertir la columna lon a numérico decimal
test$lon <- as.numeric(test$lon)

# Redondear los valores
test$lon <- round(test$lon, digits = 9)
###############Crear la variable garaje
test <- test %>%
  mutate(garaje = as.numeric(grepl("garaje", description)),
         parqueadero = as.numeric(grepl("parqueadero", description)),
         total_parqueo = garaje + parqueadero)

# Sustituir el valor "2" por "1" en total_parqueo
test$total_parqueo <- ifelse(test$total_parqueo == 2, 1, test$total_parqueo)
# Eliminamos las observaciones que no tienen información de latitud o longitud
# Identificar filas con valores no numéricos en lon
non_numeric_lon <- !is.na(test$lon) & !is.numeric(test$lon)
non_numeric_lat <- !is.na(test$lat) & !is.numeric(test$lat)

test$lon <- as.numeric(test$lon)
test$lat <- as.numeric(test$lat)

test <- test %>%
  filter(!is.na(lat) & !is.na(lon))

# Observamos la primera visualización
leaflet() %>%
  addTiles() %>%
  addCircles(lng = test$lon, lat = test$lat)

# Crear una variable binaria "tiene_terrazz" basada en la descripción
test$tiene_terraza <- as.numeric(grepl("terraza|azotea", test$description, ignore.case = TRUE))
table(test$tiene_terraza)
# Mostrar las primeras filas del dataframe con la nueva variable
head(train)
table(test$tiene_terraza)
casas_con_terrazas <- sum(test$tiene_terraza == 1)
casas_con_terrazas

## ---------------------------------Crear Piscina---------------------------------##
# Crear una variable binaria "piscina" basada en la descripción
test$Piscina <- as.numeric(grepl("piscina|picina", test$description, ignore.case = TRUE))

# Mostrar las primeras filas del dataframe con la nueva variable
head(test)
table(test$Piscina)
casas_con_piscina1 <- sum(test$Piscina == 1)
casas_con_piscina1

## ---------------------------------Crear Gimnasio---------------------------------##

test$Gimnasio <- as.numeric(grepl("gimnasio|gym", test$description, ignore.case = TRUE))

# Mostrar las primeras filas del dataframe con la nueva variable
head(test)
table(test$Gimnasio)
casas_con_gimnasio1 <- sum(test$Gimnasio == 1)
casas_con_gimnasio1

## ---------------------------------Crear Chiminea---------------------------------##

test$Chimenea <- as.numeric(grepl("chimenea|chiminea", test$description, ignore.case = TRUE))
head(test)
table(test$Chimenea)
casas_con_chimenea1 <- sum(test$Chimenea == 1)
casas_con_chimenea1

## ---------------------------------Seguridad Privada---------------------------------##

test$Seguridad <- as.numeric(grepl("vigilancia|sistema de seguridad|seguridad privada|seguridad 24|seguridad las veinticuatro horas|seguridad las 24", test$description, ignore.case = TRUE))
head(test)
table(test$Seguridad)
casas_con_seguridad1 <- sum(test$Seguridad == 1)
casas_con_seguridad1

## ---------------------------------Sala BBQ----------------------------------------##
test$tiene_bbq <- grepl("BBQ|barbacoa", test$description, ignore.case = TRUE)

# Convertir valores lógicos en 1 (Tiene BBQ) y 0 (No tiene BBQ)
test$tiene_bbq <- as.numeric(test$tiene_bbq)

# Verificar la nueva variable "tiene_bbq"
table(test$tiene_bbq)


# Definir los límites geográficos de Bogotá
limites_bogota <- getbb("Bogotá, Colombia")

# Filtrar observaciones dentro de los límites de Bogotá
test_filtrado_bogota <- test %>%
  filter(
    between(lon, limites_bogota[1, "min"], limites_bogota[1, "max"]) &
      between(lat, limites_bogota[2, "min"], limites_bogota[2, "max"])
  )

# Crear una nueva columna "color" basada en el tipo de propiedad
test <- test %>%
  mutate(color = case_when(property_type == "Apartamento" ~ "#2A9D8F",
                           property_type == "Casa" ~ "#F4A261"))

# Encontrar el centro del mapa
latitud_central <- mean(test$lat)
longitud_central <- mean(test$lon)

# Crear el mensaje en el popup con HTML
html <- paste0("<br> <b>Area:</b> ",
               as.integer(test$nueva_surface), " mt2",
               "<br> <b>Tipo de inmueble:</b> ",
               test$property_type,
               "<br> <b>Numero de alcobas:</b> ",
               as.integer(test$rooms),
               "<br> <b>Numero de baños:</b> ",
               as.integer(test$bathrooms))

##---------------------------- DISTANCIA PARQUES  ----------------------------###
##----------------------------------------------------------------------------###

ubicacion <- "Chapinero, Bogotá, Colombia"
bbox_chapinero <- getbb(ubicacion)
bbox_chapinero

parques <- opq(bbox = getbb("Bogotá, Colombia")) %>%
  add_osm_feature(key = "leisure" , value = "park")

parques_sf <- osmdata_sf(parques)
parques_geometria <- parques_sf$osm_polygons %>%
  select(osm_id, name)

centroides <- st_centroid(parques_geometria)

# Encontrar el centro del mapa
latitud_central <- mean(bbox_chapinero["lat"])
longitud_central <- mean(bbox_chapinero["lon"])

test_sf <- st_as_sf(test, coords = c("lon", "lat"), crs = 4326)
centroides_sf <- st_as_sf(centroides, coords = c("x", "y"), crs = 4326)
distancias <- st_distance(test_sf, centroides_sf)
dist_min <- apply(distancias, 1, min)
test_sf$distancia_parque <- dist_min


##---------------------------- CENTROS COMERCIALES----------------------------###
##----------------------------------------------------------------------------###

# Definir la búsqueda de centros comerciales en Chapinero
centros_comerciales_chapinero <- opq(bbox = bbox_chapinero) %>%
  add_osm_feature(key = "shop", value = "mall")

# Cambiar el formato para que sea un objeto sf (simple features)
centros_comerciales_chapinero_sf <- osmdata_sf(centros_comerciales_chapinero)
centros_comerciales_geometria <- centros_comerciales_chapinero_sf$osm_points %>%
  select(osm_id, name)
centroides_centros_comerciales <- st_centroid(centros_comerciales_geometria)

# Calcular las distancias para cada combinación inmueble - centro comercial
distancias_centros_comerciales <- st_distance(test_sf, centroides_centros_comerciales)
dist_min_centros_comerciales <- apply(distancias_centros_comerciales, 1, min)
test_sf$distancia_centros_comerciales <- dist_min_centros_comerciales

test_sf$distancia_parque <- dist_min
test_sf$distancia_centros_comerciales <- dist_min_centros_comerciales

###------------------SUPERMERCADOS Y OTROS ESTABLECIMIENTOS------------------###
##--------------------------------------------------------------------------###


## Definir la búsqueda de supermercados en Chapinero
establecimientos_chapinero <- opq(bbox = bbox_chapinero) %>%
  add_osm_feature(key = "shop", value = c("supermarket", "grocery", "convenience", "bakery", "corner shop"))
 establecimientos_chapinero_sf <- osmdata_sf(establecimientos_chapinero)

# De las features de supermercados, nos interesa su geometría y ubicación
establecimientos_geometria <-  establecimientos_chapinero_sf$osm_points %>%
  select(osm_id, name)
centroides_establecimientos <- st_centroid( establecimientos_geometria)

# Calcular las distancias para cada combinación inmueble - supermercado
distancias_establecimientos <- st_distance(test_sf, centroides_establecimientos)
dist_min_establecimientos <- apply(distancias_establecimientos, 1, min)
test_sf$distancia_establecimientos <- dist_min_establecimientos


###-------------------------TRANSMILENIO Y TRANSPORTE PUBLICO-----------------------------###
##----------------------------------------------------------------------------------------###

# Definir la ubicación de interés (Chapinero, Bogotá, Colombia)
ubicacion <- "Chapinero, Bogotá, Colombia"

# Obtener los límites geográficos (BBOX) de la ubicación
bbox_chapinero <- getbb(ubicacion)

# Definir la búsqueda de estaciones de TransMilenio en Chapinero
transmilenio_transporte_chapinero <- opq(bbox = bbox_chapinero) %>%
  add_osm_feature(key = "network", value = "TransMilenio")

transmilenio_transporte_chapinero_sf <- osmdata_sf(transmilenio_transporte_chapinero)
transmilenio_transporte_geometria <- transmilenio_transporte_chapinero_sf$osm_points %>%
  select(osm_id, geometry)

# Calcular el centroide de cada estación de TransMilenio para aproximar su ubicación como un solo punto
centroides_transmilenio_transporte <- st_centroid(transmilenio_transporte_chapinero_sf$osm_points)
distancias_transmilenio_transporte <- st_distance(test_sf, centroides_transmilenio_transporte)

# Encontrar la distancia mínima a una estación de TransMilenio
dist_min_transmilenio_transporte <- apply(distancias_transmilenio_transporte, 1, min)
test_sf$distancia_transmilenio_transporte <- dist_min_transmilenio_transporte
head(test_sf)

###----------------------CENTROS EDUCATIVOS EN CHAPINERO-------------------------- ###
##--------------------------------------------------------------------------------------###

etiquetas_educativos <- c("school", "college", "university", "library", "kindergarten")

# Definir la ubicación de interés (en este caso, Bogotá, Colombia) y buscar universidades
centros_educativos_chapinero <- opq(bbox = getbb("Bogotá, Colombia")) %>%
  add_osm_feature(key = "amenity", value = etiquetas_educativos)

# Cambiar el formato para que sea un objeto sf (simple features)
centros_educativos_chapinero_sf <- osmdata_sf(centros_educativos_chapinero)
centros_educativos_geometria <- centros_educativos_chapinero_sf$osm_points %>%
  select(osm_id, name)

# Calcular el centroide de cada universidad para aproximar su ubicación como un solo punto
centroides_centros_educativos <- st_centroid(centros_educativos_geometria)
distancias_centros_educativos <- st_distance(test_sf, centroides_centros_educativos)
dist_min_centros_educativos <- apply(distancias_centros_educativos, 1, min)

# Agregar la distancia mínima como una nueva columna en 'test_sf'
test_sf$distancia_centros_educativos <- dist_min_centros_educativos
head(test_sf)

###-----------------DISTANCIA RESTAURANTES EN CHAPINERO ----------------------###
###---------------------------------------------------------------------------###


# Definir la búsqueda de restaurantes y bares en un solo grupo en Chapinero
restaurantes_bares <- opq(bbox = bbox_chapinero) %>%
  add_osm_feature(key = "amenity", value = c("restaurant", "bar"))
restaurantes_bares_sf <- osmdata_sf(restaurantes_bares)

# De las features de restaurantes y bares en Chapinero, nos interesa su geometría y ubicación
restaurantes_bares_geometria <- restaurantes_bares_sf$osm_points %>%
  select(osm_id)
centroides_restaurantes_bares <- st_centroid(restaurantes_bares_geometria)
distancias_restaurantes_bares <- st_distance(test_sf, centroides_restaurantes_bares)

# Encontrar la distancia mínima a un restaurante o bar en Chapinero
dist_min_restaurantes_bares <- apply(distancias_restaurantes_bares, 1, min)
test_sf$distancia_restaurantes_bares <- dist_min_restaurantes_bares
head(test_sf)

##-----------------------DISTANCIA INSTITUCIONES FINANCIERAS----------------------------------##

# Definir la ubicación de interés (Chapinero, Bogotá, Colombia)
ubicacion <- "Chapinero, Bogotá, Colombia"

# Obtener los límites geográficos (BBOX) de la ubicación
bbox_chapinero <- getbb(ubicacion)

# Definir la ubicación de interés y buscar instituciones financieras
bancos <- opq(bbox = bbox_chapinero) %>%
  add_osm_feature(key = "amenity", value = "bank")

bancos_sf <- osmdata_sf(bancos)
bancos_geometria <- bancos_sf$osm_polygons %>%
  select(osm_id, name)

centroides_bancos <- st_centroid(bancos_geometria)

# Encontrar el centro del mapa
latitud_central <- mean(bbox_bogota["lat"])
longitud_central <- mean(bbox_bogota["lon"])

test1_sf <- st_as_sf(test, coords = c("lon", "lat"), crs = 4326)
centroides_bancos_sf <- st_as_sf(centroides_bancos, coords = c("x", "y"), crs = 4326)
distancias_bancos <- st_distance(test1_sf, centroides_bancos_sf)
dist_min_bancos <- apply(distancias_bancos, 1, min)
test1_sf$distancias_bancos <- dist_min_bancos

distanc1 <- test_sf %>% select(30:35) %>% st_drop_geometry()
distancp <- test1_sf %>% select(30) %>% st_drop_geometry()
test <- test %>% bind_cols(distanc1,distancp)


# Imputar los Valores para los Baños
test_b_b <- test[complete.cases(test[c("bedrooms", "bathrooms")]), ]
mediat_b_b <- mean(test_b_b$bedrooms / test_b_b$bathrooms)
test$bathrooms[is.na(test$bathrooms)] <- test$bedrooms[is.na(test$bathrooms)] / mediat_b_b
test$bathrooms <- round(test$bathrooms) 

test <- data.frame(localidad = "Chapinero", test)
test$Estrato <- "4"
test$Estrato <- as.numeric(test$Estrato)


# Renombrar las variables para una mayor comprensión de las variables que estamos trabajando
test <- test %>% rename(Precio=price) 
test <- test %>% rename(Habitaciones=bedrooms)
test <- test %>% rename(Baños=bathrooms) 
test <- test %>% rename(Area=nueva_surface)
test <- test %>% rename(M2_por_Habitación=metros_cuadrados_por_bedrooms)
test <- test %>% rename(Terraza=tiene_terraza) 
test <- test %>% rename(Sala_BBQ=tiene_bbq) 
test <- test %>% rename(Garaje=total_parqueo) 
test <- test %>% rename(Dist_Parques=distancia_parque) 
test <- test %>% rename(Dist_Transp_Publico=distancia_transmilenio_transporte) 
test <- test %>% rename(Dist_Establecimientos=distancia_establecimientos)
test <- test %>% rename(Dist_C_Comerc=distancia_centros_comerciales) 
test <- test %>% rename(Dist_Centros_Educ=distancia_centros_educativos)
test <- test %>% rename(Dist_Restaurantes=distancia_restaurantes_bares) 
test <- test %>% rename(Dist_Bancos=distancias_bancos) 

test <- test %>%
  mutate(M2_por_Habitación = round(Area / Habitaciones))

test$Precio_M2 <- NA
test$lPrecio <- NA

##--------------------------- Bases de Datos Casas y Apartamentos------------------##

##-----------------------------CASAS----------------------------------------

test_casas <- test[train$property_type == "Casa", c("property_id","title", "month", "year", "localidad","Estrato", "Precio", "lPrecio", 
                                                      "Precio_M2", "Habitaciones", "Baños", "Area","M2_por_Habitación", "lat", "lon", "Terraza", 
                                                      "Garaje", "Sala_BBQ","Piscina","Gimnasio", "Chimenea","Seguridad",
                                                      "Dist_Parques", "Dist_Transp_Publico", "Dist_Establecimientos", 
                                                      "Dist_C_Comerc", "Dist_Centros_Educ", "Dist_Restaurantes", "Dist_Bancos")]

Tabla_test_casas <- "C:/Output R/Taller 2/Taller_2/Tabla_test_casas.xlsx"  
write_xlsx(test_casas, Tabla_test_casas)

##---------------------------------------Apartamentos ----------------------------------------##

test_apart <- test[train$property_type == "Apartamento", c("property_id","title", "month", "year", "localidad","Estrato", "Precio", "lPrecio", 
                                                           "Precio_M2", "Habitaciones", "Baños", "Area","M2_por_Habitación", "lat", "lon", "Terraza", 
                                                           "Garaje", "Sala_BBQ","Piscina","Gimnasio", "Chimenea","Seguridad",
                                                           "Dist_Parques", "Dist_Transp_Publico", "Dist_Establecimientos", 
                                                           "Dist_C_Comerc", "Dist_Centros_Educ", "Dist_Restaurantes", "Dist_Bancos")]


Tabla_test_apart <- "C:/Output R/Taller 2/Taller_2/Tabla_test_apart.xlsx"  
write_xlsx(test_apart, Tabla_test_apart)

##-----------Elaboración de Modelos para pronosticar el Precio de las Casas y Apartamentos-------------------##
##-----------------------------------------------------------------------------------------------------------##

#----------------------------------------------- REGRESION LINEAL---------------------------------------------

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

# -------------------------------CREACION DE OTRAS VARIABLES-------------------------- # 
train_casas1$M2_por_Habitacion<- train_casas1$Area/train_casas1$Habitaciones
train_casas1$Habitaciones2 <- train_casas1$Habitaciones^2
train_casas1$M2_por_Habitacion_Garaje <- train_casas1$M2_por_Habitacion * train_casas1$Garaje
train_casas1$Sala_BBQ_terraza <- train_casas1$Sala_BBQ * train_casas1$Terraza
train_casas1$year <- as.character(train_casas1$year)
train_casas1$month <- as.character(train_casas1$month)
train_casas1$Fecha <- as.Date(paste0(train_casas1$year, "-", train_casas1$month, "-01"))
train_casas1$Fecha <- as.Date(train_casas1$Fecha)

# -------------------------------MODELOS DE REGRESION LINEAL CASAS--------------------------------# 

Model1 <- lm(lPrecio ~ Estrato + Habitaciones + Habitaciones2 + Baños + M2_por_Habitacion + Terraza + Garaje + Sala_BBQ + Gimnasio + Sala_BBQ_terraza + Chimenea + Seguridad + Dist_Parques + 
               Dist_Transp_Publico + Dist_Establecimientos + Dist_C_Comerc + Dist_Centros_Educ + Dist_Restaurantes + Dist_Bancos, data = train_casas1)
Model1_stargazer <- stargazer(Model1, type="text", omit.stat=c("ser","f","adj.rsq"))
Model1_stargazer <- as.data.frame(Model1_stargazer)
train_casas1$Pred_Precios <- predict(Model1, newdata = train_casas1)

# Calcular el promedio de las predicciones
lPrecios_promedio <- aggregate(train_casas1$lPrecio, by = list(train_casas1$year, train_casas1$month), FUN = mean)
colnames(lPrecios_promedio) <- c("Year", "Month", "Precio_Promedio_Casas")

lPrecios_promedio_pred <- aggregate(train_casas1$Pred_Precios, by = list(train_casas1$year, train_casas1$month), FUN = mean)
colnames(lPrecios_promedio_pred) <- c("Year", "Month", "Precio_Promedio_Casas")

# Crear un único conjunto de datos con las dos series de tiempo
lPrecios_combinado <- rbind(
  data.frame(Year = lPrecios_promedio$Year, Month = lPrecios_promedio$Month, Precio_Promedio = lPrecios_promedio$Precio_Promedio_Casas, Tipo = "Observado"),
  data.frame(Year = lPrecios_promedio_pred$Year, Month = lPrecios_promedio_pred$Month, Precio_Promedio = lPrecios_promedio_pred$Precio_Promedio_Casas, Tipo = "Predicción")
)

# Crear un gráfico de línea con ambas series
g_ols <- ggplot(lPrecios_combinado, aes(x = as.Date(paste(Year, Month, "01", sep = "-")), y = Precio_Promedio, color = Tipo)) +
  geom_line(linewidth = 1.5) +
  labs(
    title = "Evolución Precios Promedio de Casas OLS",
    x = "Fecha",
    y = "Precio Promedio"
  ) +
  scale_color_manual(values = c("Observado" = "blue", "Predicción" = "red")) +
  guides(color = guide_legend(title = NULL)) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.line = element_line(color = "gray"),
    panel.background = element_rect(fill = "transparent", color = NA),
    panel.grid = element_line(color = "gray")
  )
g_ols 


# -------------------------------MODELOS DE RIDGE CASAS--------------------------------# 

X <- as.matrix(train_casas1[, c("Estrato", "Habitaciones", "Habitaciones2", "Baños", "M2_por_Habitacion", "Terraza", "Garaje", "Sala_BBQ", "Gimnasio", "Sala_BBQ_terraza", "Chimenea", "Seguridad", "Dist_Parques", "Dist_Transp_Publico", "Dist_Establecimientos", "Dist_C_Comerc", "Dist_Centros_Educ", "Dist_Restaurantes", "Dist_Bancos")])
y <- train_casas1$lPrecio

# Ajustar un modelo de regresión Ridge
ridge_model <- glmnet(X, y, alpha = 0)  # alpha = 0 para regresión Ridge
dev.new()
g_mse <- plot(ridge_model, xvar = "lambda")

# Seleccionar el valor óptimo de lambda
cv_ridge <- cv.glmnet(X, y, alpha = 0)  # alpha = 0 para regresión Ridge
g_coef <- plot(cv_ridge)
lambda_optimo <- cv_ridge$lambda.min
lambda_optimo

# Ajustar el modelo con el valor óptimo de lambda
Model2 <- glmnet(X, y, alpha = 0, lambda = lambda_optimo)
train_casas1$Pred_Precios_rg <- predict(Model2, s = lambda_optimo, newx = X)
coef(Model2)

# Calcular el promedio de las predicciones
lPrecios_promedio_pred_rg <- aggregate(train_casas1$Pred_Precios_rg, by = list(train_casas1$year, train_casas1$month), FUN = mean)
colnames(lPrecios_promedio_pred_rg) <- c("Year", "Month", "Precio_Promedio_Casas")

# Crear un único conjunto de datos con las dos series de tiempo
lPrecios_combinado_rd <- rbind(
  data.frame(Year = lPrecios_promedio$Year, Month = lPrecios_promedio$Month, Precio_Promedio = lPrecios_promedio$Precio_Promedio_Casas, Tipo = "Observado"),
  data.frame(Year = lPrecios_promedio_pred_rg$Year, Month = lPrecios_promedio_pred_rg$Month, Precio_Promedio = lPrecios_promedio_pred_rg$Precio_Promedio_Casas, Tipo = "Predicción")
)

# Crear un gráfico de línea con ambas series
g_rd <- ggplot(lPrecios_combinado_rd, aes(x = as.Date(paste(Year, Month, "01", sep = "-")), y = Precio_Promedio, color = Tipo)) +
  geom_line(linewidth = 1.5) +
  labs(
    title = "Evolución Precios Promedio de Casas Ridge",
    x = "Fecha",
    y = "Precio Promedio"
  ) +
  scale_color_manual(values = c("Observado" = "blue", "Predicción" = "red")) +
  guides(color = guide_legend(title = NULL)) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.line = element_line(color = "gray"),
    panel.background = element_rect(fill = "transparent", color = NA),
    panel.grid = element_line(color = "gray")
  )
g_rd


# -------------------------------MODELOS LASSO CASAS--------------------------------# 

# Ajustar un modelo de regresión Lasso
lasso_model <- glmnet(X, y, alpha = 1)
dev.new()
g_mse <- plot(lasso_model, xvar = "lambda")

# Seleccionar el valor óptimo de lambda
cv_lasso <- cv.glmnet(X, y, alpha = 1)  # alpha = 1 para regresión Lasso
g_coef <- plot(cv_lasso)
lambda_optimo_lasso <- cv_lasso$lambda.min
lambda_optimo_lasso

# Ajustar el modelo Lasso con el valor óptimo de lambda
Model3 <- glmnet(X, y, alpha = 1, lambda = lambda_optimo_lasso)
train_casas1$Pred_Precios_ls <- predict(Model3, s = lambda_optimo_lasso, newx = X)
coef(Model3)

# Calcular el promedio de las predicciones
lPrecios_promedio_pred_ls <- aggregate(train_casas1$Pred_Precios_ls, by = list(train_casas1$year, train_casas1$month), FUN = mean)
colnames(lPrecios_promedio_pred_ls) <- c("Year", "Month", "Precio_Promedio_Casas")

# Crear un único conjunto de datos con las dos series de tiempo
lPrecios_combinado_ls <- rbind(
  data.frame(Year = lPrecios_promedio$Year, Month = lPrecios_promedio$Month, Precio_Promedio = lPrecios_promedio$Precio_Promedio_Casas, Tipo = "Observado"),
  data.frame(Year = lPrecios_promedio_pred_ls$Year, Month = lPrecios_promedio_pred_ls$Month, Precio_Promedio = lPrecios_promedio_pred_ls$Precio_Promedio_Casas, Tipo = "Predicción")
)

# Crear un gráfico de línea con ambas series
g_ls <- ggplot(lPrecios_combinado_ls, aes(x = as.Date(paste(Year, Month, "01", sep = "-")), y = Precio_Promedio, color = Tipo)) +
  geom_line(linewidth = 1.5) +
  labs(
    title = "Evolución Precios Promedio de Casas Lasso",
    x = "Fecha",
    y = "Precio Promedio"
  ) +
  scale_color_manual(values = c("Observado" = "blue", "Predicción" = "red")) +
  guides(color = guide_legend(title = NULL)) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.line = element_line(color = "gray"),
    panel.background = element_rect(fill = "transparent", color = NA),
    panel.grid = element_line(color = "gray")
  )
g_ls


# -------------------------------MODELO ELASTIC NET CASAS--------------------------------# 

# Modelo de regresión Elastic Net
Elasticnet_model <- glmnet(X, y, alpha = 0.5)  # alpha = 0.5 para Elastic Net

# Seleccionar el valor óptimo de lambda
cv_elasticnet <- cv.glmnet(X, y, alpha = 0.5)  
lambda_optimo_en <- cv_elasticnet$lambda.min
lambda_optimo_en

# Ajustar el modelo Elastic Net con el valor óptimo de lambda
Model4 <- glmnet(X, y, alpha = 0.5, lambda = lambda_optimo_en)
train_casas1$Pred_Precios_en <- predict(Model4, s = lambda_optimo_en, newx = X)
coef(Model4)

# Calcular el promedio de las predicciones
lPrecios_promedio_pred_en <- aggregate(train_casas1$Pred_Precios_en, by = list(train_casas1$year, train_casas1$month), FUN = mean)
colnames(lPrecios_promedio_pred_en) <- c("Year", "Month", "Precio_Promedio_Casas")

# Crear un único conjunto de datos con las dos series de tiempo
lPrecios_combinado_en <- rbind(
  data.frame(Year = lPrecios_promedio$Year, Month = lPrecios_promedio$Month, Precio_Promedio = lPrecios_promedio$Precio_Promedio_Casas, Tipo = "Observado"),
  data.frame(Year = lPrecios_promedio_pred_en$Year, Month = lPrecios_promedio_pred_en$Month, Precio_Promedio = lPrecios_promedio_pred_en$Precio_Promedio_Casas, Tipo = "Predicción")
)

# Crear un gráfico de línea con ambas series
g_en <- ggplot(lPrecios_combinado_en, aes(x = as.Date(paste(Year, Month, "01", sep = "-")), y = Precio_Promedio, color = Tipo)) +
  geom_line(linewidth = 1.5) +
  labs(
    title = "Evolución Precios Promedio de Casas Elastic Net",
    x = "Fecha",
    y = "Precio Promedio"
  ) +
  scale_color_manual(values = c("Observado" = "blue", "Predicción" = "red")) +
  guides(color = guide_legend(title = NULL)) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.line = element_line(color = "gray"),
    panel.background = element_rect(fill = "transparent", color = NA),
    panel.grid = element_line(color = "gray")
  )
g_en


Tabla_train_casas <- "C:/Output R/Taller 2/Taller_2/tabla_train_casas.xlsx"  
write_xlsx(train_casas1, Tabla_train_casas)


# -------------------------------CREACION DE OTRAS VARIABLES-------------------------- # 
train_apart1$M2_por_Habitacion<- train_apart1$Area/train_apart1$Habitaciones
train_apart1$Habitaciones2<- train_apart1$Habitaciones^2
train_apart1$M2_por_Habitacion_Garaje <- train_apart1$M2_por_Habitacion * train_apart1$Garaje
train_apart1$Sala_BBQ_terraza <- train_apart1$Sala_BBQ * train_apart1$Terraza
train_apart1$year <- as.character(train_apart1$year)
train_apart1$month <- as.character(train_apart1$month)
train_apart1$Fecha <- as.Date(paste0(train_apart1$year, "-", train_apart1$month, "-01"))
train_apart1$Fecha <- as.Date(train_apart1$Fecha)


# -------------------------------MODELOS DE REGRESION LINEAL APARTAMENTOS----------------------------# 

Model5 <- lm(lPrecio ~ Estrato + Habitaciones + Habitaciones2 + Baños + M2_por_Habitacion + Terraza + Garaje + M2_por_Habitacion_Garaje + Sala_BBQ + Gimnasio + Chimenea + Seguridad + Dist_Parques + 
               Dist_Transp_Publico + Dist_Establecimientos + Dist_C_Comerc + Dist_Centros_Educ + Dist_Restaurantes + Dist_Bancos, data = train_apart1)
Model5_stargazer <- stargazer(Model5, type="text", omit.stat=c("ser","f","adj.rsq"))
Model5_stargazer <- as.data.frame(Model5_stargazer)
train_apart1$Pred_Precios1 <- predict(Model5, newdata = train_apart1)

lPrecios_promedio1 <- aggregate(train_apart1$lPrecio, by = list(train_apart1$Fecha), FUN = mean)
colnames(lPrecios_promedio1) <- c("Fecha", "Precio_Promedio_Apart")
lPrecios_promedio1$Tipo <- "Observado"

lPrecios_promedio_pred1 <- aggregate(train_apart1$Pred_Precios1, by = list(train_apart1$Fecha), FUN = mean)
colnames(lPrecios_promedio_pred1) <- c("Fecha", "Precio_Promedio_Apart")
lPrecios_promedio_pred1$Tipo <- "Predicción"
lPrecios_promedio_pred1

g_ols <- ggplot() +
  geom_line(data = lPrecios_promedio1, aes(x = Fecha, y = Precio_Promedio_Apart, color = "Observado"), size = 1) +
  geom_line(data = lPrecios_promedio_pred1, aes(x = Fecha, y = Precio_Promedio_Apart, color = "Predicción"), size = 1) +
  labs(
    title = "Evolución Precios Promedio de Apart OLS",
    x = "Fecha",
    y = "Precio Promedio"
  ) +
  scale_color_manual(values = c("Observado" = "blue", "Predicción" = "red")) +
  guides(color = guide_legend(title = NULL)) + 
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.line = element_line(color = "gray"),
    panel.background = element_rect(fill = "transparent", color = NA),
    panel.grid = element_line(color = "gray")
  )
g_ols

# -------------------------------MODELOS DE RIDGE APARTAMENTO--------------------------------# 

X <- as.matrix(train_apart1[, c("Estrato", "Habitaciones", "Habitaciones2", "Baños", "M2_por_Habitacion", "Terraza", "Garaje","M2_por_Habitacion_Garaje", "Sala_BBQ", "Gimnasio", "Chimenea", "Seguridad", "Dist_Parques", "Dist_Transp_Publico", "Dist_Establecimientos", "Dist_C_Comerc", "Dist_Centros_Educ", "Dist_Restaurantes", "Dist_Bancos")])
y <- train_apart1$lPrecio

# Modelo de regresión Ridge
ridge_model1 <- glmnet(X, y, alpha = 0) 
dev.new()
g_mse <- plot(ridge_model1, xvar = "lambda")

# Seleccionar el valor óptimo de lambda
cv_ridge1 <- cv.glmnet(X, y, alpha = 0) 
g_coef <- plot(cv_ridge1)
lambda_opt_apart <- cv_ridge1$lambda.min
lambda_opt_apart

# Modelo con el valor óptimo de lambda
Model6 <- glmnet(X, y, alpha = 0, lambda = lambda_opt_apart)
train_apart1$Pred_Precios_rg1 <- predict(Model6, s = lambda_opt_apart, newx = X)
coef(Model6)


lPrecios_promedio_pred_rg1 <- aggregate(train_apart1$Pred_Precios_rg1, by = list(train_apart1$Fecha), FUN = mean)
colnames(lPrecios_promedio_pred_rg1) <- c("Fecha", "Precio_Promedio_Apart")
lPrecios_promedio_pred_rg1$Tipo <- "Predicción"



g_rg <- ggplot() +
  geom_line(data = lPrecios_promedio1, aes(x = Fecha, y = Precio_Promedio_Apart, color = "Observado"), size = 1) +
  geom_line(data = lPrecios_promedio_pred_rg1, aes(x = Fecha, y = Precio_Promedio_Apart, color = "Predicción"), size = 1) +
  labs(
    title = "Evolución Precios Promedio de Apart Ridge",
    x = "Fecha",
    y = "Precio Promedio"
  ) +
  scale_color_manual(values = c("Observado" = "blue", "Predicción" = "red")) +
  guides(color = guide_legend(title = NULL)) + 
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.line = element_line(color = "gray"),
    panel.background = element_rect(fill = "transparent", color = NA),
    panel.grid = element_line(color = "gray")
  )
g_rg



# -------------------------------MODELOS LASSO APARTAMENTO--------------------------------# 

# Ajustar un modelo de regresión Lasso
lasso_model1 <- glmnet(X, y, alpha = 1)
dev.new()
g_mse1 <- plot(lasso_model1, xvar = "lambda")

# Seleccionar el valor óptimo de lambda
cv_lasso1 <- cv.glmnet(X, y, alpha = 1)  # alpha = 1 para regresión Lasso
g_coef1 <- plot(cv_lasso1)
lambda_opt_ls_apart <- cv_lasso1$lambda.min
lambda_opt_ls_apart

# Ajustar el modelo Lasso con el valor óptimo de lambda
Model7 <- glmnet(X, y, alpha = 1, lambda = lambda_opt_ls_apart)
train_apart1$Pred_Precios_ls1 <- predict(Model7, s = lambda_opt_ls_apart, newx = X)
coef(Model7)


lPrecios_promedio1 <- aggregate(train_apart1$lPrecio, by = list(train_apart1$Fecha), FUN = mean)
colnames(lPrecios_promedio1) <- c("Fecha", "Precio_Promedio_Apart")
lPrecios_promedio1$Tipo <- "Observado"

lPrecios_promedio_pred_ls1 <- aggregate(train_apart1$Pred_Precios_ls1, by = list(train_apart1$Fecha), FUN = mean)
colnames(lPrecios_promedio_pred_ls1) <- c("Fecha", "Precio_Promedio_Apart")
lPrecios_promedio_pred_ls1$Tipo <- "Predicción"


g_ls <- ggplot() +
  geom_line(data = lPrecios_promedio1, aes(x = Fecha, y = Precio_Promedio_Apart, color = "Observado"), size = 1) +
  geom_line(data = lPrecios_promedio_pred_ls1, aes(x = Fecha, y = Precio_Promedio_Apart, color = "Predicción"), size = 1) +
  labs(
    title = "Evolución Precios Promedio de Apart Lasso",
    x = "Fecha",
    y = "Precio Promedio"
  ) +
  scale_color_manual(values = c("Observado" = "blue", "Predicción" = "red")) +
  guides(color = guide_legend(title = NULL)) + 
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.line = element_line(color = "gray"),
    panel.background = element_rect(fill = "transparent", color = NA),
    panel.grid = element_line(color = "gray")
  )
g_ls


# ------------------------------MODELO ELASTIC NET APARTAMENTO-------------------------------# 

# Modelo de regresión Elastic Net
Elasticnet_model1 <- glmnet(X, y, alpha = 0.5)  # alpha = 0.5 para Elastic Net

# Seleccionar el valor óptimo de lambda
cv_elasticnet1 <- cv.glmnet(X, y, alpha = 0.5)  
lambda_opt_en_apart <- cv_elasticnet1$lambda.min
lambda_opt_en_apart

# Ajustar el modelo Elastic Net con el valor óptimo de lambda
Model8 <- glmnet(X, y, alpha = 0.5, lambda = lambda_opt_en_apart)
train_apart1$Pred_Precios_en1 <- predict(Model8, s = lambda_opt_en_apart, newx = X)
coef(Model8)

lPrecios_promedio1 <- aggregate(train_apart1$lPrecio, by = list(train_apart1$Fecha), FUN = mean)
colnames(lPrecios_promedio1) <- c("Fecha", "Precio_Promedio_Apart")
lPrecios_promedio1$Tipo <- "Observado"

lPrecios_promedio_pred_en1 <- aggregate(train_apart1$Pred_Precios_en1, by = list(train_apart1$Fecha), FUN = mean)
colnames(lPrecios_promedio_pred_en1) <- c("Fecha", "Precio_Promedio_Apart")
lPrecios_promedio_pred_en1$Tipo <- "Predicción"


g_ls <- ggplot() +
  geom_line(data = lPrecios_promedio1, aes(x = Fecha, y = Precio_Promedio_Apart, color = "Observado"), size = 1) +
  geom_line(data = lPrecios_promedio_pred_en1, aes(x = Fecha, y = Precio_Promedio_Apart, color = "Predicción"), size = 1) +
  labs(
    title = "Evolución Precios Promedio de Apart Elastic Net",
    x = "Fecha",
    y = "Precio Promedio"
  ) +
  scale_color_manual(values = c("Observado" = "blue", "Predicción" = "red")) +
  guides(color = guide_legend(title = NULL)) +  
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.line = element_line(color = "gray"),
    panel.background = element_rect(fill = "transparent", color = NA),
    panel.grid = element_line(color = "gray")
  )
g_ls


tabla_train_apart <- "C:/Output R/Taller 2/Taller_2/tabla_train_apart.xlsx"  
write_xlsx(train_apart1, tabla_train_apart)

# ------------------------------PRONOSTICOS FUERA DE MUESTRA-------------------------------# 

# -------------------------------CREACION DE OTRAS VARIABLES CASAS-------------------------# 

test_casas1$Habitaciones2 <- test_casas1$Habitaciones^2
test_casas1$M2_por_Habitacion_Garaje <- test_casas1$M2_por_Habitacion * test_casas1$Garaje
test_casas1$Sala_BBQ_terraza <- test_casas1$Sala_BBQ * test_casas1$Terraza
test_casas1$year <- as.character(test_casas1$year)
test_casas1$month <- as.character(test_casas1$month)
test_casas1$Fecha <- as.Date(paste0(test_casas1$year, "-", test_casas1$month, "-01"))
test_casas1$Fecha <- as.Date(test_casas1$Fecha)

test_casas1_1 <- "C:/Output R/Taller 2/Taller_2/tabla_train_apart.xlsx"  
write_xlsx(test_casas1, tabla_train_apart)

# -------------------------------CREACION DE OTRAS VARIABLES CASAS-------------------------# 

test_apart1$Habitaciones2 <- test_apart1$Habitaciones^2
test_apart1$M2_por_Habitacion_Garaje <- test_apart1$M2_por_Habitacion * test_apart1$Garaje
test_apart1$Sala_BBQ_terraza <- test_apart1$Sala_BBQ * test_apart1$Terraza
test_apart1$year <- as.character(test_apart1$year)
test_apart1$month <- as.character(test_apart1$month)
test_apart1$Fecha <- as.Date(paste0(test_apart1$year, "-", test_apart1$month, "-01"))
test_apart1$Fecha <- as.Date(test_apart1$Fecha)

# ------------------------------PRONOSTICOS FUERA DE MUESTRA OLS-------------------------------# 

Pred_casas_ols <- exp(predict(Model1, newdata = test_casas1))
Pred_apart_ols <- exp(predict(Model5, newdata = test_apart1))
submission_template$Pred_ols_fm <- c(Pred_casas_ols, Pred_apart_ols)


# ------------------------------PRONOSTICOS FUERA DE MUESTRA RIDGE-----------------------------# 

Xc_test <- as.matrix(test_casas1[, c("Estrato", "Habitaciones", "Habitaciones2", "Baños", "M2_por_Habitacion", "Terraza", "Garaje","M2_por_Habitacion_Garaje", "Sala_BBQ", "Gimnasio", "Chimenea", "Seguridad", "Dist_Parques", "Dist_Transp_Publico", "Dist_Establecimientos", "Dist_C_Comerc", "Dist_Centros_Educ", "Dist_Restaurantes", "Dist_Bancos")])
Xa_test <- as.matrix(test_apart1[, c("Estrato", "Habitaciones", "Habitaciones2", "Baños", "M2_por_Habitacion", "Terraza", "Garaje","M2_por_Habitacion_Garaje", "Sala_BBQ", "Gimnasio", "Chimenea", "Seguridad", "Dist_Parques", "Dist_Transp_Publico", "Dist_Establecimientos", "Dist_C_Comerc", "Dist_Centros_Educ", "Dist_Restaurantes", "Dist_Bancos")])


Pred_casas_rg <- exp(predict(Model2, s = lambda_opt_apart, newx = Xc_test))
Pred_apart_rg <- exp(predict(Model6, s = lambda_optimo, newx = Xa_test))
submission_template$Pred_rg_fm <- c(Pred_casas_rg, Pred_apart_rg)

tabla_pronost <- "C:/Output R/Taller 2/Taller_2/tabla_pronosticos.xlsx"  
write_xlsx(submission_template, tabla_pronost)
