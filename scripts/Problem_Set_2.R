#Problem Set 2
#Big Data y Machine Learning para Economía Aplicada

# Celin Hernández: 202210067
# Merit Tejeda: 202210104

#install.packages("readxl")
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

#carpeta <- "C:/Output R/Taller 2/Taller_2/stores/"
carpeta <- "C:/Users/Usuario/Documents/Machine Learning/Taller_2/stores/"

archivos_a_importar <- c("test.csv", "train.csv", "submission_template.csv")

# Crea bases de datos separadas para cada archivo
for (archivo in archivos_a_importar) {
  
  nombre <- tools::file_path_sans_ext(archivo)
  
  assign(nombre, read.csv(file.path(carpeta, archivo)))
}

estrato <- read_excel("C:/Users/Usuario/Documents/Machine Learning/Taller_2/stores/estrato.xlsx", sheet = "estrato")

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

# Primero normalizaremos todo el texto
# Todo en minuscula
train <- train %>%
  mutate(description = str_to_lower(description))
# Eliminamos tildes
train <- train %>%
  mutate(description = iconv(description, from = "UTF-8", to = "ASCII//TRANSLIT"))
# Eliminamos caracteres especiales
train <- train %>%
  mutate(description = str_replace_all(description, "[^[:alnum:]]", " "))
# Eliminamos espacios extras
train <- train %>%
  mutate(description = str_trim(gsub("\\s+", " ", description)))

#### otra forma
# Crear una nueva columna "metros_cuadrados" basada en la descripción
train$metros_cuadrados <- NA

for (i in 1:nrow(train)) {
  descripcion <- train$description[i]
  
  # Buscar todas las coincidencias de números seguidos de "mts" o variantes
  coincidencias <- gregexpr("\\d+(\\.\\d+)?\\s*(mts|mts²|m²|m2|metros cuadrados|metro cuadrado)", descripcion)
  
  # Extraer todas las coincidencias
  todas_coincidencias <- regmatches(descripcion, coincidencias)[[1]]
  
  # Encontrar la coincidencia más cercana a la palabra "metros"
  metros_cuadrados <- NA
  distancia_minima <- Inf
  
  for (coincidencia in todas_coincidencias) {
    distancia_a_metros <- min(gregexpr("metros", coincidencia)[[1]])
    
    if (distancia_a_metros < distancia_minima) {
      distancia_minima <- distancia_a_metros
      metros_cuadrados <- coincidencia
    }
  }
  
  # Si se encontró una coincidencia, extraer el valor de metros cuadrados
  if (!is.na(metros_cuadrados)) {
    valor_metros <- regmatches(metros_cuadrados, gregexpr("\\d+(\\.\\d+)?", metros_cuadrados))[[1]]
    train$metros_cuadrados[i] <- as.numeric(valor_metros)
  }
}

# Obtener un resumen estadístico de la variable "metros_cuadrados"
resumen <- summary(train$metros_cuadrados)
#sustituir con NA LOS 0 de los
# Reemplazar los valores 0 con NA en la columna metros_cuadrados
train <- train %>%
  mutate(metros_cuadrados = ifelse(metros_cuadrados == 0, NA, metros_cuadrados))

# Imprimir el resumen
print(resumen)

#### remplazar metros cuadrados solo cuando surface sea na
# Crear una nueva variable "nueva_surface" que reemplace "surface" con "metros_cuadrados" cuando "surface" sea NA
train <- train %>%
  mutate(nueva_surface = ifelse(is.na(surface_total), metros_cuadrados, surface_total))
train %>%
  summarise_all(~sum(is.na(.))) %>% transpose()
# Obtener un resumen estadístico de la variable "nueva_surface"
resumen_nueva_surface <- summary(train$nueva_surface)

# Imprimir el resumen
print(resumen_nueva_surface)

frecuencias <- table(train$nueva_surface)
moda <- names(frecuencias)[which.max(frecuencias)]

train$rooms <- as.numeric(train$rooms)
train$bedrooms <- as.numeric(train$bedrooms)
train$bathrooms <- as.numeric(train$bathrooms)
train$surface_total <- as.numeric(train$surface_total)
train$surface_covered <- as.numeric(train$surface_covered)
train$nueva_surface <- as.numeric(train$nueva_surface)

train <- train %>%
  mutate(metros_cuadrados_por_bedrooms = metros_cuadrados / bedrooms)
summary(train$metros_cuadrados_por_bedrooms)

train <- train %>%
  group_by(bedrooms) %>%
  mutate(
    Q1 = quantile(nueva_surface, 0.25, na.rm = TRUE),
    Q3 = quantile(nueva_surface, 0.75, na.rm = TRUE),
    IQR_valor = Q3 - Q1,
    umbral_inferior = max(Q1 - 1.5 * IQR_valor, 49),
    umbral_superior = Q3 + 1.5 * IQR_valor
  )

# Imputar valores mínimos y máximos basados en umbrales por número de habitaciones
train$nueva_surface <- pmax(train$nueva_surface, train$umbral_inferior)
train$nueva_surface <- pmin(train$nueva_surface, train$umbral_superior)
summary(train$nueva_surface)


# Calcular la mediana de nueva_surface
mediana_nueva_surface <- median(train$nueva_surface, na.rm = TRUE)

# Sustituir los NA en nueva_surface con la mediana
train$nueva_surface[is.na(train$nueva_surface)] <- mediana_nueva_surface

# Calcular la media de nueva_surface por número de habitaciones
media_por_numero_habitaciones <- aggregate(nueva_surface ~ bedrooms, data = train, FUN = mean, na.rm = TRUE)
media_por_numero_habitaciones

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

library(dplyr)

## ---------------------------------Crear Terraza---------------------------------##
# Crear una variable binaria "tiene_terrazz" basada en la descripción
train$tiene_terraza <- as.numeric(grepl("terraza", train$description, ignore.case = TRUE))

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

train$Gimnasio <- as.numeric(grepl("gimnasio", train$description, ignore.case = TRUE))

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

# Luego creamos una variable de color que debende del tipo de immueble.


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

#CREACIÓN DE VARIABLES
# CREANDO PARQUES
# Definir la ubicación de interés (en este caso, Bogotá, Colombia)
ubicacion <- "Bogotá, Colombia"

# Obtener los límites geográficos (BBOX) de la ubicación
bbox_bogota <- getbb(ubicacion)

bbox_bogota

# Definir la ubicación de interés (en este caso, Bogotá, Colombia)
parques <- opq(bbox = getbb("Bogotá, Colombia")) %>%
  add_osm_feature(key = "leisure" , value = "park")

# Cambiar el formato para que sea un objeto sf (simple features)
parques_sf <- osmdata_sf(parques)

# De las features del parque, nos interesa su geometría y ubicación
parques_geometria <- parques_sf$osm_polygons %>%
  select(osm_id, name)

# Calcular el centroide de cada parque para aproximar su ubicación como un solo punto
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


# Define los límites geográficos de Bogotá

##############################################################
# Definir los límites geográficos de Bogotá
# Convertir los datos de train a un objeto sf y especificar el sistema de coordenadas
train_sf <- st_as_sf(train, coords = c("lon", "lat"), crs = 4326)

# Convertir los centroides de los parques a formato sf
centroides_sf <- st_as_sf(centroides, coords = c("x", "y"), crs = 4326)

# Calcular las distancias para cada combinación inmueble - parque
distancias <- st_distance(train_sf, centroides_sf)

# Encontrar la distancia mínima a un parque
dist_min <- apply(distancias, 1, min)

# Agregar la distancia mínima como una nueva columna en train_sf
train_sf$distancia_parque <- dist_min

# Visualizar el conjunto de datos train_sf
head(train_sf)

# Crear un mapa de leaflet
m <- leaflet() %>%
  addTiles() %>%
  setView(lng = mean(train$lon), lat = mean(train$lat), zoom = 12) %>%
  addCircles(lng = train$lon, lat = train$lat, radius = 100, color = "blue")

# Mostrar el mapa
m


# Write the modified data frame to Excel
#write.xlsx(data_centros, file = Tabla_S)


#########################################################################################3
# Definir la ubicación de interés (en este caso, Bogotá, Colombia)
ubicacion <- "Bogotá, Colombia"

# Obtener los límites geográficos (BBOX) de la ubicación
bbox_bogota <- getbb(ubicacion)

# Definir la ubicación de interés (en este caso, Bogotá, Colombia) y buscar centros comerciales
centros_comerciales <- opq(bbox = getbb("Bogotá, Colombia")) %>%
  add_osm_feature(key = "shop" , value = "mall")

# Cambiar el formato para que sea un objeto sf (simple features)
centros_comerciales_sf <- osmdata_sf(centros_comerciales)

# De las features de centros comerciales, nos interesa su geometría y ubicación
centros_comerciales_geometria <- centros_comerciales_sf$osm_points %>%
  select(osm_id, name)

# Calcular el centroide de cada centro comercial para aproximar su ubicación como un solo punto
centroides_centros_comerciales <- st_centroid(centros_comerciales_geometria)

# Crear una nueva columna en train_sf que contenga la distancia mínima a un centro comercial
distancias_centros_comerciales <- st_distance(train_sf, centroides_centros_comerciales)
dist_min_centros_comerciales <- apply(distancias_centros_comerciales, 1, min)
train_sf$distancia_centros_comerciales <- dist_min_centros_comerciales

# Visualizar el conjunto de datos train_sf con la nueva variable
head(train_sf)

##-----------------------DISTANCIAS SUPERMERCADOS -----------------------------##

# Definir la ubicación de interés (en este caso, Bogotá, Colombia)
ubicacion <- "Bogotá, Colombia"

# Obtener los límites geográficos (BBOX) de la ubicación
bbox_bogota <- getbb(ubicacion)

# Definir la ubicación de interés (en este caso, Bogotá, Colombia) y buscar supermercados
supermercados <- opq(bbox = getbb("Bogotá, Colombia")) %>%
  add_osm_feature(key = "shop", value = "supermarket")

# Cambiar el formato para que sea un objeto sf (simple features)
supermercados_sf <- osmdata_sf(supermercados)

# De las features de supermercados, nos interesa su geometría y ubicación
supermercados_geometria <- supermercados_sf$osm_points %>%
  select(osm_id, name)

# Calcular el centroide de cada supermercado para aproximar su ubicación como un solo punto
centroides_supermercados <- st_centroid(supermercados_geometria)

# Crear una nueva columna en train_sf que contenga la distancia mínima a un supermercado
distancias_supermercados <- st_distance(train_sf, centroides_supermercados)
dist_min_supermercados <- apply(distancias_supermercados, 1, min)
train_sf$distancia_supermercados <- dist_min_supermercados

# Visualizar el conjunto de datos train_sf con la nueva variable
head(train_sf)

###--------------------TRANSMILENEO------------------------------------------------------##
# Definir la ubicación de interés (en este caso, Bogotá, Colombia)
ubicacion <- "Bogotá, Colombia"

# Obtener los límites geográficos (BBOX) de la ubicación
bbox_bogota <- getbb(ubicacion)

# Definir la ubicación de interés (en este caso, Bogotá, Colombia) y buscar estaciones de TransMilenio
transmilenio <- opq(bbox = getbb("Bogotá, Colombia")) %>%
  add_osm_feature(key = "public_transport" , value = "station") %>%
  add_osm_feature(key = "network", value = "TransMilenio")

# Cambiar el formato para que sea un objeto sf (simple features)
transmilenio_sf <- osmdata_sf(transmilenio)

# De las features de estaciones de TransMilenio, nos interesa su geometría y ubicación
transmilenio_geometria <- transmilenio_sf$osm_points %>%
  select(osm_id, name)

# Calcular el centroide de cada estación de TransMilenio para aproximar su ubicación como un solo punto
centroides_transmilenio <- st_centroid(transmilenio_geometria)

# Crear una nueva columna en train_sf que contenga la distancia mínima a una estación de TransMilenio
distancias_transmilenio <- st_distance(train_sf, centroides_transmilenio)
dist_min_transmilenio <- apply(distancias_transmilenio, 1, min)
train_sf$distancia_transmilenio <- dist_min_transmilenio

# Visualizar el conjunto de datos train_sf con la nueva variable
head(train_sf)

##------------------------------UNIVERSIDADES-----------------------------------##
# Definir la ubicación de interés (en este caso, Bogotá, Colombia)
ubicacion <- "Bogotá, Colombia"

# Obtener los límites geográficos (BBOX) de la ubicación
bbox_bogota <- getbb(ubicacion)

etiquetas_educativos <- c("school", "college", "university", "library", "kindergarten")

# Definir la ubicación de interés (en este caso, Bogotá, Colombia) y buscar universidades
universidades <- opq(bbox = getbb("Bogotá, Colombia")) %>%
  add_osm_feature(key = "amenity", value = etiquetas_educativos)

# Cambiar el formato para que sea un objeto sf (simple features)
universidades_sf <- osmdata_sf(universidades)

# De las features de universidades, nos interesa su geometría y ubicación
universidades_geometria <- universidades_sf$osm_points %>%
  select(osm_id, name)

# Calcular el centroide de cada universidad para aproximar su ubicación como un solo punto
centroides_universidades <- st_centroid(universidades_geometria)

# Crear una nueva columna en train_sf que contenga la distancia mínima a una universidad
distancias_universidades <- st_distance(train_sf, centroides_universidades)
dist_min_universidades <- apply(distancias_universidades, 1, min)
train_sf$distancia_universidades <- dist_min_universidades
################## Aparte

# Transformar los datos a una proyección en metros (por ejemplo, UTM para Bogotá)
train_sf <- st_transform(train_sf, crs = st_crs(centroides_universidades))

# Luego, recalcular las distancias
distancias_universidades <- st_distance(train_sf, centroides_universidades)
dist_min_universidades <- apply(distancias_universidades, 1, min)
train_sf$distancia_universidades <- dist_min_universidades


##---------------------- RESTAURANTES Y BARES -------------------------------##

# Definir la ubicación de interés (Bogotá, Colombia)
ubicacion <- "Bogotá, Colombia"

# Obtener los límites geográficos (BBOX) de la ubicación
bbox_bogota <- getbb(ubicacion)

# Definir la búsqueda de restaurantes y bares en un solo grupo
restaurantes_bares <- opq(bbox = getbb("Bogotá, Colombia")) %>%
  add_osm_feature(key = "amenity", value = c("restaurant", "bar"))

# Cambiar el formato para que sea un objeto sf (simple features)
restaurantes_bares_sf <- osmdata_sf(restaurantes_bares)

# De las features de restaurantes y bares, nos interesa su geometría y ubicación
restaurantes_bares_geometria <- restaurantes_bares_sf$osm_points %>%
  select(osm_id)

# Calcular el centroide de cada restaurante y bar para aproximar su ubicación como un solo punto
centroides_restaurantes_bares <- st_centroid(restaurantes_bares_geometria)

# Crear nuevas columnas en train_sf que contengan la distancia mínima a un restaurante o bar
distancias_restaurantes_bares <- st_distance(train_sf, centroides_restaurantes_bares)

dist_min_restaurantes_bares <- apply(distancias_restaurantes_bares, 1, min)

train_sf$distancia_restaurantes_bares <- dist_min_restaurantes_bares


# Agregar las variables de distancia a la base de datos 'train'
train <- cbind(train, train_sf[c("distancia_parque", "distancia_transmilenio", "distancia_supermercados", "distancia_centros_comerciales", "distancia_universidades", "distancia_restaurantes_bares")])

# Visualizar la base de datos 'train' con las nuevas columnas de distancia
head(train)

################################################################MODELOS#################################################
train$bedrooms <- ifelse(train$bedrooms == 0, train$rooms, train$bedrooms)
mediana_bedrooms <- median(train$bedrooms, na.rm = TRUE)
train$bedrooms <- ifelse(is.na(train$bedrooms), mediana_bedrooms, train$bedrooms)

# Crear una matriz de coordenadas a partir de lat y lon
coordinates <- train %>% select(lon, lat)

# Determinar el número de clusters (puedes ajustar este valor)
num_clusters <- 18

# Ejecutar el algoritmo K-Means espacial
clusters <- kmeans(coordinates, centers = num_clusters)

# Agregar los resultados del clustering a tu conjunto de datos original
train$cluster <- as.factor(clusters$cluster)

# Ver el resultado
head(train)

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

# Ver el resultado
library(ggplot2)

ggplot(train, aes(x = localidad)) +
  geom_bar(fill = "blue") +
  labs(title = "Distribución de Localidades") +
  coord_flip()


distant <- data.frame(
  Dist_Centros_Comerciales = train$distancia_centros_comerciales,
  Dist_Supermercados = train$distancia_supermercados,
  Dist_Transmilenio = train$distancia_transmilenio,
  Dist_Universidades = train$distancia_universidades,
  Dist_Parque = train$distancia_parque,
  Dist_Rests = train$distancia_restaurantes_bares
)

distant <- data.frame(localidad = train$localidad, distant) 

promedio_distancias <- distant %>%
  group_by(localidad) %>%
  summarize(
    Dist_Centros_Comerciales = mean(Dist_Centros_Comerciales, na.rm = TRUE),
    Dist_Supermercados = mean(Dist_Supermercados, na.rm = TRUE),
    Dist_Transmilenio = mean(Dist_Transmilenio, na.rm = TRUE),
    Dist_Universidades = mean(Dist_Universidades, na.rm = TRUE),
    Dist_Parques = mean(Dist_Parque, na.rm = TRUE),
    Dist_Rests = mean(Dist_Rests, na.rm = TRUE)
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

# Establece la ubicación y el nombre del archivo de salida
#Tabla_train1 <- "C:/Output R/Taller 2/Taller_2/Tabla_1.xlsx"  
#write_xlsx(train, Tabla_train1)



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
  summarise(Parques = round(mean(distancia_parque)),
            CComer = round(mean(distancia_centros_comerciales)),
            Universidades = round(mean(distancia_universidades)),
            Transmilenio = round(mean(distancia_transmilenio)),
            Restaurantes = round(mean(distancia_restaurantes_bares)))
colnames(Precios_Distancias_C) <- c("Rango de Precios","Distancia Parques", "Distancia Centros Comerciales","Distancia Universidades","Distancia Transmilenio","Distancia Restaurantes")


# Tabla de frecuencias con la moda del estrato para casas
Precios_Distancias_D <- Precio_Aparts %>%
  group_by(Precios_Apart) %>%
  summarise(Parques = round(mean(distancia_parque)),
            CComer = round(mean(distancia_centros_comerciales)),
            Universidades = round(mean(distancia_universidades)),
            Transmilenio = round(mean(distancia_transmilenio)),
            Restautantes = round(mean(distancias_restaurantes_bares)))
colnames(Precios_Distancias_D) <- c("Rango de Precios","Distancia Parques", "Distancia Centros Comerciales","Distancia Universidades","Distancia Transmilenio","Distancia Restaurantes")


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
train <- train %>% rename(Area=nueva_surface)
train <- train %>% rename(M2_por_Habitación=metros_cuadrados_por_bedrooms)
train <- train %>% rename(Terraza=tiene_terraza) 
train <- train %>% rename(Sala_BBQ=tiene_bbq) 
train <- train %>% rename(Garaje=total_parqueo) 
train <- train %>% rename(Dist_Parques=distancia_parque) 
train <- train %>% rename(Dist_Transmilenio=distancia_transmilenio) 
train <- train %>% rename(Dist_Supermercados=distancia_supermercados)
train <- train %>% rename(Dist_C_Comerc=distancia_centros_comerciales) 
train <- train %>% rename(Dist_Universidades=distancia_universidades)
train <- train %>% rename(Dist_Restaurantes=distancia_restaurantes_bares) 
train <- train %>% rename(Estrato=estrato) 

##-----------------------------CASAS----------------------------------------

train_casas <- train[train$property_type == "Casa", c("property_id","title", "month", "year", "localidad","Estrato", "Precio", "lPrecio", 
                                                      "Precio_M2", "Habitaciones", "Baños", "Area","M2_por_Habitación", "lat", "lon", "Terraza", 
                                                      "Garaje", "Sala_BBQ","Piscina","Gimnasio", "Chimenea","Seguridad",
                                                      "Dist_Parques", "Dist_Transmilenio", "Dist_Supermercados", 
                                                      "Dist_C_Comerc", "Dist_Universidades", "Dist_Restaurantes")]

#Tabla_train_casas <- "C:/Output R/Taller 2/Taller_2/Tabla_c.xlsx"  
#write_xlsx(train_casas, Tabla_train_casas)

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
                                      Dist_Transmilenio,
                                      Dist_Supermercados,
                                      Dist_C_Comerc,
                                      Dist_Universidades,
                                      Dist_Restaurantes,
                                      Estrato)

stargazer(data.frame(Tabla_Stat), header=FALSE, type='text',title="Estadisticas Variables Seleccionadas Casas")

##---------------------------------------Apartamentos ----------------------------------------##

train_apart <- train[train$property_type == "Apartamento", c("property_id","title", "month", "year", "localidad","Estrato", "Precio", "lPrecio", 
                                                             "Precio_M2", "Habitaciones", "Baños", "Area","M2_por_Habitación", "lat", "lon", "Terraza", 
                                                             "Garaje", "Sala_BBQ","Piscina","Gimnasio", "Chimenea","Seguridad",
                                                             "Dist_Parques", "Dist_Transmilenio", "Dist_Supermercados", 
                                                             "Dist_C_Comerc", "Dist_Universidades", "Dist_Restaurantes")]


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
                                        Dist_Transmilenio,
                                        Dist_Supermercados,
                                        Dist_C_Comerc,
                                        Dist_Universidades,
                                        Dist_Restaurantes,
                                        Estrato)

stargazer(data.frame(Tabla_Stat_D), header=FALSE, type='text',title="Estadisticas Variables Seleccionadas Apartamentos")

Tabla_train <- "C:/Output R/Taller 2/Taller_2/Tabla_1.xlsx"  
write_xlsx(train, Tabla_train)

##############################################################################################################
#--------------------------------------------- CHAPINERO-----------------------------------------------------#
###################################################TEST########################################################

test %>%
  summarise_all(~sum(is.na(.))) %>% transpose()

test$surface_total[is.na(test$surface_total)] <- test$surface_covered[is.na(test$surface_total)]
test %>%
  summarise_all(~sum(is.na(.))) %>% transpose()

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
  mutate(metros_cuadrados_por_bedrooms = metros_cuadrados / bedrooms)
summary(test$metros_cuadrados_por_bedrooms)


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
test$tiene_terraza <- as.numeric(grepl("terraza", test$description, ignore.case = TRUE))
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

test$Gimnasio <- as.numeric(grepl("gimnasio", test$description, ignore.case = TRUE))

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

# Luego creamos una variable de color que debende del tipo de immueble.


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

# Crear la variable Parques
# Cargar las bibliotecas necesarias
library(osmdata)
library(sf)

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

# Definir la ubicación de interés (Chapinero, Bogotá, Colombia)
#ubicacion <- "Chapinero, Bogotá, Colombia"

# Obtener los límites geográficos (BBOX) de la ubicación de Chapinero
#bbox_chapinero <- getbb(ubicacion)

# Definir la búsqueda de centros comerciales en Chapinero
centros_comerciales_chapinero <- opq(bbox = bbox_chapinero) %>%
  add_osm_feature(key = "shop", value = "mall")

# Cambiar el formato para que sea un objeto sf (simple features)
centros_comerciales_chapinero_sf <- osmdata_sf(centros_comerciales_chapinero)

# De las features de centros comerciales, nos interesa su geometría y ubicación
centros_comerciales_geometria <- centros_comerciales_chapinero_sf$osm_points %>%
  select(osm_id, name)

# Calcular el centroide de cada centro comercial para aproximar su ubicación como un solo punto
centroides_centros_comerciales <- st_centroid(centros_comerciales_geometria)

# Convertir los datos de 'test' a un objeto sf y especificar el sistema de coordenadas
#test_sf <- st_as_sf(test, coords = c("lon", "lat"), crs = 4326)

# Calcular las distancias para cada combinación inmueble - centro comercial
distancias_centros_comerciales <- st_distance(test_sf, centroides_centros_comerciales)
dist_min_centros_comerciales <- apply(distancias_centros_comerciales, 1, min)
test_sf$distancia_centros_comerciales <- dist_min_centros_comerciales

# Mantener las distancias a parques y centros comerciales en 'test_sf'
test_sf$distancia_parque <- dist_min
test_sf$distancia_centros_comerciales <- dist_min_centros_comerciales

###-----------------------------SUPERMERCADOS-------------------------------###
##--------------------------------------------------------------------------###


## Definir la búsqueda de supermercados en Chapinero
supermercados_chapinero <- opq(bbox = bbox_chapinero) %>%
  add_osm_feature(key = "shop", value = "supermarket")

# Cambiar el formato para que sea un objeto sf (simple features)
supermercados_chapinero_sf <- osmdata_sf(supermercados_chapinero)

# De las features de supermercados, nos interesa su geometría y ubicación
supermercados_geometria <- supermercados_chapinero_sf$osm_points %>%
  select(osm_id, name)

# Calcular el centroide de cada supermercado para aproximar su ubicación como un solo punto
centroides_supermercados <- st_centroid(supermercados_geometria)

# Calcular las distancias para cada combinación inmueble - supermercado
distancias_supermercados <- st_distance(test_sf, centroides_supermercados)

# Encontrar la distancia mínima a un supermercado
dist_min_supermercados <- apply(distancias_supermercados, 1, min)

# Agregar la distancia mínima como una nueva columna en 'test_sf'
test_sf$distancia_supermercados <- dist_min_supermercados


###--------------------------------TRANSMILENIO-------------------------------------------###
##----------------------------------------------------------------------------------------###

# Definir la ubicación de interés (Chapinero, Bogotá, Colombia)
ubicacion <- "Chapinero, Bogotá, Colombia"

# Obtener los límites geográficos (BBOX) de la ubicación
bbox_chapinero <- getbb(ubicacion)

# Definir la búsqueda de estaciones de TransMilenio en Chapinero
transmilenio_chapinero <- opq(bbox = bbox_chapinero) %>%
  add_osm_feature(key = "network", value = "TransMilenio")

# Cambiar el formato para que sea un objeto sf (simple features)
transmilenio_chapinero_sf <- osmdata_sf(transmilenio_chapinero)

# De las features de estaciones de TransMilenio, nos interesa su geometría y ubicación
transmilenio_geometria <- transmilenio_chapinero_sf$osm_points %>%
  select(osm_id, geometry)

# Calcular el centroide de cada estación de TransMilenio para aproximar su ubicación como un solo punto
centroides_transmilenio <- st_centroid(transmilenio_chapinero_sf$osm_points)

# Calcular las distancias para cada combinación inmueble - estación de TransMilenio
distancias_transmilenio <- st_distance(test_sf, centroides_transmilenio)

# Encontrar la distancia mínima a una estación de TransMilenio
dist_min_transmilenio <- apply(distancias_transmilenio, 1, min)

# Agregar la distancia mínima como una nueva columna en 'test_sf'
test_sf$distancia_transmilenio <- dist_min_transmilenio
head(test_sf)

###-----------DISTANCIA UNIVERSIDADES EN CHAPINERO--------------------------------------###
##--------------------------------------------------------------------------------------###

etiquetas_educativos <- c("school", "college", "university", "library", "kindergarten")

# Definir la ubicación de interés (en este caso, Bogotá, Colombia) y buscar universidades
universidades_chapinero <- opq(bbox = getbb("Bogotá, Colombia")) %>%
  add_osm_feature(key = "amenity", value = etiquetas_educativos)

# Cambiar el formato para que sea un objeto sf (simple features)
universidades_chapinero_sf <- osmdata_sf(universidades_chapinero)

# De las features de universidades, nos interesa su geometría y ubicación
universidades_geometria <- universidades_chapinero_sf$osm_points %>%
  select(osm_id, name)

# Calcular el centroide de cada universidad para aproximar su ubicación como un solo punto
centroides_universidades <- st_centroid(universidades_geometria)

# Calcular las distancias para cada combinación inmueble - universidad
distancias_universidades <- st_distance(test_sf, centroides_universidades)

# Encontrar la distancia mínima a una universidad
dist_min_universidades <- apply(distancias_universidades, 1, min)

# Agregar la distancia mínima como una nueva columna en 'test_sf'
test_sf$distancia_universidades <- dist_min_universidades

# Visualizar el conjunto de datos 'test_sf' con las nuevas variables
head(test_sf)

###-----------------DISTANCIA RESTAURANTES EN CHAPINERO ----------------------###
###---------------------------------------------------------------------------###

# Definir la ubicación de interés (Chapinero, Bogotá, Colombia)
ubicacion_chapinero <- "Chapinero, Bogotá, Colombia"

# Obtener los límites geográficos (BBOX) de la ubicación en Chapinero
bbox_chapinero <- getbb(ubicacion_chapinero)

# Definir la búsqueda de restaurantes y bares en un solo grupo en Chapinero
restaurantes_bares <- opq(bbox = bbox_chapinero) %>%
  add_osm_feature(key = "amenity", value = c("restaurant", "bar"))

# Cambiar el formato para que sea un objeto sf (simple features)
restaurantes_bares_sf <- osmdata_sf(restaurantes_bares)

# De las features de restaurantes y bares en Chapinero, nos interesa su geometría y ubicación
restaurantes_bares_geometria <- restaurantes_bares_sf$osm_points %>%
  select(osm_id)

# Calcular el centroide de cada restaurante y bar en Chapinero para aproximar su ubicación como un solo punto
centroides_restaurantes_bares <- st_centroid(restaurantes_bares_geometria)

# Calcular las distancias para cada combinación inmueble - restaurante/bar en Chapinero
distancias_restaurantes_bares <- st_distance(test_sf, centroides_restaurantes_bares)

# Encontrar la distancia mínima a un restaurante o bar en Chapinero
dist_min_restaurantes_bares <- apply(distancias_restaurantes_bares, 1, min)

# Agregar la distancia mínima como una nueva columna en 'test_sf'
test_sf$distancia_restaurantes_bares <- dist_min_restaurantes_bares

# Visualizar el conjunto de datos 'test_sf' con la nueva variable
head(test_sf)


test<- cbind(test, test_sf[c("distancia_parque","distancia_transmilenio","distancia_supermercados","distancia_universidades", "distancia_centros_comerciales","distancia_restaurantes_bares")])

# Imputar los Valores para los Baños
test_b_b <- test[complete.cases(test[c("bedrooms", "bathrooms")]), ]
mediat_b_b <- mean(test_b_b$bedrooms / test_b_b$bathrooms)
test$bathrooms[is.na(test$bathrooms)] <- test$bedrooms[is.na(test$bathrooms)] / mediat_b_b
test$bathrooms <- round(test$bathrooms) 

test <- data.frame(localidad = "Chapinero", test)
test$Estrato <- "4"

#Tabla_test <- "C:/Output R/Taller 2/Taller_2/Tabla_2.xlsx"  
#write_xlsx(test, Tabla_test)


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
test <- test %>% rename(Dist_Transmilenio=distancia_transmilenio) 
test <- test %>% rename(Dist_Supermercados=distancia_supermercados)
test <- test %>% rename(Dist_C_Comerc=distancia_centros_comerciales) 
test <- test %>% rename(Dist_Universidades=distancia_universidades)
test <- test %>% rename(Dist_Restaurantes=distancia_restaurantes_bares) 

test <- test %>%
  mutate(M2_por_Habitación = round(Area / Habitaciones))

test$Precio_M2 <- NA
test$lPrecio <- NA

##--------------------------- Bases de Datos Casas y Apartamentos------------------##

##-----------------------------CASAS----------------------------------------

test_casas <- test[train$property_type == "Casa", c("property_id","title", "month", "year", "localidad","Estrato", "Precio", "lPrecio", 
                                                      "Precio_M2", "Habitaciones", "Baños", "Area","M2_por_Habitación", "lat", "lon", "Terraza", 
                                                      "Garaje", "Sala_BBQ","Piscina","Gimnasio", "Chimenea","Seguridad",
                                                      "Dist_Parques", "Dist_Transmilenio", "Dist_Supermercados", 
                                                      "Dist_C_Comerc", "Dist_Universidades", "Dist_Restaurantes")]


##---------------------------------------Apartamentos ----------------------------------------##

test_apart <- test[train$property_type == "Apartamento", c("property_id","title", "month", "year", "localidad","Estrato", "Precio", "lPrecio", 
                                                             "Precio_M2", "Habitaciones", "Baños", "Area","M2_por_Habitación", "lat", "lon", "Terraza", 
                                                             "Garaje", "Sala_BBQ","Piscina","Gimnasio", "Chimenea","Seguridad",
                                                             "Dist_Parques", "Dist_Transmilenio", "Dist_Supermercados", 
                                                             "Dist_C_Comerc", "Dist_Universidades", "Dist_Restaurantes")]



##---------------------------Elaboración de Modelos para pronosticar el Precio de las Casas------------------##
##-----------------------------------------------------------------------------------------------------------##

#----------------------------------------------- REGRESION LINEAL---------------------------------------------

train_casas$lPrecio_M2 = log(train_casas$Precio_M2)
train_casas$Habitaciones2 = (train_casas$Habitaciones)^2
train_casas$Terraza_piscina = train_casas$Terraza * train_casas$Piscina
train_casas$Gimnasio_seguridad = train_casas$Gimnasio * train_casas$Seguridad
train_casas$Sala_BBQ_terraza = train_casas$Sala_BBQ * train_casas$Terraza
train_casas$Area_Garaje = train_casas$Area * train_casas$Garaje

Model1 <- lm(lPrecio ~ Estrato + Habitaciones + Habitaciones2 + Baños + Area + Terraza + Area_Garaje + Garaje + Sala_BBQ + Gimnasio + Sala_BBQ_terraza + Chimenea + Seguridad + Dist_Parques + 
               Dist_Transmilenio + Dist_Supermercados + Dist_C_Comerc + Dist_Universidades + Dist_Restaurantes, data = train_casas)
Model1_stargazer <- stargazer(Model1, type="text", omit.stat=c("ser","f","adj.rsq"))
Model1_stargazer <- as.data.frame(Model1_stargazer)

modelo_final <- step(Model1, direction = "backward", trace = 0)

summary(modelo_final)



