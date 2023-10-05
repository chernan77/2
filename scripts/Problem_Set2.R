#Problem Set 2
#Big Data y Machine Learning para Economía Aplicada

# Celin Hernández: 202210067
# Merit Tejeda: 202210104
# Estefanía Laborde: 201533743

#install.packages("readxl")
library(readxl)
# Cargar pacman (contiene la función p_load)
library(pacman) 
############# Encontrar variables Externas
#install.packages("osmdata")
#install.packages("leaflet")
#install.packages("dplyr")
#install.packages("rgeos")
# install.packages("openxlsx")
library(openxlsx)


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

### Importar la Base de Datos           
# Especifica la ubicación de la carpeta que contiene los archivos CSV
carpeta <- "G:/Mi unidad/Machine Learning/Taller 2/Bases de datos/"

# Nombres de los archivos CSV que deseas importar
archivos_a_importar <- c("test.csv", "train.csv", "submission_template.csv")

# Crea bases de datos separadas para cada archivo
for (archivo in archivos_a_importar) {
  # Genera el nombre de la base de datos
  nombre <- tools::file_path_sans_ext(archivo)
  
  # Lee el archivo CSV y asigna el resultado a una base de datos con el mismo nombre
  assign(nombre, read.csv(file.path(carpeta, archivo)))
}

train %>%
  summarise_all(~sum(is.na(.))) %>% transpose()

# IMPUTACIÓN DE DATOS

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
         bedrooms = replace_na(bedrooms, 3),
         bathrooms = replace_na(bathrooms, 2),)
#### Reemplazando los ceros
train$bedrooms <- ifelse(train$bedrooms == 0, train$rooms, train$bedrooms)

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
moda <- as.numeric(names(frecuencias)[which.max(frecuencias)])

### Analisis para nueva surface
library(dplyr)


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

# Instala y carga la librería ggplot2 si aún no está instalada o cargada
# install.packages("ggplot2")
library(ggplot2)

# Crear un gráfico de dispersión (scatter plot) para la correlación entre precio y metros cuadrados
ggplot(train, aes(x = nueva_surface, y = price)) +
  geom_point() +
  labs(x = "Metros Cuadrados", y = "Precio") +
  ggtitle("Correlación entre Precio y Metros Cuadrados")

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

# Definir los límites geográficos de Bogotá (sin Chapinero)

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

###

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

############Creando la variable de Universidades
# Obtener los datos de universidades en Bogotá desde OpenStreetMap
universidades_osm <- opq(bbox = getbb(ubicacion)) %>%
  add_osm_feature(key = "amenity", value = "university") %>%
  osmdata_sf()

# Convertir los datos de universidades a un objeto sf
universidades_sf <- universidades_osm$osm_points

# Convertir train_sf al sistema de coordenadas EPSG:4326 si aún no lo está
train_sf <- st_transform(train_sf, crs = 4326)

# Calcular las distancias entre los inmuebles en train_sf y las universidades
distancias <- st_distance(train_sf, universidades_sf)

# Encontrar la distancia mínima a una universidad para cada inmueble
dist_min <- apply(distancias, 1, min)

# Agregar la distancia mínima como una nueva columna en train_sf
train_sf$distancia_universidad <- dist_min

# Visualizar el conjunto de datos con la nueva variable
head(train_sf)


### Centro comerciales

# Obtener los datos de centros comerciales en Bogotá desde OpenStreetMap
centros_comerciales_osm <- opq(bbox = getbb(ubicacion)) %>%
  add_osm_feature(key = "shop", value = "mall") %>%
  osmdata_sf()

# Convertir los datos de centros comerciales a un objeto sf
centros_comerciales_sf <- centros_comerciales_osm$osm_points

# Convertir train_sf al sistema de coordenadas EPSG:4326 si aún no lo está
train_sf <- st_transform(train_sf, crs = 4326)

# Calcular las distancias entre los inmuebles en train_sf y los centros comerciales
distancias <- st_distance(train_sf, centros_comerciales_sf)

# Encontrar la distancia mínima a un centro comercial para cada inmueble
dist_min <- apply(distancias, 1, min)

# Agregar la distancia mínima como una nueva columna en train_sf
train_sf$distancia_centro_comercial <- dist_min
# Supongamos que centros_comerciales_sf es una lista
# Convierte la lista en un data frame si tiene una estructura tabular adecuada
write.xlsx(centros_comerciales_df, file = "C:/Users/Usuario/Documents/Machine Learning/Taller_2/stores/mi_base_de_datos.xlsx")
