#Problem Set 2
#Big Data y Machine Learning para Economía Aplicada

# Celin Hernández: 202210067
# Merit Tejeda: 202210104
# Estefanía Laborde: 201533743

library(dplyr)
install.packages("geosphere")
library(geosphere)
install.packages("stringr")
library(stringr)
install.packages("readxl")
library(readxl)
#Cargar pacman (contiene la función p_load)
library(pacman) 
############# Encontrar variables Externas
install.packages("osmdata")
install.packages("leaflet")
install.packages("dplyr")
install.packages("rgeos")
install.packages("openxlsx")
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
    umbral_inferior = max(Q1 - 1.5 * IQR_valor, 15),
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

#######Crear Terraza 
# Crear una variable binaria "tiene_terrazz" basada en la descripción
train$tiene_terraza <- as.numeric(grepl("terraza", train$description, ignore.case = TRUE))

# Mostrar las primeras filas del dataframe con la nueva variable
head(train)
table(train$tiene_terraza)
casas_con_terrazas <- sum(train$tiene_terraza == 1)
casas_con_terrazas
# Crear una nueva columna "tiene_bbq" basada en la descripción
train$tiene_bbq <- grepl("BBQ|barbacoa", train$description, ignore.case = TRUE)

# Convertir valores lógicos en 1 (Tiene BBQ) y 0 (No tiene BBQ)
train$tiene_bbq <- as.numeric(train$tiene_bbq)

# Verificar la nueva variable "tiene_bbq"
table(train$tiene_bbq)

# Definir los límites geográficos de Bogotá (sin Chapinero)

# Definir los límites geográficos de Bogotá
limites_bogota <- getbb("Bogotá, Colombia")

# Filtrar observaciones dentro de los límites de Bogotá
train_filtrado_bogota <- train %>%
  filter(
    between(lon, limites_bogota[1, "min"], limites_bogota[1, "max"]) &
      between(lat, limites_bogota[2, "min"], limites_bogota[2, "max"])
  )
# Escalamos para que se pueda graficar AQUI
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

# Specify the file path
#Tabla_S <- "C:/Users/Usuario/Documents/Machine Learning/Taller_2/stores/Tabla_S1.xlsx"

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
######################################################PARTE 2##############################3333
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

### Exportar 
# Ruta donde deseas guardar el archivo de Excel
# Specify the file path
# Exclude the "geometry" column from the data frame

#supermercados_geometria2<- data.frame(supermercados_geometria2)
#supermercados_sf <- supermercados_sf[, !names(supermercados_sf) %in% "geometry"]
#supermercados_geometria2 <- supermercados_geometria2[, !names(supermercados_geometria2) %in% "geometry"]

# Specify the file path
#ruta <- 'G:/Mi unidad/Machine Learning/Taller 2/Tabla_Super.xlsx'
#ruta2 <- 'G:/Mi unidad/Machine Learning/Taller 2/Tabla_Super2.xlsx'

# Write the modified data frame to Excel
#write.xlsx(supermercados_sf, file = ruta)
#write.xlsx(supermercados_geometria2, file = ruta2)


#################################Transmileneo
# Define la ubicación de la zona de interés en Bogotá

###TRANSMILENEO##
# Definir la ubicación de interés (en este caso, Bogotá, Colombia)
# Definir la ubicación de interés (en este caso, Bogotá, Colombia)
# Definir la ubicación de interés (Chapinero, Bogotá, Colombia)
# Obtener los límites geográficos (BBOX) de la ubicación
bbox_bogota <- getbb(ubicacion)

# Definir la ubicación de interés (en este caso, Bogotá, Colombia) y buscar estaciones de TransMilenio
transmilenio <- opq(bbox = getbb("Bogotá, Colombia")) %>%
  #add_osm_feature(key = "public_transport" , value = "station") %>%
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
############UNIVERSIDADES
# Definir la ubicación de interés (en este caso, Bogotá, Colombia)
ubicacion <- "Bogotá, Colombia"

# Obtener los límites geográficos (BBOX) de la ubicación
bbox_bogota <- getbb(ubicacion)

# Definir la ubicación de interés (en este caso, Bogotá, Colombia) y buscar universidades
universidades <- opq(bbox = getbb("Bogotá, Colombia")) %>%
  add_osm_feature(key = "amenity", value = "university")

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
################## Aparte RESTAURANTES
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

# Visualizar el conjunto de datos train_sf con la nueva variable
head(train_sf)

#### Pasar todas las variables a la base de datos de Train

# Agregar las variables de distancia a la base de datos 'train'
train <- cbind(train, train_sf[c("distancia_parque", "distancia_transmilenio", "distancia_supermercados", "distancia_centros_comerciales", "distancia_universidades", "distancia_restaurantes_bares")])

# Visualizar la base de datos 'train' con las nuevas columnas de distancia
head(train)

################################################################MODELOS#################################################

# Crear una especificación de modelo de regresión ridge
# Definir una especificación de modelo lineal utilizando 'linear_reg'
#ridge_spec <- linear_reg(mixture = 0, penalty = 0) %>%
  # Establecer el modo del modelo como 'regresión'
#set_mode("regression") %>%
  # Configurar el motor del modelo como 'glmnet', que se utiliza para modelos ridge  y lasso
#set_engine("glmnet")
#ridge_fit <- fit(ridge_spec, price ~ nueva_surface +rooms + bedroooms + bathrooms+ property_type +total_parqueo+ tiene_terraza+tiene_bbq, data = train)

###### Crear modelos#####
# Crear una matriz de coordenadas a partir de lat y lon
coordinates <- train %>% select(lon, lat)

# Determinar el número de clusters (puedes ajustar este valor)
num_clusters <- 5

# Ejecutar el algoritmo K-Means espacial
clusters <- kmeans(coordinates, centers = num_clusters)

# Agregar los resultados del clustering a tu conjunto de datos original
train$cluster <- as.factor(clusters$cluster)

# Ver el resultado
head(train)
# Definir las coordenadas de Chapinero (ajusta estas coordenadas)
chapinero_lon <- -74.0598
chapinero_lat <- 4.6372

# Calcular la distancia entre cada punto y las coordenadas de Chapinero
train$distance_to_chapinero <- distGeo(coordinates, c(chapinero_lon, chapinero_lat))

# Definir una distancia umbral para considerar que un punto está en Chapinero (ajusta esta distancia según tu criterio)
umbral_distancia <- 1  # por ejemplo, 1 kilómetro

# Asignar la localidad como "Chapinero" si la distancia es menor que el umbral
train$localidad <- ifelse(train$distance_to_chapinero < umbral_distancia, "Chapinero", "Otra Localidad")

# Eliminar la columna de distancia si ya no es necesaria
train <- train %>% select(-distance_to_chapinero)

summary(train$localidad)

# Ver el resultado
head(train)

##########################################################################################################################

# Localidades de Bogota
coordenadas_santa_fe <- c(-74.060071, 4.611765)    # Coordenadas de Santa Fe
coordenadas_usaquen <- c(-74.033055, 4.736126)     # Coordenadas de Usaquén
coordenadas_teusaquillo <- c(-74.083913, 4.635771) # Coordenadas de Teusaquillo
coordenadas_candelaria <- c(-74.080661, 4.601209)  # Coordenadas de La Candelaria

# Define umbrales de distancia en grados para cada localidad
umbral_grados_santa_fe <- 0.02       # Umbral para Santa Fe (aproximadamente 2.2 km en latitud y longitud)
umbral_grados_usaquen <- 0.03        # Umbral para Usaquén (aproximadamente 3.3 km en latitud y longitud)
umbral_grados_teusaquillo <- 0.02    # Umbral para Teusaquillo (aproximadamente 2.2 km en latitud y longitud)
umbral_grados_candelaria <- 0.02     # Umbral para La Candelaria (aproximadamente 2.2 km en latitud y longitud)

# Calcula la distancia en grados entre cada punto y las coordenadas de cada localidad
train$distancia_santa_fe <- abs(train$lon - coordenadas_santa_fe[1]) + abs(train$lat - coordenadas_santa_fe[2])
train$distancia_usaquen <- abs(train$lon - coordenadas_usaquen[1]) + abs(train$lat - coordenadas_usaquen[2])
train$distancia_teusaquillo <- abs(train$lon - coordenadas_teusaquillo[1]) + abs(train$lat - coordenadas_teusaquillo[2])
train$distancia_candelaria <- abs(train$lon - coordenadas_candelaria[1]) + abs(train$lat - coordenadas_candelaria[2])

# Asigna cada punto a una localidad basado en las distancias y umbrales
train$localidad <- "Otra Localidad"  # Valor predeterminado
train$localidad[train$distancia_santa_fe < umbral_grados_santa_fe] <- "Santa Fe"
train$localidad[train$distancia_usaquen < umbral_grados_usaquen] <- "Usaquén"
train$localidad[train$distancia_teusaquillo < umbral_grados_teusaquillo] <- "Teusaquillo"
train$localidad[train$distancia_candelaria < umbral_grados_candelaria] <- "La Candelaria"

# Elimina las columnas de distancia si ya no son necesarias
train <- train %>% select(-distancia_santa_fe, -distancia_usaquen, -distancia_teusaquillo, -distancia_candelaria)

# Ver el resultado
library(ggplot2)

# Crear un gráfico de barras
ggplot(train, aes(x = localidad)) +
  geom_bar(fill = "blue") +
  labs(title = "Distribución de Localidades")

# Resumen de frecuencia de las localidades
frecuencias <- table(train$localidad)

# Mostrar el resumen de frecuencias
print(frecuencias)

########################################################################################################################
# Coordenadas de Chapinero (latitud y longitud)
# Define una función para extraer el nombre del barrio o colonia
# Instala y carga la librería dplyr si aún no está instalada
###################################################TEST#########################################################
####
#Imputacion del precio

test %>%
  summarise_all(~sum(is.na(.))) %>% transpose()

test$surface_total[is.na(test$surface_total)] <- test$surface_covered[is.na(test$surface_total)]
test %>%
  summarise_all(~sum(is.na(.))) %>% transpose()

#Encontremos la moda de número de habitaciones 3, cuartos y número de baños
test$bedrooms <- ifelse(test$bedrooms == 0, test$rooms, test$bedrooms)                                                    

### Analisis descriptivo de rooms 3
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

test <- test %>%
  mutate(metros_cuadrados_por_bedrooms = metros_cuadrados / bedrooms)

summary(test$metros_cuadrados_por_bedrooms)

test <- test %>%
  group_by(bedrooms) %>%
  mutate(
    Q1 = quantile(nueva_surface, 0.25, na.rm = TRUE),
    Q3 = quantile(nueva_surface, 0.75, na.rm = TRUE),
    IQR_valor = Q3 - Q1,
    umbral_inferior = max(Q1 - 1.5 * IQR_valor, 15),
    umbral_superior = Q3 + 1.5 * IQR_valor
  )
# Imputar valores mínimos y máximos basados en umbrales por número de habitaciones
test$nueva_surface <- pmax(test$nueva_surface, test$umbral_inferior)
test$nueva_surface <- pmin(test$nueva_surface, test$umbral_superior)
summary(test$nueva_surface)

boxplot_data <- boxplot(test$nueva_surface, plot = FALSE)
boxplot_data
# Calcular la mediana de nueva_surface
mediana_nueva_surface <- median(test$nueva_surface, na.rm = TRUE)

# Sustituir los NA en nueva_surface con la mediana
test$nueva_surface[is.na(test$nueva_surface)] <- mediana_nueva_surface

# Calcular la media de nueva_surface por número de habitaciones
media_por_numero_habitaciones <- aggregate(nueva_surface ~ bedrooms, data = test, FUN = mean, na.rm = TRUE)
media_por_numero_habitaciones

# Calcular la mediana de nueva_surface
mediana_nueva_surface <- median(test$nueva_surface, na.rm = TRUE)

# Sustituir los NA en nueva_surface con la mediana
test$nueva_surface[is.na(test$nueva_surface)] <- mediana_nueva_surface

# Calcular la media de nueva_surface por número de habitaciones
media_por_numero_habitaciones <- aggregate(nueva_surface ~ bedrooms, data = test, FUN = mean, na.rm = TRUE)
media_por_numero_habitaciones
summary(test$nueva_surface)

library(ggplot2)
ggplot(test, aes(x = nueva_surface)) +
  geom_histogram(bins = 30, fill = "blue", color = "black", alpha = 0.7) +
  scale_y_log10()

# Crear una tabla de frecuencias
frecuencias <- table(test$nueva_surface)

# Convertir la tabla de frecuencias en un data frame para facilitar su manipulación
frecuencias_df <- as.data.frame(frecuencias)

# Cambiar el nombre de las columnas
colnames(frecuencias_df) <- c("Valor", "Frecuencia")

# Ordenar la tabla de frecuencias por valor (opcional)
frecuencias_df <- frecuencias_df[order(frecuencias_df$Valor), ]

# Mostrar la tabla de frecuencias
print(frecuencias_df)
# Identificar filas con el valor extremo en nueva_surface igual a 7457
fila_valor_extremo <- which(test$nueva_surface == 7457)

# Imprimir el número de fila
print(fila_valor_extremo)

# Crear la variable metros_cuadrados_por_habitacion
test <- test %>%
  filter(nueva_surface != 7457) %>%
  group_by(property_type) %>%
  mutate(metros_cuadrados_por_habitacion = nueva_surface / bedrooms)

# Calcular el promedio de metros cuadrados por habitación por tipo de propiedad
resumen_metros_cuadrados <- test %>%
  group_by(property_type) %>%
  summarize(Promedio_Metros_Cuadrados = mean(metros_cuadrados_por_habitacion, na.rm = TRUE))

# Ver el resumen
print(resumen_metros_cuadrados)

test$nueva_surface <- ifelse(test$nueva_surface == 7457, 61.2 * 10, test$nueva_surface)
test$nueva_surface[test$nueva_surface == 7457] <- 61.2 * 10
summary(test$nueva_surface)

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
# Crear una nueva columna "tiene_bbq" basada en la descripción
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

# Definir la ubicación de interés (Chapinero, Bogotá, Colombia)
ubicacion <- "Chapinero, Bogotá, Colombia"

# Obtener los límites geográficos (BBOX) de la ubicación de Chapinero
bbox_chapinero <- getbb(ubicacion)

# Definir la búsqueda de parques en Chapinero
parques_chapinero <- opq(bbox = bbox_chapinero) %>%
  add_osm_feature(key = "leisure", value = "park")

# Obtener datos de OSM para los parques en Chapinero
osm_data_chapinero <- osmdata_sf(parques_chapinero)

# Cambiar el formato de los parques para que sean objetos sf (simple features)
parques_sf <- osm_data_chapinero$osm_polygons


# Convertir los datos de 'test' a un objeto sf y especificar el sistema de coordenadas
test_sf <- st_as_sf(test, coords = c("lon", "lat"), crs = 4326)

# Convertir los centroides de los parques a formato sf
centroides_sf <- st_as_sf(centroides, crs = 4326)

# Calcular las distancias para cada combinación inmueble - parque
distancias <- st_distance(test_sf, centroides_sf)

# Encontrar la distancia mínima a un parque
dist_min <- apply(distancias, 1, min)

# Agregar la distancia mínima como una nueva columna en 'test_sf'
test_sf$distancia_parque <- dist_min

# Visualizar el conjunto de datos 'test_sf'
head(test_sf)

# Specify the file path
#Tabla_Test <- "G:/Mi unidad/Machine Learning/Taller 2/Tabla_T.xlsx"

#write.xlsx(test, file = Tabla_Test)

######################################### CENTROS COMERCIALES ###########################################################################

# Definir la ubicación de interés (Chapinero, Bogotá, Colombia)
ubicacion <- "Chapinero, Bogotá, Colombia"

# Obtener los límites geográficos (BBOX) de la ubicación de Chapinero
bbox_chapinero <- getbb(ubicacion)

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
test_sf <- st_as_sf(test, coords = c("lon", "lat"), crs = 4326)

# Calcular las distancias para cada combinación inmueble - centro comercial
distancias_centros_comerciales <- st_distance(test_sf, centroides_centros_comerciales)

# Encontrar la distancia mínima a un centro comercial
dist_min_centros_comerciales <- apply(distancias_centros_comerciales, 1, min)

# Agregar la distancia mínima como una nueva columna en 'test_sf'
test_sf$distancia_centros_comerciales <- dist_min_centros_comerciales

# Visualizar el conjunto de datos 'test_sf' con la nueva variable
head(test_sf)
############### SUPERMERCADOS###########################################
# Cargar las bibliotecas necesarias
library(osmdata)
library(sf)

# Definir la ubicación de interés (Chapinero, Bogotá, Colombia)
ubicacion <- "Chapinero, Bogotá, Colombia"

# Obtener los límites geográficos (BBOX) de la ubicación de Chapinero
bbox_chapinero <- getbb(ubicacion)

# Definir la búsqueda de supermercados en Chapinero
supermercados_chapinero <- opq(bbox = bbox_chapinero) %>%
  add_osm_feature(key = "shop", value = "supermarket")

# Cambiar el formato para que sea un objeto sf (simple features)
supermercados_chapinero_sf <- osmdata_sf(supermercados_chapinero)

# De las features de supermercados, nos interesa su geometría y ubicación
supermercados_geometria <- supermercados_chapinero_sf$osm_points %>%
  select(osm_id, name)

# Calcular el centroide de cada supermercado para aproximar su ubicación como un solo punto
centroides_supermercados <- st_centroid(supermercados_geometria)

# Convertir los datos de 'test' a un objeto sf y especificar el sistema de coordenadas
test_sf <- st_as_sf(test, coords = c("lon", "lat"), crs = 4326)

# Calcular las distancias para cada combinación inmueble - supermercado
distancias_supermercados <- st_distance(test_sf, centroides_supermercados)

# Encontrar la distancia mínima a un supermercado
dist_min_supermercados <- apply(distancias_supermercados, 1, min)

# Agregar la distancia mínima como una nueva columna en 'test_sf'
test_sf$distancia_supermercados <- dist_min_supermercados
# Visualizar el conjunto de datos 'test_sf' con la nueva variable
head(test_sf)

######################################################TRANSMILENEO####################################
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

#### BUSCAR UNIVERSIDADES EN CHAPINERO
# Definir la búsqueda de universidades en Chapinero
universidades_chapinero <- opq(bbox = bbox_chapinero) %>%
  add_osm_feature(key = "amenity", value = "university")

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
###############################################RESTAURANTES######################################################################
# Definir la ubicación de interés (Chapinero, Bogotá, Colombia)
ubicacion_chapinero <- "Chapinero, Bogotá, Colombia"

# Obtener los límites geográficos (BBOX) de la ubicación en Chapinero
bbox_chapinero <- getbb(ubicacion_chapinero)

# Definir la búsqueda de restaurantes y bares en un solo grupo en Chapinero
restaurantes_bares_chapinero <- opq(bbox = bbox_chapinero) %>%
  add_osm_feature(key = "amenity", value = c("restaurant", "bar"))

# Cambiar el formato para que sea un objeto sf (simple features)
restaurantes_bares_chapinero_sf <- osmdata_sf(restaurantes_bares_chapinero)

# De las features de restaurantes y bares en Chapinero, nos interesa su geometría y ubicación
restaurantes_bares_geometria_chapinero <- restaurantes_bares_chapinero_sf$osm_points %>%
  select(osm_id)

# Calcular el centroide de cada restaurante y bar en Chapinero para aproximar su ubicación como un solo punto
centroides_restaurantes_bares_chapinero <- st_centroid(restaurantes_bares_geometria_chapinero)

# Calcular las distancias para cada combinación inmueble - restaurante/bar en Chapinero
distancias_restaurantes_bares_chapinero <- st_distance(test_sf, centroides_restaurantes_bares_chapinero)

# Encontrar la distancia mínima a un restaurante o bar en Chapinero
dist_min_restaurantes_bares_chapinero <- apply(distancias_restaurantes_bares_chapinero, 1, min)

# Agregar la distancia mínima como una nueva columna en 'test_sf'
test_sf$distancia_restaurantes_bares_chapinero <- dist_min_restaurantes_bares_chapinero

# Visualizar el conjunto de datos 'test_sf' con la nueva variable
head(test_sf)
###############################RESTAURANTES################################################

test<- cbind(test, test_sf[c("distancia_restaurantes_bares_chapinero", "distancia_universidades", "distancia_transmilenio", "distancia_supermercados", "distancia_centros_comerciales", "distancia_parque")])

