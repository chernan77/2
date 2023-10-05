#Problem Set 2
#Big Data y Machine Learning para Economía Aplicada

# Celin Hernández: 202210067
# Merit Tejeda: 202210104
# Estefanía Laborde: 201533743


#librerias requeridas
install.packages("rvest")
install.packages("httr")
install.packages("dplyr")

# Instala y carga las bibliotecas necesarias
library(rvest)
library(httr)
library(dplyr)
library(stringr)


# Lista de URLs
urlS <- sprintf("https://www.properati.com.co/s/chapinero-bogota-d-c/casa/venta/%d", 1:14)

# Función para importar datos de una URL
Import_data <- function(url) {
  webpage <- read_html(url)
  return(webpage)
}

# Utiliza un bucle for para leer cada URL y almacenar los resultados en la lista Datos
Datos <- vector("list", length(urlS))
for (i in seq_along(urlS)) {
  Datos[[i]] <- Import_data(urlS[i])
}


# Function to extract property names from a single HTML document
ExtractPropertyNames <- function(webpage) {
  nombres <- webpage %>%
    html_nodes(".listing-card__title") %>%
    html_text()
  return(nombres)
}

# Use lapply to apply the function to each HTML document in the 'Datos' list
nombres_list <- lapply(Datos, ExtractPropertyNames)
nombres <- unlist(nombres_list)


# Function to extract property names from a single HTML document
ExtractPropertyPrice <- function(webpage) {
  precios <- webpage %>%
    html_nodes(".listing-card__price-wrapper > div") %>%
    html_text()
  return(precios)
}

# Use lapply to apply the function to each HTML document in the 'Datos' list
precios_list <- lapply(Datos, ExtractPropertyPrice)
precios <- unlist(precios_list)


# Function to extract property names from a single HTML document
ExtractPropertyCaract <- function(webpage) {
  cartes <- webpage %>%
    html_nodes("div.listing-card__properties") %>%
    html_text() %>%
   return(Caracteristicas)
}

# Use lapply to apply the function to each HTML document in the 'Datos' list
cartes_list <- lapply(Datos, ExtractPropertyCaract)
cartes <- unlist(cartes_list)
cartes

habitacion <- numeric(length(ExtractPropertyCaract))
bano <- numeric(length(ExtractPropertyCaract))
metros <- numeric(length(ExtractPropertyCaract))




Tabla_1 <- data.frame(nombres, precios) 




