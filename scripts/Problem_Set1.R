# Taller 1: Big Data y Machine Learning para Econonomia Aplicada

## INTEGRANTES
# Merit Salome Tejeda Amaya
#Celin Eliud Hernandez
#Estefania Laborde

# Instalamos los Paquetes
#install.packages("rvest") #para la extracción y manipulación de datos de páginas web.
#install.packages("xml2") #para trabajar con documentos XML
#install.packages("purrr") #para aplicar el scraping a múltiples URLs se utiliza en la función Import_data
#install.packages("writexl") # para exportar datos a un archivo de Excel (.xlsx)
#install.packages("tidyverse") #facilitar la manipulación, visualización y análisis de datos
#install.packages("kableExtra") #para mejorar la apariencia y el formato de tablas creadas
#install.packages("knitr") #para la generación de informes y documentos dinámicos a partir de código R
#install.packages("flextable") #para crear tablas flexibles y estilizadas en documentos de R
#install.packages("officer")  #se utiliza para la creación y modificación de documentos de Microsoft Word (.docx) de forma programática
#install.packages("ggplot2") # Para los graficos
#install.packages("boot")   #para llevar a cabo el Bootstrap y calcular intervalos de confianza
#install.packages("lmtest") # pruebas y diagnósticos sobre modelos de regresión lineal
#install.packages("car")   #para el análisis de regresión y diagnóstico de modelos de regresión lineal y no lineal
#install.packages("dplyr") #para la manipulación de datos
#install.packages("xtable") #para crear tablas de formato LaTeX, HTML o texto a partir de objetos de R como dataframes
#install.packages("DT") #para crear y renderizar tablas interactivas y dinámicas en aplicaciones web utilizando el lenguaje de programación R.
#install.packages("jpeg") # para almacenar y compartir imágenes digitales
#install.packages("openxlsx") #para trabajar con archivos Excel (.xlsx)
#install.packages("readxl") #para leer datos desde archivos de Excel
#install.packages("cowplot") #extensión de "ggplot2" en R que facilita la creación de gráficos complejos
#install.packages("modeest") # para calcular y estimar la moda 
#install.packages("psych")
install.packages("yardstick")
install.packages("tidymodels")
install.packages("rsample")
install.packages("recipes")
install.packages("parsnip")
install.packages("workflows")
installed.packages("glm")
install.packages("caret")

# Cargar los Paquetes
library(caret)
library(workflows)
library(parsnip)
library(recipes)
library(rsample)
library(yardstick)
library(modeest)
library(rvest)
library(purrr)
library(dplyr)
library(writexl)
library(kableExtra)
library(knitr)
library(flextable)
library(ggplot2)
library(boot)
library(lmtest)
library(car)
library(xtable)
library(DT)
library(jpeg)
require(pacman)
library(stargazer)
library(readxl)
library(openxlsx)
library(cowplot)
library(psych)
library(tidymodels)
p_load(tidyverse, skimr, stargazer, tidymodels, broom,knitr,kableExtra)

#-----------------------------------------------EJERCICIO 1----------------------------------------------#
#Importar data
# Se importa la data desde la página web "https://ignaciomsarmiento.github.io/GEIH2018 sample/ que contiene la data de 
#la encuesta  GEIH de Bogota from the 2018 "MediciC3n de Pobreza Monetaria y Desigualdad Report
#este código se utiliza para realizar web scraping en varias páginas web cuyas URL se encuentran en el vector urls

urls <- c("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_1.html","https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_2.html","https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_3.html","https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_4.html","https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_5.html","https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_6.html","https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_7.html","https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_8.html","https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_9.html","https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_10.html")

Import_data <- function(urls) {
  webpage <- read_html(urls)
  
  # Realiza el web scraping según la estructura de la página
  table2 <- webpage %>% 
    html_nodes("table") %>% 
    html_table(fill = TRUE)
  
  return(table2)
  
}

Tablas <- map(urls, Import_data)
dataframes <- map(Tablas, as.data.frame)
Tabla_Total <- bind_rows(dataframes) # Se combinan todas las tablas en un unico dataframe llamado Tabla_Total

####-------------------------------------Limpieza de Datos------------------------------------------####

Tabla_2 <- Tabla_Total %>% filter(age > 18) #Excluir datos de individuos menores a 18 años
Tabla_2 <- Tabla_2 %>% filter(dsi == 0) # Excluir los desempleados
Tabla_2 <- Tabla_2 %>% filter(pea == 1) # Excluir la población económicamente inactiva
Tabla_2 <- Tabla_2 %>% rename(w_hora=y_salary_m_hu) # Renombrar la variable Dependiente
Porc_NA <- mean(is.na(Tabla_2$w_hora))* 100 # determinar que % de NA tiene w_hora
cat("La variable w_hora contiene un % de NA=40.32' es:",Porc_NA, "%\n")
Tabla_3 <- Tabla_2[!is.na(Tabla_2$w_hora),] #selecciona todas las filas donde la variable w_hora (Variable de interess) no tenga valores faltantes
# se seleccionan las columnas que no superan el umbral de 30% de valores faltantes.
Criterio <- 30
Tabla_4 <- Tabla_3 %>%
  select(where(~(sum(is.na(.)) / length(.)) * 100 <= Criterio))

# Identificar las columnas con NA
Col_NA <- colnames(Tabla_4)[colSums(is.na(Tabla_4)) > 0]
cat("Columnas que contienen valores NA:\n")
cat(Col_NA, "\n")
Porc_NA1 <- mean(is.na(Tabla_4$maxEducLevel))* 100 # % de NA en maxEducLevel

# Calcular la moda en maxEducLevel e Imputarla en esta columna
Moda_MLE <- as.character(names(sort(table(Tabla_4$maxEducLevel), decreasing = TRUE)[1]))
Tabla_4$maxEducLevel[is.na(Tabla_4$maxEducLevel)] <- Moda_MLE

# Se calcula una variable del total Remuneraciones Monetarias Extras
Tabla_4 <- Tabla_4 %>%
  mutate(RME = p6510s1 + p6545s1+p6580s1+p6630s1a1+p6630s2a1+p6630s3a1+p6630s4a1
         +p6630s6a1)
summary(Tabla_4$RME)
Porc0_RME <- (sum(Tabla_4$RME == 0, na.rm = TRUE) / sum(!is.na(Tabla_4$RME))) * 100

# Se calcula una variable para Remuneraciones Extras en Especies
Tabla_4 <- Tabla_4 %>%
  mutate(RES = p6585s1a1+p6585s2a1+p6585s3a1+p6585s4a1+p6590s1+p6600s1+p6610s1+p6620s1)
summary(Tabla_4$RES)
Porc0_RES <- (sum(Tabla_4$RES == 0, na.rm = TRUE) / sum(!is.na(Tabla_4$RES))) * 100

#Tabla_htm <- Tabla_2 %>% filter(totalHoursWorked) 
Tabla_2_1 <- Tabla_2 %>% filter(formal == 1)
Tabla_2_0 <- Tabla_2 %>% filter(informal == 1)

Graph_ht1 <- ggplot(Tabla_2_1, aes(x = totalHoursWorked, y = w_hora)) +
  geom_point(alpha = 0.5, color = "red") +
  labs(x = "Horas Trabajadas Sector Formal", y = "Salario por Hora", title = "Grafica 3: Colombia 2018:Salario y Horas Trabajadas Formal") +
  theme_minimal() +
  theme(panel.grid.major = element_line(color = "gray"),
        axis.line = element_line(color = "gray"),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.title = element_text(hjust = 0.5))
Graph_ht1

Graph_ht0 <- ggplot(Tabla_2_0, aes(x = totalHoursWorked, y = w_hora)) +
  geom_point(alpha = 0.5, color = "red") +
  labs(x = "Horas Trabajadas Sector Informal", y = "Salario por Hora", title = "Grafica 4: Colombia 2018:Salario y Horas Trabajadas Informal") +
  theme_minimal() +
  theme(panel.grid.major = element_line(color = "gray"),
        axis.line = element_line(color = "gray"),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.title = element_text(hjust = 0.5))
Graph_ht0

plot_grid(Graph_ht1,Graph_ht0, ncol=2)

# Imputar el valor del umbral a los valores > 72
media_ht <- round(mean(Tabla_4$totalHoursWorked))
desv_ht <-  round(sd(Tabla_4$totalHoursWorked))
umbral <- media_ht + 2*desv_ht
Tabla_4$totalHoursWorked[Tabla_4$totalHoursWorked > 72] <- umbral

# Calcular la moda en p6426 (Experiencia) e Imputarla en esta columna
Moda_Exp <- as.numeric(names(sort(table(Tabla_4$p6426), decreasing = TRUE)[1]))
Tabla_4$p6426[Tabla_4$p6426 > 68] <- Moda_Exp

# Renombrar las variables para una mayor comprensión de las variables que estamos trabajando
Tabla_4 <- Tabla_4 %>% rename(c_mne=p6210) #Nivel educativo más alto
Tabla_4 <- Tabla_4 %>% rename(Edad=age) # edad
Tabla_4 <- Tabla_4 %>% rename(Sexo=sex) #sexo
Tabla_4 <- Tabla_4 %>% rename(n_esc_apr=p6210s1) #escolaridad
Tabla_4 <- Tabla_4 %>% rename(c_ocup=p6240) #ocupación
Tabla_4 <- Tabla_4 %>% rename(exp=p6426) #experiencia 
Tabla_4 <- Tabla_4 %>% rename(n_hsem=hoursWorkUsual) #horas trabajadas en la semana
Tabla_4 <- Tabla_4 %>% rename(Size_empresa=p6870) # No. de Empleados por empresa
Tabla_4 <- Tabla_4 %>% rename(c_cotiz=p6920) #cotiza en fondo de pensiones
Tabla_4 <- Tabla_4 %>% rename(Horas_trabajadas=totalHoursWorked)
Tabla_4 <- Tabla_4 %>% rename(Ing_Total=ingtot) #Ingreso Total
Tabla_4 <- Tabla_4 %>% rename(Educ1=maxEducLevel) #Maximo nivel educativo
Tabla_4 <- Tabla_4 %>% rename(Estrato=estrato1) #Estrato de energia
Tabla_4 <- Tabla_4 %>% rename(Ingreso_Mon_1=impa) #Ingreso monetario de la primera actividad antes de imputación
Tabla_4 <- Tabla_4 %>% rename(Ingreso_Mon_2=isa) #Ingreso monetario de la segunda actividad antes de imputación
Tabla_4$Educ <- as.integer(Tabla_4$Educ1)

Tabla_4$oficio <- as.factor(Tabla_4$oficio)

#Analisis Descriptivo de los Datos:

#Selección de Variables a Analizar
Tabla_4 <- Tabla_4 %>%
  select(Estrato, Sexo, Edad, Educ, n_esc_apr, exp,RES, RME, 
         n_hsem, Size_empresa, c_cotiz,pet, Ingreso_Mon_1, Ingreso_Mon_2,
         Ing_Total, depto, c_mne, college, cotPension,oficio,
         dsi, pea, inac, Horas_trabajadas, formal, informal, cuentaPropia, microEmpresa, 
         sizeFirm, y_salary_m,  w_hora, y_ingLab_m, y_ingLab_m_ha, y_total_m, y_total_m_ha)

# Revisión de stadística descriptivas de variables para el Modelo
Tabla_Stat <- Tabla_4  %>% select(Horas_trabajadas, 
                                  Educ, 
                                  Edad, 
                                  exp,
                                  Size_empresa,
                                  Estrato)
stargazer(data.frame(Tabla_Stat), header=FALSE, type='text',title="Estadisticas Descriptivas Variables Seleccionadas")

## Asignando etiquetas a la variable Nivel Educativo
Tabla_4 <- Tabla_4 %>%
  mutate(Nivel_Educativo = case_when(
    Educ == 1 ~ "Ninguna",
    Educ == 2 ~ "Preescolar",
    Educ == 3 ~ "Primaria Incompleta",
    Educ == 4 ~ "Primaria Completa",
    Educ == 5 ~ "Secundaria Incompleta",
    Educ == 6 ~ "Secundaria Completa",
    Educ == 7 ~ "Universitaria",
    TRUE ~ as.character(Educ) 
  ))

# Tabla segmentada por nivel educativo y Salario
Tabla_Educ <- Tabla_4 %>%
  group_by(Nivel_Educativo) %>%
  summarize(
    Relative_W_hora = (sum(w_hora) / sum(Tabla_4$w_hora)) * 100,  
    Relative_Salary = (sum(y_salary_m) / sum(Tabla_4$y_salary_m)) * 100, 
    Relative_IngresoT = (sum(Ing_Total) / sum(Tabla_4$Ing_Total)) * 100 
  )
colnames(Tabla_Educ) <- c("Nivel Educativo", "Salario por hora", "Salario Mensual","Ingreso Total")
Tabla_Educ

Tabla_Educ <- Tabla_Educ %>%
  kbl() %>%
  kable_styling(full_width = FALSE)
Tabla_Educ

#### Analisis descriptivo del ingreso
Ingresos <- Tabla_4%>%
  select(y_salary_m, Ingreso_Mon_1, Ingreso_Mon_2, Ing_Total, RES, RME,
         w_hora, y_ingLab_m, y_ingLab_m_ha)
colnames(Ingresos) <- c("Salario Mensual", "Ingreso Monetario 1","Ingreso Monetario 2",
                        "Ingreso Total", "Rem. Adicionales en Especie", "Rem. Monetarias Adicionales",
                        "Salario por hora", "Ingreso Laboral Mensual", "Ingreso Laboral por Hora")
res_stat <- summary(Ingresos)
res_stat_m <- stargazer(data.frame(Ingresos), digits=1, header=FALSE, type='text',title="Estadisticas Descriptivas Variables Ingresos")
res_stat_m <- as.data.frame(res_stat_m)
#Tabla_res_stat <- "C:/Output R/Taller_1/Taller_1/Tabla_Stat.xlsx"
#write_xlsx(res_stat_m, path = Tabla_res_stat)


# Se crea Rangos de edades que formara parte de una Tabla
Tabla_4 <- Tabla_4 %>%
  mutate(RangoEdad = cut(Edad, breaks = c(18, 25, 30, 40, 50, 60, 90),
                         labels = c("18-25", "26-35", "36-45", "46-55", "56-65", "66-90")))

# Tabla de Ingresos por rango de edades
Tabla_ingresos_edad <- Tabla_4 %>%
  group_by(RangoEdad) %>%
  summarise(Salario_Hora = mean(w_hora),
            Sal_Mensual = mean(y_salary_m),
            Ingreso_Total = mean(Ing_Total),
            Nivel_Educ = round(mean(Educ)),
            Estrat = round(mlv(Estrato)),
            Tam_empresa = round(mlv(Size_empresa)),
            CantidadPersonas = round((sum(Sexo) / sum(Tabla_4$Sexo)) * 100,digits = 1))
colnames(Tabla_ingresos_edad) <- c("Rango de Edad", "Salario por hora", "Salario Mensual","Ingreso Total","Nivel Educativo Medio","Estrato","Size Empresas", "% de Individuos")
Tabla_ingresos_edad

Tabla_ingresos_edad <- Tabla_ingresos_edad %>%
  kbl() %>%
  kable_styling(full_width = FALSE)
Tabla_ingresos_edad


# Transformación de variables para el Modelo en Base a la teoría de Mincer
Tabla_4$Sector <- ifelse(Tabla_4$formal == 1 & Tabla_4$informal == 0, 1, 0) # dummy del sector formal e informal
Tabla_4$lw_hora <- log(Tabla_4$w_hora) # Transformación Logaritmica del salario por hora
Tabla_4$exp2 <- Tabla_4$exp^2  # Construcción de la variable experiencia al cuadrado
Tabla_4$Horas_trabajadas2 <- Tabla_4$Horas_trabajadas^2  # Construcción de la variable horas trabajadas al cuadrado
Tabla_4$Edad2 <- Tabla_4$Edad^2 # Construcción de la variable Edad al cuadrado
Media_w_hora <- mean(Tabla_4$w_hora)
Tabla_5 <- Tabla_4
Tabla_5 <- Tabla_5 %>%
  mutate(Sexo = ifelse(Sexo == 0, "Mujer", "Hombre"))

Tabla_Sexo <- Tabla_5 %>%
  group_by(Sexo) %>%
  summarize(Ingreso_Promedio = mean(w_hora),
            Salario_Promedio = mean(y_salary_m),
            Ingreso_Laboral = mean(y_ingLab_m_ha),
            Ingreso_Total = mean(y_total_m),
            Individuos = n())
colnames(Tabla_Sexo) <- c("Sexo", "Salario Real por Hora","Salario Nominal Mensual","Ingreso Laboral","Ingreso Total" ,"Cantidad")
Tabla_Sexo <- as.data.frame(Tabla_Sexo)
#Tabla_S <- "C:/Output R/Taller_1/Taller_1/Tabla_S1.xlsx"
#write_xlsx(Tabla_Sexo, path = Tabla_S)

# grafica del Log(w_hora), respecto a la Educación
#Link_C1 <- "C:/Output R/Taller_1/Taller_1/views/graph1.jpeg"
#jpeg(file = Link_C1, width = 800, height = 300)
Graph_we <- ggplot(Tabla_4, aes(x = Nivel_Educativo, y = lw_hora)) +
  geom_point(alpha = 0.5, color = "red") +
  labs(x = "Educación", y = "Salario por Hora", title = "Grafica 1: Colombia 2018:Relación entre Salario y la Educación") +
  theme_minimal() +
  theme(panel.grid.major = element_line(color = "gray"),
        axis.line = element_line(color = "gray"),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.title = element_text(hjust = 0.5))
Graph_we
dev.off() 

# grafica del Log(w_hora), respecto a la Educación
#Link_C1 <- "C:/Output R/Taller_1/Taller_1/views/graph1.jpeg"
#jpeg(file = Link_C1, width = 800, height = 300)

#Tabla_4 <- as.data.frame(Tabla_4)
#Tabla_4_Ex <- "C:/Output R/Taller_1/Taller_1/Tabla_Excel.xlsx"
#write_xlsx(Tabla_4, path = Tabla_4_Ex)


###---------------------------------------Regresión Ejercicio 1-------------------------------------------#
Mod <- lm.fit <- lm(lw_hora ~ Educ + exp + exp2 + Sexo + Edad + Horas_trabajadas + Size_empresa + Sector + Estrato, data = Tabla_4)
Mod_stargazer <- stargazer(Mod,type="text", omit.stat=c("ser","f","adj.rsq"),  digits = 3)
Mod_stargazer <- as.data.frame(Mod_stargazer)
#Reg <- "C:/Output R/Taller_1/Taller_1/Mod_stargazer.xlsx"
#write_xlsx(Mod_stargazer, path = Reg )

# Significancia Económica parámetros
Coef <- Mod$coefficients
SE0 <- (exp(Coef)-1)*100
Sig_Economica <- round((SE0/Media_w_hora)*100, digits = 3)
Sig_Economica <- as.data.frame(Sig_Economica)
#T1 <- "C:/Output R/Taller_1/Taller_1/T1_Se.xlsx"
#write_xlsx(Sig_EconC3mica, path = T1 )


##--------------------------------------------EJERCICIO_3----------------------------------------------##

#---------------------------------Incisio 3.A) Regresion Table------------------------------------------#
#Modelo de regresión no lineal:
Mod2 <- lm(lw_hora ~ Edad + Edad2, data = Tabla_4)
Mod2_stargazer <- stargazer(Mod2, type="text", omit.stat=c("ser","f","adj.rsq"))
Mod2_stargazer <- as.data.frame(Mod2_stargazer)
#Reg <- "C:/Output R/Taller_1/Taller_1/Mod2_stargazer.xlsx"
#write_xlsx(Mod2_stargazer, path = Reg )

#---------------------------------3.B) Significancia EconC3mica parC!metros--------------------------------#
Coefs <- Mod2$coefficients
SE1 <- (exp(Coefs)-1)*100
Sig_Economica1 <- round(SE1/Media_w_hora*100, digits = 3)
Sig_Economica1 <- as.data.frame(Sig_Economica1)
#T2 <- "C:/Output R/Taller_1/Taller_1/T2_Se.xlsx"
#write_xlsx(Sig_Economica1, path = T2)

#------------------3.C) Crear la Edad Maxima y construir los intervalos de confianza con boostrap-----------#
## Se calcula la edad Maxima mediante Boostrap
library(boot)
Edad_Mod2 <-function(data,index){
  
  Mod2 <-lm(lw_hora ~ Edad + Edad2, data = Tabla_4, subset = index)
  
  Coefs<-Mod2$coefficients
  
  b1<-Coefs[1] 
  b2<-Coefs[2]
  b3<-Coefs[3] 
  
  Edad_Maxima <- round(b2/(-2*b3))
  
  return(Edad_Maxima)
}

set.seed(123)
Res_Edad <- boot(data=Tabla_4, Edad_Mod2,R=1000)
Res_Edad

Tabla_EdadM = Res_Edad$t

Intervalo_Edadm <- quantile(Tabla_EdadM,c(0.025,0.975)) # Intervalo de Confianza Boostrap
print(Intervalo_Edadm)

# Secuencia de edades para el gráfico de tal forma que se puedan obtener la función concava del perfil de Ingres
Edad_seq <- seq(min(Tabla_4$Edad), max(Tabla_4$Edad))
print (Edad_seq)

# Prediccion del Perfil de Ingreso
Perfil_Ingreso <- exp(predict(Mod2, newdata = data.frame(Edad = Edad_seq, Edad2 = Edad_seq^2)))

# Calcular los intervalos de confianza para el Grafico perfil de ingresos
Interv_Conf <- predict(Mod2, newdata = data.frame(Edad = Edad_seq, Edad2 = Edad_seq^2), interval = "confidence")

# Extraer los limites inferior (lwr) y superior (upr) de los intervalos de confianza
lwr <- exp(Interv_Conf[, "lwr"])
upr <- exp(Interv_Conf[, "upr"])

## Se construye el GrC!fico del Perfil de Ingreso
#Link_C <- "C:/Output R/Taller_1/Taller_1/views/graph2.jpeg"
#jpeg(file = Link_C, width = 900, height = 600)
plot(Edad_seq, Perfil_Ingreso, type = "l", xlab = "Edad", ylab = "Salario por Hora Estimado", main = "Grafica 2: Perfil Estimado de Edad-Ingresos")  # Vuelve a crear el grC!fico dentro de png()
lines(Edad_seq, lwr, col = "red", lty = 2)
lines(Edad_seq, upr, col = "red", lty = 2)
Edad_Max <- Edad_seq[which.max(Perfil_Ingreso)]
text(Edad_Max, max(Perfil_Ingreso), labels = "Max", pos = 1, col = "blue", cex = 1.5, srt = 0)
dev.off()  

#---------------------------------------------------------------------------------------------------
#----------------------------------------------------Ejercicio_4------------------------------------#

#(a) Begin by estimating and discussing the unconditional wage gap:log(w) = b1+ b2Female + u (3)
#where Female is an indicator that takes one if the individual in the sample is

### Crear la variable mujer
Tabla_4$mujer <- ifelse(Tabla_4$Sexo == 0, 1, 0)

#-----------------------------------------------Incisio 4.A-------------------------------------#
brecha_salarial <- lm(lw_hora ~ mujer, data = Tabla_4)
Modm_stargazer <- stargazer(brecha_salarial, type="text", digits=3, omit.stat=c("ser","f","adj.rsq"))
Modm_stargazer <- as.data.frame(Modm_stargazer)
#Regm <- "C:/Output R/Taller_1/Taller_1/Modm_stargazer.xlsx"
#write_xlsx(Modm_stargazer, path = Regm)

#----------------------------------------------INCISO 4.Bi----------------------------------------#
# Regresión log(w_hora) sobre las demas variables
Reg_bs1<-lm(lw_hora ~ mujer + Edad +Edad2 + Educ + exp + exp2 + Size_empresa + Horas_trabajadas + Sector + Estrato, data =Tabla_4)
stargazer(Reg_bs1,type="text",digits=3, omit.stat=c("ser","f","adj.rsq"))

#1) Regresion var=mujer sobre las demas variables (Reg1)
Tabla_4 <-Tabla_4 %>% mutate(Mujer_Resid=lm(mujer~ Edad + Edad2 + Educ + exp + exp2 + Size_empresa + Horas_trabajadas + Sector + Estrato,Tabla_4)$residuals)

#2) Regresión log(w_hora) sobre las demas variables excepto mujer
Tabla_4 <-Tabla_4 %>% mutate(lw_hora_Resid=lm(lw_hora~ Edad + Edad2+ Educ + exp + exp2 + Size_empresa + Horas_trabajadas + Sector + Estrato,Tabla_4)$residuals) #Residuals of mpg~foreign

#3) Regresión de los residuos de la Reg1 sobre los residuos de la Reg2
Reg_bs2<-lm(lw_hora_Resid ~ Mujer_Resid,Tabla_4)

Mod3_stargazer <- stargazer(Reg_bs1,Reg_bs2,type="text",digits=6, omit.stat=c("ser","f","adj.rsq")) 
Mod3_stargazer <- as.data.frame(Mod3_stargazer)
#Reg3 <- "C:/Output R/Taller_1/Taller_1/Mod3_stargazer.xlsx"
#write_xlsx(Mod3_stargazer, path = Reg3)

# Significancia Económica parámetros
Coefs2 <- Reg_bs1$coefficients
SE2 <- (exp(Coefs2)-1)*100
Sig_Economica2 <- round(SE2/Media_w_hora*100, digits = 3)
Sig_Economica2 <- as.data.frame(Sig_Economica2)
#T3 <- "C:/Output R/Taller_1/Taller_1/T3_Se.xlsx"
#write_xlsx(Sig_Economica2, path = T3)

# Significancia EconC3mica parámetros
Coefs3 <- Reg_bs2$coefficients
SE3 <- (exp(Coefs3)-1)*100
Sig_Economica3 <- round(SE3/Media_w_hora*100, digits = 3)
Sig_Economica3 <- as.data.frame(Sig_Economica3)


#------------------------------------------------------ Inciso 4.Bii------------------------------------#

B <- 1000  

# Matrices para almacenar los resultados de fwl_mod1 y fwl_mod2
coef_mod1 <- matrix(NA, nrow = B, ncol = 2)  # Coeficientes de fwl_mod1
coef_mod2 <- matrix(NA, nrow = B, ncol = 2)  # Coeficientes de fwl_mod2


# Realizar el proceso de Bootstrap
set.seed(123)
boots_fwl <- for (i in 1:B) {
  # Crear una muestra bootstrap
  sample_indices <- sample(1:nrow(Tabla_4), replace = TRUE)
  sample_data <- Tabla_4[sample_indices, ]
  
  # Ajustar brecha_salarial1 en la muestra bootstrap
  fwl_mod1 <- lm(lw_hora ~ mujer + Edad + Edad2 + Educ + exp + exp2 + Size_empresa + Horas_trabajadas + Sector + Estrato, data = sample_data)
  
  # Regresion var=mujer sobre las demas variables (Reg1)
  sample_data<- sample_data %>%
    mutate(Mujer_Resid  = lm(mujer ~ Edad + Edad2 + Educ + exp + exp2 + Size_empresa + Horas_trabajadas + Sector + Estrato, data = sample_data)$residuals)
  
  # Regresión log(w_hora) sobre las demas variables excepto mujer
  sample_data<- sample_data %>%
    mutate(lw_hora_Resid = lm(lw_hora ~ Edad + Edad2 + Educ + exp + exp2 + Size_empresa + Horas_trabajadas + Sector + Estrato, data = sample_data)$residuals)
  
  #Regresión de los residuos de la Reg1 sobre los residuos de la Reg2
  fwl_mod2 <- lm(lw_hora_Resid ~ Mujer_Resid, data = sample_data)
  
  # Almacenar los coeficientes estimados y errores estándar
  coef_mod1[i, 1] <- coef(fwl_mod1)["mujer"]
  coef_mod1[i, 2] <- summary(fwl_mod1)$coefficients["mujer", "Std. Error"]
  
  coef_mod2[i, 1] <- coef(fwl_mod2)["Mujer_Resid"]
  coef_mod2[i, 2] <- summary(fwl_mod2)$coefficients["Mujer_Resid", "Std. Error"]
  
}

Mod4_stargazer <- stargazer(fwl_mod1,fwl_mod2,type="text",digits=3, omit.stat=c("ser","f","adj.rsq")) 
Mod4_stargazer <- as.data.frame(Mod4_stargazer)
#Reg4 <- "C:/Output R/Taller_1/Taller_1/Mod4_stargazer.xlsx"
#write_xlsx(Mod4_stargazer, path = Reg4)

#Comparativo de regresiones con y sin Bootstrap:
Mod_Comparativos <- stargazer(Reg_bs1,Reg_bs2, fwl_mod1, fwl_mod2, type="text",digits=3, 
                              omit.stat=c("ser","f","adj.rsq"),
                              notes = c("Notas: (1) y (2) con muestra C:nica y (3) y (4) con Bootstrap"))
Mod_Comparativos <- as.data.frame(Mod_Comparativos)
#Reg_Comp <- "C:/Output R/Taller_1/Taller_1/Mod_Comparativos.xlsx"
#write_xlsx(Mod_Comparativos, path = Reg_Comp)

print(coef_mod1)
print(coef_mod2)

#################################################Ejercicio 4.c##############################################

### Cálculo de la diferencias de edades

n <- 1000
# Vectores para almacenar las edades máximas y las diferencias en edades máximas
edadh <- numeric(n)
edadm <- numeric(n)
diff <- numeric(n)

set.seed(123)
for (i in 1:n) {
  # Genera una muestra bootstrap
  sample_diff <- sample(1:nrow(Tabla_4), replace = TRUE)
  datos_diff <- Tabla_4[sample_diff, ]
  
  Mod5 <- lm(lw_hora ~ Edad + Edad2 + mujer + Edad*mujer + Edad2*mujer, data = datos_diff)
  coefs2 <- coef(Mod5)
  
  # El coeficiente correspondiente a 'Edad' en la regresión lineal
  d1 <- coefs2['Edad']
  d2 <- coefs2['Edad2']
  d3 <- coefs2['Edad:mujer']
  d4 <- coefs2['Edad2:mujer']
  d5 <- coefs2['mujer']
  
  # C.P.O hombres
  edad_maxh <- round(-d1 / (2 * d2))
  
  # C.P.O mujer
  edad_maxm <- round((-d1 - d3) / (2 * (d2 + d4)))
  
  # Diferencia en edades máximas entre hombres y mujeres
  difmax <- edad_maxh - edad_maxm
  
  # Almacena los resultados en los vectores
  edadh[i] <- edad_maxh
  edadm[i] <- edad_maxm
  diff[i] <- difmax
  
}

# Intervalo de confianza para la diferencia en edades máximas
nivel_confianza <- 0.95
quantil_inf <- (1 - nivel_confianza) / 2
quantil_sup <- 1 - quantil_inf

intervalo_confianza_diferencia <- quantile(diff, c(quantil_inf, quantil_sup))

#### Tabla de Regresion Boostrap de las Diferencias de Edades Maximas Mujer y Hombre
Mod5_stargazer <- stargazer(Mod5, type="text", omit.stat=c("ser","f","adj.rsq"),  digits = 3)
Mod5_stargazer <- as.data.frame(Mod5_stargazer)
#Reg5 <- "C:/Output R/Taller_1/Taller_1/Mod5_stargazer.xlsx"
#write_xlsx(Mod5_stargazer, path = Reg5)

# Significancia EconC3mica parC!metros
Coefs4 <- Mod5$coefficients
SE3 <- (exp(Coefs4)-1)*100
Sig_Economica3 <- round(SE2/Media_w_hora*100, digits = 3)
Sig_Economica3 <- as.data.frame(Sig_Economica3)
#T4 <- "C:/Output R/Taller_1/Taller_1/T4_Se.xlsx"
#write_xlsx(Sig_Economica3, path = T4 )

#### Creacion del GrC!fico 

# Secuencia de edades para utilizar en el perfil de ingreso segun el Size de la muestra de la base de datos
Edad_f = seq(min(Tabla_4$Edad), max(Tabla_4$Edad), length.out = 7378)

# Predicciones utilizando el modelo para hombres y mujeres
predh <- (predict(Mod5, newdata = data.frame(Edad = Edad_f, Edad2 = Edad_f^2, mujer = 0)))
predm <- (predict(Mod5, newdata = data.frame(Edad = Edad_f, Edad2 = Edad_f^2, mujer = 1)))

# Intervalos de confianza para las predicciones de ingresos de hombres y mujeres
int_hombres <-(predict(Mod5, newdata = data.frame(Edad = Edad_f, Edad2 = Edad_f^2, mujer = 0), interval = "confidence"))
int_mujeres <- (predict(Mod5, newdata = data.frame(Edad = Edad_f, Edad2 = Edad_f^2, mujer = 1), interval = "confidence"))

# Se crea dos bases de datos con las edades segun hombre y mujer
edadh <- data.frame(
  Edad = seq(min(Tabla_4$Edad), max(Tabla_4$Edad), length.out = 7378),
  Edad2 = seq(min(Tabla_4$Edad), max(Tabla_4$Edad)^2, length.out = 7378),
  mujer = 0)

edadm <- data.frame(
  Edad = seq(min(Tabla_4$Edad), max(Tabla_4$Edad), length.out = 7378),
  Edad2 = seq(min(Tabla_4$Edad), max(Tabla_4$Edad)^2, length.out = 7378),
  mujer = 1)

# Se crean datos con las edades y las predicciones segun genero

dfpredh <- data.frame(
  Edad = edadh$Edad,
  Prediccionesh = predh,
  Genero = 'Hombres'
)

dfpredm <- data.frame(
  Edad = edadm$Edad,
  Prediccionesm = predm,
  Genero = 'Mujeres'
  
)

graph_m <- ggplot(dfpredm, aes(x = Edad, y = Prediccionesm)) +
  geom_line(color = 'red') +
  geom_ribbon(aes(ymin = int_mujeres[, "lwr"], ymax = int_mujeres[, "upr"]), fill = 'red', alpha = 0.2) +
  labs(x = 'Edad', y = 'Perfil de Ingreso', title = expression(atop("PredicciC3n del Perfil de Ingreso Mujeres", ""))) +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.line = element_line(color = "gray"))+
  theme(panel.background = element_rect(fill = "transparent", color = NA),
        panel.grid = element_line(color = "gray")) +
  coord_cartesian(xlim = c(18, 86), ylim = c(6.8, 9)) 
graph_m

graph_h <- ggplot(dfpredh, aes(x = Edad, y = Prediccionesh)) +
  geom_line(color = 'blue') +
  geom_ribbon(aes(ymin = int_hombres[, "lwr"], ymax = int_hombres[, "upr"]), fill = 'blue', alpha = 0.2) +
  labs(x = 'Edad', y = 'Perfil de Ingreso', title = expression(atop("PredicciC3n del Perfil de Ingreso Hombres", ""))) +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.line = element_line(color = "gray"))+
  theme(panel.background = element_rect(fill = "transparent", color = NA),
        panel.grid = element_line(color = "gray")) +
  coord_cartesian(xlim = c(18, 86), ylim = c(7.6, 9)) 
graph_h

#GrC!fica Conjunta Hombre-Mujer
#Link_C1 <- "C:/Output R/Taller_1/Taller_1/views/graph4.jpeg"
#jpeg(file = Link_C1, width = 1200, height = 600)
plot_grid(graph_h,graph_m, ncol=2)
#dev.off()  

## -----------------------------------------------------------------------------
##
## Punto 5: Predicting earnings . In the previous sections, you estimated some 
## specifications with inference in mind. In this subsection, we will evaluate 
## the predictive power of these specifications.
##
## -----------------------------------------------------------------------------


# a. Split Sample:--------------------------------------------------------------
set.seed(123)

sample_split <- initial_split(Tabla_4, prop = .7)

# Create test and train data frames 

train <- training(sample_split)
test  <- testing(sample_split)



model1 <- recipe(lw_hora~ Edad + Edad2, data = train)
model2 <- recipe(lw_hora~ Sexo, data = train)
model3 <- recipe(lw_hora~ Edad + Edad2 + Sexo, data = train)
model4 <- recipe(lw_hora~ Edad + Edad2 + Sexo + Horas_trabajadas , data = train)  %>%
  step_dummy(all_factor_predictors())
model5 <- recipe(lw_hora~ Edad + Edad2 + Sexo + Horas_trabajadas + Estrato, data = train)  %>%
  step_dummy(all_factor_predictors())
model6 <- recipe(lw_hora~ Edad + Edad2 + Sexo + Horas_trabajadas + Estrato + Educ, data = train)  %>%
  step_dummy(all_factor_predictors())
model7 <- recipe(lw_hora~ Edad + Edad2 + Sexo + Horas_trabajadas + Estrato + Educ + Sector, data = train)  %>%
    step_dummy(all_factor_predictors())
model8 <- recipe(lw_hora~ Edad + Edad2 + Sexo + Horas_trabajadas + Estrato + Educ + Sector + Size_empresa, data = train)  %>%
    step_dummy(all_factor_predictors())
model9 <- recipe(lw_hora~ Edad + Edad2 + Sexo + Horas_trabajadas + Estrato + Educ + Sector + Size_empresa + exp, data = train)  %>%
    step_dummy(all_factor_predictors())
model10 <- recipe(lw_hora~ Edad + Sexo + Horas_trabajadas + Estrato + Sector +Educ + Size_empresa + exp , data = train)  %>%
    step_dummy(all_factor_predictors())
model11 <- recipe(lw_hora~ Edad + Edad2 + Sexo + Horas_trabajadas + Estrato + Educ + Sector + Size_empresa + exp, data = train)  %>%
    step_interact(terms = ~ Sexo:Educ + Sexo:Estrato) %>%
    step_dummy(all_factor_predictors())
model12 <- recipe(lw_hora~ Edad + Edad2 + Sexo + Horas_trabajadas + Estrato + Educ + Sector + Size_empresa + exp, data = train)  %>%
  step_interact(terms = ~ Sexo:Educ + Sexo:Sector) %>%
  step_dummy(all_factor_predictors())
model13 <- recipe(lw_hora~ Edad + Edad2 + Sexo + Horas_trabajadas + Estrato + Educ + Sector + Size_empresa + exp, data = train)  %>%
  step_interact(terms = ~ Sexo:Educ + Sexo:Horas_trabajadas) %>%
  step_dummy(all_factor_predictors())
model14 <- recipe(lw_hora~ Edad + Edad2 + Sexo + Horas_trabajadas + Estrato + Educ + Sector + Size_empresa + exp + oficio, data = train)  %>%
  step_interact(terms = ~ Sexo:Educ + Sexo:Horas_trabajadas) %>%
  step_dummy(all_factor_predictors())
model15 <- recipe(lw_hora~ Edad + Edad2 + Sexo + Horas_trabajadas + Estrato + Educ + Sector + Size_empresa + exp + oficio + Horas_trabajadas2 , data = train)  %>%
  step_interact(terms = ~ Sexo:Educ + Sexo:Horas_trabajadas) %>%
  step_dummy(all_factor_predictors())
model16 <- recipe(lw_hora~ Edad + Edad2 + Sexo + Horas_trabajadas + Estrato + Educ + Sector + Size_empresa + exp + exp2 + oficio + Horas_trabajadas2 , data = train)  %>%
  step_interact(terms = ~ Sexo:Educ + Sexo:Horas_trabajadas) %>%
    step_dummy(all_factor_predictors())

#List models

modelos<-list(model1, model2, model3, model4, model5, model6, model7, model8, model9, model10, model11, model12, model13, model14,model15,model16 )


# Loop to FIT models with the package tidymodels


fit_model <- function(x, df=train) {
  
  linear_model <- linear_reg()
  
  workflow <- workflow() %>% 
    
    add_recipe(x) %>% 
    add_model(lm_model) %>% 
    model_with_fit <- workflow %>% 
    fit(data = df)
  
  model_with_fit
  
}


workflows_finales <- lapply(modelos, function(x){fit_model(x, train)})


# Create loop to test


predicciones_wf <- function(w, df_test=test) {
  predicciones_finales <- predict(w, new_data = df_test) %>% 
  bind_cols(df_test) %>% 
  predicciones_finales
  
}

# Sacar RMSE

rmse_predicciones <- function(pred) {
  rmse_validation <- rmse(pred, truth = lw_hora, estimate = .pred)
  rmse_validation$.estimate
  
}

pred_totales<- lapply(workflows_finales, function (w){predicciones_wf (w, test)})
rmse_total <- lapply(pred_totales, function (pred){rmse_predicciones(pred)})

# Tabla de RMSE

rmse <- data.frame(rmse_total)
colnames(rmse)<- c('model1', 'model2', 'model3', 'model4', 'model5', 'model6', 'model7', 'model8', 'model9','model10', 'model11','model12','model13','model14','model15','model16')
rmse <- t(rmse)
rmse <- data.frame(rmse)
#Tabla_rmse <- "C:/Output R/Taller_1/Taller_1/rmse.xlsx"
#write_xlsx(rmse , path = Tabla_rmse)

# Elegir los modelos para menor RMSE:

workflows_loocv <- rmse_df$Workflow[order(rmse_df$RMSE)[1:2]]

# Calcular los errores de predicción y calculo el Histograma
prediction_errors <- pred_totales$.pred - test$lw_hora
hist(prediction_errors, main = "Distribución de Errores de Predicción", xlab = "Error de Predicción")
summary(prediction_errors)


## D. LOOCV:

loocv_model1 <- vector("numeric", length = nrow(Tabla_4)) #Primer Modelo
for (i in seq_len(nrow(Tabla_4))) {
    loocv_tabla4 <- Tabla_4[-i, ]
    loocv_model_with_fit <- modelos[[2]] %>% fit(data = loocv_tabla4)
    pred <- predict(loocv_model_with_fit, new_data = slice(Tabla_4, i))$.pred
    loocv_model1[i] <- pred
    print(paste0("Iteración: ", i))
  
}

loocv_prediccion_final <- data.frame(lw_hora = Tabla_4$lw_hora, loocv_model1 = loocv_model1)
loocv_rmse <- rmse(data = loocv_prediccion_final, truth = lw_hora, estimate = loocv_model1)
loocv_rmse

loocv_model2 <- vector("numeric", length = nrow(Tabla_4)) #Segundo Modelo


for (i in seq_len(nrow(Tabla_4))) {
    loocv_tabla4 <- Tabla_4[-i, ]
    loocv_model_with_fit <- modelos[[15]] %>% fit(data = loocv_tabla4)
    pred <- predict(loocv_model_with_fit, new_data = slice(Tabla_4, i))$.pred
    loocv_model1[i] <- pred
    print(paste0("Iteration: ", i))
   
}

loocv_prediccion_final <- data.frame(lw_hora = Tabla_4$lw_hora, loocv_model2 = loocv_model2)
loocv_rmse <- rmse(data = loocv_prediccion_final, truth = lw_hora, estimate = loocv_model2)
loocv_rmse






