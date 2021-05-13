library("rgbif")
library("plyr")
library(dismo)
library(ENMeval)
library(maxnet)
library(raster)
library(rworldxtra)
library(sf)
library(tidyverse)
library(readxl) ## Necesario para leer excel


## Desde el video de "Crosso", 
library(ggmap)
library(ggplot2)
library(raster)
library(maptools)
library("rnaturalearth")
library("rnaturalearthdata")
library("sf")

library(ggspatial)



punctaUS<- occ_search(scientificName =
                        "Hypoponera punctatissima",
                      fields = c('scientificName','decimalLatitude',
                                 'decimalLongitude'),
                      hasCoordinate=TRUE, country = 'US', limit = 5000) 


## Pasando estos datos descargados a .CSV para editar (quitar filas vacias, duplicados y otros) en excel

punctaUS2.data <- punctaUS$data

write.csv(punctaUS2.data, "C:\\Users\\radio\\Documents\\RSTUDIO Saves\\punctagbifUS.csv")

## Probando si puedo cargar los datos desde excel scv:



puntausa<-read_excel(file.choose(),sheet = "punctausa1")
head(punta3)
head(puncta_occ)
head(puncta_SF)


##!!! Despues de muchos errores al pasar los datos directamente desde el excel con la función read_excel, producto de la clase de las columnas. 
## Por lejos lo mejor es dejar estos datos de manera inmediata como dataframe, esto se consigue:
# 1) Tras editar el excel con los datos (sacando duplicados y NA), copiar y pegar los datos en un archivo de texto simple (.TXT)
# 2) leer estos datos desde el environment, en "Import dataset" -> "From text" y buscar el archivo, editar los parámetros básicos y listo.
# 3) Lo anterior evita un montón de errores al computar las funciones. 


## Inicio de modelamiento

##Transformar a SF

##Notar que coords=c(1,2) hace referencia a la columna 1 y 2 de mis datos que contienen las coordenadas, ver bien, ya que usualmente es la especie, lat, lon y ah? ser?a C(2,3). En mi caso solo tengo lat, long
puncta_SF <- punctatxt %>% st_as_sf(coords = c(1, 2), crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")



##Generando pol?gono y Buffer

Hull <- puncta_SF %>% st_union() %>% st_convex_hull()
Buffer <- Hull %>% st_buffer(dist = 3) %>% st_as_sf()

plot(Buffer)

##Pol?gono en mapa


data("countriesHigh")
SA <- countriesHigh %>% st_as_sf() %>% st_make_valid() %>% st_crop(Buffer)

ggplot()+geom_sf(data=SA)+geom_sf(data=puncta_SF)+theme_bw()

## Aqu? tuve que dejar la resoluci?n en 2.5 para poder ver todo el mundo, sin definir una lon, lat, que pide al definir la res a 0.5
Bioclimatic <- getData(name = "worldclim", var = "bio", res = 2.5)
## capas climaticas
Bioclimatic <- Bioclimatic %>% crop(Buffer) %>% trim()
## Cortamos el Tile
names(Bioclimatic) <- str_remove_all(names(Bioclimatic), "_43")

##Generando Background (puntos de ausencia de la sp.). Aus hace referencia a ausencia, aunque correctamente debería llamarse background

##aca transformé primero las presencias contenidas en "punta3" a data frame en el objeto A, 
set.seed(2020)

#Y ahora pude asignar el mutate y dejarlo en el objeto "Pres", todo esto para poder unir Pres con Aus


Pres <- punctatxt %>% dplyr::select(longitude, latitude) %>% mutate(Pres = 1)%>% 
  as.data.frame() 


Aus <- dismo::randomPoints(mask = Bioclimatic[[1]], n = 5000) %>% 
  as.data.frame() %>% rename(longitude = x, latitude = y) %>% 
  mutate(Pres = 0)

## Uní las filas con rbind, pues con bind_rows me generaba un error pues en Press las primeras 2 columnas no estaban como dataframes sino como Character y Double

Pres_Aus <- rbind(Pres, Aus)
Pres_Aus_Sf <- Pres_Aus %>% st_as_sf(coords = c(1, 2), crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")

## Extracción de datos
Condiciones <- raster::extract(Bioclimatic, Pres_Aus_Sf) %>% 
  as.data.frame() %>% bind_cols(Pres_Aus)

## Modelando

set.seed(2020)
Mod1 <- maxnet(p = Condiciones$Pres, data = Condiciones[, 1:19],regmult = 1, maxnet.formula(p = Condiciones$Pres, data = Condiciones[,1:19], classes = "lq"))
Mod4 <- maxnet(p = Condiciones$Pres, data = Condiciones[, 1:19], 
               regmult = 1)

plot(Mod4, c("bio1", "bio2", "bio3", "bio12"))
Prediction <- predict(Bioclimatic, Mod4, type = "cloglog")

plot(Prediction, colNA = "black")


####


Eval <- dismo::evaluate(p = Pres[, 1:2], a = Aus[, 1:2], model = Mod4, x = Bioclimatic, type = "cloglog")


EvalDF <- Eval@confusion %>% as.data.frame %>% mutate(Threshold = Eval@t) %>% 
  mutate(TP_TN = (tp/nrow(punctatxt)) + (tn/5000))


EvalThres <- EvalDF %>% dplyr::filter(TP_TN == max(TP_TN))


Prediction <- Prediction %>% as("SpatialPixelsDataFrame") %>% as.data.frame() %>% mutate(Binary = ifelse(layer >= 
                                                                                                           EvalThres$Threshold, "Presencia", "Ausencia"))


### USando ENMeval

Results <- ENMevaluate(occ = punctatxt[, c(1, 2)], env = Bioclimatic, 
                       RMvalues = c(0.75, 1, 1.25), n.bg = 5000, method = "randomkfold", 
                       overlap = F, kfolds = 5, bin.output = T, fc = c("L", "LQ", 
                                                                       "LQP"), rasterPreds = T)


View(Results@results)

Models <- Results@results
Models$ID <- 1:nrow(Models)
Models <- Models %>% arrange(AICc)


BestModels <- Results@models[[Models$ID[1]]]
## Prediccion del mejor modelo
Prediction <- predict(Bioclimatic, BestModels, type = "cloglog") 


plot(Prediction, colNA = "black")