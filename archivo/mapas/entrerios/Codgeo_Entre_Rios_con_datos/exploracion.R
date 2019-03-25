library(ggplot2)
library(ggmap)
library(RColorBrewer)
library(purrr)


# resolvì errores de instalacion: sudo apt-get install libproj-dev libgdal-dev
install.packages("rgdal")
# resolvi problema instalando units: sudo apt-get install libudunits2-dev
install.packages("sf")

library(tidyverse)
library(sf)
require(devtools)
devtools::install_github("dkahle/ggmap", ref = "tidyup")
library(ggmap)
register_google(key = "AIzaSyDv-pvCNcmXOdmju_2nyZ-3Ilwk2SUDGJo") 
ggmap(get_googlemap())  

# Consulta exitosa
ggmap(get_map("Entre Ríos, Argentina", source = "stamen", zoom = 7, maptype = "terrain"))


geodata <- data.frame(
  lat = c(-34.6555,-34.6559,-34.7085,-34.5877,-34.6995,-34.6037,-34.7678,-34.7133, -34.7966),
  lon = c(-58.6452,-58.6167,-58.5859,-58.532,-58.3921,-58.3816,-58.3792,-58.3711, -58.276)
)


#https://www.datanalytics.com/libro_r/ejemplos-1.html
# HoustonMap +
# stat_density2d(aes(x = lon, y = lat, fill = ..level.., alpha = ..level..),
#                size = 2, data = crimes.houston,
#                geom = "polygon"
# )

# Trabajando con provincia y dptos
# https://datosgobar.github.io/datos-lab/radios-censales-1/#fn1
https://www.indec.gov.ar/ftp/cuadros/territorio/codgeo/Codgeo_Entre_Rios_con_datos.zip
unzip("Codgeo_Entre_Rios_con_datos.zip")
erios <- list.files()[[4]]
radios_entrerios <- st_read(erios)
radios_entrerios <- st_transform(radios_entrerios, crs = 4326)
summary(radios_entrerios)

ggplot() + geom_sf(data = radios_entrerios) +
  labs(title = "Radios Censales de Entre Ríos",
       subtitle = "Fuente: INDEC")

library(leaflet)

leaflet() %>%
  addTiles() %>%
  addProviderTiles("CartoDB.Positron") %>%
  # Mapeando los poligonos de Cordoba
  addPolygons(data = radios_entrerios,
              color = "grey", weight = 1, smoothFactor = 0.5,
              opacity = 1.0)

#https://journal.r-project.org/archive/2013-1/kahle-wickham.pdf

ggmap(get_map("Entre Ríos, Argentina", source = "stamen", zoom = 7, maptype = "terrain"))  

#############################################################################################

# descarga de archivo poligonos departamentos de Entre Rios
#https://analisisydecision.es/mapas-municipales-de-argentina-con-r/
argentina = readRDS("gadm36_ARG_2_sp.rds")
erios = argentina[argentina$NAME_1=="Entre Ríos",]

# guardar entre rios y convertirlo a vector 
library(rgdal)
saveRDS(erios, file = "erios.rds")

erios <- as.data.frame(erios)

causas_iniciadas_fueropaz_2018 <- causas_iniciadas_fueropaz_2018 %>% 
  mutate(circunscripcion = str_replace_all(circunscripcion, "Chajarí", "Federación")) %>% 
  group_by(circunscripcion) %>% 
  summarise(cantidad_inicidados = sum(Total, na.rm = T)) %>% 
  rename(NAME_2 = circunscripcion)

erios@data <- plyr::join(erios@data, causas_iniciadas_fueropaz_2018)

pal <- colorQuantile("YlGn", NULL, n = 5)
state_popup <- paste0("<strong>Estado: </strong>", 
                      argentina$NAME_1, 
                      "<br><strong>Femicidios: </strong>", 
                      argentina$Femicidios)

leaflet(data = erios) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(fillColor = ~pal(cantidad_inicidados), 
              fillOpacity = 0.8, 
              color = "#BDBDC3", 
              weight = 1)

HoustonMap +geom_point(aes(x = lon, y = lat, 
                           size = offense,colour = offense), data = violent_crimes )

parana <- geocode("Paraná")

jdospaz_gcode <- geocode(jdospaz$localidad)

> geocode(jdospaz$localidad)
Source : https://maps.googleapis.com/maps/api/geocode/json?address=Alcaraz&key=xxx-pvCNcmXOdmju_2nyZ-3Ilwk2SUDGJo
Source : https://maps.googleapis.com/maps/api/geocode/json?address=Basavilbaso&key=xxx-pvCNcmXOdmju_2nyZ-3Ilwk2SUDGJo
Source : https://maps.googleapis.com/maps/api/geocode/json?address=Bovril&key=xxx-pvCNcmXOdmju_2nyZ-3Ilwk2SUDGJo
Warning: Geocoding "Bovril" failed with error:
  
  
  Source : https://maps.googleapis.com/maps/api/geocode/json?address=Caseros&key=xxx-pvCNcmXOdmju_2nyZ-3Ilwk2SUDGJo
Source : https://maps.googleapis.com/maps/api/geocode/json?address=Ceibas&key=xxx-pvCNcmXOdmju_2nyZ-3Ilwk2SUDGJo
Source : https://maps.googleapis.com/maps/api/geocode/json?address=Cerrito&key=xxx-pvCNcmXOdmju_2nyZ-3Ilwk2SUDGJo
Source : https://maps.googleapis.com/maps/api/geocode/json?address=Crespo&key=xxx-pvCNcmXOdmju_2nyZ-3Ilwk2SUDGJo
Source : https://maps.googleapis.com/maps/api/geocode/json?address=Estancia+Grande&key=xxx-pvCNcmXOdmju_2nyZ-3Ilwk2SUDGJo
Warning: Geocoding "Estancia Grande" failed with error:
  
  
  Source : https://maps.googleapis.com/maps/api/geocode/json?address=Galarza&key=xxx-pvCNcmXOdmju_2nyZ-3Ilwk2SUDGJo
Source : https://maps.googleapis.com/maps/api/geocode/json?address=General+Campos&key=xxx-pvCNcmXOdmju_2nyZ-3Ilwk2SUDGJo
Source : https://maps.googleapis.com/maps/api/geocode/json?address=Hasenkamp&key=xxx-pvCNcmXOdmju_2nyZ-3Ilwk2SUDGJo
Source : https://maps.googleapis.com/maps/api/geocode/json?address=Hernandarias&key=xxx-pvCNcmXOdmju_2nyZ-3Ilwk2SUDGJo
Source : https://maps.googleapis.com/maps/api/geocode/json?address=Hernandez&key=xxx-pvCNcmXOdmju_2nyZ-3Ilwk2SUDGJo
Source : https://maps.googleapis.com/maps/api/geocode/json?address=Ibicuy&key=xxx-pvCNcmXOdmju_2nyZ-3Ilwk2SUDGJo
Source : https://maps.googleapis.com/maps/api/geocode/json?address=Larroque&key=xxx-pvCNcmXOdmju_2nyZ-3Ilwk2SUDGJo
Source : https://maps.googleapis.com/maps/api/geocode/json?address=Lucas+Gonzalez&key=xxx-pvCNcmXOdmju_2nyZ-3Ilwk2SUDGJo
Source : https://maps.googleapis.com/maps/api/geocode/json?address=Macia&key=xxx-pvCNcmXOdmju_2nyZ-3Ilwk2SUDGJo
Source : https://maps.googleapis.com/maps/api/geocode/json?address=Mansilla&key=xxx-pvCNcmXOdmju_2nyZ-3Ilwk2SUDGJo
Source : https://maps.googleapis.com/maps/api/geocode/json?address=Maria+Grande&key=xxx-pvCNcmXOdmju_2nyZ-3Ilwk2SUDGJo
Source : https://maps.googleapis.com/maps/api/geocode/json?address=Oro+Verde&key=xxx-pvCNcmXOdmju_2nyZ-3Ilwk2SUDGJo
Source : https://maps.googleapis.com/maps/api/geocode/json?address=Pueblo+Brugo&key=xxx-pvCNcmXOdmju_2nyZ-3Ilwk2SUDGJo
Source : https://maps.googleapis.com/maps/api/geocode/json?address=Ramirez&key=xxx-pvCNcmXOdmju_2nyZ-3Ilwk2SUDGJo
Source : https://maps.googleapis.com/maps/api/geocode/json?address=San+Benito&key=xxx-pvCNcmXOdmju_2nyZ-3Ilwk2SUDGJo
Source : https://maps.googleapis.com/maps/api/geocode/json?address=Segui&key=xxx-pvCNcmXOdmju_2nyZ-3Ilwk2SUDGJo
Warning: Geocoding "Segui" failed with error:
  
  
  Source : https://maps.googleapis.com/maps/api/geocode/json?address=Santa+Elena&key=xxx-pvCNcmXOdmju_2nyZ-3Ilwk2SUDGJo
Source : https://maps.googleapis.com/maps/api/geocode/json?address=San+Jaime+de+la+Frontera&key=xxx-pvCNcmXOdmju_2nyZ-3Ilwk2SUDGJo
Source : https://maps.googleapis.com/maps/api/geocode/json?address=San+Jose&key=xxx-pvCNcmXOdmju_2nyZ-3Ilwk2SUDGJo
Source : https://maps.googleapis.com/maps/api/geocode/json?address=Sauce+de+Luna&key=xxx-pvCNcmXOdmju_2nyZ-3Ilwk2SUDGJo
Source : https://maps.googleapis.com/maps/api/geocode/json?address=Ubajay&key=xxx-pvCNcmXOdmju_2nyZ-3Ilwk2SUDGJo
Source : https://maps.googleapis.com/maps/api/geocode/json?address=Urdinarrain&key=xxx-pvCNcmXOdmju_2nyZ-3Ilwk2SUDGJo
Source : https://maps.googleapis.com/maps/api/geocode/json?address=Villa+Clara&key=xxx-pvCNcmXOdmju_2nyZ-3Ilwk2SUDGJo
Source : https://maps.googleapis.com/maps/api/geocode/json?address=Villa+Dominguez&key=xxx-pvCNcmXOdmju_2nyZ-3Ilwk2SUDGJo
Source : https://maps.googleapis.com/maps/api/geocode/json?address=Villa+Elisa&key=xxx-pvCNcmXOdmju_2nyZ-3Ilwk2SUDGJo
Source : https://maps.googleapis.com/maps/api/geocode/json?address=Viale&key=xxx-pvCNcmXOdmju_2nyZ-3Ilwk2SUDGJo
Source : https://maps.googleapis.com/maps/api/geocode/json?address=Villa+Mantero&key=xxx-pvCNcmXOdmju_2nyZ-3Ilwk2SUDGJo
Source : https://maps.googleapis.com/maps/api/geocode/json?address=Villa+Paranacito&key=xxx-pvCNcmXOdmju_2nyZ-3Ilwk2SUDGJo
Source : https://maps.googleapis.com/maps/api/geocode/json?address=Villa+San+Marcial&key=xxx-pvCNcmXOdmju_2nyZ-3Ilwk2SUDGJo
Source : https://maps.googleapis.com/maps/api/geocode/json?address=Villa+Urquiza&key=xxx-pvCNcmXOdmju_2nyZ-3Ilwk2SUDGJo
Source : https://maps.googleapis.com/maps/api/geocode/json?address=Aranguren&key=xxx-pvCNcmXOdmju_2nyZ-3Ilwk2SUDGJo
