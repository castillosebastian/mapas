library(ggplot2)
library(ggmap)
library(RColorBrewer)
library(purrr)
library(stringr)
library(tibble)
library(leaflet)
#install.packages("geojsonio")
library(geojsonio)
library(tidytext)
library(RCurl)
library(dplyr)
library(mapproj)
library(rgdal)

# Solucion problema paquetes 1: jqr #https://github.com/rstudio/shinyapps-package-dependencies/issues/108
# sudo add-apt-repository -y ppa:opencpu/jqr
# sudo apt-get update
# sudo apt-get install libjq-dev
# Solucion problema paquete 2: sudo apt-get install -y libv8-dev #https://github.com/jeroen/V8#installation
# Solucion prblema paquete 3: sudo apt-get install -y libprotobuf-dev protobuf-compiler #https://stackoverflow.com/questions/47121880/r-package-tmap-protolite-installation-failed


# Departamentos
# url <- "http://ign.gob.ar/archivos/sig250/geojson/departamentos.zip"
# erdptos <- "erios.geojson"
# download.file(url, destfile=erdptos,method="libcurl")

# Trabajando con departamentos Instituto Geográfico Nacional (IGN) 
# unzip("departamentos.zip", exdir = "~/R/mapas/archivo/mapa_pcia_muni")
# unzip("provincias.zip", exdir = "~/R/mapas/archivo/mapa_pcia_muni")
# perios_json <- geojson_read("provincias.geojson", what = "sp")
# erios_json <- geojson_read("departamentos.geojson", what = "sp")
# erios <- fortify(erios_json)

##################------------------########################################
register_google(key = "AIzaSyDv-pvCNcmXOdmju_2nyZ-3Ilwk2SUDGJo") 

entrerios <- c(lon = -59.228658, lat = -31.872508)
ermap <- ggmap(get_googlemap(center = entrerios, source = "google",  maptype = "terrain", color = "bw", zoom = 7, 
                             size = c(640, 640), scale = 2, darken = c(0.1, "white")))  

# departamentos de entre rios # https://github.com/mgaitan/departamentos_argentina.git
eriospoly <- geojson_read("departamentos-entre_rios.json", what = "sp")
class(eriospoly)

# Resultado 
plot(eriospoly)


# Exploracion
# https://blog.diegovalle.net/2016/10/crime-maps-of-mexico-city.html
causas_iniciadas_fueropaz_2018 <- read_delim("causas_iniciadas_fueropaz_2018.csv", 
                                             ";", escape_double = FALSE, trim_ws = TRUE)

causas_iniciadas_fueropaz_2018 <- causas_iniciadas_fueropaz_2018 %>% 
  group_by(circunscripcion) %>% 
  summarise(cantidad_iniciados = sum(Total, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(circunscripcion = toupper(circunscripcion)) %>% 
  mutate(circunscripcion = iconv(circunscripcion,from="UTF-8",to="ASCII//TRANSLIT"))



#fortify the data for ggplot2
departamento <- fortify(eriospoly, region = "departamento")
departamento_map <- left_join(departamento, causas_iniciadas_fueropaz_2018, by = c("id" = "circunscripcion"))

# to do: dividir por poblacion
#sector.map$rate <- sector.map$count / sector.map$population * 10^5

ermap +
  geom_polygon(data = departamento_map,
             aes_string("long", "lat", group = "id", fill = "cantidad_iniciados"), colour = "grey", 
             size = 0.30) +
  #scale_fill_viridis(option="plasma") +
  scale_fill_gradient(low="blue", high="red") +
  labs(title = "Causas Iniciadas",
       subtitle = "Distribución Territorial",
       caption = "Fuente: STJER - APGE")
  theme_nothing(legend = TRUE)


##### PROXIMOS PASOS #################################################################
# Crear funcion para plotear
draw_gmap <- function(map, crimeName, bb, pal, fill = "rate", alpha=.9) {
  ggmap(get_map(location = bb)) +
    geom_polygon(data= subset(map, crime == crimeName),
                 aes_string("long", "lat", group = "group", fill = fill),
                 color = "#666666", size = .1,
                 alpha = alpha) +
    coord_map() +
    ggtitle(crimeName) +
    #scale_fill_viridis(option="plasma") +
    scale_fill_continuous(low = brewer.pal(9, pal)[1],
                          high = brewer.pal(9, pal)[9],
                          space = "Lab", na.value = "grey50",
                          guide = "colourbar") +
    theme_nothing(legend = TRUE)
}

bb.sector <- bbox(sectores)
draw_gmap(sector.map, "HOMICIDIO DOLOSO", bb.sector, "Reds", "rate")


