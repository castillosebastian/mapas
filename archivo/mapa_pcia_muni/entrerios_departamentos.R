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
library(readxl)

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
 

entrerios <- c(lon = -59.228658, lat = -31.872508)
ermap <- ggmap(get_googlemap(center = entrerios, source = "google",  maptype = "terrain", color = "bw", zoom = 7, 
                             size = c(640, 640), scale = 2, darken = c(0.1, "white")))  

# ermap2 <- ggmap(get_stamenmap(bbox = c(left = -61.127930, bottom = -34.179998, right =
#                                          -57.337646, top = -30.040566), source = "stamen",  maptype = "terrain", color = "bw")) 
                           
ermap2 <- ggmap(get_googlemap(center = entrerios, source = "google",  maptype = "roadmap", color = "bw", zoom = 7, 
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

causas_iniciadas_fueropaz_2018 <- causas_iniciadas_fueropaz_2018 %>% 
  mutate(circunscripcion = ifelse(circunscripcion == "CHAJARI", "FEDERACION", circunscripcion)) %>% 
  group_by(circunscripcion) %>% 
  summarise(cantidad_iniciados = sum(cantidad_iniciados, na.rm = T)) %>% 
  ungroup()
              

# `Población estimada al 1 de julio de cada año calendario por sexo, según departamento. Provincia de Entre Ríos. Años 2010-2025`
poblacionxdto <- read_excel("proy_1025_depto_entre_rios.xls")

poblacionxdto <- poblacionxdto %>% 
  select(1,11:17) %>% 
  slice(4,7,9:25)
colnames(poblacionxdto) <- poblacionxdto[1, ]
coln <- paste0("A", colnames(poblacionxdto)[2:8])
coln
colnames(poblacionxdto)[2:8] <- coln
poblacionxdto <- poblacionxdto %>% 
  filter(!Departamento %in% c("Departamento", "Total"))

poblacionxdto <- poblacionxdto %>% 
  rename(circunscripcion = Departamento) %>% 
  mutate(circunscripcion = toupper(circunscripcion)) %>% 
  mutate(circunscripcion = iconv(circunscripcion,from="UTF-8",to="ASCII//TRANSLIT"))

saveRDS(poblacionxdto, "poblacion_xdpto_19to25.rds")

# Causas Iniciadas cada 100.000 h 
causas_iniciadas_fueropaz_2018 <- causas_iniciadas_fueropaz_2018 %>% 
  left_join(poblacionxdto %>% select(circunscripcion, A2019), by = "circunscripcion") %>% 
  mutate(inicx100mh = cantidad_iniciados / (A2019/100000))


#fortify the data for ggplot2
departamento <- fortify(eriospoly, region = "departamento")
departamento_map <- left_join(departamento, causas_iniciadas_fueropaz_2018, by = c("id" = "circunscripcion"))

# to do: dividir por poblacion
#sector.map$rate <- sector.map$count / sector.map$population * 10^5
ermap +
  geom_polygon(data = departamento_map,
             aes_string("long", "lat", group = "id", fill = "inicx100mh"), colour = "grey", 
             size = 0.30) +
  #scale_fill_viridis(option="plasma") +
  scale_fill_gradient(low="blue", high="red") +
  labs(title = "Causas Iniciadas",
       subtitle = "Distribución Territorial",
       caption = "Fuente: STJER - APGE") +
  theme_nothing(legend = TRUE)



ermap2 +
  geom_polygon(data = departamento_map,
               aes_string("long", "lat", group = "id", fill = "inicx100mh"), colour = "grey", 
               size = 0.30) +
  labs(title = "Causas Iniciadas",
       subtitle = "Cartografiado Cuantitativo",
       caption = "Fuente: STJER - APGE") +
  theme_nothing(legend = TRUE) +
  theme(legend.position = "bottom") +
  scale_fill_gradient(low="blue", high="red", 
    # here we use guide_colourbar because it is still a continuous scale
    guide = guide_colorbar(
      direction = "horizontal",
      barheight = unit(2, units = "mm"),
      barwidth = unit(50, units = "mm"),
      draw.ulim = F,
      title.position = 'top',
      # some shifting around
      title.hjust = 0.5,
      label.hjust = 0.5,
      label.position = "bottom"
    ))
  
  


  
##### PROXIMOS PASOS #################################################################
# Crear funcion para plotear
draw_gmap <- function(map, crimeName, bb, pal, fill = "rate", alpha=.9) {
  
  # ojo circunscripcion chajari unir con federacion
  # ojo elegir año de comparación con proyeccion Sys.Date poblacional.
  
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


