
# librerias ------------------------------------------------------------

library(tidyverse)
library(readxl)
library(leaflet)

library(geoR)
library(spdep)
library(tmap)
library(gstat)
library(raster)


# levanto dataset ----------------------------------------------------------

# comunas caba
comunas <- st_read('https://bitsandbricks.github.io/data/CABA_comunas.geojson')
class(comunas)

ggplot() +
  geom_sf(data = comunas)

# propiedades
df_caba = readRDS('data/df_caba.rds')
class(df_caba)

# reviso integridad de los datos -------------------------------------------

# mapa
leaflet() %>% 
  addTiles() %>% 
  addCircleMarkers(data = df_caba,
                   lat = ~lat,
                   lng = ~lon,
                   radius = 1)

# hay datos erroneos, tengo que corregir
# paso df_caba a sf
propcaba_geo = df_caba  %>% 
  filter(!is.na(lat), !is.na(lon)) %>% 
  st_as_sf(coords = c("lat", "lon"), crs = 4326)
class(propcaba_geo)

# chequeo que tengan la misma proyeccion
st_crs(propcaba_geo)
st_crs(comunas)

# me quedo con la interseccion
inter = st_intersection(comunas, propcaba_geo, sparse = FALSE)
class(inter)
inter$geometry # devuelve 0 features

# grafico corregido
ggplot() + 
  geom_sf(data = comunas) +
  geom_sf(data = propcaba_geo)

