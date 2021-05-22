
# librerias ------------------------------------------------------------

library(tidyverse)
library(readxl)
library(leaflet)
library(sf)

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
  st_as_sf(coords = c("lon", "lat"), crs = 4326)
class(propcaba_geo)

# chequeo que tengan la misma proyeccion
st_crs(propcaba_geo)
st_crs(comunas)

# me quedo con la interseccion
propcaba_geo = st_intersection(comunas, propcaba_geo, sparse = FALSE)
class(propcaba_geo)
propcaba_geo$geometry

# grafico corregido
ggplot() + 
  geom_sf(data = comunas) +
  geom_sf(data = propcaba_geo)

# salvo dataset
saveRDS(propcaba_geo, 'data/propcaba_geo')

