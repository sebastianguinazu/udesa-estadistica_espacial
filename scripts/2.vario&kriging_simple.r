
# librerias ------------------------------------------------------------

library(dplyr)
library(readxl)
library(leaflet)
library(ggplot2)

library(geoR)
library(spdep)
library(tmap)
library(gstat)
library(raster)

set.seed(123)

# levanto dataset ----------------------------------------------------------

propcaba_geo = readRDS('data/propcaba_geo')

# muestra
propcaba_geo_spl = propcaba_geo[sample(nrow(propcaba_geo), 1e4), ]

# lo paso a st
propcaba_geo_sp = propcaba_geo_spl %>%
  st_drop_geometry() %>% as.data.frame() %>% 
  cbind(propcaba_geo_spl %>% st_coordinates())


# variograma simple --------------------------------------------------------

coordinates(propcaba_geo_sp) = ~X+Y
class(propcaba_geo_sp)

v_wt = variogram(price~1, propcaba_geo_sp)
plot(v_wt, main = '1')

v_wt_exp = fit.variogram(v_wt, vgm(1.2e10, "Exp", 0.1, 1e9))
plot(v_wt, v_wt_exp)

v_wt_sph = fit.variogram(v_wt, vgm(1.2e10, "Sph", 0.1, 1e9))
plot(v_wt, v_wt_sph)


# analizo tendencia con x e y ----------------------------------------------

propcaba_geo_df = propcaba_geo_spl %>%
  st_drop_geometry() %>% as.data.frame() %>% 
  cbind(propcaba_geo_spl %>% st_coordinates())
        
ggplot(propcaba_geo_df, aes(x = X, y = price)) +
  geom_point() + stat_smooth()

ggplot(propcaba_geo_df, aes(x = Y, y = price)) +
  geom_point() + stat_smooth()


v_t = variogram(price~X+Y, propcaba_geo_sp)
plot(v_t, main = '2')
