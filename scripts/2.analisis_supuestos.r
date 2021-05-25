
# librerias -------------------------------------------------------------------

library(dplyr)
library(readxl)
library(ggplot2)

library(geoR)
library(gstat)
library(raster)
library(sf)
library(spdep)

set.seed(123)


# levanto dataset -------------------------------------------------------------

propcaba_geo = readRDS('data/propcaba_geo.RDS')

# lo paso a data.frame
propcaba_geo_df = propcaba_geo %>%
  st_drop_geometry() %>% as.data.frame() %>% 
  cbind(propcaba_geo %>% st_coordinates()) %>% 
  as.data.frame()

# grafico
ggplot() + 
  geom_sf(data = propcaba_geo_df %>% 
            st_as_sf(coords = c("X", "Y"), crs = 4326),
          color = 'grey', alpha = 0.1) 

# # elimino los que estan abajo de 34.65 (pocos puntos)
# propcaba_geo_df = propcaba_geo_df %>% filter(Y>(-34.65))


# parto en train y test -------------------------------------------------------

# todas las decisiones las voy a tomar sobre la muestra de train
# la muestra de test se va a usar al final para comparar modelos

# 70% of the sample size
smp_size = floor(0.85 * nrow(propcaba_geo_df))

## set the seed to make your partition reproducible
set.seed(123)
train_ind = sample(seq_len(nrow(propcaba_geo_df)), size = smp_size)

propcaba_geo_train = propcaba_geo_df[train_ind, ] # 51447
propcaba_geo_test = propcaba_geo_df[-train_ind, ] # 9080


# calculo regresiones para sacar tendencia (lo uso mas adelante) --------------

# entreno un modelo para sacar la tendencia 
modt = lm(pricem2 ~ X + Y, data = propcaba_geo_train)
summary(modt)
propcaba_geo_train$pricem2_wt = modt$residuals

# entreno un modelo para sacar la tendencia con covariables
modtcov = lm(pricem2 ~ X + Y + surface_total + rooms + surface_covered + bathrooms,
          data = propcaba_geo_train)
summary(modtcov)
propcaba_geo_train$pricem2_wt_cov = modtcov$residuals

# veo distribucion
ggplot(propcaba_geo_train) +
  geom_histogram(aes(x = pricem2))

ggplot(propcaba_geo_train) +
  geom_histogram(aes(x = pricem2_wt))

ggplot(propcaba_geo_train) +
  geom_histogram(aes(x = pricem2_wt_cov))

# lo paso a sp
propcaba_geo_sp = propcaba_geo_train
coordinates(propcaba_geo_sp) = ~X+Y
class(propcaba_geo_sp)


# reviso autocorrelacion ------------------------------------------------------

# calculo del indice de moran:
pixel = coordinates(propcaba_geo_train[25:26])
# en grilla, se define un vecindario; los vecinos de cada punto, 
# son todas las posiciones que están a una distancia 
# mayor a 0 y menor que 50, del punto
grilla = dnearneigh(pixel,0,10)
#plot(grilla ,pixel)
pesos = nb2listw(grilla, style = "W")

# test Moran
moran.test(propcaba_geo_train$pricem2, nb2listw(grilla, style = "W"),randomisation=FALSE)
# p-value < 2.2e-16 -> se rechaza la HN de que no hay autocorrelacion


# variable origial: isotropia -------------------------------------------------

# analizo tendencia con x e y
ggplot(propcaba_geo_train, aes(x = X, y = pricem2)) +
  geom_point() + stat_smooth()

ggplot(propcaba_geo_train, aes(x = Y, y = pricem2)) +
  geom_point() + stat_smooth()

# analizo isotropia
propcaba_geo_gd = as.geodata(propcaba_geo_train[sample(nrow(propcaba_geo_train), 1000),]
                             , coords.col = 26:27, data.col=25)
propcaba_geo_gd = propcaba_geo_gd %>% jitterDupCoords(max=0.1)
# plot(propcaba_geo_gd) # no deberia haber tendencia.
 
# para calcular los variogramas direccionales:

# agregamos la var: "dir" y le pasamos un valor en radianes (pi/4) por ej.
# agregamos la var: "tol" y le damos la tolerancia en radianes tambien.
v1 = variog(propcaba_geo_gd, main="nombre1", dir=0, tol=pi/8, max.dist = 1.45e-01)
v2 = variog(propcaba_geo_gd, main="nombre2", dir=pi/2, tol=pi/8, max.dist = 1.45e-01)
v3 = variog(propcaba_geo_gd, main="nombre3", dir=pi/4, tol=pi/8, max.dist = 1.45e-01)
v4 = variog(propcaba_geo_gd, main="nombre4", dir=pi*3/4, tol=pi/8, max.dist = 1.45e-01)

# los graficamos juntos para compararlos y ver si son isotropicos:
par(mfrow=c(1,1))
plot(v1)
plot(v2)
plot(v3)
plot(v4)

# comparar varianza por comuna
propcaba_geo_train %>% group_by(comunas) %>% 
  summarise(varianza = sd(pricem2)^2,
            comunas_n = n()) %>% filter(barrio> 100) %>% View()


# variable sin tendencia: isotropia -------------------------------------------

# analizo tendencia con x e y
ggplot(propcaba_geo_train, aes(x = X, y = pricem2_wt)) +
  geom_point() + stat_smooth()

ggplot(propcaba_geo_train, aes(x = Y, y = pricem2_wt)) +
  geom_point() + stat_smooth()

# analizo isotropia
propcaba_geo_gd = as.geodata(propcaba_geo_train[sample(nrow(propcaba_geo_train), 1000),]
                             , coords.col = 26:27, data.col=28)
propcaba_geo_gd = propcaba_geo_gd %>% jitterDupCoords(max=0.1)
# plot(propcaba_geo_gd) # no deberia haber tendencia.

# para calcular los variogramas direccionales:

# agregamos la var: "dir" y le pasamos un valor en radianes (pi/4) por ej.
# agregamos la var: "tol" y le damos la tolerancia en radianes tambien.
v1 = variog(propcaba_geo_gd, main="nombre1", dir=0, tol=pi/8, max.dist = 1.45e-01)
v2 = variog(propcaba_geo_gd, main="nombre2", dir=pi/2, tol=pi/8, max.dist = 1.45e-01)
v3 = variog(propcaba_geo_gd, main="nombre3", dir=pi/4, tol=pi/8, max.dist = 1.45e-01)
v4 = variog(propcaba_geo_gd, main="nombre4", dir=pi*3/4, tol=pi/8, max.dist = 1.45e-01)

# los graficamos juntos para compararlos y ver si son isotropicos:
par(mfrow=c(1,1))
plot(v1)
plot(v2)
plot(v3)
plot(v4)

# comparar varianza por comuna
propcaba_geo_train %>% group_by(comunas) %>% 
  summarise(varianza = sd(pricem2_wt)^2,
            comunas_n = n()) %>% filter(barrio> 100) %>% View()


# variable sin tendencia y con covs: isotropia --------------------------------

# analizo tendencia con x e y
ggplot(propcaba_geo_train, aes(x = X, y = pricem2_wt_cov)) +
  geom_point() + stat_smooth()

ggplot(propcaba_geo_train, aes(x = Y, y = pricem2_wt_cov)) +
  geom_point() + stat_smooth()

# analizo isotropia
propcaba_geo_gd = as.geodata(propcaba_geo_train[sample(nrow(propcaba_geo_train), 1000),]
                             , coords.col = 26:27, data.col=29)
propcaba_geo_gd = propcaba_geo_gd %>% jitterDupCoords(max=0.1)
# plot(propcaba_geo_gd) # no deberia haber tendencia.

# para calcular los variogramas direccionales:

# agregamos la var: "dir" y le pasamos un valor en radianes (pi/4) por ej.
# agregamos la var: "tol" y le damos la tolerancia en radianes tambien.
v1 = variog(propcaba_geo_gd, main="nombre1", dir=0, tol=pi/8, max.dist = 1.45e-01)
v2 = variog(propcaba_geo_gd, main="nombre2", dir=pi/2, tol=pi/8, max.dist = 1.45e-01)
v3 = variog(propcaba_geo_gd, main="nombre3", dir=pi/4, tol=pi/8, max.dist = 1.45e-01)
v4 = variog(propcaba_geo_gd, main="nombre4", dir=pi*3/4, tol=pi/8, max.dist = 1.45e-01)

# los graficamos juntos para compararlos y ver si son isotropicos:
par(mfrow=c(1,1))
plot(v1)
plot(v2)
plot(v3)
plot(v4)

# comparar varianza por comuna
propcaba_geo_train %>% group_by(comunas) %>% 
  summarise(varianza = sd(pricem2_wt_cov)^2,
            comunas_n = n()) %>% filter(barrio> 100) %>% View()


# guardo datasets -------------------------------------------------------------

# guardo dataframes train y test
saveRDS(propcaba_geo_train, 'data/propcaba_geo_train.RDS')
saveRDS(propcaba_geo_test, 'data/propcaba_geo_test.RDS')
