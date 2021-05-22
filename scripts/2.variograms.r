
# librerias -------------------------------------------------------------------

library(dplyr)
library(readxl)
library(ggplot2)

library(geoR)
library(gstat)
library(raster)

set.seed(123)

# levanto dataset -------------------------------------------------------------

propcaba_geo = readRDS('data/propcaba_geo')

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

# elimino los que estan abajo de 34.65 (pocos puntos)
propcaba_geo_df = propcaba_geo_df %>% filter(Y>(-34.65))

# parto en train y test -------------------------------------------------------

# 70% of the sample size
smp_size = floor(0.85 * nrow(propcaba_geo_df))

## set the seed to make your partition reproducible
set.seed(123)
train_ind = sample(seq_len(nrow(propcaba_geo_df)), size = smp_size)

propcaba_geo_train = propcaba_geo_df[train_ind, ]
propcaba_geo_test = propcaba_geo_df[-train_ind, ]

# calculo regresiones para sacar tendencia (lo uso mas adelante) --------------

# entreno un modelo para sacar la tendencia 
modt = lm(pricem2 ~ X + Y, data = propcaba_geo_train)
summary(modt)
propcaba_geo_train$pricem2_wt = modt$residuals

# entreno un modelo para sacar la tendencia (lo uso mas adelante)
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


# revisar autocorrelacion e isotropia -----------------------------------------

propcaba_geo_gd = as.geodata(propcaba_geo_train[sample(nrow(propcaba_geo_train), 1000),]
                             , coords.col = 26:27, data.col=25)
plot(propcaba_geo_gd) # no deberia haber tendencia.
 
# # para calcular los variogramas direccionales:
# # agregamos la var: "dir" y le pasamos un valor en radianes (pi/4) por ej.
# # agregamos la var: "tol" y le damos la tolerancia en radianes tambien.
# v1 = variog(propcaba_geo_gd, dir=0)
# v2 = variog(propcaba_geo_gd, dir=pi/4)
# # los graficamos juntos para compararlos y ver si son isotropicos:
# par(mfrow=c(1,2))
# plot(v1)
# plot(v2)
# 
# vario.varias = plot(variog4(propcaba_geo_gd, uvec = seq(0,5,l=11)))
# 
# # comparar varianza por comuna
# 


# variograma simple -----------------------------------------------------------

# practico
v_wt = variogram(pricem2~1, propcaba_geo_sp)
plot(v_wt, main = '1')

# teorico exponencial
v_wt_exp = fit.variogram(v_wt, vgm(5.5e5, "Exp", 0.1, 3e5))
plot(v_wt, v_wt_exp)
attr(v_wt_exp, 'SSErr') # 3.932363e+19

# teorico esferico
v_wt_sph = fit.variogram(v_wt, vgm(5.5e5, "Sph", 0.1, 3e5))
plot(v_wt, v_wt_sph)
attr(v_wt_sph, 'SSErr') # 1.017259e+20


# variograma con tendencia ----------------------------------------------------

# analizo tendencia con x e y

ggplot(propcaba_geo_train, aes(x = X, y = pricem2)) +
  geom_point() + stat_smooth()

ggplot(propcaba_geo_train, aes(x = Y, y = pricem2)) +
  geom_point() + stat_smooth()

# ajusto el variograma practico
v_t = variogram(pricem2~X+Y, propcaba_geo_sp)
plot(v_t, main = '2')

# teorico exponencial
v_t_exp = fit.variogram(v_t, vgm(5e5, "Exp", 0.1, 3e5))
plot(v_t, v_t_exp)
attr(v_t_exp, 'SSErr') # 3.065848e+19

# teorico esferico
v_t_sph = fit.variogram(v_t, vgm(5e5, "Sph", 0.1, 3e5))
plot(v_t, v_t_sph)
attr(v_t_sph, 'SSErr') # 8.511122e+19

# chequeo si me da lo mismo si saco la tendencia en un paso a parte:

# ajusto el variograma practico
v_t2 = variogram(pricem2_wt ~ 1, propcaba_geo_sp)
plot(v_t, main = '2')

# teorico exponencial
v_t_exp2 = fit.variogram(v_t2, vgm(5e5, "Exp", 0.1, 3e5))
plot(v_t2, v_t_exp2)
attr(v_t_exp2, 'SSErr') # 3.065848e+19

# teorico esferico
v_t_sph2 = fit.variogram(v_t2, vgm(5e5, "Sph", 0.1, 3e5))
plot(v_t2, v_t_sph2)
attr(v_t_sph, 'SSErr') # 8.511122e+19


# calculo el variograma del modelo con covariables ----------------------------

# ajusto el variograma practico
v_tcov = variogram(pricem2 ~ X + Y + surface_total + rooms + surface_covered + bathrooms,
                   propcaba_geo_sp)
plot(v_tcov, main = '2')

# teorico exponencial
v_tcov_exp = fit.variogram(v_tcov, vgm(4.5e5, "Exp", 0.1, 2.5e5))
plot(v_tcov, v_tcov_exp)
attr(v_tcov_exp, 'SSErr') # 2.919422e+19

# teorico esferico
v_tcov_sph = fit.variogram(v_tcov, vgm(4.5e5, "Sph", 0.1, 2.5e5))
plot(v_tcov, v_tcov_sph)
attr(v_tcov_sph, 'SSErr') # 7.501486e+19

# chequeo si me da lo mismo si saco la tendencia en un paso a parte:

# ajusto el variograma practico
v_tcov2 = variogram(pricem2_wt_cov ~ 1, propcaba_geo_sp)
plot(v_tcov2, main = '2')

# teorico exponencial
v_tcov_exp2 = fit.variogram(v_tcov2, vgm(5e5, "Exp", 0.1, 3e5))
plot(v_tcov2, v_tcov_exp2)
attr(v_tcov_exp2, 'SSErr') # 2.919422e+19

# teorico esferico
v_tcov_sph = fit.variogram(v_tcov2, vgm(5e5, "Sph", 0.1, 3e5))
plot(v_tcov, v_tcov_sph2)
attr(v_tcov_sph, 'SSErr') # 7.501486e+19


# conclusiones y guardo -------------------------------------------------------

# en el variog simple funciono mejor el esferico, en los otros dos el expo

list_variograms = list( var_wt = list(v_wt, v_wt_sph)
                        ,var_t = list(v_t, v_t_exp)
                        ,var_tcov = list(v_tcov, v_tcov_exp))

# guardo variogramas
saveRDS(list_variograms, 'models/list_variograms.RDS')

# guardo dataframes train y test
saveRDS(propcaba_geo_train, 'data/propcaba_geo_train.RDS')
saveRDS(propcaba_geo_test, 'data/propcaba_geo_test.RDS')
