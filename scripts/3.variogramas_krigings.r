
# cosas a probar:

# kriging con los modelos del script2
# performance predictiva de esos modelos en el dataset validacion
# probar opcion de estimar variogramas segun performance en validacion
# performance predictiva de variogramas estimados en paso anterior

# librerias -------------------------------------------------------------------

library(dplyr)
library(readxl)
library(ggplot2)

library(geoR)
library(gstat)
library(raster)


# levanto datasets ------------------------------------------------------------

propcaba_geo_train = readRDS('data/propcaba_geo_train.RDS')
propcaba_geo_test = readRDS('data/propcaba_geo_test.RDS')

nrow(propcaba_geo_train) # 51447
nrow(propcaba_geo_test) # 9080

propcaba_geo_test = propcaba_geo_test[sample(nrow(propcaba_geo_test), 100),]

# paso a formato sp
propcaba_train_sp = propcaba_geo_train
coordinates(propcaba_train_sp) = ~X+Y
class(propcaba_train_sp)

propcaba_test_sp = propcaba_geo_test
coordinates(propcaba_test_sp) = ~X+Y
class(propcaba_test_sp)

# mapa de mis datos train y test
ggplot() + 
  geom_sf(data = propcaba_geo_train  %>% 
            st_as_sf(coords = c("X", "Y"), crs = 4326),
          color = 'red', alpha = 0.01) +
  geom_sf(data = propcaba_geo_test  %>% 
            st_as_sf(coords = c("X", "Y"), crs = 4326),
          color = 'blue', alpha = 0.05)


# variograma simple -----------------------------------------------------------

# practico
v_wt = variogram(pricem2~1, propcaba_train_sp)
plot(v_wt, main = '1')

# teorico exponencial
v_wt_exp = fit.variogram(v_wt, vgm(5.5e5, "Exp", 0.1, 3e5))
plot(v_wt, v_wt_exp)
attr(v_wt_exp, 'SSErr') # 4.002855e+19

# teorico esferico
v_wt_sph = fit.variogram(v_wt, vgm(5.5e5, "Sph", 0.1, 3e5))
plot(v_wt, v_wt_sph)
attr(v_wt_sph, 'SSErr') # 8.473316e+19


# variograma con tendencia ----------------------------------------------------

# ajusto el variograma practico
v_t = variogram(pricem2~X+Y, propcaba_train_sp)
plot(v_t, main = '2')

# teorico exponencial
v_t_exp = fit.variogram(v_t, vgm(5e5, "Exp", 0.1, 3e5))
plot(v_t, v_t_exp)
attr(v_t_exp, 'SSErr') # 1.882637e+19

# teorico esferico
v_t_sph = fit.variogram(v_t, vgm(5e5, "Sph", 0.1, 3e5))
plot(v_t, v_t_sph)
attr(v_t_sph, 'SSErr') # 2.291317e+19

# chequeo si me da lo mismo si saco la tendencia en un paso a parte:

# ajusto el variograma practico
v_t2 = variogram(pricem2_wt ~ 1, propcaba_geo_sp)
plot(v_t, main = '2')

# teorico exponencial
v_t_exp2 = fit.variogram(v_t2, vgm(5e5, "Exp", 0.1, 3e5))
plot(v_t2, v_t_exp2)
attr(v_t_exp2, 'SSErr') # 1.882638e+19

# teorico esferico
v_t_sph2 = fit.variogram(v_t2, vgm(5e5, "Sph", 0.1, 3e5))
plot(v_t2, v_t_sph2)
attr(v_t_sph, 'SSErr') # 2.291317e+19


# calculo el variograma del modelo con covariables ----------------------------

# ajusto el variograma practico
v_tcov = variogram(pricem2 ~ X + Y + surface_total + rooms + surface_covered + bathrooms,
                   propcaba_geo_sp)
plot(v_tcov, main = '2')

# teorico exponencial
v_tcov_exp = fit.variogram(v_tcov, vgm(4.5e5, "Exp", 0.1, 3e5))
plot(v_tcov, v_tcov_exp)
attr(v_tcov_exp, 'SSErr') # 2.028487e+19

# teorico esferico
v_tcov_sph = fit.variogram(v_tcov, vgm(4.5e5, "Sph", 0.1, 3e5))
plot(v_tcov, v_tcov_sph)
attr(v_tcov_sph, 'SSErr') # 1.901461e+19

# chequeo si me da lo mismo si saco la tendencia en un paso a parte:

# ajusto el variograma practico
v_tcov2 = variogram(pricem2_wt_cov ~ 1, propcaba_geo_sp)
plot(v_tcov2, main = '2')

# teorico exponencial
v_tcov_exp2 = fit.variogram(v_tcov2, vgm(4.5e5, "Exp", 0.1, 3e5))
plot(v_tcov2, v_tcov_exp2)
attr(v_tcov_exp2, 'SSErr') # 2.028487e+19

# teorico esferico
v_tcov_sph2 = fit.variogram(v_tcov2, vgm(4.5e5, "Sph", 0.1, 3e5))
plot(v_tcov, v_tcov_sph2)
attr(v_tcov_sph, 'SSErr') # 1.901461e+19


# prediccion con el kriging ---------------------------------------------------

# sin tendencia
krg_wt = krige(pricem2 ~ 1,
               propcaba_train_sp,
               propcaba_test_sp,
               model = v_wt_exp,
               nmax = 1)

length(krg_wt$var1.pred) # 11122
sum(!is.na(krg_wt$var1.pred)) # 11122

propcaba_geo_test$pricem2_pred = krg_wt$var1.pred
mean((propcaba_geo_test$pricem2-propcaba_geo_test$pricem2_pred)^2) # 287.446

# https://gis.stackexchange.com/questions/329290/r-gstat-krige-na-predictions-increasing-with-larger-nmax
# propcaba_train_sp = SpatialPointsDataFrame(jitter(coordinates(propcaba_train_sp),factor=0.5), propcaba_train_sp@data)
# propcaba_test_sp = SpatialPointsDataFrame(jitter(coordinates(propcaba_test_sp),factor=0.5), propcaba_test_sp@data)

# con tendencia
krg_t = krige(pricem2 ~ X + Y,
              propcaba_train_sp,
              propcaba_test_sp,
              model = list_variograms$var_t[[2]],
              nmax = 1)

length(krg_t$var1.pred) # 11122
sum(!is.na(krg_t$var1.pred)) # 4778

propcaba_geo_test$pricem2_pred_t = krg_t$var1.pred

propcaba_geo_test_t = propcaba_geo_test %>% filter(!is.na(pricem2_pred_t)) %>% 
  mutate(se=(pricem2-pricem2_pred_t)^2)
mean(propcaba_geo_test_t$se) # 1.353.996

# con tendencia y covariables
krg_tcov = krige(pricem2 ~ X + Y + surface_total + rooms +
                   surface_covered + bathrooms,
                 propcaba_train_sp,
                 propcaba_test_sp,
                 model = list_variograms$var_tcov[[2]],
                 nmax = 1)

length(krg_tcov$var1.pred) # 11122
sum(!is.na(krg_tcov$var1.pred)) # 4845

propcaba_geo_test$pricem2_pred_tcov = krg_tcov$var1.pred

propcaba_geo_test_tcov = propcaba_geo_test %>% filter(!is.na(pricem2_pred_tcov)) %>% 
  mutate(se=(pricem2-pricem2_pred_tcov)^2)
mean(propcaba_geo_test_tcov$se) # 8.574109e+96


# prediccion con tendencia utilizando los errores -----------------------------

# kriging 
krg_tcov2 = krige(pricem2_wt_cov ~ 1,
                  propcaba_train_sp,
                  propcaba_test_sp,
                  model = v_tcov_exp2,
                  nmax = 1)
length(krg_tcov2$var1.pred) # 11122
sum(!is.na(krg_tcov2$var1.pred)) # 4845

# recupero el precio con esta var
propcaba_geo_test$pricem2_pred22 = krg_tcov2$var1.pred
propcaba_geo_test$pricem2_pred2 = propcaba_geo_test$pricem2_pred + krg_tcov2$var1.pred

# error cuadratico final 
mean((propcaba_geo_test$pricem2-propcaba_geo_test$pricem2_pred2)^2)

