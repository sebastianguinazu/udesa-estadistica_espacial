
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
library(sf)


# levanto datasets ------------------------------------------------------------

propcaba_geo_train = readRDS('data/propcaba_geo_train.RDS')

# tengo que partir la muestra de train
nrow(propcaba_geo_train) # 51447

# 70% of the sample size
smp_size = floor(0.80 * nrow(propcaba_geo_train))

## set the seed to make your partition reproducible
set.seed(123)
dev_ind = sample(seq_len(nrow(propcaba_geo_train)), size = smp_size)

propcaba_geo_dev = propcaba_geo_train[dev_ind, ] # 41157
propcaba_geo_mod = propcaba_geo_train[-dev_ind, ] # 10290

# paso a formato sp
propcaba_dev_sp = propcaba_geo_dev
coordinates(propcaba_dev_sp) = ~X+Y

propcaba_mod_sp = propcaba_geo_mod
coordinates(propcaba_mod_sp) = ~X+Y

set.seed(123)
# https://gis.stackexchange.com/questions/329290/r-gstat-krige-na-predictions-increasing-with-larger-nmax
propcaba_dev_sp = SpatialPointsDataFrame(jitter(coordinates(propcaba_dev_sp),factor=0.2), propcaba_dev_sp@data)
propcaba_mod_sp = SpatialPointsDataFrame(jitter(coordinates(propcaba_mod_sp),factor=0.2), propcaba_mod_sp@data)

# mapa de mis datos train y test
# ggplot() + 
#   geom_sf(data = propcaba_dev_sp  %>% 
#             st_as_sf(coords = c("X", "Y"), crs = 4326),
#           color = 'red', alpha = 0.01) +
#   geom_sf(data = propcaba_mod_sp  %>% 
#             st_as_sf(coords = c("X", "Y"), crs = 4326),
#           color = 'blue', alpha = 0.05)


# variograma simple -----------------------------------------------------------

# empirico
v_wt = variogram(pricem2~1, propcaba_dev_sp)
plot(v_wt, main = 'Variograma con variable original')

# teorico exponencial
v_wt_exp = fit.variogram(v_wt, vgm(5.5e5, "Exp", 0.1, 3e5))
plot(v_wt, v_wt_exp, main = 'Variograma con variable original y modelo Exp')
attr(v_wt_exp, 'SSErr') # 2.548332e+19

# teorico esferico
v_wt_sph = fit.variogram(v_wt, vgm(5.5e5, "Sph", 0.1, 3e5))
plot(v_wt, v_wt_sph, main = 'Variograma con variable original y modelo Sph')
attr(v_wt_sph, 'SSErr') # 5.402098e+19


# variograma con tendencia ----------------------------------------------------

# ajusto el variograma empirico
v_t = variogram(pricem2_wt ~ 1, propcaba_dev_sp)
plot(v_t, main = 'Variograma con variable sin trend')

# teorico exponencial
v_t_exp = fit.variogram(v_t, vgm(5e5, "Exp", 0.1, 3e5))
plot(v_t, v_t_exp, main = 'Variograma con variable sin trend y modelo Exp')
attr(v_t_exp, 'SSErr') # 1.270442e+19

# teorico esferico
v_t_sph = fit.variogram(v_t, vgm(5e5, "Sph", 0.1, 3e5))
plot(v_t, v_t_sph, main = 'Variograma con variable sin trend y modelo Sph')
attr(v_t_sph, 'SSErr') # 1.263963e+19

# # ajusto el variograma empirico
# v_t2 = variogram(pricem2~X+Y, propcaba_dev_sp)
# plot(v_t2, main = '2')
# 
# # teorico exponencial
# v_t_exp2 = fit.variogram(v_t2, vgm(5e5, "Exp", 0.1, 3e5))
# plot(v_t, v_t_exp2)
# attr(v_t_exp2, 'SSErr') # 1.882637e+19
# 
# # teorico esferico
# v_t_sph2 = fit.variogram(v_t2, vgm(5e5, "Sph", 0.1, 3e5))
# plot(v_t2, v_t_sph2)
# attr(v_t_sph2, 'SSErr') # 1.263963e+19


# calculo el variograma del modelo con covariables ----------------------------

# ajusto el variograma empirico
v_tcov = variogram(pricem2_wt_cov ~ 1, propcaba_dev_sp)
plot(v_tcov, main = 'Variograma con var sin trend y covs')

# teorico exponencial
v_tcov_exp = fit.variogram(v_tcov, vgm(4.5e5, "Exp", 0.1, 3e5))
plot(v_tcov, v_tcov_exp, main = 'Variograma con variable sin trend y cov Exp')
attr(v_tcov_exp, 'SSErr') # 1.513218e+19

# teorico esferico
v_tcov_sph = fit.variogram(v_tcov, vgm(4.5e5, "Sph", 0.1, 3e5))
plot(v_tcov, v_tcov_sph, main = 'Variograma con variable sin trend y cov Exp')
attr(v_tcov_sph, 'SSErr') # 1.04601e+19

# # ajusto el variograma empirico
# v_tcov2 = variogram(pricem2 ~ X + Y + surface_total + rooms + surface_covered + bathrooms,
#                    propcaba_dev_sp)
# plot(v_tcov2, main = '2')
# 
# # teorico exponencial
# v_tcov_exp2 = fit.variogram(v_tcov2, vgm(4.5e5, "Exp", 0.1, 3e5))
# plot(v_tcov2, v_tcov_exp2)
# attr(v_tcov_exp2, 'SSErr') # 2.028487e+19
# 
# # teorico esferico
# v_tcov_sph2 = fit.variogram(v_tcov2, vgm(4.5e5, "Sph", 0.1, 3e5))
# plot(v_tcov2, v_tcov_sph2)
# attr(v_tcov_sph2, 'SSErr') # 1.901461e+19


# prediccion con el kriging ---------------------------------------------------

# sin tendencia
krg_wt = krige(pricem2 ~ 1,
               propcaba_dev_sp,
               propcaba_mod_sp,
               model = v_wt_exp,
               nmax = 20,
               nmin = 10)

length(krg_wt$var1.pred) # 10290
sum(!is.na(krg_wt$var1.pred)) # 10290

propcaba_mod_sp$pricem2_pred = krg_wt$var1.pred
mean((propcaba_mod_sp$pricem2-propcaba_mod_sp$pricem2_pred)^2) # 264.543

# con tendencia
krg_t = krige(pricem2_wt~1,
              propcaba_dev_sp,
              propcaba_mod_sp,
              model = v_t_exp,
              nmax = 20,
              nmin = 10)

length(krg_t$var1.pred) # 10290
sum(!is.na(krg_t$var1.pred)) # 10290

propcaba_mod_sp$pricem2_pred_t = krg_t$var1.pred
mean((propcaba_mod_sp$pricem2_wt-propcaba_mod_sp$pricem2_pred_t)^2) # 264.799

# con tendencia y covariables
krg_tcov = krige(pricem2_wt_cov~1,
              propcaba_dev_sp,
              propcaba_mod_sp,
              model = v_tcov_exp,
              nmax = 20,
              nmin = 10)

length(krg_tcov$var1.pred) # 10290
sum(!is.na(krg_tcov$var1.pred)) # 10290

propcaba_mod_sp$pricem2_pred_tcov = krg_tcov$var1.pred
mean((propcaba_mod_sp$pricem2_wt_cov-propcaba_mod_sp$pricem2_pred_tcov)^2) # 248.184


# guardo objetos --------------------------------------------------------------

# dataset con variables predichas
propcaba_dev_df = as.data.frame(propcaba_dev_sp)
propcaba_mod_df = as.data.frame(propcaba_mod_sp)

saveRDS(propcaba_dev_df,'data/propcaba_dev_df.RDS')
saveRDS(propcaba_mod_df,'data/propcaba_mod_df.RDS')

# variogramas
var_list = list('v_wt_exp' = v_wt_exp,
                'v_t_exp' = v_t_exp, 
                'v_tcov_exp' = v_tcov_exp)

saveRDS(var_list, 'models/var_list.RDS')
