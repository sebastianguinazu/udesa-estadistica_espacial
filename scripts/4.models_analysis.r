
# desafio: entrenar distintos modelos y comparar performance
# modelo 0:  pred lineal sin analisis geo
# modelo 1:  pred geoestadistica sin tratamiento de tendencia ni covs
# modelo 2a: pred geoestadistica con tratamiento de tendencia sin covs
# modelo 2b: pred con modelo lineal con input prediccion de krigring (trend sin covs)
# modelo 3:  pred con modelo lineal con input prediccion de krigring (trend con covs)


# librerias -------------------------------------------------------------------

library(dplyr)
library(ggplot2)

library(gstat)


# levanto datasets ------------------------------------------------------------

propcaba_dev_df = readRDS('data/propcaba_dev_df.RDS')
propcaba_mod_df = readRDS('data/propcaba_mod_df.RDS')
propcaba_test_df = readRDS('data/propcaba_geo_test.RDS')

var_list = readRDS('models/var_list.RDS')

# paso a formato sp
propcaba_dev_sp = propcaba_dev_df
coordinates(propcaba_dev_sp) = ~X+Y

propcaba_mod_sp = propcaba_mod_df
coordinates(propcaba_mod_sp) = ~X+Y

propcaba_test_sp = propcaba_test_df
coordinates(propcaba_test_sp) = ~X+Y

set.seed(123)
# https://gis.stackexchange.com/questions/329290/r-gstat-krige-na-predictions-increasing-with-larger-nmax
propcaba_test_sp = SpatialPointsDataFrame(jitter(coordinates(propcaba_test_sp),factor=0.2), propcaba_test_sp@data)


# reentreno modelo lineal usando residuos -------------------------------------

# entrenamos modelos con el dataset mod, que guarda las predicciones del kriging
# del script3 que se construyo con el dataset dev. luego estos modelos se usan
# para comparar la performance predictiva final con el dataset test

# a. entreno un modelo con la prediccion sin tendencia sin covs
modt = lm(pricem2 ~ X + Y + pricem2_pred_t, data = propcaba_mod_df)
summary(modt)

# b. entreno un modelo con la prediccion sin tendencia con covs
modt2 = lm(pricem2 ~ X + Y + surface_total + rooms + surface_covered + 
            bathrooms + pricem2_pred_t, data = propcaba_mod_df)
summary(modt)

# c. entreno un modelo con la prediccion sin tendencia ni covs y covs
modtcov = lm(pricem2 ~ X + Y + surface_total + rooms + surface_covered + 
               bathrooms + pricem2_pred_tcov,
             data = propcaba_mod_df)
summary(modtcov)


# modelo 0. benchmark: modelo lineal simple -----------------------------------

modbase = lm(pricem2 ~ X + Y + surface_total + rooms + surface_covered + bathrooms,
             data = propcaba_test_df)
propcaba_test_df$pricem2_pred = predict(modbase, propcaba_test_df)
propcaba_test_df$pricem2_res = propcaba_test_df$pricem2 - propcaba_test_df$pricem2_pred

mean((propcaba_test_df$pricem2-propcaba_test_df$pricem2_pred)^2) # 405.803


# modelo 1. modelo geo sin sacar tendencia ------------------------------------

krg_wt = krige(pricem2 ~ 1,
               propcaba_dev_sp,
               propcaba_test_sp,
               model = var_list$v_wt_exp,
               nmax = 20,
               nmin = 10)

length(krg_wt$var1.pred) # 9080
sum(!is.na(krg_wt$var1.pred)) # 9080

propcaba_test_df$pricem2_pred_wt = krg_wt$var1.pred
mean((propcaba_test_df$pricem2-propcaba_test_df$pricem2_pred_wt)^2) # 263.872


# modelo 2.a. modelo geo con la prediccion sin tendencia ----------------------

krg_t = krige(pricem2_wt ~ 1,
                 propcaba_dev_sp,
                 propcaba_test_sp,
                 model = var_list$v_t_exp,
                 nmax = 20,
                 nmin = 10)

length(krg_t$var1.pred) # 9080
sum(!is.na(krg_t$var1.pred)) # 9080

propcaba_test_df$pricem2_pred_t = krg_t$var1.pred

# modelo 2.a: uso predict para predecir con el modelo lineal "a" (sin covs)
propcaba_test_df$pricem2_pred_t_f = predict(modt, propcaba_test_df)

# performance 2.a
mean((propcaba_test_df$pricem2-propcaba_test_df$pricem2_pred_t_f)^2) # 264216

# modelo 2.b: uso predict para predecir con el modelo lineal "b" (con covs)
propcaba_test_df$pricem2_pred_t_f = predict(modt2, propcaba_test_df)

# performance
mean((propcaba_test_df$pricem2-propcaba_test_df$pricem2_pred_t_f)^2) # 246.913


# modelo 3: modelo geo con tendencia + modelo lineal con covariables ----------

krg_tcov = krige(pricem2_wt_cov ~ 1,
               propcaba_dev_sp,
               propcaba_test_sp,
               model = var_list$v_tcov_exp,
               nmax = 20,
               nmin = 10)

length(krg_tcov$var1.pred) # 9080
sum(!is.na(krg_tcov$var1.pred)) # 9080

propcaba_test_df$pricem2_pred_tcov = krg_tcov$var1.pred 

# uso predict para predecir con el modelo lineal
propcaba_test_df$pricem2_pred_tcov_f = predict(modtcov, propcaba_test_df)

# performance
mean((propcaba_test_df$pricem2-propcaba_test_df$pricem2_pred_tcov_f)^2) # 243.888


# comparo coeficientes de regresion (analisis de sesgo) -----------------------

# entreno modelo sin tratamiento geo
modbase = lm(pricem2 ~ X + Y + surface_total + rooms + surface_covered + 
               bathrooms,
             data = propcaba_mod_df)
modbase$coefficients

# armo dataframe con los coeficientes
modcof_comp = cbind(c(modbase$coefficients, NA),
                    modtcov$coefficients) %>% 
  as.data.frame() %>% 
  mutate(dif = V1 - V2)


# bonus: calculo el modelo sin tendencia directo con el kriging ---------------

# primero tengo que calcular el variograma
v_t2 = variogram(pricem2~X+Y, propcaba_dev_sp)
v_t_exp2 = fit.variogram(v_t2, vgm(5e5, "Exp", 0.1, 3e5))
plot(v_t2, v_t_exp2)

# calculo el kriging
krg_t2 = krige(pricem2 ~ X + Y,
               propcaba_dev_sp,
               propcaba_test_sp,
               model = v_t_exp2,
               nmax = 20,
               nmin = 10)

length(krg_wt$var1.pred) # 9080
sum(!is.na(krg_wt$var1.pred)) # 9080

propcaba_test_df$pricem2_pred2 = krg_t2$var1.pred
mean((propcaba_test_df$pricem2-propcaba_test_df$pricem2_pred2)^2) # 266.500
