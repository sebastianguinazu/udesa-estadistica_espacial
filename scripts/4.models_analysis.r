
# desafio: entrenar distintos modelos y comparar performance


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

# entreno un modelo para sacar la tendencia 
modt = lm(pricem2 ~ X + Y + pricem2_pred_t, data = propcaba_mod_df)
summary(modt)

# entreno un modelo para sacar la tendencia con covariables
modtcov = lm(pricem2 ~ X + Y + surface_total + rooms + surface_covered + 
               bathrooms + pricem2_pred_tcov,
             data = propcaba_mod_df)
summary(modtcov)


# benchmark: modelo lineal simple ---------------------------------------------

modtcov = lm(pricem2 ~ X + Y + surface_total + rooms + surface_covered + bathrooms,
             data = propcaba_test_df)
propcaba_test_df$pricem2_pred = predict(modtcov, propcaba_test_df)
propcaba_test_df$pricem2_res = propcaba_test_df$pricem2 - propcaba_test_df$pricem2_pred

mean((propcaba_test_df$pricem2-propcaba_test_df$pricem2_pred)^2) # 405.803


# modelo 1: modelo geo sin tendencia ------------------------------------------

krg_wt = krige(pricem2 ~ 1,
               propcaba_dev_sp,
               propcaba_test_sp,
               model = var_list$v_wt_exp,
               nmax = 20,
               nmin = 10)

length(krg_wt$var1.pred) # 9080
sum(!is.na(krg_wt$var1.pred)) # 9080

propcaba_test_df$pricem2_pred = krg_wt$var1.pred
mean((propcaba_test_df$pricem2-propcaba_test_df$pricem2_pred)^2) # 263.872


# modelo 2: modelo geo con tendencia + modelo lineal --------------------------

# krg_tcov = krige(pricem2_wt_cov ~ 1,
#                  propcaba_dev_sp,
#                  propcaba_test_sp,
#                  model = var_list$v_tcov_exp,
#                  nmax = 20,
#                  nmin = 10)
# 
# length(krg_tcov$var1.pred) # 9080
# sum(!is.na(krg_tcov$var1.pred)) # 9080
# 
# propcaba_test_df$pricem2_pred_tcov = krg_tcov$var1.pred 
# 
# # uso predict para predecir con el modelo lineal
# propcaba_test_df$pricem2_pred_tcov_f = predict(modtcov, propcaba_test_df)
# 
# # performance
# mean((propcaba_test_df$pricem2-propcaba_test_df$pricem2_pred_tcov_f)^2) 



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
