
# desafio: entrenar distintos modelos y comparar performance


# librerias -------------------------------------------------------------------

library(dlyr)



# levanto datasets ------------------------------------------------------------





# voy a usar este modelo como benchmark  de prediccion en el dataset test -----

modtcov = lm(pricem2 ~ X + Y + surface_total + rooms + surface_covered + bathrooms,
             data = propcaba_geo_train)
propcaba_geo_test$pricem2_pred = predict(modtcov, propcaba_geo_test)
propcaba_geo_test$pricem2_res = propcaba_geo_test$pricem2 - propcaba_geo_test$pricem2_pred

mean((propcaba_geo_test$pricem2-propcaba_geo_test$pricem2_pred)^2) # 400.518

# ggplot() + 
#   geom_sf(data = propcaba_geo_test  %>% 
#             st_as_sf(coords = c("X", "Y"), crs = 4326),
#           color = 'blue', alpha = 0.05,
#           size = propcaba_geo_test$pricem2_res)