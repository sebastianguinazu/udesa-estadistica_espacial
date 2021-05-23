
library(readr)
library(dplyr)

# levanto la data ----------------------------------

df = read_csv('raw/ar_properties.csv')
df %>% head()

# caba ventas
df_caba = df %>% filter(l2 == 'Capital Federal',
                        operation_type == 'Venta',
                        property_type %in% c('Casa', 'Departamento', 'PH'))

df_caba %>% nrow() # 172997

df_caba %>% colnames()


# analizo missings ---------------------------------------------------

df_caba %>% is.na() %>% colSums() / (df_caba %>% nrow()) * 100

# me quedo con las variables que tienen menos de 40% missing
keep_var = (df_caba %>% is.na() %>% colSums() / (df_caba %>% nrow()) * 100) < 60

df_caba = df_caba[keep_var]

# los que no tienen lat, long y price los saco
df_caba = df_caba  %>% 
  filter(!is.na(lat), !is.na(lon), !is.na(price), !is.na(rooms),
         !is.na(bathrooms), !is.na(surface_total), !is.na(surface_covered),
         !is.na(bedrooms))

df_caba %>% nrow() # 66081


# analizo distribucion y creo vars ----------------------------------

# desciptivo price
df_caba$price %>% summary()

# genero variable de precio por m2
df_caba = df_caba %>%
  mutate(pricem2 = price / surface_total) %>% 
  filter(!is.na(surface_total) & !is.na(rooms) 
         & !is.na(bathrooms) & !is.na(surface_covered))

# desciptivo pricem2
df_caba$pricem2 %>% summary()


# saco outliers ------------------------------------------------------

# eliminamos outliers
q_75 = quantile(df_caba$pricem2, probs = 0.75, na.rm = TRUE)
iqr = IQR(df_caba$pricem2, na.rm = TRUE)

df_caba <- df_caba %>% 
  filter(pricem2 > 1000,
         pricem2 < (q_75 + 1.5 * iqr))

df_caba$pricem2 %>% summary()

ggplot() +
  geom_histogram(data = df_caba,
                 aes(x = pricem2), bins = 50)

ggplot() +
  geom_boxplot(data = df_caba,
               aes(x = pricem2))

qqnorm(df_caba$pricem2)

# guardo base final
saveRDS(df_caba, 'data/df_caba.rds')

