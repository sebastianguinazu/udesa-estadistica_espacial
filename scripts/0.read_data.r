
library(readr)
library(dplyr)

# levanto la data ----------------------------------

df = read_csv('raw/ar_properties.csv')
df %>% head()

# caba ventas

df_caba = df %>% filter(l2 == 'Capital Federal',
                        operation_type == 'Venta')

df_caba %>% nrow()
# 172997

df_caba %>% colnames()


# analizo missings ---------------------------------

df_caba %>% is.na() %>% colSums() / (df_caba %>% nrow()) * 100

# me quedo con las variables que tienen menos de 40% missing
keep_var = (df_caba %>% is.na() %>% colSums() / (df_caba %>% nrow()) * 100) < 60

df_caba = df_caba[keep_var]

# los que no tienen lat, long y price los saco
df_caba = df_caba  %>% 
  filter(!is.na(lat), !is.na(lon), !is.na(price))

# analizo distribucion y saco outliers -------------------

# desciptivo
df_caba$price %>% summary()

# Eliminamos outliers
q_75 = quantile(df_caba$price, probs = 0.75, na.rm = TRUE)
iqr = IQR(df_caba$price, na.rm = TRUE)

df_caba <- df_caba %>% 
  filter(price > 20000,
         price < (q_75 + 1.5 * iqr))

df_caba$price %>% summary()

ggplot() +
  geom_histogram(data = df_caba,
                 aes(x = price), bins = 50)

ggplot() +
  geom_boxplot(data = df_caba,
               aes(x = price))

qqnorm(df_caba$price)


# guardo base final
saveRDS(df_caba, 'data/df_caba.rds')
