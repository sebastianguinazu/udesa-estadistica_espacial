
library(readr)
library(dplyr)

# levanto la data ----------------------------------

df = read_csv('data/ar_properties.csv')
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

saveRDS(df_caba, 'data/df_caba.rds')
