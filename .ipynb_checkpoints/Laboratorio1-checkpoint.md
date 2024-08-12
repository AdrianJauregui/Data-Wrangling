## Problema 1

library(readxl)
library(dplyr)
library(readr)


leer_excel1 <- read_excel("01-2023.xlsx") %>% mutate(Fecha = "01-2023")
leer_excel2 <- read_excel("02-2023.xlsx") %>% mutate(Fecha = "02-2023")
leer_excel3 <- read_excel("03-2023.xlsx") %>% mutate(Fecha = "03-2023")
leer_excel4 <- read_excel("04-2023.xlsx") %>% mutate(Fecha = "04-2023")
leer_excel5 <- read_excel("05-2023.xlsx") %>% mutate(Fecha = "05-2023")
leer_excel6 <- read_excel("06-2023.xlsx") %>% mutate(Fecha = "06-2023")
leer_excel7 <- read_excel("07-2023.xlsx") %>% mutate(Fecha = "07-2023")
leer_excel8 <- read_excel("08-2023.xlsx") %>% mutate(Fecha = "08-2023")
leer_excel9 <- read_excel("09-2023.xlsx") %>% mutate(Fecha = "09-2023")
leer_excel10 <- read_excel("10-2023.xlsx") %>% mutate(Fecha = "10-2023")
leer_excel11 <- read_excel("11-2023.xlsx") %>% mutate(Fecha = "11-2023")


df_unificado <- bind_rows(
  leer_excel1,
  leer_excel2,
  leer_excel3,
  leer_excel4,
  leer_excel5,
  leer_excel6,
  leer_excel7,
  leer_excel8,
  leer_excel9,
  leer_excel10,
  leer_excel11
)
df_unificado <- df_unificado %>% select(-TIPO)
df_unificado <- df_unificado %>% select(-...11)
summary(df_unificado)
head(df_unificado)


tail(df_unificado)

write.csv(df_unificado, "df_unificado.csv", row.names = FALSE)


## Problema 2 

lista_vectores <- list(
  c(1, 2, 2, 3, 4),
  c(5, 5, 6, 7, 7, 7),
  c(8, 9, 9, 10)
)

calcular_moda <- function(vector) {
  tabla_freq <- table(vector) 
  moda <- as.numeric(names(tabla_freq[tabla_freq == max(tabla_freq)])) 
  return(moda)
}

modas <- lapply(lista_vectores, calcular_moda)
print(modas)


## Problema 3 
leer_txt <- read_delim("INE_PARQUE_VEHICULAR_080219.txt", delim = "|")
head(leer_txt)