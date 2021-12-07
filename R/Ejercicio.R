library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(readxl)
# Import data -------------------------------------------------------------####
aviones<-read_csv("data/aviones.csv")
# Supongamos que queremos hacer una matriz de -----------------------------#### 
aviones %>%
  pivot_wider(names_from = "modelo", values_from="asientos")
# Mas o menos, pero no queremos valores repetidos -------------------------####
aviones %>%
  select(fabricante, modelo, asientos) %>%
  pivot_wider(names_from = "modelo", values_from="asientos")
# Okay ahora hay que quitat los duplocados --------------------------------####
aviones %>%
  select(fabricante, modelo, asientos) %>%
  distinct() %>%
  pivot_wider(names_from = "modelo", values_from="asientos")
# Perfecto, pero ahora queremos que los NA sean 0 -------------------------####
como_matriz_aviones<-aviones %>%
  select(fabricante, modelo, asientos) %>%
  distinct() %>%
  pivot_wider(names_from = "modelo", 
              values_from="asientos", 
              values_fill=0)
# Ahora como la regresamos a su formato largo -----------------------------####
como_matriz_aviones %>%
  pivot_longer(!fabricante, names_to="modelos", values_to="asientos") %>%
  arrange(asientos)
# Como ordenarias de mayor a menor?
# Import data -------------------------------------------------------------####
encuesta<-read_delim("data/encuesta.tsv", delim = "\t")
# Ahora separar filas y columnas
encuesta %>%
  separate(ingreso, sep=" - ", into=c("ingreso_min", "ingreso_max")) 
# Remplazar los NAs por un caracter ---------------------------------------####
encuesta %>%
  separate(ingreso, sep=" - ", into=c("ingreso_min", "ingreso_max"))  %>%
  replace_na(list(ingreso_max = "No aplica")) 
# Arreglar la columna de ingreso minimo -----------------------------------####
encuesta %>%
  separate(ingreso, sep=" - ", into=c("ingreso_min", "ingreso_max"))  %>%
  replace_na(list(ingreso_max = "No aplica")) %>%
  mutate(ingreso_min=str_replace_all(ingreso_min, " o más", ""))
# Import data -------------------------------------------------------------####
vehiculos<-read_xlsx("data/vehiculos.xlsx")
# Separar una columna en multiples filas ----------------------------------####
vehiculos %>%
  separate_rows(transmision, sep=" ")
# Crear una columna nueva basados en otra ---------------------------------####
vehiculos %>%
  separate_rows(transmision, sep=" ") %>%
  filter(transmision == "Automática") %>%
  mutate(Decada =case_when(
    str_detect(anio, "199") ~ "90s",
    str_detect(anio, "198") ~ "80s",
    str_detect(anio, "200") ~ "2000s",
    str_detect(anio, "201") ~ "2010s",
  ))


