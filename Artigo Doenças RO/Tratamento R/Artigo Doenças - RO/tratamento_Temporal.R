#Bibliotecas
library(readr)
library(tidyverse)
library(stringdist)
library(readxl)
library(writexl)
library(esquisse)

# Plotar 3 mais
df_doenças<- doenças %>% 
  filter(Diagnostico_N == 2 |
           Diagnostico_N == 22 |
           Diagnostico_N == 50)

table(df_doenças$Diagnostico_N)


df_doenças$Diagnostico_N <-  df_doenças$Diagnostico_N %>% recode("2" ="Abscesso",
                                                                 "22" ="Aspecto Repugnante (Post Mortem)",
                                                                 "50" ="Dermatose")
#### recode ####
df_doenças$Municipio_N<-df_doenças$Municipio_N %>% recode(
  "1" = "Porto Velho",
  "2" = "Guajará-Mirim",
  "3" = "Vilhena",
  "4" = "São Francisco do Guaporé",
  "5" = "Nova Mamoré",
  "6" = "Machadinho d'Oeste",
  "7" = "São Miguel do Guaporé",
  "8" = "Alta Floresta d'Oeste",
  "9" = "Ji-Paraná",
  "10" = "Candeias do Jamari",
  "11" = "Pimenta Bueno",
  "12" = "Pimenteiras do Oeste",
  "13" = "Chupinguaia",
  "14" = "Governador Jorge Teixeira",
  "15" = "Costa Marques",
  "16" = "Espigão d'Oeste",
  "17" = "Ariquemes",
  "18" = "Itapuã do Oeste",
  "19" = "Alto Alegre dos Parecis",
  "20" = "Cujubim",
  "21" = "Cacoal",
  "22" = "Seringueiras",
  "23" = "Campo Novo de Rondônia",
  "24" = "Buritis",
  "25" = "Vale do Anari",
  "26" = "Corumbiara",
  "27" = "Alvorada d'Oeste",
  "28" = "Jaru",
  "29" = "Cerejeiras",
  "30" = "Alto Paraíso",
  "31" = "Parecis",
  "32" = "Theobroma",
  "33" = "Ouro Preto do Oeste",
  "34" = "Cacaulândia",
  "35" = "Monte Negro",
  "36" = "Presidente Médici",
  "37" = "Rio Crespo",
  "38" = "Nova Brasilândia d'Oeste",
  "39" = "Rolim de Moura",
  "40" = "Colorado do Oeste",
  "41" = "Cabixi",
  "42" = "Santa Luzia d'Oeste",
  "43" = "Mirante da Serra",
  "44" = "Vale do Paraíso",
  "45" = "Castanheiras",
  "46" = "Novo Horizonte do Oeste",
  "47" = "Urupá",
  "48" = "Nova União",
  "49" = "Ministro Andreazza",
  "50" = "Primavera de Rondônia",
  "51" = "São Felipe d'Oeste",
  "52" = "Teixeirópolis")


###### Por período ###

esquisser(df_doenças)

#Junando mes ano
# Separando por Diagnóstioco Abscesso
AbscessoMesAno  <- df_doenças %>% 
  mutate (ANO_MES = apply(df_doenças[, 1:2], 1, paste, collapse = "/")) %>% 
  select(ANO_MES, UF, Municipio_N, Diagnostico_N, QUANTIDADE) %>% 
  filter(Diagnostico_N == "Abscesso")


#write.table(AbscessoMesAno, file='AbscessoMesAno.csv', sep=',', dec=',', row.names=FALSE)

# Separando por Diagnóstioco Adenite
CisticercoseCalcificadaMesAno  <- df_doenças %>% 
  mutate (ANO_MES = apply(df_doenças[, 1:2], 1, paste, collapse = "/")) %>% 
  select(ANO_MES, UF, Municipio_N, Diagnostico_N, QUANTIDADE) %>% 
  filter(Diagnostico_N == "Cisticercose Calcificada")


#write.table(AdeniteMesAno, file='CisticercoseCalcificadaMesAno.csv', sep=',', dec=',', row.names=FALSE)

