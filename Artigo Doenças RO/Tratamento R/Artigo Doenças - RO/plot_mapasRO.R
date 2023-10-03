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


#SEparação Abcesso por cidade dos DF

Abcesso <-df_doenças %>% 
  filter(Diagnostico_N == "Abscesso")

soma<- c()  
for (i in unique(Abcesso$Municipio_N)) {
  soma[i] <- sum(Abcesso$QUANTIDADE[Abcesso$Municipio_N == i])
}
Abcesso$soma <- soma[Abcesso$Municipio_N]
Abcesso<- Abcesso %>% select(Municipio_N,UF,Diagnostico_N,soma)





# Separaando Aspec Repugnante por cidade
Aspecto_Rep <- df_doenças %>% 
  filter(Diagnostico_N == "Aspecto Repugnante (Post Mortem)") 

soma<- c()  
for (i in unique(Aspecto_Rep$Municipio_N)) {
  soma[i] <- sum(Aspecto_Rep$QUANTIDADE[Aspecto_Rep$Municipio_N == i])
}
Aspecto_Rep$soma <- soma[Aspecto_Rep$Municipio_N]
Aspecto_Rep<- Aspecto_Rep %>% select(Municipio_N,UF,Diagnostico_N,soma)




# Separação de Dermatose por cidade
Dermatose <- df_doenças %>% 
  filter(Diagnostico_N == "Dermatose") 

soma<- c()  
for (i in unique(Dermatose$Municipio_N)) {
  soma[i] <- sum(Dermatose$QUANTIDADE[Dermatose$Municipio_N == i])
}
Dermatose$soma <- soma[Dermatose$Municipio_N]
Dermatose<- Dermatose %>% select(Municipio_N,UF,Diagnostico_N,soma)



RO <-read.csv("C:/Users/carlo/Documents/Projetos/Artigo Doenças RO/Tratamento R/Artigo Doenças - RO/Banco de dados/geoinfo-master/latitude-longitude-cidades.csv",sep = ";")


RO<-RO %>% 
  filter(uf == "RO" ) %>% 
  select(municipio,longitude,latitude) %>% 
  mutate ("Municipio_N" = gsub('`', "'", municipio)) %>% 
  select(Municipio_N,longitude,latitude)








RO_Abcesso<- full_join(RO,Abcesso,by = "Municipio_N")
RO_Abcesso<- distinct(RO_Abcesso, Municipio_N, .keep_all = TRUE)
RO_Abcesso$soma <- as.double( RO_Abcesso$soma)

RO_Aspecto_Rep<- full_join(RO,Aspecto_Rep,by = "Municipio_N")
RO_Aspecto_Rep<- distinct(RO_Aspecto_Rep, Municipio_N, .keep_all = TRUE)
RO_Aspecto_Rep$soma <- as.double( RO_Aspecto_Rep$soma)

RO_Dermatose<- full_join(RO,Dermatose,by = "Municipio_N")
RO_Dermatose<- distinct(RO_Dermatose, Municipio_N, .keep_all = TRUE)
RO_Dermatose$soma <- as.double( RO_Dermatose$soma)

## Mapa das 3 diagnósticos mais incidentes 
library(leaflet)
leaflet(RO_Abcesso) %>%
  addTiles() %>%
  addCircles(lng = ~longitude, 
             lat = ~latitude, 
             weight = 1,
             popup = RO_Abcesso$Municipio_N,
             radius = RO_Abcesso$soma, color = "red")

library(leaflet)
leaflet(RO_Aspecto_Rep) %>%
  addTiles() %>%
  addCircles(lng = ~longitude, 
             lat = ~latitude, 
             weight = 1,
             popup = RO_Aspecto_Rep$Municipio_N,
             radius = RO_Aspecto_Rep$soma, color = "red")

library(leaflet)
leaflet(RO_Dermatose) %>%
  addTiles() %>%
  addCircles(lng = ~longitude, 
             lat = ~latitude, 
             weight = 1,
             popup = RO_Dermatose$Municipio_N,
             radius = RO_Dermatose$soma,color = "red")

